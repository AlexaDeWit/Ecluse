-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Prototype for #1835: synchronous metric records leave the request path and a flush hands
them to the SDK. Two shapes: 'Sharded' accumulates per capability in cells that only ever hold
evaluated values, and 'Queued' sends each record to one owner through an STM queue.
-}
module Ecluse.Runtime.Telemetry.Batched (
    Batcher,
    BatchShape (..),
    newBatcher,
    batchedMeterProvider,
    flushBatcher,
    runFlusher,
    batchFlushHook,
    renderExport,
) where

import Control.Concurrent.MVar qualified as MVar
import Control.Concurrent.STM (TQueue, flushTQueue, newTQueueIO, writeTQueue)
import Data.HashMap.Strict qualified as HashMap
import Data.Vector qualified as Vector
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getNumCapabilities, threadCapability)
import GHC.Exts (casMutVar#, readMutVar#)
import GHC.IO (IO (IO))
import GHC.IORef (IORef (IORef), atomicSwapIORef)
import GHC.STRef (STRef (STRef))
import OpenTelemetry.Attributes (Attributes)
import OpenTelemetry.Exporter.Metric (
    GaugeDataPoint (..),
    HistogramDataPoint (..),
    MetricExport (..),
    NumberValue (..),
    ResourceMetricsExport (..),
    ScopeMetricsExport (..),
    SumDataPoint (..),
 )
import OpenTelemetry.Metric.Core (
    Counter (..),
    Gauge (..),
    Histogram (..),
    Meter (..),
    MeterProvider (..),
    UpDownCounter (..),
    defaultAdvisoryParameters,
 )
import UnliftIO.Concurrent (myThreadId, threadDelay)

-- | Which prototype shape records take.
data BatchShape = Sharded | Queued
    deriving stock (Eq, Show)

-- | The registered flush actions, the owner's queue, and the lock that keeps two flushes apart.
data Batcher = Batcher
    { bShape :: BatchShape
    , bFlushes :: IORef [IO ()]
    , bQueue :: TQueue (IO ())
    , bLock :: MVar.MVar ()
    }

newBatcher :: BatchShape -> IO Batcher
newBatcher shape = Batcher shape <$> newIORef [] <*> newTQueueIO <*> MVar.newMVar ()

-- In the queued shape a record is the SDK call itself, run later by whoever flushes.
enqueue :: Batcher -> IO () -> IO ()
enqueue batcher action = atomically (writeTQueue (bQueue batcher) action)

-- Instruments register at boot only, so this cell sees no request-path traffic.
registerFlush :: Batcher -> IO () -> IO ()
registerFlush batcher action = atomicModifyIORef' (bFlushes batcher) (\actions -> (action : actions, ()))

-- | Hand every accumulated record to the SDK. Runs off the request path.
flushBatcher :: Batcher -> IO ()
flushBatcher batcher = MVar.withMVar (bLock batcher) $ \() -> case bShape batcher of
    Sharded -> readIORef (bFlushes batcher) >>= sequence_ . reverse
    Queued -> atomically (flushTQueue (bQueue batcher)) >>= sequence_

-- | Flush on an interval, forever.
runFlusher :: Int -> Batcher -> IO ()
runFlusher micros batcher = forever (threadDelay micros >> flushBatcher batcher)

{- | Register the flush as an observable callback on an SDK meter, so every collection (a scrape
or the periodic reader) first drains the accumulators. The gauge observes nothing and exports nothing.
-}
batchFlushHook :: Meter -> Batcher -> IO ()
batchFlushHook meter batcher =
    void (meterCreateObservableGaugeInt64 meter "ecluse.internal.batch_flush" Nothing Nothing defaultAdvisoryParameters [const (flushBatcher batcher)])

{- | Wrap a provider so its synchronous Int64 counters, up-down counters, gauges and histograms
batch. An optional shadow provider receives every record directly, for an exactness check.
-}
batchedMeterProvider :: Batcher -> Maybe MeterProvider -> MeterProvider -> MeterProvider
batchedMeterProvider batcher shadow inner =
    inner
        { meterProviderGetMeter = \scope -> do
            meter <- meterProviderGetMeter inner scope
            shadowMeter <- traverse (`meterProviderGetMeter` scope) shadow
            pure (batchedMeter batcher shadowMeter meter)
        }

batchedMeter :: Batcher -> Maybe Meter -> Meter -> Meter
batchedMeter batcher shadow meter =
    meter
        { meterCreateCounterInt64 = \n u d a -> do
            real <- meterCreateCounterInt64 meter n u d a
            copy <- traverse (\m -> meterCreateCounterInt64 m n u d a) shadow
            enabled <- counterEnabled real
            if enabled then batchedSum batcher (counterAdd real) (counterAdd <$> copy) (\add -> real{counterAdd = add}) else pure real
        , meterCreateUpDownCounterInt64 = \n u d a -> do
            real <- meterCreateUpDownCounterInt64 meter n u d a
            copy <- traverse (\m -> meterCreateUpDownCounterInt64 m n u d a) shadow
            enabled <- upDownCounterEnabled real
            if enabled then batchedSum batcher (upDownCounterAdd real) (upDownCounterAdd <$> copy) (\add -> real{upDownCounterAdd = add}) else pure real
        , meterCreateHistogram = \n u d a -> do
            real <- meterCreateHistogram meter n u d a
            copy <- traverse (\m -> meterCreateHistogram m n u d a) shadow
            enabled <- histogramEnabled real
            if enabled then batchedHistogram batcher real (histogramRecord <$> copy) else pure real
        , meterCreateGaugeInt64 = \n u d a -> do
            real <- meterCreateGaugeInt64 meter n u d a
            copy <- traverse (\m -> meterCreateGaugeInt64 m n u d a) shadow
            enabled <- gaugeEnabled real
            if enabled then batchedGauge batcher real (gaugeRecord <$> copy) else pure real
        }

-- One cell per capability. A thread may move between capabilities, so a cell still updates by CAS.
newtype Shards a = Shards (Vector.Vector (IORef a))

newShards :: a -> IO (Shards a)
newShards blank = do
    count <- getNumCapabilities
    Shards <$> Vector.replicateM (max 1 count) (newIORef blank)

localShard :: Shards a -> IO (IORef a)
localShard (Shards cells) = do
    (capability, _) <- threadCapability =<< myThreadId
    pure (Vector.unsafeIndex cells (capability `mod` Vector.length cells))

drainShards :: Shards a -> a -> IO [a]
drainShards (Shards cells) blank = traverse (`atomicSwapIORef` blank) (Vector.toList cells)

-- Install the evaluated result, never a thunk another thread could find under evaluation. The
-- value read is handed to the CAS unchanged, so pointer equality holds when nothing intervened.
casModify :: IORef a -> (a -> a) -> IO ()
casModify (IORef (STRef var)) change = IO loop
  where
    loop s = case readMutVar# var s of
        (# s1, old #) ->
            let !new = change old
             in case casMutVar# var old new s1 of
                    (# s2, 0#, _ #) -> (# s2, () #)
                    (# s2, _, _ #) -> loop s2

batchedSum :: Batcher -> (Int64 -> Attributes -> IO ()) -> Maybe (Int64 -> Attributes -> IO ()) -> ((Int64 -> Attributes -> IO ()) -> instrument) -> IO instrument
batchedSum batcher add shadow wrap
    | bShape batcher == Queued = pure . wrap $ \delta attrs -> do
        traverse_ (\record -> record delta attrs) shadow
        enqueue batcher (add delta attrs)
    | otherwise = do
    shards <- newShards HashMap.empty
    registerFlush batcher $ do
        drained <- drainShards shards HashMap.empty
        let totals = foldl' (HashMap.unionWith (+)) HashMap.empty drained
        HashMap.foldrWithKey (\attrs delta rest -> add delta attrs >> rest) pass totals
    pure . wrap $ \delta attrs -> do
        traverse_ (\record -> record delta attrs) shadow
        cell <- localShard shards
        casModify cell (HashMap.insertWith (+) attrs delta)

-- Samples newest first. The fields are strict, so a cell holds no thunk.
data Samples = NoSamples | Sample !Double !Samples

batchedHistogram :: Batcher -> Histogram -> Maybe (Double -> Attributes -> IO ()) -> IO Histogram
batchedHistogram batcher real shadow
    | bShape batcher == Queued = pure real{histogramRecord = \value attrs -> traverse_ (\record -> record value attrs) shadow >> enqueue batcher (histogramRecord real value attrs)}
    | otherwise = do
    shards <- newShards HashMap.empty
    registerFlush batcher $ do
        drained <- drainShards shards HashMap.empty
        for_ drained $ HashMap.foldrWithKey (\attrs samples rest -> replay attrs (oldestFirst samples []) >> rest) pass
    pure
        real
            { histogramRecord = \value attrs -> do
                traverse_ (\record -> record value attrs) shadow
                cell <- localShard shards
                casModify cell (HashMap.insertWith prepend attrs (Sample value NoSamples))
            }
  where
    prepend new old = case new of
        Sample value _ -> Sample value old
        NoSamples -> old
    oldestFirst NoSamples acc = acc
    oldestFirst (Sample value rest) acc = oldestFirst rest (value : acc)
    replay attrs = traverse_ (\value -> histogramRecord real value attrs)

-- The last value and when it was recorded, so a flush keeps the newest across capabilities.
data Latest = Latest !Word64 !Int64

batchedGauge :: Batcher -> Gauge Int64 -> Maybe (Int64 -> Attributes -> IO ()) -> IO (Gauge Int64)
batchedGauge batcher real shadow
    | bShape batcher == Queued = pure real{gaugeRecord = \value attrs -> traverse_ (\record -> record value attrs) shadow >> enqueue batcher (gaugeRecord real value attrs)}
    | otherwise = do
    shards <- newShards HashMap.empty
    registerFlush batcher $ do
        drained <- drainShards shards HashMap.empty
        let latest = foldl' (HashMap.unionWith newer) HashMap.empty drained
        HashMap.foldrWithKey (\attrs (Latest _ value) rest -> gaugeRecord real value attrs >> rest) pass latest
    pure
        real
            { gaugeRecord = \value attrs -> do
                traverse_ (\record -> record value attrs) shadow
                stamp <- getMonotonicTimeNSec
                cell <- localShard shards
                casModify cell (HashMap.insertWith newer attrs (Latest stamp value))
            }
  where
    newer a@(Latest stampA _) b@(Latest stampB _) = if stampA >= stampB then a else b

-- | One line per data point: name, attributes and values, without timestamps or exemplars.
renderExport :: [ResourceMetricsExport] -> [Text]
renderExport resources =
    sort
        [ line
        | resource <- resources
        , scope <- Vector.toList (resourceMetricsScopes resource)
        , metric <- Vector.toList (scopeMetricsExports scope)
        , line <- metricLines metric
        ]
  where
    metricLines = \case
        MetricExportSum{mesName = name, mesSumPoints = points} ->
            [name <> " " <> show (sumDataPointAttributes p) <> " sum=" <> number (sumDataPointValue p) | p <- Vector.toList points]
        MetricExportHistogram{mehName = name, mehPoints = points} ->
            [ name
                <> " "
                <> show (histogramDataPointAttributes p)
                <> " count="
                <> show (histogramDataPointCount p)
                <> " buckets="
                <> show (Vector.toList (histogramDataPointBucketCounts p))
                <> " min="
                <> show (histogramDataPointMin p)
                <> " max="
                <> show (histogramDataPointMax p)
                <> " sum~"
                <> show (roundTo (histogramDataPointSum p))
            | p <- Vector.toList points
            ]
        MetricExportGauge{megName = name, megGaugePoints = points} ->
            [name <> " " <> show (gaugeDataPointAttributes p) <> " gauge=" <> number (gaugeDataPointValue p) | p <- Vector.toList points]
        MetricExportExponentialHistogram{meehName = name} -> [name <> " exponential histogram (not compared)"]
    number = \case
        IntNumber i -> show i
        DoubleNumber d -> show d
    roundTo :: Double -> Double
    roundTo x = fromIntegral (round (x * 1e6) :: Integer) / 1e6
