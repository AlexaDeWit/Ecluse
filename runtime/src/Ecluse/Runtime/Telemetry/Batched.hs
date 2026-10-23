-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Synchronous metric records batched per capability, so a request thread never waits on the
SDK's single storage cell. A flush gives the SDK one summed delta per counter series, the newest
value per gauge series, and every histogram sample in recording order, each under the span it was
recorded in, so exemplars keep their trace. Flushes run every second, before every collection and
before shutdown, so an export sees every record made before it. Observable and Double instruments
pass straight through, and the SDK stays the aggregator and exporter.
-}
module Ecluse.Runtime.Telemetry.Batched (
    batchMeterProvider,
) where

import Data.Atomics (atomicModifyIORefCAS_)
import Data.HashMap.Strict qualified as HashMap
import GHC.Clock (getMonotonicTimeNSec)
import OpenTelemetry.Attributes (Attributes)
import OpenTelemetry.Context (insertSpan, lookupSpan, removeSpan)
import OpenTelemetry.Context.ThreadLocal (adjustContext, getContext)
import OpenTelemetry.Metric.Core (
    Counter (counterAdd, counterEnabled),
    Gauge (gaugeEnabled, gaugeRecord),
    Histogram (histogramEnabled, histogramRecord),
    Meter (meterCreateCounterInt64, meterCreateGaugeInt64, meterCreateHistogram, meterCreateObservableGaugeInt64, meterCreateUpDownCounterInt64),
    MeterProvider (meterProviderGetMeter, meterProviderShutdown),
    UpDownCounter (upDownCounterAdd, upDownCounterEnabled),
    defaultAdvisoryParameters,
    getMeter,
 )
import OpenTelemetry.Trace.Core (SpanContext, getSpanContext, isValid, wrapSpanContext)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (finally, uninterruptibleMask_)
import UnliftIO.MVar (withMVar)

import Ecluse.Core.Telemetry.Span (ecluseScope)
import Ecluse.Runtime.Telemetry.Batched.Cells (drainCells, modifyOwnCell, newCells)

{- | Wrap an SDK provider so its synchronous Int64 counters, up-down counters and gauges, and its
histograms, record per capability. Also returns the flusher's step: wait a second, then flush.
-}
batchMeterProvider :: MeterProvider -> IO (MeterProvider, IO ())
batchMeterProvider sdk = do
    batch <- Batch <$> newIORef [] <*> newMVar ()
    hookMeter <- getMeter sdk ecluseScope
    -- The SDK runs observable callbacks before every collection. This one observes nothing.
    void (meterCreateObservableGaugeInt64 hookMeter "ecluse.metric_flush" Nothing Nothing defaultAdvisoryParameters [const (flushBatch batch)])
    let wrapped =
            sdk
                { meterProviderGetMeter = fmap (batchedMeter batch) . meterProviderGetMeter sdk
                , meterProviderShutdown = \timeout -> flushBatch batch >> meterProviderShutdown sdk timeout
                }
    pure (wrapped, threadDelay flushIntervalMicros >> flushBatch batch)

flushIntervalMicros :: Int
flushIntervalMicros = 1_000_000

-- One flush per batched instrument, and the lock that keeps a gauge's flushed values in order.
data Batch = Batch
    { batchFlushes :: IORef [IO ()]
    , batchLock :: MVar ()
    }

{- Waiting for the lock may be interrupted, but the drain and replay may not, so a cancelled flush
loses no record. The flushing thread's own context is restored after the replay sets spans on it. -}
flushBatch :: Batch -> IO ()
flushBatch batch = withMVar (batchLock batch) $ \() -> uninterruptibleMask_ $ do
    flushes <- readIORef (batchFlushes batch)
    original <- getContext
    sequence_ (reverse flushes) `finally` adjustContext (const original)

-- Instruments are created at boot, so the list of flushes stays bounded.
registerFlush :: Batch -> IO () -> IO ()
registerFlush batch flush = atomicModifyIORefCAS_ (batchFlushes batch) (flush :)

batchedMeter :: Batch -> Meter -> Meter
batchedMeter batch meter =
    meter
        { meterCreateCounterInt64 = \name unit description advice -> do
            inner <- meterCreateCounterInt64 meter name unit description advice
            whenEnabled (counterEnabled inner) inner ((\add -> inner{counterAdd = add}) <$> batchedSum batch (counterAdd inner))
        , meterCreateUpDownCounterInt64 = \name unit description advice -> do
            inner <- meterCreateUpDownCounterInt64 meter name unit description advice
            whenEnabled (upDownCounterEnabled inner) inner ((\add -> inner{upDownCounterAdd = add}) <$> batchedSum batch (upDownCounterAdd inner))
        , meterCreateGaugeInt64 = \name unit description advice -> do
            inner <- meterCreateGaugeInt64 meter name unit description advice
            whenEnabled (gaugeEnabled inner) inner ((\record -> inner{gaugeRecord = record}) <$> batchedGauge batch (gaugeRecord inner))
        , meterCreateHistogram = \name unit description advice -> do
            inner <- meterCreateHistogram meter name unit description advice
            whenEnabled (histogramEnabled inner) inner ((\record -> inner{histogramRecord = record}) <$> batchedHistogram batch (histogramRecord inner))
        }

-- A disabled instrument, such as one a view drops, stays as the SDK built it.
whenEnabled :: IO Bool -> a -> IO a -> IO a
whenEnabled enabled inner batched = enabled >>= bool (pure inner) batched

-- A record's monotonic time and the span it was made in. The newest becomes the series' exemplar.
data Traced = Traced Word64 SpanContext

-- A sum series' net delta since the last flush.
data Pending = Pending Int64 (Maybe Traced)

-- A gauge series' newest value, by monotonic time.
data Reading = Reading Word64 Int64 (Maybe Traced)

-- A histogram sample with its monotonic time, so a flush replays samples in recording order.
data Sample = Sample Word64 Double (Maybe SpanContext)

-- A series recorded since the last flush is flushed even when its net delta is zero, as the SDK would show it.
batchedSum :: Batch -> (Int64 -> Attributes -> IO ()) -> IO (Int64 -> Attributes -> IO ())
batchedSum batch add = do
    cells <- newCells HashMap.empty
    registerFlush batch $ do
        totals <- foldl' (HashMap.unionWith addPending) HashMap.empty <$> drainCells cells
        for_ (HashMap.toList totals) $ \(attrs, Pending delta traced) -> underSpan (tracedSpan <$> traced) (add delta attrs)
    pure $ \delta attrs -> do
        recorded <- recordedSpan
        at <- getMonotonicTimeNSec
        modifyOwnCell cells (HashMap.insertWith addPending attrs (Pending delta (stamp at recorded)))
  where
    addPending (Pending a tracedA) (Pending b tracedB) = Pending (a + b) (newest tracedA tracedB)

batchedGauge :: Batch -> (Int64 -> Attributes -> IO ()) -> IO (Int64 -> Attributes -> IO ())
batchedGauge batch record = do
    cells <- newCells HashMap.empty
    registerFlush batch $ do
        latest <- foldl' (HashMap.unionWith newerReading) HashMap.empty <$> drainCells cells
        for_ (HashMap.toList latest) $ \(attrs, Reading _ value traced) -> underSpan (tracedSpan <$> traced) (record value attrs)
    pure $ \value attrs -> do
        recorded <- recordedSpan
        at <- getMonotonicTimeNSec
        modifyOwnCell cells (HashMap.insertWith newerReading attrs (Reading at value (stamp at recorded)))
  where
    newerReading (Reading atA valueA tracedA) (Reading atB valueB tracedB)
        | atA >= atB = Reading atA valueA (newest tracedA tracedB)
        | otherwise = Reading atB valueB (newest tracedA tracedB)

-- The SDK takes no pre-aggregated histogram, so samples replay one by one, oldest first.
batchedHistogram :: Batch -> (Double -> Attributes -> IO ()) -> IO (Double -> Attributes -> IO ())
batchedHistogram batch record = do
    cells <- newCells HashMap.empty
    registerFlush batch $ do
        series <- foldl' (HashMap.unionWith (<>)) HashMap.empty <$> drainCells cells
        for_ (HashMap.toList series) $ \(attrs, samples) ->
            for_ (sortWith sampleAt samples) $ \(Sample _ value recorded) -> underSpan recorded (record value attrs)
    pure $ \value attrs -> do
        recorded <- recordedSpan
        at <- getMonotonicTimeNSec
        let !sample = Sample at value recorded
        modifyOwnCell cells (HashMap.insertWith (const (sample :)) attrs [sample])
  where
    sampleAt (Sample at _ _) = at

-- The calling thread's valid span, read as the SDK's trace-based exemplar filter reads it.
recordedSpan :: IO (Maybe SpanContext)
recordedSpan =
    getContext >>= \context -> case lookupSpan context of
        Nothing -> pure Nothing
        Just active -> do
            recorded <- getSpanContext active
            pure $! if isValid recorded then Just recorded else Nothing

-- Built strictly, so a cell never holds an unevaluated 'Traced'.
stamp :: Word64 -> Maybe SpanContext -> Maybe Traced
stamp at = \case
    Nothing -> Nothing
    Just recorded -> Just $! Traced at recorded

-- Returns one of its arguments, never a new thunk.
newest :: Maybe Traced -> Maybe Traced -> Maybe Traced
newest a b = case (a, b) of
    (Just (Traced atA _), Just (Traced atB _)) -> if atA >= atB then a else b
    _ -> a <|> b

tracedSpan :: Traced -> SpanContext
tracedSpan (Traced _ recorded) = recorded

-- Make one SDK call in the recorded span, so the SDK takes its exemplar from that span's trace.
underSpan :: Maybe SpanContext -> IO () -> IO ()
underSpan recorded call = adjustContext (maybe removeSpan (insertSpan . wrapSpanContext) recorded) >> call
