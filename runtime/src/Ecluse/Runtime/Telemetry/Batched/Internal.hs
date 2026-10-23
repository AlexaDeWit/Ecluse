-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The batch, the wrapped meters and the flush behind "Ecluse.Runtime.Telemetry.Batched", which
documents the batching and re-exports the curated surface. Importing this module opts out of that
stability promise, the convention @text@ and @bytestring@ use, so production code imports the
public one.
-}
module Ecluse.Runtime.Telemetry.Batched.Internal (
    -- * The production wrapper
    batchMeterProvider,

    -- * The batch
    Batch (..),
    newBatch,
    batchedProvider,
    flushBatch,
) where

import Data.Atomics (atomicModifyIORefCAS_)
import Data.HashMap.Strict qualified as HashMap
import GHC.Clock (getMonotonicTimeNSec)
import OpenTelemetry.Attributes (Attributes)
import OpenTelemetry.Context (insertSpan, removeSpan)
import OpenTelemetry.Context.ThreadLocal (adjustContext, getContext)
import OpenTelemetry.Internal.Common.Types (InstrumentationLibrary)
import OpenTelemetry.Metric.Core (
    AdvisoryParameters (advisoryAttributeKeys, advisoryExplicitBucketBoundaries, advisoryHistogramAggregation),
    Counter (counterAdd, counterEnabled),
    Gauge (gaugeEnabled, gaugeRecord),
    Histogram (histogramEnabled, histogramRecord),
    HistogramAggregation,
    Meter (meterCreateCounterInt64, meterCreateGaugeInt64, meterCreateHistogram, meterCreateObservableGaugeInt64, meterCreateUpDownCounterInt64, meterInstrumentationScope),
    MeterProvider (meterProviderGetMeter),
    UpDownCounter (upDownCounterAdd, upDownCounterEnabled),
    defaultAdvisoryParameters,
    getMeter,
 )
import OpenTelemetry.Trace.Core (SpanContext, getActiveSpanContext, isValid, wrapSpanContext)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (catchAny, finally, tryAny, uninterruptibleMask_)
import UnliftIO.MVar (modifyMVar, withMVar)

import Ecluse.Core.Telemetry.Span (ecluseScope)
import Ecluse.Runtime.Telemetry.Batched.Cells (drainCells, modifyOwnCell, newCells)

{- | Wrap an SDK provider so its synchronous Int64 counters, up-down counters and gauges, and its
histograms, record per capability. Also returns the flusher's step: wait a second, then flush.
-}
batchMeterProvider :: (SomeException -> IO ()) -> MeterProvider -> IO (MeterProvider, IO ())
batchMeterProvider report sdk = do
    batch <- newBatch getMonotonicTimeNSec report
    wrapped <- batchedProvider batch sdk
    pure (wrapped, threadDelay flushIntervalMicros >> flushBatch batch)

flushIntervalMicros :: Int
flushIntervalMicros = 1_000_000

-- | What the wrapped meters share: their instruments, the flushes, and the lock that orders flushes.
data Batch = Batch
    { batchInstruments :: MVar Instruments
    , batchFlushes :: IORef [IO ()]
    , batchLock :: MVar ()
    , batchClock :: IO Word64
    -- ^ Monotonic nanoseconds, which order a series' records across capabilities.
    , batchReport :: SomeException -> IO ()
    -- ^ Takes the fault of a series whose replay into the SDK threw. That series is dropped.
    }

-- | A batch over a monotonic clock in nanoseconds, reporting each series it drops.
newBatch :: IO Word64 -> (SomeException -> IO ()) -> IO Batch
newBatch clock report = do
    instruments <- newMVar (Instruments HashMap.empty HashMap.empty HashMap.empty HashMap.empty)
    flushes <- newIORef []
    lock <- newMVar ()
    pure Batch{batchInstruments = instruments, batchFlushes = flushes, batchLock = lock, batchClock = clock, batchReport = report}

{- | Wrap a provider's meters. Its SDK must run observable callbacks before every collection,
because one of them flushes, so every export, the reader's last at shutdown included, is current.
-}
batchedProvider :: Batch -> MeterProvider -> IO MeterProvider
batchedProvider batch sdk = do
    hookMeter <- getMeter sdk ecluseScope
    void (meterCreateObservableGaugeInt64 hookMeter "ecluse.metric_flush" Nothing Nothing defaultAdvisoryParameters [const (flushBatch batch `catchAny` batchReport batch)])
    pure sdk{meterProviderGetMeter = fmap (batchedMeter batch) . meterProviderGetMeter sdk}

{- | Replay everything recorded so far into the SDK. Waiting for the lock may be interrupted, but
the drain and replay may not, so a cancelled flush loses no record.
-}
flushBatch :: Batch -> IO ()
flushBatch batch = withMVar (batchLock batch) $ \() -> uninterruptibleMask_ $ do
    flushes <- readIORef (batchFlushes batch)
    original <- getContext
    sequence_ (reverse flushes) `finally` adjustContext (const original)

registerFlush :: Batch -> IO () -> IO ()
registerFlush batch flush = atomicModifyIORefCAS_ (batchFlushes batch) (flush :)

-- A series whose replay throws is reported and dropped, and the other series still replay.
replaySeries :: Batch -> IO () -> IO ()
replaySeries batch replay = tryAny replay >>= either (batchReport batch) pure

-- What the SDK keys an instrument by, from the arguments that create it.
type InstrumentKey = (InstrumentationLibrary, Text, Maybe Text, Maybe Text, (Maybe [Double], Maybe [Text], Maybe HistogramAggregation))

-- Every batched instrument by key, so creating one again returns it rather than adding a flush.
data Instruments = Instruments
    { knownCounters :: HashMap.HashMap InstrumentKey (Counter Int64)
    , knownUpDownCounters :: HashMap.HashMap InstrumentKey (UpDownCounter Int64)
    , knownGauges :: HashMap.HashMap InstrumentKey (Gauge Int64)
    , knownHistograms :: HashMap.HashMap InstrumentKey Histogram
    }

batchedMeter :: Batch -> Meter -> Meter
batchedMeter batch meter =
    meter
        { meterCreateCounterInt64 = \name unit description advice ->
            createOnce batch knownCounters (\known i -> i{knownCounters = known}) (keyOf name unit description advice) $ do
                inner <- meterCreateCounterInt64 meter name unit description advice
                whenEnabled (counterEnabled inner) inner ((\add -> inner{counterAdd = add}) <$> batchedSum batch (counterAdd inner))
        , meterCreateUpDownCounterInt64 = \name unit description advice ->
            createOnce batch knownUpDownCounters (\known i -> i{knownUpDownCounters = known}) (keyOf name unit description advice) $ do
                inner <- meterCreateUpDownCounterInt64 meter name unit description advice
                whenEnabled (upDownCounterEnabled inner) inner ((\add -> inner{upDownCounterAdd = add}) <$> batchedSum batch (upDownCounterAdd inner))
        , meterCreateGaugeInt64 = \name unit description advice ->
            createOnce batch knownGauges (\known i -> i{knownGauges = known}) (keyOf name unit description advice) $ do
                inner <- meterCreateGaugeInt64 meter name unit description advice
                whenEnabled (gaugeEnabled inner) inner ((\record -> inner{gaugeRecord = record}) <$> batchedGauge batch (gaugeRecord inner))
        , meterCreateHistogram = \name unit description advice ->
            createOnce batch knownHistograms (\known i -> i{knownHistograms = known}) (keyOf name unit description advice) $ do
                inner <- meterCreateHistogram meter name unit description advice
                whenEnabled (histogramEnabled inner) inner ((\record -> inner{histogramRecord = record}) <$> batchedHistogram batch (histogramRecord inner))
        }
  where
    keyOf name unit description advice =
        ( meterInstrumentationScope meter
        , name
        , unit
        , description
        , (advisoryExplicitBucketBoundaries advice, advisoryAttributeKeys advice, advisoryHistogramAggregation advice)
        )

createOnce ::
    Batch ->
    (Instruments -> HashMap.HashMap InstrumentKey a) ->
    (HashMap.HashMap InstrumentKey a -> Instruments -> Instruments) ->
    InstrumentKey ->
    IO a ->
    IO a
createOnce batch known remember key create = modifyMVar (batchInstruments batch) $ \instruments ->
    case HashMap.lookup key (known instruments) of
        Just existing -> pure (instruments, existing)
        Nothing -> create <&> \made -> (remember (HashMap.insert key made (known instruments)) instruments, made)

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
        for_ (HashMap.toList totals) $ \(attrs, Pending delta traced) ->
            replaySeries batch (underSpan (tracedSpan <$> traced) (add delta attrs))
    pure $ \delta attrs -> do
        recorded <- recordedSpan
        at <- batchClock batch
        modifyOwnCell cells (HashMap.insertWith addPending attrs (Pending delta (stamp at recorded)))
  where
    addPending (Pending a tracedA) (Pending b tracedB) = Pending (a + b) (newest tracedA tracedB)

-- The newest reading flushed per series, so one drained late from another capability's cell is skipped.
batchedGauge :: Batch -> (Int64 -> Attributes -> IO ()) -> IO (Int64 -> Attributes -> IO ())
batchedGauge batch record = do
    cells <- newCells HashMap.empty
    flushedAt <- newIORef HashMap.empty
    registerFlush batch $ do
        latest <- foldl' (HashMap.unionWith newerReading) HashMap.empty <$> drainCells cells
        before <- readIORef flushedAt
        for_ (HashMap.toList latest) $ \(attrs, Reading at value traced) ->
            unless (maybe False (at <) (HashMap.lookup attrs before)) $
                replaySeries batch (underSpan (tracedSpan <$> traced) (record value attrs))
        writeIORef flushedAt (HashMap.unionWith max (readingAt <$> latest) before)
    pure $ \value attrs -> do
        recorded <- recordedSpan
        at <- batchClock batch
        modifyOwnCell cells (HashMap.insertWith newerReading attrs (Reading at value (stamp at recorded)))
  where
    newerReading (Reading atA valueA tracedA) (Reading atB valueB tracedB)
        | atA >= atB = Reading atA valueA (newest tracedA tracedB)
        | otherwise = Reading atB valueB (newest tracedA tracedB)
    readingAt (Reading at _ _) = at

-- The SDK takes no pre-aggregated histogram, so samples replay one by one, oldest first.
batchedHistogram :: Batch -> (Double -> Attributes -> IO ()) -> IO (Double -> Attributes -> IO ())
batchedHistogram batch record = do
    cells <- newCells HashMap.empty
    registerFlush batch $ do
        series <- foldl' (HashMap.unionWith (<>)) HashMap.empty <$> drainCells cells
        for_ (HashMap.toList series) $ \(attrs, samples) ->
            replaySeries batch $
                for_ (sortWith sampleAt samples) $
                    \(Sample _ value recorded) -> underSpan recorded (record value attrs)
    pure $ \value attrs -> do
        recorded <- recordedSpan
        at <- batchClock batch
        let !sample = Sample at value recorded
        modifyOwnCell cells (HashMap.insertWith (const (sample :)) attrs [sample])
  where
    sampleAt (Sample at _ _) = at

-- The calling thread's valid span, read as the SDK's trace-based exemplar filter reads it.
recordedSpan :: IO (Maybe SpanContext)
recordedSpan = getActiveSpanContext <&> (>>= \active -> if isValid active then Just active else Nothing)

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
