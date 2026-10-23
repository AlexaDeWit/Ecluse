-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Runtime.Telemetry.BatchedSpec (spec) where

import Control.Concurrent (forkOn)
import Data.List (lookup)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Vector qualified as V
import GHC.Clock (getMonotonicTimeNSec)
import Test.Hspec
import UnliftIO (bracket, forConcurrently_, throwIO, tryAny)

import OpenTelemetry.Attributes (Attributes, addAttribute, defaultAttributeLimits, emptyAttributes)
import OpenTelemetry.Exporter.InMemory.Metric (inMemoryMetricExporter)
import OpenTelemetry.Exporter.InMemory.Span (inMemoryListExporter)
import OpenTelemetry.Exporter.Metric (
    GaugeDataPoint (..),
    HistogramDataPoint (..),
    MetricExemplar (..),
    MetricExport (..),
    NumberValue (DoubleNumber, IntNumber),
    ResourceMetricsExport,
    SumDataPoint (..),
 )
import OpenTelemetry.Instrumentation.HttpClient.Raw (createHttpClientMetricsFromMeter)
import OpenTelemetry.MeterProvider (SdkMeterEnv, collectResourceMetrics)
import OpenTelemetry.Metric (
    PeriodicMetricReaderHandle (stopPeriodicMetricReader),
    createMeterProvider,
    defaultPeriodicMetricReaderOptions,
    defaultSdkMeterProviderOptions,
    forkPeriodicMetricReader,
    shutdownMeterProvider,
 )
import OpenTelemetry.Metric.Core (
    AdvisoryParameters (advisoryExplicitBucketBoundaries),
    Counter (counterAdd),
    Gauge (gaugeRecord),
    Histogram (histogramRecord),
    MeterProvider (meterProviderGetMeter),
    ObservableResult (observe),
    UpDownCounter (upDownCounterAdd),
    defaultAdvisoryParameters,
    getMeter,
    meterCreateCounterDouble,
    meterCreateCounterInt64,
    meterCreateGaugeInt64,
    meterCreateHistogram,
    meterCreateObservableGaugeInt64,
    meterCreateUpDownCounterInt64,
 )
import OpenTelemetry.Resource (emptyMaterializedResources)
import OpenTelemetry.Trace (Tracer, createTracerProvider, defaultSpanArguments, emptyTracerProviderOptions, inSpan, inSpan', makeTracer, shutdownTracerProvider, tracerOptions)
import OpenTelemetry.Trace.Core (getSpanContext)
import OpenTelemetry.Trace.Core qualified as TraceCore
import OpenTelemetry.Trace.Id (spanIdBytes, traceIdBytes)

import Ecluse.Core.Telemetry.Span (ecluseScope)
import Ecluse.Runtime.Telemetry.Batched (batchMeterProvider)
import Ecluse.Runtime.Telemetry.Batched.Internal (Batch (batchFlushes), batchedProvider, newBatch)
import Ecluse.Runtime.Test.Telemetry (gaugePoints, metricExports, sumPoints, sumPointsIn)
import Ecluse.Test.Support (withCapabilities)

-- | A batched provider exports what the plain SDK would for the same records, and an export never misses a record made before it.
spec :: Spec
spec = do
    exactnessSpec
    collectionSpec
    instrumentSpec
    gaugeOrderSpec
    faultSpec
    exemplarSpec

exactnessSpec :: Spec
exactnessSpec = describe "batchMeterProvider against the plain SDK" $ do
    it "exports the same data points for the same records" $
        withProviders $ \plain batched -> withTracer $ \tracer -> do
            pair <- (,) <$> newProbe (fst plain) <*> newProbe (fst batched)
            traverse_ (replay tracer pair) script
            expected <- collectResourceMetrics (snd plain)
            actual <- collectResourceMetrics (snd batched)
            pointLines actual `shouldBe` pointLines expected
            histogramSums actual `shouldMatchSums` histogramSums expected
            -- The comparison covers exemplars only if the plain SDK made some.
            pointLines expected `shouldSatisfy` any (T.isInfixOf "MetricExemplar")

    it "exports the same counts, buckets and totals while threads on several capabilities record at once" $
        withCapabilities 4 $
            withProviders $ \plain batched -> do
                (plainProbe, batchedProbe) <- (,) <$> newProbe (fst plain) <*> newProbe (fst batched)
                forConcurrently_ [1 .. 8 :: Int] $ \writer -> for_ [1 .. 500 :: Int] $ \i -> do
                    let attrs = if even (writer + i) then getAttrs else putAttrs
                        seconds = fromIntegral (i `mod` 40) / 8
                    for_ [plainProbe, batchedProbe] $ \probe -> do
                        counterAdd (probeRequests probe) 1 attrs
                        upDownCounterAdd (probeActive probe) (if even i then 1 else -1) attrs
                        histogramRecord (probeDuration probe) seconds attrs
                expected <- collectResourceMetrics (snd plain)
                actual <- collectResourceMetrics (snd batched)
                pointLines actual `shouldBe` pointLines expected
                histogramSums actual `shouldMatchSums` histogramSums expected

collectionSpec :: Spec
collectionSpec = describe "flushing" $ do
    it "flushes before every collection, so a collection right after a record sees it" $
        withBatched $ \(provider, meterEnv) -> do
            probe <- newProbe provider
            counterAdd (probeRequests probe) 2 getAttrs
            gaugeRecord (probeDepth probe) 5 getAttrs
            sumPoints "probe.requests" meterEnv `shouldReturn` [(getAttrs, 2)]
            gaugePoints "probe.depth" meterEnv `shouldReturn` [(getAttrs, 5)]
            counterAdd (probeRequests probe) 3 getAttrs
            sumPoints "probe.requests" meterEnv `shouldReturn` [(getAttrs, 5)]

    it "flushes a series whose changes cancel out, as the SDK alone would show it" $
        withBatched $ \(provider, meterEnv) -> do
            probe <- newProbe provider
            upDownCounterAdd (probeActive probe) 1 putAttrs
            upDownCounterAdd (probeActive probe) (-1) putAttrs
            sumPoints "probe.active" meterEnv `shouldReturn` [(putAttrs, 0)]

    it "flushes into the periodic reader's final export at shutdown, so the last records are exported" $ do
        (exporter, exported) <- inMemoryMetricExporter
        (sdk, meterEnv) <- createMeterProvider emptyMaterializedResources defaultSdkMeterProviderOptions
        (provider, _) <- batchMeterProvider (const pass) sdk
        periodic <- forkPeriodicMetricReader meterEnv exporter defaultPeriodicMetricReaderOptions
        probe <- newProbe provider
        counterAdd (probeRequests probe) 4 getAttrs
        stopPeriodicMetricReader periodic
        finalExport <- take 1 . reverse <$> readIORef exported
        sumPointsIn "probe.requests" finalExport `shouldBe` [(getAttrs, 4)]
        shutdown provider

instrumentSpec :: Spec
instrumentSpec = describe "instrument creation" $ do
    it "returns the first instrument for a repeated key, so creating it again adds no flush" $
        withBatch getMonotonicTimeNSec $ \batch provider meterEnv -> do
            meter <- getMeter provider ecluseScope
            let create description = meterCreateCounterInt64 meter "probe.requests" (Just "{request}") description defaultAdvisoryParameters
            replicateM_ 100 (create Nothing >>= \counter -> counterAdd counter 1 getAttrs)
            flushCount batch `shouldReturn` 1
            sumPoints "probe.requests" meterEnv `shouldReturn` [(getAttrs, 100)]
            void (create (Just "Requests served."))
            flushCount batch `shouldReturn` 2

    it "adds no flush when the http-client instrumentation creates its metrics on every request" $
        withBatch getMonotonicTimeNSec $ \batch provider _ -> do
            meter <- getMeter provider ecluseScope
            replicateM_ 50 (createHttpClientMetricsFromMeter meter)
            flushCount batch `shouldReturn` 3

gaugeOrderSpec :: Spec
gaugeOrderSpec = describe "gauge readings from two capabilities" $
    it "keep the newest, across cells and against an older reading drained after a newer one was flushed" $
        withCapabilities 2 $ do
            clock <- newIORef 0
            withBatch (readIORef clock) $ \_ provider meterEnv -> do
                meter <- getMeter provider ecluseScope
                gauge <- meterCreateGaugeInt64 meter "probe.depth" Nothing Nothing defaultAdvisoryParameters
                let recordOn capability at value = onCapability capability (writeIORef clock at >> gaugeRecord gauge value getAttrs)
                recordOn 0 30 3 >> recordOn 1 25 4
                gaugePoints "probe.depth" meterEnv `shouldReturn` [(getAttrs, 3)]
                recordOn 1 40 5
                gaugePoints "probe.depth" meterEnv `shouldReturn` [(getAttrs, 5)]
                recordOn 0 35 6
                gaugePoints "probe.depth" meterEnv `shouldReturn` [(getAttrs, 5)]

faultSpec :: Spec
faultSpec = describe "a series whose replay throws" $
    it "is reported and dropped alone, and the collection still finishes" $ do
        faults <- newIORef (0 :: Int)
        (sdk, meterEnv) <- createMeterProvider emptyMaterializedResources defaultSdkMeterProviderOptions
        batch <- newBatch getMonotonicTimeNSec (\_ -> modifyIORef' faults (+ 1))
        provider <- batchedProvider batch (refusingCounterRecords putAttrs sdk)
        probe <- newProbe provider
        let accepted = methodAttrs <$> ["GET", "HEAD", "POST", "PATCH", "DELETE", "OPTIONS", "TRACE", "CONNECT", "PURGE", "LINK"]
        for_ (putAttrs : accepted) $ \attrs -> counterAdd (probeRequests probe) 1 attrs
        sumPoints "probe.requests" meterEnv >>= (`shouldMatchList` map (,1) accepted)
        readIORef faults `shouldReturn` 1
        shutdown provider

exemplarSpec :: Spec
exemplarSpec = describe "exemplars" $
    it "carry the span a record was made in through a flush, stamped at the flush" $
        withBatched $ \(provider, meterEnv) -> withTracer $ \tracer -> do
            probe <- newProbe provider
            recorded <- inSpan' tracer "request" defaultSpanArguments $ \active -> do
                counterAdd (probeRequests probe) 1 getAttrs
                gaugeRecord (probeDepth probe) 6 getAttrs
                histogramRecord (probeDuration probe) 0.25 getAttrs
                getSpanContext active
            -- Records outside any span leave the exemplar on the last one made in a span, as the SDK does.
            counterAdd (probeRequests probe) 2 getAttrs
            histogramRecord (probeDuration probe) 0.5 getAttrs
            beforeFlush <- realtimeNanos
            exemplars <- exemplarsByMetric <$> collectResourceMetrics meterEnv
            map fst exemplars `shouldMatchList` ["probe.depth", "probe.duration", "probe.requests"]
            for_ exemplars $ \(_, exemplar) -> do
                metricExemplarTraceId exemplar `shouldBe` traceIdBytes (TraceCore.traceId recorded)
                metricExemplarSpanId exemplar `shouldBe` spanIdBytes (TraceCore.spanId recorded)
                metricExemplarTimeUnixNano exemplar `shouldSatisfy` (>= beforeFlush)
            -- A counter's exemplar holds the flushed delta, a histogram's its own sample.
            metricExemplarValue <$> lookup "probe.requests" exemplars `shouldBe` Just (Just (IntNumber 3))
            metricExemplarValue <$> lookup "probe.duration" exemplars `shouldBe` Just (Just (DoubleNumber 0.25))

-- One instrument of each batched kind, and two that pass straight through to the SDK.
data Probe = Probe
    { probeRequests :: Counter Int64
    , probeActive :: UpDownCounter Int64
    , probeDepth :: Gauge Int64
    , probeDuration :: Histogram
    , probeRatio :: Counter Double
    }

newProbe :: MeterProvider -> IO Probe
newProbe provider = do
    meter <- getMeter provider ecluseScope
    void (meterCreateObservableGaugeInt64 meter "probe.open" Nothing (Just "Open handles.") defaultAdvisoryParameters [\result -> observe result 3 getAttrs])
    Probe
        <$> meterCreateCounterInt64 meter "probe.requests" (Just "{request}") (Just "Requests served.") defaultAdvisoryParameters
        <*> meterCreateUpDownCounterInt64 meter "probe.active" (Just "{request}") (Just "Requests in flight.") defaultAdvisoryParameters
        <*> meterCreateGaugeInt64 meter "probe.depth" Nothing (Just "Queue depth.") defaultAdvisoryParameters
        <*> meterCreateHistogram meter "probe.duration" (Just "s") (Just "Request duration.") defaultAdvisoryParameters{advisoryExplicitBucketBoundaries = Just [0.01, 0.1, 1, 10]}
        <*> meterCreateCounterDouble meter "probe.ratio" Nothing Nothing defaultAdvisoryParameters

-- One record, made in a fresh span or in none.
data Step = InSpan (Probe -> IO ()) | Unspanned (Probe -> IO ())

-- Spanned and unspanned records interleave, so each series' exemplar must come from the right one.
script :: [Step]
script =
    [ InSpan (\probe -> counterAdd (probeRequests probe) 1 getAttrs)
    , Unspanned (\probe -> counterAdd (probeRequests probe) 2 getAttrs)
    , InSpan (\probe -> counterAdd (probeRequests probe) 1 putAttrs)
    , Unspanned (\probe -> upDownCounterAdd (probeActive probe) 1 getAttrs)
    , InSpan (\probe -> upDownCounterAdd (probeActive probe) 1 putAttrs)
    , Unspanned (\probe -> upDownCounterAdd (probeActive probe) (-1) getAttrs)
    , InSpan (\probe -> gaugeRecord (probeDepth probe) 3 getAttrs)
    , Unspanned (\probe -> gaugeRecord (probeDepth probe) 9 getAttrs)
    , InSpan (\probe -> gaugeRecord (probeDepth probe) 4 putAttrs)
    , Unspanned (\probe -> counterAdd (probeRatio probe) 0.5 getAttrs)
    ]
        <> zipWith durationStep [0 :: Int ..] [0.004, 0.01, 0.05, 0.1, 0.3, 1, 2.5, 10, 42, 0 / 0, 1 / 0]
  where
    durationStep i seconds =
        (if even i then InSpan else Unspanned) (\probe -> histogramRecord (probeDuration probe) seconds (if i < 5 then getAttrs else putAttrs))

-- Make one step's record in both probes, in the same span.
replay :: Tracer -> (Probe, Probe) -> Step -> IO ()
replay tracer (plain, batched) = \case
    InSpan record -> inSpan tracer "record" defaultSpanArguments (record plain >> record batched)
    Unspanned record -> record plain >> record batched

{- One line per data point, without what a flush changes by design: timestamps, a sum's or gauge's
exemplar value, and a histogram's sum, which 'histogramSums' compares within float rounding. -}
pointLines :: [ResourceMetricsExport] -> [Text]
pointLines = sort . concatMap metricLines . metricExports
  where
    metricLines = \case
        metric@MetricExportSum{mesSumPoints = points} -> line metric{mesSumPoints = V.empty} . settleSum <$> toList points
        metric@MetricExportGauge{megGaugePoints = points} -> line metric{megGaugePoints = V.empty} . settleGauge <$> toList points
        metric@MetricExportHistogram{mehPoints = points} -> line metric{mehPoints = V.empty} . settleHistogram <$> toList points
        metric@MetricExportExponentialHistogram{} -> [show metric]
    line :: (Show point) => MetricExport -> point -> Text
    line metric point = show metric <> " " <> show point
    settleSum point =
        point{sumDataPointStartTimeUnixNano = 0, sumDataPointTimeUnixNano = 0, sumDataPointExemplars = valueless <$> sumDataPointExemplars point}
    settleGauge point =
        point{gaugeDataPointStartTimeUnixNano = 0, gaugeDataPointTimeUnixNano = 0, gaugeDataPointExemplars = valueless <$> gaugeDataPointExemplars point}
    settleHistogram point =
        point
            { histogramDataPointStartTimeUnixNano = 0
            , histogramDataPointTimeUnixNano = 0
            , histogramDataPointSum = 0
            , histogramDataPointExemplars = untimed <$> histogramDataPointExemplars point
            }
    untimed exemplar = exemplar{metricExemplarTimeUnixNano = 0}
    valueless exemplar = (untimed exemplar){metricExemplarValue = Nothing}

-- Each histogram point's sum, keyed by metric and attributes.
histogramSums :: [ResourceMetricsExport] -> [(Text, Double)]
histogramSums batches =
    sortWith
        fst
        [ (name <> " " <> show (histogramDataPointAttributes point), histogramDataPointSum point)
        | MetricExportHistogram{mehName = name, mehPoints = points} <- metricExports batches
        , point <- toList points
        ]

-- The sums of the same samples added in another order agree to within float rounding.
shouldMatchSums :: [(Text, Double)] -> [(Text, Double)] -> Expectation
shouldMatchSums actual expected = do
    map fst actual `shouldBe` map fst expected
    zipWith (\(_, a) (_, b) -> abs (a - b) <= 1e-9 * max 1 (abs b)) actual expected `shouldSatisfy` and

exemplarsByMetric :: [ResourceMetricsExport] -> [(Text, MetricExemplar)]
exemplarsByMetric batches = concatMap exemplarsOf (metricExports batches)
  where
    exemplarsOf = \case
        MetricExportSum{mesName = name, mesSumPoints = points} -> (name,) <$> concatMap (toList . sumDataPointExemplars) points
        MetricExportGauge{megName = name, megGaugePoints = points} -> (name,) <$> concatMap (toList . gaugeDataPointExemplars) points
        MetricExportHistogram{mehName = name, mehPoints = points} -> (name,) <$> concatMap (toList . histogramDataPointExemplars) points
        MetricExportExponentialHistogram{} -> []

-- A plain SDK provider and a batched one, each with the meter environment a collection reads.
withProviders :: ((MeterProvider, SdkMeterEnv) -> (MeterProvider, SdkMeterEnv) -> IO a) -> IO a
withProviders use =
    bracket (createMeterProvider emptyMaterializedResources defaultSdkMeterProviderOptions) (shutdown . fst) $ \plain ->
        withBatched (use plain)

withBatched :: ((MeterProvider, SdkMeterEnv) -> IO a) -> IO a
withBatched = bracket acquire (shutdown . fst)
  where
    acquire = do
        (sdk, meterEnv) <- createMeterProvider emptyMaterializedResources defaultSdkMeterProviderOptions
        (provider, _flushStep) <- batchMeterProvider (const pass) sdk
        pure (provider, meterEnv)

-- A batch over a plain SDK provider and a given monotonic clock, with no fault expected.
withBatch :: IO Word64 -> (Batch -> MeterProvider -> SdkMeterEnv -> IO a) -> IO a
withBatch clock use =
    bracket (createMeterProvider emptyMaterializedResources defaultSdkMeterProviderOptions) (shutdown . fst) $ \(sdk, meterEnv) -> do
        batch <- newBatch clock throwIO
        provider <- batchedProvider batch sdk
        use batch provider meterEnv

-- How many instruments a batch flushes.
flushCount :: Batch -> IO Int
flushCount batch = length <$> readIORef (batchFlushes batch)

-- Run an action on one capability and wait for it.
onCapability :: Int -> IO () -> IO ()
onCapability capability act = do
    done <- newEmptyMVar
    _ <- forkOn capability (tryAny act >>= putMVar done)
    takeMVar done >>= either throwIO pure

-- An SDK provider whose counters throw on one attribute set.
refusingCounterRecords :: Attributes -> MeterProvider -> MeterProvider
refusingCounterRecords refused sdk = sdk{meterProviderGetMeter = fmap refusing . meterProviderGetMeter sdk}
  where
    refusing meter = meter{meterCreateCounterInt64 = \name unit description advice -> refuse <$> meterCreateCounterInt64 meter name unit description advice}
    refuse counter = counter{counterAdd = \value attrs -> if attrs == refused then throwIO RecordRefused else counterAdd counter value attrs}

data RecordRefused = RecordRefused
    deriving stock (Show)

instance Exception RecordRefused

shutdown :: MeterProvider -> IO ()
shutdown provider = void (shutdownMeterProvider provider Nothing)

-- A tracer with no processor drops every span with an empty trace id, which makes no exemplar.
withTracer :: (Tracer -> IO a) -> IO a
withTracer use = do
    (processor, _) <- inMemoryListExporter
    bracket (createTracerProvider [processor] emptyTracerProviderOptions) (\provider -> void (shutdownTracerProvider provider Nothing)) $ \provider ->
        use (makeTracer provider ecluseScope tracerOptions)

-- The SDK stamps exemplars with the realtime clock, in nanoseconds since the epoch.
realtimeNanos :: IO Word64
realtimeNanos = floor . (* 1_000_000_000) <$> getPOSIXTime

getAttrs :: Attributes
getAttrs = methodAttrs "GET"

putAttrs :: Attributes
putAttrs = methodAttrs "PUT"

methodAttrs :: Text -> Attributes
methodAttrs = addAttribute defaultAttributeLimits emptyAttributes "http.request.method"
