-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Bounded S3 downloads and typed response-body transport faults.
module Ecluse.Runtime.Cve.Sync.S3Spec (spec) where

import Conduit (runConduit, yieldMany, (.|))
import Data.Conduit.Combinators qualified as C
import Data.Text qualified as T
import Katip (runKatipContextT)
import OpenTelemetry.Exporter.InMemory.Span (inMemoryListExporter)
import OpenTelemetry.Metric (noopMeterProvider)
import OpenTelemetry.Trace (createTracerProvider, emptyTracerProviderOptions, forceFlushTracerProvider)
import OpenTelemetry.Trace.Core (ImmutableSpan (spanHot), SpanHot (hotStatus), SpanStatus (Error))
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy, shouldThrow)
import UnliftIO.Async (poll, withAsync)

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI), ecosystemName)
import Ecluse.Core.Fault (TransportCause (TransportUnreachable), renderTransportCause)
import Ecluse.Core.Telemetry.Metrics (AdvisorySyncResult (AdvisoryFetchFailed))
import Ecluse.Runtime.Cve.Sync.Internal (runCveSync)
import Ecluse.Runtime.Cve.Sync.S3 (OsvDbCapExceeded (OsvDbCapExceeded), cappedAt, s3CveFetch)
import Ecluse.Runtime.Telemetry.Escape (withEscapeHandler)
import Ecluse.Runtime.Telemetry.Internal (Telemetry (TelemetryEnabled), TelemetryProviders (TelemetryProviders))
import Ecluse.Runtime.Telemetry.Tracing.Internal (advisorySyncTracingPortOf)
import Ecluse.Runtime.Test.Cve (notifyOnly, oneAttempt, waitFor, withSyncEnvFor)
import Ecluse.Runtime.Test.S3 (brokenResponse, recordingStatuses, withS3, withTlsS3)
import Ecluse.Test.Log (memoryLogEnv)
import Ecluse.Test.Port (noopAdvisorySyncMetricsPort, passthroughAdvisorySyncTracingPort, recordingAdvisorySyncMetricsPort)
import Ecluse.Test.Stub.Socket (resetAfterPartialBody, withTlsServer)

-- A GetObject body the store cuts short, which amazonka surfaces as a bare HTTP exception. The
-- attempt span and the fetch report carry its typed fault, never the object key or a signed header.
resetDownloadSpec :: Spec
resetDownloadSpec = describe "a GetObject body that resets mid-download" $
    for_ [(Npm, "npm.db"), (PyPI, "pypi.db")] $ \(eco, key) ->
        it ("is a typed transport fault on the " <> toString (ecosystemName eco) <> " attempt span and in the fetch report, never the signed request") $
            withSyncEnvFor eco $ \_ _ envWith ->
                withS3 (brokenResponse "200 OK" "Content-Length: 20\r\n\r\nshort") $ \awsEnv -> do
                    (processor, spansRef) <- inMemoryListExporter
                    tracerProvider <- createTracerProvider [processor] (withEscapeHandler emptyTracerProviderOptions)
                    (logEnv, readMessages) <- memoryLogEnv
                    let tracing = advisorySyncTracingPortOf (TelemetryEnabled (TelemetryProviders tracerProvider noopMeterProvider))
                        env = envWith (s3CveFetch awsEnv "test-advisories" key 1_000_000)
                        reported = find (T.isInfixOf "sync fetch failed") <$> readMessages
                    runKatipContextT logEnv () mempty . withAsync (runCveSync noopAdvisorySyncMetricsPort tracing env oneAttempt (notifyOnly pass)) $ \_ ->
                        liftIO (waitFor "the fetch report" (isJust <$> reported))
                    report <- fromMaybe "" <$> reported
                    report `shouldSatisfy` T.isInfixOf "sync fetch failed: 127.0.0.1:"
                    for_ [key, "X-Amz-"] $ \leak -> report `shouldSatisfy` (not . T.isInfixOf leak)
                    _ <- forceFlushTracerProvider tracerProvider Nothing
                    statuses <- traverse (fmap hotStatus . readIORef . spanHot) =<< readIORef spansRef
                    statuses `shouldSatisfy` any (\case Error message -> T.isPrefixOf "127.0.0.1:" message && T.isSuffixOf message report; _ -> False)

-- A TLS body reset after the GetObject's 200 counts as a failed fetch, and the task keeps polling.
tlsResetDownloadSpec :: Spec
tlsResetDownloadSpec = describe "a GetObject body that resets mid-download over TLS" $
    for_ [(Npm, "npm.db"), (PyPI, "pypi.db")] $ \(eco, key) ->
        it ("counts a typed unreachable fault on the " <> toString (ecosystemName eco) <> " sync, which keeps running") $
            withSyncEnvFor eco $ \_ _ envWith ->
                withTlsS3 (withTlsServer resetAfterPartialBody) $ \plainEnv -> do
                    (awsEnv, statuses) <- recordingStatuses plainEnv
                    (metrics, readAttempts, _) <- recordingAdvisorySyncMetricsPort
                    (logEnv, readMessages) <- memoryLogEnv
                    let env = envWith (s3CveFetch awsEnv "test-advisories" key 1_000_000)
                        reported = find (T.isInfixOf "sync fetch failed") <$> readMessages
                    running <- runKatipContextT logEnv () mempty . withAsync (runCveSync metrics passthroughAdvisorySyncTracingPort env oneAttempt (notifyOnly pass)) $ \task ->
                        liftIO (waitFor "the fetch report" (isJust <$> reported) >> isNothing <$> poll task)
                    running `shouldBe` True
                    statuses `shouldReturn` [200, 200]
                    readAttempts `shouldReturn` [(eco, AdvisoryFetchFailed)]
                    report <- fromMaybe "" <$> reported
                    report `shouldSatisfy` T.isInfixOf "sync fetch failed: 127.0.0.1:"
                    report `shouldSatisfy` T.isSuffixOf (" (" <> renderTransportCause TransportUnreachable <> ")")
                    for_ [key, "X-Amz-"] $ \leak -> report `shouldSatisfy` (not . T.isInfixOf leak)

spec :: Spec
spec = do
    resetDownloadSpec
    tlsResetDownloadSpec
    describe "cappedAt" $ do
        it "passes a stream that ends exactly at the cap through unchanged" $ do
            out <- runConduit (yieldMany (["ab", "cd"] :: [ByteString]) .| cappedAt 4 .| C.sinkList)
            mconcat out `shouldBe` ("abcd" :: ByteString)

        it "throws the confined cap exception the moment the stream oversteps the cap" $
            -- The conduit's mid-stream escape. The adapter boundary ('s3Download')
            -- folds it into the 'OsvDbTooLarge' value on the 'CveFetch' channel.
            runConduit (yieldMany (["ab", "cde"] :: [ByteString]) .| cappedAt 4 .| C.sinkList)
                `shouldThrow` (== OsvDbCapExceeded 4)
