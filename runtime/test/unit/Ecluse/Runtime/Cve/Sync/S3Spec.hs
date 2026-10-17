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
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, shouldThrow)
import UnliftIO.Async (withAsync)

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI), ecosystemName)
import Ecluse.Runtime.Cve.Sync.Internal (runCveSync)
import Ecluse.Runtime.Cve.Sync.S3 (OsvDbCapExceeded (OsvDbCapExceeded), cappedAt, s3CveFetch)
import Ecluse.Runtime.Telemetry.Escape (withEscapeHandler)
import Ecluse.Runtime.Telemetry.Internal (Telemetry (TelemetryEnabled), TelemetryProviders (TelemetryProviders))
import Ecluse.Runtime.Telemetry.Tracing.Internal (advisorySyncTracingPortOf)
import Ecluse.Runtime.Test.Cve (notifyOnly, oneAttempt, waitFor, withSyncEnvFor)
import Ecluse.Runtime.Test.S3 (brokenResponse, withS3)
import Ecluse.Test.Log (memoryLogEnv)
import Ecluse.Test.Port (noopAdvisorySyncMetricsPort)

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

spec :: Spec
spec = do
    resetDownloadSpec
    describe "cappedAt" $ do
        it "passes a stream that ends exactly at the cap through unchanged" $ do
            out <- runConduit (yieldMany (["ab", "cd"] :: [ByteString]) .| cappedAt 4 .| C.sinkList)
            mconcat out `shouldBe` ("abcd" :: ByteString)

        it "throws the confined cap exception the moment the stream oversteps the cap" $
            -- The conduit's mid-stream escape. The adapter boundary ('s3Download')
            -- folds it into the 'OsvDbTooLarge' value on the 'CveFetch' channel.
            runConduit (yieldMany (["ab", "cde"] :: [ByteString]) .| cappedAt 4 .| C.sinkList)
                `shouldThrow` (== OsvDbCapExceeded 4)
