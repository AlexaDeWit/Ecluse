-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The S3 upload adapter for the compiled OSV artifact: @PutObject@ an @osv.db@ into the
configured bucket, over an S3 env built for an optional custom endpoint
('Ecluse.Runtime.Aws.S3.buildS3Env'). It takes a __resolved__ 'AwsEndpoint' rather than the
application config, so it holds no dependency on the composition shell.
-}
module Ecluse.Runtime.Pilot.Export (
    exportToS3,
) where

import Conduit (MonadResource)
import Control.Monad.Catch (MonadThrow)
import Katip (KatipContext, Severity (..), katipAddContext, logFM, ls, sl)
import System.Directory (getFileSize)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (withException)

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3
import Ecluse.Core.Telemetry.Record (timedSeconds)
import Ecluse.Core.Telemetry.Span (withOptionalSpan)
import Ecluse.Runtime.Aws.Env (AwsEndpoint)
import Ecluse.Runtime.Aws.S3 (buildS3Env)
import OpenTelemetry.Trace.Core (Span, SpanKind (Client), SpanStatus (Error), TracerProvider, addAttribute, setStatus)

{- | Upload an OSV artifact to @bucketName@ under the caller's @keyText@, which it derives from the
configured store so the proxy's sync reads this object. A failed upload logs and errors its span.
-}
exportToS3 :: (MonadResource m, MonadUnliftIO m, MonadThrow m, KatipContext m) => Maybe TracerProvider -> Maybe AwsEndpoint -> Text -> Text -> FilePath -> m ()
exportToS3 mTracerProvider mEndpoint bucketName keyText dbPath = do
    size <- liftIO $ getFileSize dbPath

    withOptionalSpan mTracerProvider Client "ecluse.pilot.osv.upload" $ \mSpan -> do
        stampUploadSpan mSpan bucketName keyText size
        katipAddContext (sl "bucket" bucketName <> sl "object_key" keyText <> sl "bytes" size) $
            logFM InfoS (ls ("Uploading " <> toText dbPath <> " to S3 bucket " <> bucketName))

        env <- liftIO $ buildS3Env mEndpoint
        body <- liftIO $ AWS.chunkedFile 1048576 dbPath
        let request = S3.newPutObject (S3.BucketName bucketName) (S3.ObjectKey keyText) body

        -- Only the send is timed: building the env and the chunked body is not upload time.
        (_, elapsed) <-
            timedSeconds $
                withException (void (AWS.send env request)) (reportUploadFailure mSpan bucketName keyText)

        katipAddContext (sl "bucket" bucketName <> sl "bytes" size <> sl "duration_s" elapsed) $
            logFM InfoS "S3 upload complete"

stampUploadSpan :: (MonadIO m) => Maybe Span -> Text -> Text -> Integer -> m ()
stampUploadSpan mSpan bucketName keyText size =
    whenJust mSpan $ \sp -> do
        addAttribute sp "ecluse.osv.bucket" bucketName
        addAttribute sp "ecluse.osv.object_key" keyText
        addAttribute sp "ecluse.osv.bytes" (show size :: Text)

-- Observation only: 'withException' re-raises, so the upload still fails after this runs.
reportUploadFailure :: (KatipContext m) => Maybe Span -> Text -> Text -> SomeException -> m ()
reportUploadFailure mSpan bucketName keyText err = do
    whenJust mSpan $ \sp -> setStatus sp (Error ("S3 upload failed: " <> show err))
    logFM ErrorS (ls ("S3 upload failed for " <> keyText <> " to bucket " <> bucketName <> ": " <> show err))
