-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The S3 upload adapter for the compiled OSV artifact.

The amazonka-facing half of Pilot's export: @PutObject@ a compiled @osv.db@ into the
configured bucket over an S3 env built for an optional custom endpoint
('Ecluse.Runtime.Aws.S3.buildS3Env'). It takes a __pre-parsed__ endpoint tuple rather
than the application config, so it is an ecosystem-agnostic cloud adapter with no
dependency on the composition shell; the Pilot export loop resolves the endpoint from
config and passes it down.
-}
module Ecluse.Runtime.Pilot.Export (
    exportToS3,
) where

import Conduit (MonadResource)
import Control.Monad.Catch (MonadThrow)
import GHC.Clock (getMonotonicTime)
import Katip (KatipContext, Severity (..), katipAddContext, logFM, ls, sl)
import System.Directory (getFileSize)
import System.FilePath (takeFileName)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (bracket, withException)

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3
import Ecluse.Runtime.Aws.S3 (buildS3Env)
import OpenTelemetry.Context qualified as Ctx
import OpenTelemetry.Trace.Core (SpanKind (Client), SpanStatus (Error), TracerProvider, addAttribute, createSpan, defaultSpanArguments, endSpan, kind, makeTracer, setStatus, tracerOptions)

{- | Upload a compiled OSV artifact to the given S3 bucket, dialling an optional
custom endpoint (the pre-parsed @(secure, host, port)@ resolved from configuration).

The @PutObject@ runs inside an @ecluse.pilot.osv.upload@ client span (inert when
telemetry is off) carrying the bucket, object key, and byte count. A failed upload is
logged at the call site and marks the span errored before it propagates to the export
loop's supervisor, which otherwise sees only an opaque restart.
-}
exportToS3 :: (MonadResource m, MonadUnliftIO m, MonadThrow m, KatipContext m) => Maybe TracerProvider -> Maybe (Bool, Text, Int) -> Text -> FilePath -> m ()
exportToS3 mTracerProvider mEndpoint bucketName dbPath = do
    let keyText = toText (takeFileName dbPath)
        mTracer = (\tp -> makeTracer tp "ecluse" tracerOptions) <$> mTracerProvider
    size <- liftIO $ getFileSize dbPath

    bracket
        (traverse (\t -> createSpan t Ctx.empty "ecluse.pilot.osv.upload" defaultSpanArguments{kind = Client}) mTracer)
        (mapM_ (`endSpan` Nothing))
        $ \mSpan -> do
            forM_ mSpan $ \sp -> do
                addAttribute sp "ecluse.osv.bucket" bucketName
                addAttribute sp "ecluse.osv.object_key" keyText
                addAttribute sp "ecluse.osv.bytes" (show size :: Text)
            katipAddContext (sl "bucket" bucketName <> sl "object_key" keyText <> sl "bytes" size) $
                logFM InfoS (ls ("Uploading " <> toText dbPath <> " to S3 bucket " <> bucketName))

            env <- liftIO $ buildS3Env mEndpoint
            body <- liftIO $ AWS.chunkedFile 1048576 dbPath
            let req = S3.newPutObject (S3.BucketName bucketName) (S3.ObjectKey keyText) body

            start <- liftIO getMonotonicTime
            withException
                (void $ AWS.send env req)
                ( \(e :: SomeException) -> do
                    forM_ mSpan $ \sp -> setStatus sp (Error ("S3 upload failed: " <> show e))
                    logFM ErrorS (ls ("S3 upload failed for " <> keyText <> " to bucket " <> bucketName <> ": " <> show e))
                )
            elapsed <- liftIO getMonotonicTime

            katipAddContext (sl "bucket" bucketName <> sl "bytes" size <> sl "duration_s" (elapsed - start)) $
                logFM InfoS "S3 upload complete"
