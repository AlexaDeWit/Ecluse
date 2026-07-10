{-# LANGUAGE OverloadedStrings #-}

{- | The S3 upload adapter for the compiled OSV artifact.

The amazonka-facing half of Pilot's export: build an S3-configured @amazonka@ env
(honouring an optional custom endpoint override) and @PutObject@ a compiled
@osv.db@ into the configured bucket. It takes a __pre-parsed__ endpoint tuple
rather than the application config, so it is an ecosystem-agnostic cloud adapter
with no dependency on the composition shell; the caller (the Pilot export loop, or
the proxy's CVE-sync consumer for 'buildS3Env') resolves the endpoint from config
and passes it down.
-}
module Ecluse.Runtime.Pilot.Export (
    exportToS3,
    buildS3Env,
) where

import Conduit (MonadResource)
import Control.Monad.Catch (MonadThrow)
import Katip (KatipContext, Severity (..), logFM, ls)
import System.FilePath (takeFileName)

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3

{- | Upload a compiled OSV artifact to the given S3 bucket, dialling an optional
custom endpoint (the pre-parsed @(secure, host, port)@ resolved from configuration).
-}
exportToS3 :: (MonadResource m, MonadThrow m, KatipContext m) => Maybe (Bool, Text, Int) -> Text -> FilePath -> m ()
exportToS3 mEndpoint bucketName dbPath = do
    logFM InfoS (ls ("Uploading " <> toText dbPath <> " to S3 bucket " <> bucketName))

    env <- liftIO $ buildS3Env mEndpoint
    let key = S3.ObjectKey (toText (takeFileName dbPath))

    body <- liftIO $ AWS.chunkedFile 1048576 dbPath
    let req = S3.newPutObject (S3.BucketName bucketName) key body

    void $ AWS.send env req

    logFM InfoS "S3 upload complete"

{- | Build an @amazonka@ env for S3, applying an optional custom endpoint override
(the pre-parsed @(secure, host, port)@). Shared by the Pilot export producer and the
proxy's CVE-sync consumer, both of which resolve the override from configuration and
pass the parsed tuple in.
-}
buildS3Env :: Maybe (Bool, Text, Int) -> IO AWS.Env
buildS3Env mEndpoint = do
    env <- AWS.newEnv AWS.discover
    pure $ case mEndpoint of
        Just endpoint -> AWS.configureService (customS3Endpoint endpoint) env
        Nothing -> env

customS3Endpoint :: (Bool, Text, Int) -> AWS.Service
customS3Endpoint (secure, host, port) =
    (AWS.setEndpoint secure (encodeUtf8 host) port S3.defaultService)
        { AWS.s3AddressingStyle = AWS.S3AddressingStylePath
        }
