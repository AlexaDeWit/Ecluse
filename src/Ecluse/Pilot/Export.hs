{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Pilot.Export (
    runExportLoop,
    exportToS3,
    buildS3Env,
) where

import Conduit (MonadResource, runResourceT)
import Control.Monad.Catch (MonadThrow)
import System.FilePath (takeFileName)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (catchAny)

import Katip (KatipContext, Severity (..), logFM, ls)

import Ecluse.Composition (parseEndpointUrl)
import Ecluse.Config (AppConfig (..), Config (..))
import Ecluse.Pilot.Osv.Compile (compileOsvToSqlite)
import Ecluse.Telemetry (Telemetry)

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3

runExportLoop :: (MonadThrow m, MonadUnliftIO m, KatipContext m) => Telemetry -> Config -> m ()
runExportLoop telemetry config = do
    let appCfg = configApp config
    case cfgVulnerabilityDatabaseBucket appCfg of
        Nothing -> do
            logFM InfoS "No S3 bucket configured for OSV database export; export loop disabled."
            forever $ threadDelay (24 * 60 * 60 * 1000000)
        Just bucketName -> do
            logFM InfoS (ls ("S3 export loop starting up. Target bucket: " <> bucketName))
            forever $ do
                catchAny (runResourceT $ exportNpm telemetry appCfg bucketName) $ \e ->
                    logFM ErrorS (ls ("Export failed: " <> show e :: String))
                threadDelay ((round (cfgCveSyncInterval appCfg) :: Int) * 1000000)

exportNpm :: (MonadResource m, MonadThrow m, MonadUnliftIO m, KatipContext m) => Telemetry -> AppConfig -> Text -> m ()
exportNpm telemetry appCfg bucketName = do
    logFM InfoS "Starting npm OSV database compilation"
    dbPath <- compileOsvToSqlite telemetry (cfgOsvDataDir appCfg) "npm" "https://osv-vulnerabilities.storage.googleapis.com/npm/all.zip"
    exportToS3 appCfg bucketName dbPath

exportToS3 :: (MonadResource m, MonadThrow m, KatipContext m) => AppConfig -> Text -> FilePath -> m ()
exportToS3 appCfg bucketName dbPath = do
    logFM InfoS (ls ("Uploading " <> toText dbPath <> " to S3 bucket " <> bucketName))

    env <- liftIO $ buildS3Env appCfg
    let key = S3.ObjectKey (toText (takeFileName dbPath))

    body <- liftIO $ AWS.chunkedFile 1048576 dbPath
    let req = S3.newPutObject (S3.BucketName bucketName) key body

    void $ AWS.send env req

    logFM InfoS "S3 upload complete"

buildS3Env :: AppConfig -> IO AWS.Env
buildS3Env appCfg = do
    env <- AWS.newEnv AWS.discover
    return $ case cfgAwsEndpointUrl appCfg of
        Just url -> case parseEndpointUrl url of
            Just (secure, host, port) ->
                AWS.configureService
                    ( (AWS.setEndpoint secure (encodeUtf8 host) port S3.defaultService)
                        { AWS.s3AddressingStyle = AWS.S3AddressingStylePath
                        }
                    )
                    env
            Nothing -> env
        Nothing -> env
