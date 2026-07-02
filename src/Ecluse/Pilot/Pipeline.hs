{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Pilot.Pipeline (
    runExportPipeline,
) where

import Conduit (MonadThrow, runResourceT)
import Control.Monad.Primitive (PrimMonad)
import Katip (KatipContext, Severity (..), logFM, ls)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (catchAny)

import Ecluse.Boot (BootEnv (..))
import Ecluse.Config (AppConfig (..))
import Ecluse.Pilot.Export.S3 (uploadOsvToS3)
import Ecluse.Pilot.Osv.Compile (compileOsvToSqlite)

-- | Run the full export pipeline for a specific ecosystem OSV URL.
runExportPipeline :: (MonadUnliftIO m, KatipContext m, PrimMonad m, MonadThrow m) => BootEnv -> Text -> String -> m ()
runExportPipeline env ecosystem url = do
    let cfg = beConfig env
        scratchDir = cfgOsvScratchDir cfg
        mBucket = cfgPilotS3Bucket cfg

    case mBucket of
        Nothing -> logFM WarningS (ls ("Pilot S3 Bucket is not configured. Skipping upload for " <> ecosystem))
        Just bucket -> do
            logFM InfoS (ls ("Starting export pipeline for " <> ecosystem))
            catchAny
                ( do
                    dbFile <- runResourceT $ compileOsvToSqlite (beTelemetry env) scratchDir ecosystem url
                    uploadOsvToS3 bucket dbFile
                    logFM InfoS (ls ("Completed export pipeline for " <> ecosystem))
                )
                (\e -> logFM ErrorS (ls ("Export pipeline failed for " <> ecosystem <> ": " <> show e)))
