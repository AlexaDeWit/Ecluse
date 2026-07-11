{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Pilot.S3ExportSpec (
    spec,
) where

import Control.Monad.Trans.Resource (runResourceT)
import Data.Text qualified as T
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe)
import TestContainers (containerAddress)
import UnliftIO (throwIO)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (catchAny)

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3
import Amazonka.S3.ListObjectsV2 qualified as S3
import Amazonka.S3.Types.Object qualified as S3Object
import Ecluse.Composition.MirrorQueue (parseEndpointUrl)
import Ecluse.Config (AppConfig (..), Config (..), loadConfig)
import Ecluse.Integration.Ministack (withMinistack)
import Ecluse.Runtime.Pilot.Export (exportToS3)
import Katip (Environment (..), initLogEnv, runKatipContextT)

-- We just need a basic spec to test bucket creation and export loop

spec :: Spec
spec = do
    describe "S3 Export Integration" $ do
        aroundAll withMinistack $ do
            it "uploads OSV databases to S3" $ \container -> do
                withSystemTempDirectory "ecluse-osv-test" $ \tmpDir -> do
                    let (host, port) = containerAddress container 4566
                        endpoint = "http://" <> host <> ":" <> T.pack (show port)
                        bucket = "test-osv-bucket"

                    env <- AWS.newEnv AWS.discover
                    let base =
                            AWS.configureService
                                ( (AWS.setEndpoint False (encodeUtf8 host) port S3.defaultService)
                                    { AWS.s3AddressingStyle = AWS.S3AddressingStylePath
                                    }
                                )
                                env
                        regioned = base{AWS.region = AWS.Region' "us-east-1"}

                    -- Retry loop for S3.newCreateBucket
                    let createBucketLoop retries = do
                            catchAny (void $ runResourceT $ AWS.send regioned (S3.newCreateBucket (S3.BucketName bucket))) $ \e -> do
                                if retries > 0
                                    then do
                                        liftIO $ threadDelay 500000 -- 500ms
                                        createBucketLoop (retries - 1)
                                    else throwIO e
                    createBucketLoop (20 :: Int)

                    -- create a dummy DB
                    let dummyDb = tmpDir <> "/dummy.sqlite"
                    liftIO $ writeFile dummyDb "dummy sqlite data"

                    -- Configure AppConfig
                    fullConfig <- case loadConfig [] Nothing of
                        Right c -> pure c
                        Left e -> fail ("Config error: " <> show e)
                    let appCfg =
                            (configApp fullConfig)
                                { cfgVulnerabilityDatabaseBucket = Just bucket
                                , cfgAwsEndpointUrl = Just endpoint
                                }

                    -- Run exportToS3 with Katip context
                    logEnv <- liftIO $ initLogEnv "ecluse-test" (Environment "test")
                    runKatipContextT logEnv () mempty (runResourceT $ exportToS3 (cfgAwsEndpointUrl appCfg >>= parseEndpointUrl) bucket dummyDb)

                    -- Verify upload
                    resp <- runResourceT $ AWS.send base (S3.newListObjectsV2 (S3.BucketName bucket))
                    let objects = fromMaybe [] (S3.contents resp)

                    length objects `shouldBe` 1
                    case objects of
                        [obj] -> S3Object.key obj `shouldBe` S3.ObjectKey "dummy.sqlite"
                        _ -> fail ("Expected 1 object, got " <> show (length objects))
