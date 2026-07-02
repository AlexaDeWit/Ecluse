{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Ecluse.PilotS3ExportSpec (spec) where

import Amazonka qualified as AWS
import Amazonka.Auth qualified as AWS.Auth
import Amazonka.S3 qualified as S3
import Amazonka.S3.ListObjectsV2 qualified as S3
import Amazonka.S3.Types.Object (object_key)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (unpack)
import Katip (Environment (..), Katip (..), KatipContext (..), LogEnv, initLogEnv)
import System.Environment (setEnv)
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldSatisfy)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (threadDelay)

import Lens.Micro ((^.))

import Ecluse.Boot (BootEnv (..))
import Ecluse.Config (Config (..), loadConfig)
import Ecluse.Core.Queue.Sqs (SqsEndpoint (endpointHost, endpointPort))
import Ecluse.Integration.Ministack (endpointFor, withMinistack)
import Ecluse.Pilot.Pipeline (runExportPipeline)
import Ecluse.Telemetry (telemetryDisabled)
import Ecluse.Test.Stub (stubBaseUrl, withStub)
import Network.HTTP.Types.Status (status200)

newtype TestM a = TestM {runTestM :: ReaderT LogEnv (ResourceT IO) a}
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadResource, MonadThrow, PrimMonad, MonadUnliftIO)

instance Katip TestM where
    getLogEnv = TestM ask
    localLogEnv f (TestM m) = TestM (local f m)

instance KatipContext TestM where
    getKatipContext = pure mempty
    localKatipContext _ m = m
    getKatipNamespace = pure mempty
    localKatipNamespace _ m = m

spec :: Spec
spec = around withMinistack $ describe "Pilot S3 Export Pipeline" $ do
    it "compiles an OSV zip and uploads the SQLite DB to S3" $ \container -> do
        le <- initLogEnv "test" (Environment "test")

        -- configure AWS S3 endpoint
        let endpoint = endpointFor container
            hostStr = endpointHost endpoint
            ipHost = if hostStr == "localhost" then "127.0.0.1" else hostStr
            endpointUrl = "http://" <> ipHost <> ":" <> show (endpointPort endpoint)
            bucketName = "test.osv.bucket"

        -- wait for LocalStack to fully initialize S3
        liftIO $ threadDelay 5000000

        setEnv "AWS_ENDPOINT_URL_S3" (unpack endpointUrl)
        setEnv "AWS_ACCESS_KEY_ID" "test"
        setEnv "AWS_SECRET_ACCESS_KEY" "test"
        setEnv "AWS_REGION" "us-east-1"

        envAws <- AWS.Auth.fromKeys "test" "test" <$> AWS.newEnvNoAuth
        let regioned = envAws{AWS.region = AWS.Region' "us-east-1"}
            envS3 =
                AWS.configureService
                    ( AWS.setEndpoint
                        False
                        (encodeUtf8 (toText ipHost))
                        (endpointPort endpoint)
                        S3.defaultService
                    )
                    regioned

        -- create bucket in ministack
        _ <- runResourceT $ AWS.send envS3 (S3.newCreateBucket (S3.BucketName bucketName))

        -- build configuration
        let cfgResult =
                loadConfig
                    [ ("ECLUSE_PILOT_S3_BUCKET", unpack bucketName)
                    , ("ECLUSE_OSV_SCRATCH_DIR", "/tmp/osv-scratch")
                    ]
                    Nothing
        (cfg, c) <- case cfgResult of
            Left err -> fail (show err)
            Right c -> pure (configApp c, c)

        let bootEnv =
                BootEnv
                    { beConfig = cfg
                    , beConfigFull = c
                    , beTelemetry = telemetryDisabled
                    , beLogEnv = le
                    }

        -- start the stub OSV server
        zipData <- LBS.readFile "test/unit/fixtures/osv/sample.zip"
        withStub status200 zipData $ \stub -> do
            let url = unpack (stubBaseUrl stub) <> "/sample.zip"

            runResourceT $
                runReaderT
                    ( runTestM $
                        runExportPipeline bootEnv "npm" url
                    )
                    le

        -- Verify that the file exists in the S3 bucket
        listRes <- runResourceT $ AWS.send envS3 (S3.newListObjectsV2 (S3.BucketName bucketName))
        let objects = listRes ^. S3.listObjectsV2Response_contents
        objects `shouldSatisfy` isJust
        objs <- maybe (fail "Expected objects") pure objects
        length objs `shouldBe` 1

        obj <- case objs of
            [x] -> pure x
            _ -> fail "Expected exactly 1 object"
        let key = obj ^. object_key
        -- In compileOsvToSqlite, it names it ecosystem-v0.1.0-osv.db
        let expectedKey = "npm-v0.1.0-osv.db"
        key `shouldBe` S3.ObjectKey expectedKey
