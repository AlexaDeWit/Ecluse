-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Pilot's advisory-database upload against a real S3, the @ministack@ container's.
It proves the object lands under its own file name through the ambient
@AWS_ENDPOINT_URL@ override a released image carries. Needs a Docker daemon.
-}
module Ecluse.Pilot.S3ExportSpec (
    spec,
) where

import Control.Monad.Trans.Resource (runResourceT)
import Data.Text qualified as T
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe)
import TestContainers (containerAddress)

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3
import Amazonka.S3.ListObjectsV2 qualified as S3
import Amazonka.S3.Types.Object qualified as S3Object
import Ecluse.Config.Ambient (parseEndpointUrl)
import Ecluse.Integration.Ministack (withMinistack)
import Ecluse.Runtime.Pilot.Export (exportToS3)
import Ecluse.Test.Poll (retryingIO)
import Katip (Environment (..), initLogEnv, runKatipContextT)

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

                    -- The readiness wait only proves the port accepts connections, so the S3
                    -- gateway may still be warming when the first CreateBucket lands.
                    retryingIO 21 500_000 (void (runResourceT (AWS.send regioned (S3.newCreateBucket (S3.BucketName bucket)))))

                    let dummyDb = tmpDir <> "/dummy.sqlite"
                    liftIO $ writeFile dummyDb "dummy sqlite data"

                    -- The endpoint override is the ambient AWS_ENDPOINT_URL a released
                    -- image carries, passed straight through as the composition root does.
                    logEnv <- liftIO $ initLogEnv "ecluse-test" (Environment "test")
                    runKatipContextT logEnv () mempty (runResourceT $ exportToS3 Nothing (parseEndpointUrl endpoint) bucket dummyDb)

                    resp <- runResourceT $ AWS.send base (S3.newListObjectsV2 (S3.BucketName bucket))
                    let objects = fromMaybe [] (S3.contents resp)

                    length objects `shouldBe` 1
                    case objects of
                        [obj] -> S3Object.key obj `shouldBe` S3.ObjectKey "dummy.sqlite"
                        _ -> fail ("Expected 1 object, got " <> show (length objects))
