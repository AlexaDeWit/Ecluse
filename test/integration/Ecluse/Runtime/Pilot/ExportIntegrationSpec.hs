-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Pilot's advisory-database upload against a real S3, the @ministack@ container's.
It proves the object lands under the key the configured store derives, through the
ambient @AWS_ENDPOINT_URL@ override a released image carries. Needs a Docker daemon.
-}
module Ecluse.Runtime.Pilot.ExportIntegrationSpec (
    spec,
) where

import Control.Monad.Trans.Resource (runResourceT)
import Data.Text qualified as T
import System.FilePath (takeFileName)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe)
import TestContainers (containerAddress)

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3
import Amazonka.S3.ListObjectsV2 qualified as S3
import Amazonka.S3.Types.Object qualified as S3Object
import Ecluse.Config.AdvisoryStore (advisoryObjectKey, advisoryStoreBucket, mkAdvisoryStoreUrl)
import Ecluse.Config.Ambient (parseEndpointUrl)
import Ecluse.Integration.Ministack (withMinistack)
import Ecluse.Runtime.Aws.S3 (buildS3Env)
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
                        endpointUrl = "http://" <> host <> ":" <> T.pack (show port)

                    -- Bucket and object key both derive from the configured store, the way the
                    -- export loop derives them, so the two cannot drift apart unnoticed.
                    store <- either (fail . toString) pure (mkAdvisoryStoreUrl "advisories.url" "s3://test-osv-bucket")
                    let bucket = advisoryStoreBucket store

                    -- The override is the ambient AWS_ENDPOINT_URL a released image carries,
                    -- resolved through the same parser the boot uses.
                    endpoint <- either (const (fail ("S3ExportSpec: unparseable endpoint for " <> toString host))) pure (parseEndpointUrl endpointUrl)
                    base <- buildS3Env (Just endpoint)
                    let regioned = base{AWS.region = AWS.Region' "us-east-1"}

                    -- The readiness wait only proves the port accepts connections, so the S3
                    -- gateway may still be warming when the first CreateBucket lands.
                    retryingIO 21 500_000 (void (runResourceT (AWS.send regioned (S3.newCreateBucket (S3.BucketName bucket)))))

                    let dummyDb = tmpDir <> "/dummy.sqlite"
                    liftIO $ writeFile dummyDb "dummy sqlite data"

                    logEnv <- liftIO $ initLogEnv "ecluse-test" (Environment "test")
                    let objectKey = advisoryObjectKey store (takeFileName dummyDb)
                    runKatipContextT logEnv () mempty (runResourceT $ exportToS3 Nothing (Just endpoint) bucket objectKey dummyDb)

                    resp <- runResourceT $ AWS.send base (S3.newListObjectsV2 (S3.BucketName bucket))
                    let objects = fromMaybe [] (S3.contents resp)

                    length objects `shouldBe` 1
                    case objects of
                        [obj] -> S3Object.key obj `shouldBe` S3.ObjectKey "dummy.sqlite"
                        _ -> fail ("Expected 1 object, got " <> show (length objects))
