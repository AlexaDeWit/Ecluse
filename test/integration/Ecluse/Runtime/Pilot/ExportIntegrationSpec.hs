-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Advisory publication and consumer polling against the ministack S3 service.
Requires a Docker daemon.
-}
module Ecluse.Runtime.Pilot.ExportIntegrationSpec (
    spec,
) where

import Control.Monad.Trans.Resource (runResourceT)
import Data.Text qualified as T
import System.FilePath (takeFileName)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, aroundAll, describe, expectationFailure, it, shouldBe, shouldReturn, shouldSatisfy)
import TestContainers (containerAddress)

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3
import Amazonka.S3.Lens qualified as S3L
import Amazonka.S3.ListObjectsV2 qualified as S3
import Amazonka.S3.Types.Object qualified as S3Object
import Ecluse.Config.AdvisoryStore (advisoryObjectKey, advisoryStoreBucket, mkAdvisoryStoreUrl)
import Ecluse.Config.Ambient (parseEndpointUrl)
import Ecluse.Core.Cve.Slot (AdvisorySource (..), currentAdvisorySource, generationInstalledAt, newCveSlot)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Integration.Ministack (withMinistack)
import Ecluse.Runtime.Aws.S3 (buildS3Env)
import Ecluse.Runtime.Cve.Sync (SyncEnv (..), SyncOutcome (..), newS3CveSource, s3CveFetchFor, syncStep)
import Ecluse.Runtime.Pilot.Export (exportToS3)
import Ecluse.Test.Osv (mkMinimalValidDb)
import Ecluse.Test.Poll (pollUntil, retryingIO)
import Katip (Environment (..), initLogEnv, runKatipContextT)
import Lens.Micro ((^.))

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

            it "uploads again when the artifact's bytes have not changed" $ \container -> do
                -- Republishing one accepted artifact preserves its bytes, including built_at.
                withSystemTempDirectory "ecluse-osv-republish" $ \tmpDir -> do
                    let (host, port) = containerAddress container 4566
                        endpointUrl = "http://" <> host <> ":" <> T.pack (show port)
                    store <- either (fail . toString) pure (mkAdvisoryStoreUrl "advisories.url" "s3://test-osv-republish-bucket")
                    let bucket = advisoryStoreBucket store
                    endpoint <- either (const (fail ("S3ExportSpec: unparseable endpoint for " <> toString host))) pure (parseEndpointUrl endpointUrl)
                    base <- buildS3Env (Just endpoint)
                    let regioned = base{AWS.region = AWS.Region' "us-east-1"}
                    retryingIO 21 500_000 (void (runResourceT (AWS.send regioned (S3.newCreateBucket (S3.BucketName bucket)))))

                    let dbPath = tmpDir <> "/unchanged.sqlite"
                        objectKey = advisoryObjectKey store (takeFileName dbPath)
                    mkMinimalValidDb dbPath "pkg-a"
                    logEnv <- liftIO $ initLogEnv "ecluse-test" (Environment "test")
                    let export = runKatipContextT logEnv () mempty (runResourceT $ exportToS3 Nothing (Just endpoint) bucket objectKey dbPath)
                        storedObject = do
                            resp <- runResourceT $ AWS.send base (S3.newListObjectsV2 (S3.BucketName bucket))
                            pure (listToMaybe (fromMaybe [] (S3.contents resp)))

                    export
                    published <- storedObject
                    -- Without this the comparison below would pass on an absent first listing.
                    published `shouldSatisfy` isJust
                    source <- newS3CveSource (Just endpoint)
                    slot <- newCveSlot
                    let env = SyncEnv (s3CveFetchFor source bucket objectKey (512 * 1024 * 1024)) Npm (tmpDir <> "/consumer.sqlite") slot
                    initialSync <- syncStep env Nothing
                    acceptedEtag <- case initialSync of
                        SyncSwapped etag _ -> pure etag
                        other -> fail ("expected first artifact swap, got " <> show other)
                    installed <- generationInstalledAt slot
                    (asPushedAt =<<) <$> currentAdvisorySource slot
                        `shouldReturn` ((^. S3L.object_lastModified) <$> published)

                    -- The store stamps whole seconds, so the export repeats until the stamp has
                    -- to have moved. A publisher that wrote only on a change never moves it.
                    let advancedPast before object = fmap S3Object.lastModified object > fmap S3Object.lastModified before
                    pollUntil 21 500_000 id (export >> (advancedPast published <$> storedObject))
                        >>= (`shouldBe` True)

                    -- The bytes never changed, so the object is the same one, re-published.
                    republished <- storedObject
                    fmap S3Object.eTag republished `shouldBe` fmap S3Object.eTag published
                    syncStep env (Just acceptedEtag) >>= \case
                        SyncUnchanged -> pass
                        other -> expectationFailure ("expected metadata-only observation, got " <> show other)
                    (asPushedAt =<<) <$> currentAdvisorySource slot
                        `shouldReturn` ((^. S3L.object_lastModified) <$> republished)
                    generationInstalledAt slot `shouldReturn` installed
