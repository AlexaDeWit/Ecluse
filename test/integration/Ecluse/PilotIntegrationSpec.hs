-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Pilot publication against the S3 emulator. A refused compile, OSV or required EPSS, leaves the
previous object's identity and modification time as they were. An EPSS failure where the mount's
rules do not depend on it still publishes the OSV data, in both the one-shot and scheduled modes.
-}
module Ecluse.PilotIntegrationSpec (spec) where

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3
import Amazonka.S3.ListObjectsV2 qualified as S3
import Amazonka.S3.Types.Object qualified as S3Object
import Codec.Compression.GZip qualified as GZip
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Network.HTTP.Types.Status (status200, status404)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe, shouldNotBe, shouldReturn, shouldSatisfy, shouldThrow)
import UnliftIO.Async (withAsync)
import UnliftIO.Exception (finally, try)

import Ecluse.Config (AppConfig (cfgAdvisories), Config (configApp, configMounts), loadConfig, mountEpssRequirement)
import Ecluse.Config.Ambient (parseEndpointUrl)
import Ecluse.Core.Cve (AdvisoryRange (arEpss), CveDb (..), CveLookup (..), openCveDb)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI), ecosystemName)
import Ecluse.Core.Osv.Compile (PilotEpssRequired)
import Ecluse.Core.Osv.Schema (EpssRequirement (EpssOptional, EpssRequired), osvDbFileName)
import Ecluse.Core.Osv.Stream (PilotIngestAborted (..))
import Ecluse.Integration.Ministack (endpointFor, quietLogEnv, withMinistack)
import Ecluse.Pilot (PilotCompileOptions (..), runExportLoop, runPilotCompile)
import Ecluse.Pilot.Plan (ExportTarget (ExportTarget), exportLoopPlan)
import Ecluse.Runtime.Aws.Env (AwsEndpoint)
import Ecluse.Runtime.Aws.S3 (buildS3Env)
import Ecluse.Runtime.Cve.Sync.Internal (CveFetch (fetchDownload), newS3CveSource, s3CveFetchFor)
import Ecluse.Runtime.Telemetry (telemetryDisabled)
import Ecluse.Test.Cve (namesFix)
import Ecluse.Test.Log (runQuietKatip)
import Ecluse.Test.Osv (CorpusVersion (CorpusV1, CorpusV2), osvCorpusZip, osvZipOf)
import Ecluse.Test.OsvDb (denyIfEpssRules, epssFixtureFile, stubSourceEnv, withSourceStubs)
import Ecluse.Test.Poll (pollUntil, retryingIO)
import Ecluse.Test.Stub (Captured (capMethod, capPath), Stub, allCaptured, stubBaseUrl, withStub)

spec :: Spec
spec = aroundAll withMinistack $ do
    describe "Pilot refuses zero relevant rows before S3 publication" $
        for_ [("empty", osvZipOf []), ("wrong-ecosystem", LBS.readFile "test/unit/fixtures/osv/sample.zip")] $ \(label, rejectedZip) ->
            for_ [False, True] $ \hasPrevious ->
                it (label <> " archive, previous=" <> show hasPrevious) $ \container ->
                    withSystemTempDirectory "ecluse-pilot-rejected" $ \outDir -> do
                        let endpoint = endpointFor container
                            bucket = "pilot-" <> toText label <> if hasPrevious then "-replacement" else "-first"
                        aws <- createStore endpoint bucket
                        logEnv <- quietLogEnv
                        epssData <- LBS.readFile epssFixtureFile
                        goodZip <- osvCorpusZip CorpusV1
                        badZip <- rejectedZip
                        let compile target zipData = withSourceStubs zipData (status200, epssData) $ \sources _ -> do
                                let env = ("ECLUSE_ADVISORIES__URL", toString ("s3://" <> bucket)) : sources
                                config <- either (fail . show) pure (loadConfig env Nothing)
                                runPilotCompile logEnv telemetryDisabled (Just target) config (uploadOptions outDir){pcoEcosystem = "pypi"}
                        when hasPrevious (void (compile endpoint goodZip))
                        before <- snapshot aws bucket
                        length before `shouldBe` if hasPrevious then 1 else 0
                        withStub status200 LBS.empty $ \observer -> do
                            target <- either (fail . show) pure (parseEndpointUrl (stubBaseUrl observer))
                            compile target badZip `shouldThrow` (\(PilotIngestAborted _) -> True)
                            allCaptured observer >>= (`shouldBe` [])
                            after <- snapshot aws bucket
                            after `shouldBe` before
                            void (compile target goodZip)
                            requests <- allCaptured observer
                            map capMethod requests `shouldBe` ["PUT"]

    describe "one-shot EPSS failure under the mount's rules" $
        for_ [True, False] $ \epssRule ->
            it ("publishes only where no rule depends on EPSS, EPSS rule=" <> show epssRule) $ \container ->
                withSystemTempDirectory "ecluse-pilot-epss-upload" $ \outDir -> do
                    let endpoint = endpointFor container
                        bucket = if epssRule then "pilot-epss-required" else "pilot-epss-optional"
                    aws <- createStore endpoint bucket
                    logEnv <- quietLogEnv
                    epss <- LBS.readFile epssFixtureFile
                    let compile target version (feedStatus, feed) = do
                            archive <- osvCorpusZip version
                            withSourceStubs archive (feedStatus, feed) $ \sources epssStub -> do
                                config <- either (fail . show) pure (loadConfig (oneShotEnv bucket epssRule sources) Nothing)
                                outcome <- try (runPilotCompile logEnv telemetryDisabled (Just target) config (uploadOptions outDir))
                                map capPath <$> allCaptured epssStub `shouldReturn` ["/epss.csv.gz"]
                                pure (outcome :: Either PilotEpssRequired FilePath)
                    path <- compile endpoint CorpusV1 (status200, epss) >>= either (fail . displayException) pure
                    before <- snapshot aws bucket
                    length before `shouldBe` 1
                    if epssRule
                        then withStub status200 LBS.empty $ \observer -> do
                            target <- either (fail . show) pure (parseEndpointUrl (stubBaseUrl observer))
                            compile target CorpusV2 (status404, "") >>= (`shouldSatisfy` isLeft)
                            allCaptured observer `shouldReturn` []
                            snapshot aws bucket `shouldReturn` before
                        else do
                            compile endpoint CorpusV2 (status404, "") `shouldReturn` Right path
                            after <- snapshot aws bucket
                            map objectTag after `shouldNotBe` map objectTag before
                            withPublished endpoint bucket Npm EpssOptional path $ \lookup' -> do
                                namesFix lookup' "corpus-revoked" "1.2.0" `shouldReturn` True
                                map arEpss <$> cveAdvisoriesFor lookup' "corpus-revoked" `shouldReturn` [Nothing]

    describe "scheduled EPSS enrichment per ecosystem" $
        for_ [True, False] $ \feedUp ->
            it ("keeps each ecosystem's loop to its own requirement, feed up=" <> show feedUp) $ \container ->
                withSystemTempDirectory "ecluse-pilot-epss-scheduled" $ \dir -> do
                    let endpoint = endpointFor container
                        bucket = if feedUp then "pilot-scheduled-up" else "pilot-scheduled-down"
                    aws <- createStore endpoint bucket
                    archive <- osvCorpusZip CorpusV1
                    withStub status200 archive $ \osvStub -> do
                        seeded <- if feedUp then pure [] else seedNpm endpoint bucket dir osvStub >> snapshot aws bucket
                        withStub (if feedUp then status200 else status404) scheduledFeed $ \epssStub -> do
                            config <- either (fail . show) pure (loadConfig (scheduledEnv bucket (dir </> "data") osvStub epssStub) Nothing)
                            let targets = [ExportTarget eco (mountEpssRequirement mount) | (eco, mount) <- Map.toAscList (configMounts config)]
                            plan <- maybe (fail "a configured store planned no export") pure (exportLoopPlan (cfgAdvisories (configApp config)) targets)
                            withAsync (runQuietKatip (runExportLoop telemetryDisabled (Just endpoint) config plan)) $ \_ -> do
                                requests <- pollUntil 240 500_000 ((>= 2) . length) (allCaptured epssStub)
                                map capPath requests `shouldBe` replicate 2 "/epss.csv.gz"
                                published <- pollUntil 240 500_000 ((== 2) . length) (snapshot aws bucket)
                                map objectKey published `shouldBe` map artifactKey [Npm, PyPI]
                                if feedUp
                                    then for_ [(Npm, "corpus-mixed"), (PyPI, "redis")] $ \(eco, package) ->
                                        withPublished endpoint bucket eco EpssRequired (dir </> "data" </> osvDbFileName (ecosystemName eco)) $ \lookup' ->
                                            map arEpss <$> cveAdvisoriesFor lookup' package `shouldReturn` [Just 0.75]
                                    else do
                                        filter ((== artifactKey Npm) . objectKey) published `shouldBe` seeded
                                        withPublished endpoint bucket PyPI EpssOptional (dir </> "data" </> osvDbFileName "pypi") $ \lookup' ->
                                            map arEpss <$> cveAdvisoriesFor lookup' "redis" `shouldReturn` [Nothing]

-- Scores the advisory CorpusV1 shares between npm's corpus-mixed and PyPI's redis.
scheduledFeed :: LByteString
scheduledFeed = GZip.compress "cve,epss,percentile\nCVE-2026-10001,0.875,0.9\nCVE-2026-10006,0.75,0.9\n"

-- An npm mount, with or without an EPSS rule, reading its advisory sources from @sources@.
oneShotEnv :: Text -> Bool -> [(String, String)] -> [(String, String)]
oneShotEnv bucket epssRule sources =
    [ ("ECLUSE_SERVER__PUBLIC_URL", "https://proxy.example.test")
    , ("ECLUSE_MOUNTS__NPM__ENABLED", "true")
    , ("ECLUSE_ADVISORIES__URL", toString ("s3://" <> bucket))
    ]
        <> [("ECLUSE_MOUNTS__NPM__RULES", denyIfEpssRules) | epssRule]
        <> sources

-- npm with an EPSS rule beside PyPI without one, on an interval no case waits out.
scheduledEnv :: Text -> FilePath -> Stub -> Stub -> [(String, String)]
scheduledEnv bucket dataDir osvStub epssStub =
    [ ("ECLUSE_SERVER__PUBLIC_URL", "https://proxy.example.test")
    , ("ECLUSE_MOUNTS__NPM__ENABLED", "true")
    , ("ECLUSE_MOUNTS__NPM__RULES", denyIfEpssRules)
    , ("ECLUSE_MOUNTS__PYPI__ENABLED", "true")
    , ("ECLUSE_ADVISORIES__URL", toString ("s3://" <> bucket))
    , ("ECLUSE_ADVISORIES__DATA_DIR", dataDir)
    , ("ECLUSE_ADVISORIES__COMPILE_INTERVAL", "3600")
    ]
        <> stubSourceEnv osvStub epssStub

uploadOptions :: FilePath -> PilotCompileOptions
uploadOptions outDir = PilotCompileOptions{pcoEcosystem = "npm", pcoOutDir = outDir, pcoUpload = True}

-- Publish a qualified npm artifact ahead of the scheduled loop, from a feed that answers.
seedNpm :: AwsEndpoint -> Text -> FilePath -> Stub -> IO ()
seedNpm endpoint bucket dir osvStub = do
    logEnv <- quietLogEnv
    epss <- LBS.readFile epssFixtureFile
    withStub status200 epss $ \epssStub -> do
        config <- either (fail . show) pure (loadConfig (oneShotEnv bucket True (stubSourceEnv osvStub epssStub)) Nothing)
        void (runPilotCompile logEnv telemetryDisabled (Just endpoint) config (uploadOptions (dir </> "seed")))

createStore :: AwsEndpoint -> Text -> IO AWS.Env
createStore endpoint bucket = do
    base <- buildS3Env (Just endpoint)
    let aws = base{AWS.region = AWS.Region' "us-east-1"}
    retryingIO 21 500_000 (void (runResourceT (AWS.send aws (S3.newCreateBucket (S3.BucketName bucket)))))
    pure aws

-- A stored object's key, ETag, modification time, and size, so an unchanged store compares equal.
type StoredObject = (S3.ObjectKey, S3.ETag, AWS.RFC822, Integer)

snapshot :: AWS.Env -> Text -> IO [StoredObject]
snapshot aws bucket = do
    response <- runResourceT (AWS.send aws (S3.newListObjectsV2 (S3.BucketName bucket)))
    pure [(S3Object.key object, S3Object.eTag object, S3Object.lastModified object, S3Object.size object) | object <- fromMaybe [] (S3.contents response)]

objectKey :: StoredObject -> S3.ObjectKey
objectKey (key, _, _, _) = key

objectTag :: StoredObject -> S3.ETag
objectTag (_, tag, _, _) = tag

artifactKey :: Ecosystem -> S3.ObjectKey
artifactKey = S3.ObjectKey . toText . osvDbFileName . ecosystemName

{- | Download the ecosystem's stored object as the sync does, check it is the local artifact byte
for byte, and open it under the given requirement.
-}
withPublished :: AwsEndpoint -> Text -> Ecosystem -> EpssRequirement -> FilePath -> (CveLookup -> IO a) -> IO a
withPublished endpoint bucket eco requirement compiled use = do
    source <- newS3CveSource (Just endpoint)
    let downloaded = compiled <> ".downloaded"
    fetchDownload (s3CveFetchFor source bucket (toText (osvDbFileName (ecosystemName eco))) (512 * 1024 * 1024)) downloaded >>= \case
        Left fault -> fail ("the published artifact did not download: " <> show fault)
        Right _ -> pass
    expected <- readFileBS compiled
    readFileBS downloaded `shouldReturn` expected
    openCveDb eco requirement downloaded >>= \case
        Left rejection -> fail ("the published artifact was refused: " <> show rejection)
        Right db -> use (cveDbLookup db) `finally` cveDbClose db
