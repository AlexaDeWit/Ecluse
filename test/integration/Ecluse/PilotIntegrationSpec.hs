-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Pilot publication preserves the prior S3 generation after a refused compile or a refused
publication. EPSS failure blocks only mounts whose rules require it.
-}
module Ecluse.PilotIntegrationSpec (spec) where

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3
import Amazonka.S3.ListObjectsV2 qualified as S3
import Amazonka.S3.Types.Object qualified as S3Object
import Codec.Compression.GZip qualified as GZip
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Katip (Severity (ErrorS), runKatipContextT)
import Network.HTTP.Types.Status (status200, status404)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe, shouldNotBe, shouldReturn, shouldSatisfy, shouldThrow)
import UnliftIO.Async (withAsync)
import UnliftIO.Exception (finally, try)

import Ecluse.Config (AppConfig (cfgAdvisories), Config (configApp, configMounts), loadConfig, mountEpssRequirement)
import Ecluse.Config.Ambient (parseEndpointUrl)
import Ecluse.Core.Cve (AdvisoryRange (arEpss), CveDb (..), CveLookup (..))
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI), ecosystemName)
import Ecluse.Core.Osv.Compile (PilotEpssRequired)
import Ecluse.Core.Osv.Publication (ObjectRevision (ObjectRevision), PublicationRefusal (..), Rebaseline (Rebaseline), mkPercent)
import Ecluse.Core.Osv.Schema (EpssRequirement (EpssOptional, EpssRequired), osvDbFileName)
import Ecluse.Core.Osv.Stream (PilotIngestAborted (..))
import Ecluse.Integration.Ministack (MinistackPool, endpointFor, objectMetadata, quietLogEnv, withMinistack)
import Ecluse.Pilot (PilotCompileOptions (..), UploadRequest (..), runExportLoop, runPilotCompile)
import Ecluse.Pilot.Plan (ExportTarget (ExportTarget), exportLoopPlan)
import Ecluse.Runtime.Aws.Env (AwsEndpoint)
import Ecluse.Runtime.Aws.S3 (buildS3Env)
import Ecluse.Runtime.Cve.Sync.Internal (CveFetch (fetchDownload), newS3CveSource, s3CveFetchFor)
import Ecluse.Runtime.Pilot.Export (PublicationStop (..), StoreFault (StoreNoRevision))
import Ecluse.Runtime.Telemetry (telemetryDisabled)
import Ecluse.Test.Corpus.Advisories (AdvisoryInputs (aiOsvZip), fillerTargets, syntheticAdvisories)
import Ecluse.Test.Cve (namesFix)
import Ecluse.Test.Log (memoryLogEnv, memorySeverityLogEnv)
import Ecluse.Test.Osv (CorpusVersion (CorpusV1, CorpusV2), mkMinimalValidDb, osvCorpusZip, osvZipOf)
import Ecluse.Test.OsvDb (denyIfEpssRules, epssFixtureFile, epssFreeRules, openCveDb, stubSourceEnv, withSourceStubs)
import Ecluse.Test.Poll (pollUntil, retryingIO)
import Ecluse.Test.Stub (Captured (capMethod, capPath), Stub, allCaptured, stubBaseUrl, withStub)

spec :: MinistackPool -> Spec
spec pool = aroundAll (withMinistack pool) $ do
    describe "Pilot refuses zero relevant rows before S3 publication" $
        for_ [("empty", osvZipOf []), ("wrong-ecosystem", LBS.readFile "test/unit/fixtures/osv/sample.zip")] $ \(label, rejectedZip) ->
            for_ [False, True] $ \hasPrevious ->
                it (caseLabel ("integration/pilot/native-001/" <> toText label <> "/" <> (if hasPrevious then "replacement" else "first")) (toText (label <> " archive, previous=" <> show hasPrevious :: String))) $ \container ->
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
                            -- An accepted compile reaches the store, starting with its metadata-only read.
                            -- The observer answers it with no revision, so nothing is written.
                            compile target goodZip `shouldThrow` (== PublicationFaulted StoreNoRevision)
                            requests <- allCaptured observer
                            map capMethod requests `shouldBe` ["HEAD"]

    describe "one-shot EPSS failure under the mount's rules" $
        for_ [True, False] $ \epssRule ->
            it (caseLabel ("integration/pilot/native-002/" <> (if epssRule then "epss-required" else "epss-unused")) (toText ("publishes only where no rule depends on EPSS, EPSS rule=" <> show epssRule :: String))) $ \container ->
                -- An EPSS deny and the shipped differential remediation both read EPSS scores.
                for_ (if epssRule then [("pilot-epss-required", Just denyIfEpssRules), ("pilot-epss-shipped", Nothing)] else [("pilot-epss-optional", Just epssFreeRules)]) $ \(bucket, rules) ->
                    withSystemTempDirectory "ecluse-pilot-epss-upload" $ \outDir -> do
                        let endpoint = endpointFor container
                        aws <- createStore endpoint bucket
                        logEnv <- quietLogEnv
                        epss <- LBS.readFile epssFixtureFile
                        let compile target version (feedStatus, feed) = do
                                archive <- osvCorpusZip version
                                withSourceStubs archive (feedStatus, feed) $ \sources epssStub -> do
                                    config <- either (fail . show) pure (loadConfig (oneShotEnv bucket rules sources) Nothing)
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
            it (caseLabel ("integration/pilot/native-003/" <> (if feedUp then "feed-up" else "feed-down")) (toText ("keeps each ecosystem's loop to its own requirement, feed up=" <> show feedUp :: String))) $ \container ->
                withSystemTempDirectory "ecluse-pilot-epss-scheduled" $ \dir -> do
                    let endpoint = endpointFor container
                        bucket = if feedUp then "pilot-scheduled-up" else "pilot-scheduled-down"
                    aws <- createStore endpoint bucket
                    archive <- osvCorpusZip CorpusV1
                    (logEnv, logged) <- memoryLogEnv
                    withStub status200 archive $ \osvStub -> do
                        seeded <- if feedUp then pure [] else seedNpm endpoint bucket dir osvStub >> snapshot aws bucket
                        withStub (if feedUp then status200 else status404) scheduledFeed $ \epssStub -> do
                            config <- either (fail . show) pure (loadConfig (scheduledEnv bucket (dir </> "data") osvStub epssStub) Nothing)
                            let targets = [ExportTarget eco (mountEpssRequirement mount) | (eco, mount) <- Map.toAscList (configMounts config)]
                            plan <- maybe (fail "a configured store planned no export") pure (exportLoopPlan (cfgAdvisories (configApp config)) targets)
                            withAsync (runKatipContextT logEnv () mempty (runExportLoop telemetryDisabled (Just endpoint) config plan)) $ \_ -> do
                                requests <- pollUntil 240 500_000 ((>= 2) . length) (allCaptured epssStub)
                                map capPath requests `shouldBe` replicate 2 "/epss.csv.gz"
                                published <- pollUntil 240 500_000 ((== 2) . length) (snapshot aws bucket)
                                map objectKey published `shouldBe` map artifactKey [Npm, PyPI]
                                if feedUp
                                    then for_ [(Npm, "corpus-mixed"), (PyPI, "redis")] $ \(eco, package) ->
                                        withPublished endpoint bucket eco EpssRequired (dir </> "data" </> osvDbFileName (ecosystemName eco)) $ \lookup' ->
                                            map arEpss <$> cveAdvisoriesFor lookup' package `shouldReturn` [Just 0.75]
                                    else do
                                        -- The loops run side by side, so read npm's object only once its cycle has refused.
                                        let npmObjects = filter ((== artifactKey Npm) . objectKey)
                                            settled (messages, objects) = any npmEpssRefusal messages || npmObjects objects /= seeded
                                        (messages, current) <- pollUntil 240 500_000 settled ((,) <$> logged <*> snapshot aws bucket)
                                        npmObjects current `shouldBe` seeded
                                        messages `shouldSatisfy` any npmEpssRefusal
                                        withPublished endpoint bucket PyPI EpssOptional (dir </> "data" </> osvDbFileName "pypi") $ \lookup' ->
                                            map arEpss <$> cveAdvisoriesFor lookup' "redis" `shouldReturn` [Nothing]

    describe "a one-shot rebaseline binds to the revision it names" $
        for_ [Npm, PyPI] $ \eco ->
            it (caseLabel ("integration/pilot/native-004/" <> ecosystemName eco) "refuses a sharp shrink, then accepts it once under an operator rebaseline of the stored revision") $ \container ->
                withSystemTempDirectory "ecluse-pilot-rebaseline" $ \outDir -> do
                    let endpoint = endpointFor container
                        bucket = "pilot-rebaseline-" <> ecosystemName eco
                    aws <- createStore endpoint bucket
                    logEnv <- quietLogEnv
                    large <- aiOsvZip <$> syntheticAdvisories eco (fillerTargets 400)
                    small <- aiOsvZip <$> syntheticAdvisories eco (fillerTargets 20)
                    epss <- LBS.readFile epssFixtureFile
                    let options = (uploadOptions outDir){pcoEcosystem = ecosystemName eco}
                        compile archive upload = withSourceStubs archive (status200, epss) $ \sources _ -> do
                            config <- either (fail . show) pure (loadConfig (oneShotEnvFor eco bucket sources) Nothing)
                            runPilotCompile logEnv telemetryDisabled (Just endpoint) config options{pcoUpload = upload}
                        refused refusal = \case
                            PublicationRefused found -> refusal found
                            _ -> False
                    void (compile large Upload)
                    before <- snapshot aws bucket
                    compile small Upload `shouldThrow` refused (\case ShrinkPastBaseline _ -> True; _ -> False)
                    allowance <- maybe (fail "fixture percentage") pure (mkPercent 90)
                    compile small (UploadRebaselined (Rebaseline (ObjectRevision "\"not-the-stored-revision\"") allowance))
                        `shouldThrow` refused (\case RebaselineMismatch _ _ -> True; _ -> False)
                    snapshot aws bucket `shouldReturn` before
                    stored <- maybe (fail "no stored revision") pure (listToMaybe (map objectTag before))
                    void (compile small (UploadRebaselined (Rebaseline (ObjectRevision (decodeUtf8 (unETag stored))) allowance)))
                    metadata <- objectMetadata aws bucket (artifactKey eco)
                    rebaselined <- snapshot aws bucket
                    (HashMap.lookup "baseline-adjustment" metadata, HashMap.lookup "baseline-bytes" metadata)
                        `shouldBe` (Just "operator-rebaseline", show . objectSize <$> listToMaybe rebaselined)

    describe "the scheduled loop over a store it cannot order against" $
        for_ [Npm, PyPI] $ \eco ->
            it (caseLabel ("integration/pilot/native-005/" <> ecosystemName eco) "logs the refusal at ERROR, keeps the stored object, and keeps no local artifact") $ \container ->
                withSystemTempDirectory "ecluse-pilot-loop-refusal" $ \dir -> do
                    let endpoint = endpointFor container
                        bucket = "pilot-loop-refusal-" <> ecosystemName eco
                    aws <- createStore endpoint bucket
                    mkMinimalValidDb (dir </> "legacy.db") "pkg-a"
                    legacy <- readFileLBS (dir </> "legacy.db")
                    void (runResourceT (AWS.send aws (S3.newPutObject (S3.BucketName bucket) (artifactKey eco) (AWS.toBody legacy))))
                    before <- snapshot aws bucket
                    archive <- osvCorpusZip CorpusV1
                    epss <- LBS.readFile epssFixtureFile
                    (logEnv, logged) <- memorySeverityLogEnv
                    withSourceStubs archive (status200, epss) $ \sources _ -> do
                        config <- either (fail . show) pure (loadConfig (loopEnvFor eco bucket (dir </> "data") sources) Nothing)
                        let targets = [ExportTarget mounted (mountEpssRequirement mount) | (mounted, mount) <- Map.toAscList (configMounts config)]
                            stoppedLine = T.isInfixOf "Advisory publication stopped" . snd
                        plan <- maybe (fail "a configured store planned no export") pure (exportLoopPlan (cfgAdvisories (configApp config)) targets)
                        withAsync (runKatipContextT logEnv () mempty (runExportLoop telemetryDisabled (Just endpoint) config plan)) $ \_ -> do
                            stops <- filter stoppedLine <$> pollUntil 240 500_000 (any stoppedLine) logged
                            stops `shouldSatisfy` (not . null)
                            map snd stops `shouldSatisfy` all (T.isInfixOf "publication-version")
                            map fst stops `shouldSatisfy` all (== ErrorS)
                    snapshot aws bucket `shouldReturn` before
                    doesFileExist (dir </> "data" </> osvDbFileName (ecosystemName eco)) `shouldReturn` False

-- The supervisor's line for npm's cycle failing on its required EPSS feed.
npmEpssRefusal :: Text -> Bool
npmEpssRefusal message = "pilot-export-npm: " `T.isPrefixOf` message && "requires EPSS enrichment" `T.isInfixOf` message

-- Scores the advisory CorpusV1 shares between npm's corpus-mixed and PyPI's redis.
scheduledFeed :: LByteString
scheduledFeed = GZip.compress "cve,epss,percentile\nCVE-2026-10001,0.875,0.9\nCVE-2026-10006,0.75,0.9\n"

-- An npm mount under these rule patches, reading its advisory sources from @sources@.
oneShotEnv :: Text -> Maybe String -> [(String, String)] -> [(String, String)]
oneShotEnv bucket rules sources =
    [ ("ECLUSE_SERVER__PUBLIC_URL", "https://proxy.example.test")
    , ("ECLUSE_MOUNTS__NPM__ENABLED", "true")
    , ("ECLUSE_ADVISORIES__URL", toString ("s3://" <> bucket))
    ]
        <> [("ECLUSE_MOUNTS__NPM__RULES", patch) | Just patch <- [rules]]
        <> sources

-- One mounted ecosystem under the shipped rules, uploading to @bucket@.
oneShotEnvFor :: Ecosystem -> Text -> [(String, String)] -> [(String, String)]
oneShotEnvFor eco bucket sources =
    [ ("ECLUSE_SERVER__PUBLIC_URL", "https://proxy.example.test")
    , ("ECLUSE_MOUNTS__" <> toString (T.toUpper (ecosystemName eco)) <> "__ENABLED", "true")
    , ("ECLUSE_ADVISORIES__URL", toString ("s3://" <> bucket))
    ]
        <> sources

-- The same mount under the scheduled loop, on an interval no case waits out.
loopEnvFor :: Ecosystem -> Text -> FilePath -> [(String, String)] -> [(String, String)]
loopEnvFor eco bucket dataDir sources =
    oneShotEnvFor eco bucket sources
        <> [("ECLUSE_ADVISORIES__DATA_DIR", dataDir), ("ECLUSE_ADVISORIES__COMPILE_INTERVAL", "3600")]

unETag :: S3.ETag -> ByteString
unETag (S3.ETag tag) = tag

-- npm with an EPSS rule beside PyPI with none, on an interval no case waits out.
scheduledEnv :: Text -> FilePath -> Stub -> Stub -> [(String, String)]
scheduledEnv bucket dataDir osvStub epssStub =
    [ ("ECLUSE_SERVER__PUBLIC_URL", "https://proxy.example.test")
    , ("ECLUSE_MOUNTS__NPM__ENABLED", "true")
    , ("ECLUSE_MOUNTS__NPM__RULES", denyIfEpssRules)
    , ("ECLUSE_MOUNTS__PYPI__ENABLED", "true")
    , ("ECLUSE_MOUNTS__PYPI__RULES", epssFreeRules)
    , ("ECLUSE_ADVISORIES__URL", toString ("s3://" <> bucket))
    , ("ECLUSE_ADVISORIES__DATA_DIR", dataDir)
    , ("ECLUSE_ADVISORIES__COMPILE_INTERVAL", "3600")
    ]
        <> stubSourceEnv osvStub epssStub

uploadOptions :: FilePath -> PilotCompileOptions
uploadOptions outDir = PilotCompileOptions{pcoEcosystem = "npm", pcoOutDir = outDir, pcoUpload = Upload}

-- Publish a qualified npm artifact ahead of the scheduled loop, from a feed that answers.
seedNpm :: AwsEndpoint -> Text -> FilePath -> Stub -> IO ()
seedNpm endpoint bucket dir osvStub = do
    logEnv <- quietLogEnv
    epss <- LBS.readFile epssFixtureFile
    withStub status200 epss $ \epssStub -> do
        config <- either (fail . show) pure (loadConfig (oneShotEnv bucket (Just denyIfEpssRules) (stubSourceEnv osvStub epssStub)) Nothing)
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

objectSize :: StoredObject -> Integer
objectSize (_, _, _, size) = size

artifactKey :: Ecosystem -> S3.ObjectKey
artifactKey = S3.ObjectKey . toText . osvDbFileName . ecosystemName

-- | Download the stored object as the sync does, check it equals the local artifact, and open it under the requirement.
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
