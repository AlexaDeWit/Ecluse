-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.PilotSpec (spec) where

import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text (unpack)
import Data.Text qualified as T
import Database.SQLite.Simple (close, open, query_)
import Katip (LogEnv)
import Network.HTTP.Types.Status (Status, status200, status404)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import UnliftIO (timeout)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO)

import Ecluse.Composition.Support (expectConfig)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Osv.Compile (PilotEpssRequired (perEcosystem))
import Ecluse.Core.Osv.Schema (EpssRequirement (EpssOptional))
import Ecluse.Core.Supervision (BackoffSchedule (BackoffSchedule, bsBaseMicros, bsCapMicros))
import Ecluse.Pilot (PilotCompileOptions (..), PilotUploadUnconfigured (..), runPilotCompile, superviseExportCycles)
import Ecluse.Pilot.Plan (ExportTarget (ExportTarget, etEcosystem, etEpss))
import Ecluse.Runtime.Telemetry (telemetryDisabled)
import Ecluse.Test.Log (captureJsonLog, newTestLogEnv, runQuietKatip)
import Ecluse.Test.OsvDb (denyIfEpssRules, epssFixtureFile, metaOf, scoresOf, stubSourceEnv)
import Ecluse.Test.Stub (Captured (capPath), Stub, allCaptured, stubBaseUrl, withStub)

spec :: Spec
spec = do
    describe "superviseExportCycles (one supervised cycle loop per ecosystem)" $
        it "keeps a faulting ecosystem's backoff off every other ecosystem's cadence" $ do
            -- The schedule is a five-second fixed retry, so a shared loop would let the healthy
            -- ecosystem tick at most once inside the window. Its own loop ticks throughout.
            faulted <- newIORef (0 :: Int)
            healthy <- newIORef (0 :: Int)
            let schedule = BackoffSchedule{bsBaseMicros = 5_000_000, bsCapMicros = 5_000_000}
                targetFor eco = ExportTarget{etEcosystem = eco, etEpss = EpssOptional}
                cycleFor target = case etEcosystem target of
                    Npm -> do
                        atomicModifyIORef' faulted (\n -> (n + 1, ()))
                        throwIO FeedDown
                    _ -> do
                        atomicModifyIORef' healthy (\n -> (n + 1, ()))
                        threadDelay 1_000
            _ <- timeout 200_000 (runQuietKatip (superviseExportCycles schedule (targetFor Npm :| [targetFor PyPI]) cycleFor))
            readIORef healthy >>= (`shouldSatisfy` (>= 5))
            -- The faulting pass spends its own cadence and nothing else's.
            readIORef faulted `shouldReturn` 1

    describe "runPilotCompile (one-shot compile mode)" $ do
        it "compiles a served OSV zip into the requested directory and returns the artifact's path" $ do
            le <- newTestLogEnv
            appCfg <- expectConfig [] Nothing
            zipData <- LBS.readFile "test/unit/fixtures/osv/sample.zip"
            epssData <- LBS.readFile epssFixtureFile
            withSystemTempDirectory "ecluse-pilot-compile" $ \outDir -> do
                dbFile <- withStub status200 zipData $ \stub ->
                    withStub status200 epssData $ \epssStub ->
                        runPilotCompile
                            le
                            telemetryDisabled
                            Nothing
                            appCfg
                            (compileOptions (stubBaseUrl stub) (stubBaseUrl epssStub) outDir)
                takeDirectory dbFile `shouldBe` outDir
                exists <- doesFileExist dbFile
                exists `shouldBe` True
                conn <- open dbFile
                rows <- query_ conn "SELECT package_name, epss_score FROM package_vulnerability_ranges" :: IO [(Text, Maybe Double)]
                close conn
                -- The one-shot mode joins the EPSS feed too, so the row carries the score
                -- the fixture feed holds for the advisory's CVE alias.
                rows `shouldBe` [("hono", Just 0.75)]

        it "reads the configured epssFeedUrl when the run passes no override" $ do
            le <- newTestLogEnv
            zipData <- LBS.readFile "test/unit/fixtures/osv/sample.zip"
            epssData <- LBS.readFile epssFixtureFile
            withSystemTempDirectory "ecluse-pilot-compile" $ \outDir ->
                withStub status200 zipData $ \stub ->
                    withStub status200 epssData $ \epssStub -> do
                        -- The scheduled daemon passes no override either, so a feed URL
                        -- hardcoded again would reach the real upstream instead of this stub.
                        let feedUrl = unpack (stubBaseUrl epssStub) <> "/epss.csv.gz"
                        appCfg <- expectConfig [("ECLUSE_ADVISORIES__EPSS_FEED_URL", feedUrl)] Nothing
                        let opts = (compileOptions (stubBaseUrl stub) (stubBaseUrl epssStub) outDir){pcoEpssSource = Nothing}
                        dbFile <- runPilotCompile le telemetryDisabled Nothing appCfg opts
                        conn <- open dbFile
                        rows <- query_ conn "SELECT package_name, epss_score FROM package_vulnerability_ranges" :: IO [(Text, Maybe Double)]
                        close conn
                        rows `shouldBe` [("hono", Just 0.75)]

        for_ [("with", Just denyIfEpssRules), ("without", Nothing)] $ \(label, rule) ->
            it ("joins the feed's scores " <> label <> " an EPSS rule, fetching the feed once") $
                withSystemTempDirectory "ecluse-pilot-policy" $ \outDir ->
                    withPolicyCompile outDir rule (status200, Nothing) $ \compile epssStub -> do
                        dbFile <- newTestLogEnv >>= compile
                        scoresOf dbFile `shouldReturn` [Just 0.75]
                        Map.lookup "epss_status" <$> metaOf dbFile `shouldReturn` Just "available"
                        map capPath <$> allCaptured epssStub `shouldReturn` ["/epss.csv.gz"]

        it "publishes nothing and fails when a mount with an EPSS rule meets a failed feed" $
            withSystemTempDirectory "ecluse-pilot-policy" $ \outDir -> do
                dbFile <- withPolicyCompile outDir (Just denyIfEpssRules) (status200, Nothing) $ \compile _ -> newTestLogEnv >>= compile
                published <- readFileBS dbFile
                withPolicyCompile outDir (Just denyIfEpssRules) (status404, Just "") $ \failing epssStub -> do
                    (newTestLogEnv >>= failing) `shouldThrow` (\refusal -> perEcosystem refusal == "npm")
                    map capPath <$> allCaptured epssStub `shouldReturn` ["/epss.csv.gz"]
                readFileBS dbFile `shouldReturn` published
                listDirectory outDir `shouldReturn` [takeFileName dbFile]

        it "publishes OSV data with unavailable enrichment, and returns, when a mount without one meets a failed feed" $
            withSystemTempDirectory "ecluse-pilot-policy" $ \outDir ->
                withPolicyCompile outDir Nothing (status404, Just "") $ \compile epssStub -> do
                    (dbFile, logged) <- captureJsonLog compile
                    scoresOf dbFile `shouldReturn` [Nothing]
                    Map.lookup "epss_status" <$> metaOf dbFile `shouldReturn` Just "unavailable"
                    map capPath <$> allCaptured epssStub `shouldReturn` ["/epss.csv.gz"]
                    filter (T.isInfixOf "EPSS enrichment unavailable for npm") (lines logged)
                        `shouldSatisfy` \warned -> length warned == 1 && all (T.isInfixOf "\"sev\":\"Warning\"") warned
                    logged `shouldSatisfy` (not . T.isInfixOf "Aborting OSV compile")

        it "requires the feed, and warns, for an ecosystem the configuration does not mount" $ do
            zipData <- LBS.readFile "test/unit/fixtures/osv/sample.zip"
            withSystemTempDirectory "ecluse-pilot-unmounted" $ \outDir ->
                withStub status200 zipData $ \osvStub ->
                    withStub status404 "" $ \epssStub -> do
                        config <- expectConfig (stubSourceEnv osvStub epssStub) Nothing
                        let opts = (compileOptions "" "" outDir){pcoSource = Nothing, pcoEpssSource = Nothing}
                        (_, logged) <- captureJsonLog $ \logEnv ->
                            runPilotCompile logEnv telemetryDisabled Nothing config opts
                                `shouldThrow` (\refusal -> perEcosystem refusal == "npm")
                        filter (T.isInfixOf "mounts no npm ecosystem") (lines logged)
                            `shouldSatisfy` \warned -> length warned == 1 && all (T.isInfixOf "\"sev\":\"Warning\"") warned
                        doesFileExist (outDir </> "npm-osv-schema4.db") `shouldReturn` False

        it "fails loudly when an upload is requested without a configured advisory store" $ do
            le <- newTestLogEnv
            appCfg <- expectConfig [] Nothing
            zipData <- LBS.readFile "test/unit/fixtures/osv/sample.zip"
            epssData <- LBS.readFile epssFixtureFile
            withSystemTempDirectory "ecluse-pilot-compile" $ \outDir -> do
                let action = withStub status200 zipData $ \stub ->
                        withStub status200 epssData $ \epssStub ->
                            runPilotCompile
                                le
                                telemetryDisabled
                                Nothing
                                appCfg
                                (compileOptions (stubBaseUrl stub) (stubBaseUrl epssStub) outDir){pcoUpload = True}
                action `shouldThrow` (== PilotUploadUnconfigured)

        it "refuses that upload before it compiles anything" $ do
            le <- newTestLogEnv
            appCfg <- expectConfig [] Nothing
            withSystemTempDirectory "ecluse-pilot-compile" $ \dir -> do
                -- A file stands where the output directory's parent would be, so
                -- 'compileOsvToSqlite's createDirectoryIfMissing fails if the run reaches it.
                writeFileText (dir </> "blocker") ""
                let outDir = dir </> "blocker" </> "out"
                    opts = (compileOptions unreachable unreachable outDir){pcoUpload = True}
                runPilotCompile le telemetryDisabled Nothing appCfg opts
                    `shouldThrow` (== PilotUploadUnconfigured)

-- | The upstream outage one ecosystem's cycle suffers while the other keeps compiling.
data FeedDown = FeedDown
    deriving stock (Show)

instance Exception FeedDown

{- | Hand the case a compile of npm into @outDir@, against an npm mount carrying these rules, with the
EPSS stub answering this status and body (the fixture feed when 'Nothing').
-}
withPolicyCompile :: FilePath -> Maybe String -> (Status, Maybe LByteString) -> ((LogEnv -> IO FilePath) -> Stub -> IO a) -> IO a
withPolicyCompile outDir rule (feedStatus, feedBody) use = do
    zipData <- LBS.readFile "test/unit/fixtures/osv/sample.zip"
    epssData <- maybe (LBS.readFile epssFixtureFile) pure feedBody
    withStub status200 zipData $ \osvStub ->
        withStub feedStatus epssData $ \epssStub -> do
            config <-
                expectConfig
                    ( [ ("ECLUSE_SERVER__PUBLIC_URL", "https://proxy.example.test")
                      , ("ECLUSE_MOUNTS__NPM__ENABLED", "true")
                      ]
                        <> [("ECLUSE_MOUNTS__NPM__RULES", r) | Just r <- [rule]]
                        <> stubSourceEnv osvStub epssStub
                    )
                    Nothing
            let opts = (compileOptions "" "" outDir){pcoSource = Nothing, pcoEpssSource = Nothing}
            use (\logEnv -> runPilotCompile logEnv telemetryDisabled Nothing config opts) epssStub

-- No listener answers here, so a fetch that starts fails rather than reaching an upstream.
unreachable :: Text
unreachable = "http://127.0.0.1:1"

compileOptions :: Text -> Text -> FilePath -> PilotCompileOptions
compileOptions baseUrl epssBaseUrl outDir =
    PilotCompileOptions
        { pcoEcosystem = "npm"
        , pcoSource = Just (unpack baseUrl <> "/all.zip")
        , pcoEpssSource = Just (unpack epssBaseUrl <> "/epss.csv.gz")
        , pcoOutDir = outDir
        , pcoUpload = False
        }
