-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.PilotSpec (spec) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text (unpack)
import Database.SQLite.Simple (close, open, query_)
import Network.HTTP.Types.Status (status200)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Ecluse.Composition.Support (expectAppConfig)
import Ecluse.Pilot (PilotCompileOptions (..), PilotUploadUnconfigured (..), runPilotCompile)
import Ecluse.Runtime.Telemetry (telemetryDisabled)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.OsvDb (epssFixtureFile)
import Ecluse.Test.Stub (stubBaseUrl, withStub)

spec :: Spec
spec = do
    describe "runPilotCompile (one-shot compile mode)" $ do
        it "compiles a served OSV zip into the requested directory and returns the artifact's path" $ do
            le <- newTestLogEnv
            appCfg <- expectAppConfig [] Nothing
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
                        appCfg <- expectAppConfig [("ECLUSE_ADVISORIES__EPSS_FEED_URL", feedUrl)] Nothing
                        let opts = (compileOptions (stubBaseUrl stub) (stubBaseUrl epssStub) outDir){pcoEpssSource = Nothing}
                        dbFile <- runPilotCompile le telemetryDisabled Nothing appCfg opts
                        conn <- open dbFile
                        rows <- query_ conn "SELECT package_name, epss_score FROM package_vulnerability_ranges" :: IO [(Text, Maybe Double)]
                        close conn
                        rows `shouldBe` [("hono", Just 0.75)]

        it "fails loudly when an upload is requested without a configured advisory store" $ do
            le <- newTestLogEnv
            appCfg <- expectAppConfig [] Nothing
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
            appCfg <- expectAppConfig [] Nothing
            withSystemTempDirectory "ecluse-pilot-compile" $ \dir -> do
                -- A file stands where the output directory's parent would be, so
                -- 'compileOsvToSqlite's createDirectoryIfMissing fails if the run reaches it.
                writeFileText (dir </> "blocker") ""
                let outDir = dir </> "blocker" </> "out"
                    opts = (compileOptions unreachable unreachable outDir){pcoUpload = True}
                runPilotCompile le telemetryDisabled Nothing appCfg opts
                    `shouldThrow` (== PilotUploadUnconfigured)

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
