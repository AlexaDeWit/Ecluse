-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Smoke tier: compile the /live/ osv.dev npm export through the same one-shot path operators
script ('Ecluse.Pilot.runPilotCompile'), then check the artifact's advisory population. It is the
drift alarm for the upstream feed: a schema change osv.dev makes that our parser silently drops
collapses the row counts here, long before a production sync would surface it.
-}
module Ecluse.PilotSmokeSpec (spec) where

import Control.Exception (try)
import Database.SQLite.Simple (Connection, Only (Only), Query, close, open, query_)
import Katip (Environment (..), initLogEnv)
import Network.HTTP.Client (HttpException)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Ecluse.Config (AppConfig, Config (configApp), loadConfig)
import Ecluse.Pilot (PilotCompileOptions (..), runPilotCompile)
import Ecluse.Runtime.Telemetry (telemetryDisabled)

spec :: Spec
spec = describe "osv.dev npm export (live oracle)" $
    it "compiles the live export into an artifact with a plausible advisory population" $ do
        le <- initLogEnv "smoke" (Environment "smoke")
        appCfg <- defaultAppConfig
        withSystemTempDirectory "ecluse-osv-smoke" $ \outDir -> do
            outcome <-
                try $
                    runPilotCompile
                        le
                        telemetryDisabled
                        Nothing
                        appCfg
                        PilotCompileOptions
                            { pcoEcosystem = "npm"
                            , pcoSource = Nothing
                            , pcoEpssSource = Nothing
                            , pcoOutDir = outDir
                            , pcoUpload = False
                            }
            case outcome of
                Left (e :: HttpException) ->
                    pendingWith ("osv.dev unreachable: " <> show e)
                Right dbFile -> do
                    conn <- open dbFile
                    total <- countOf conn "SELECT COUNT(*) FROM package_vulnerability_ranges"
                    lodash <- countOf conn "SELECT COUNT(*) FROM package_vulnerability_ranges WHERE package_name = 'lodash'"
                    scored <- countOf conn "SELECT COUNT(*) FROM package_vulnerability_ranges WHERE epss_score IS NOT NULL"
                    close conn
                    -- Floors, not exact counts: the live dataset only grows. Dropping below
                    -- a floor means the parser and the feed no longer agree.
                    total `shouldSatisfy` (>= 1000)
                    lodash `shouldSatisfy` (>= 1)
                    -- The live EPSS join: the npm feed always carries CVE-aliased advisories,
                    -- so a zero here means the feed's shape and our parse no longer agree.
                    scored `shouldSatisfy` (>= 1)

{- | The one row a @COUNT@ query answers, so a floor assertion reads the count itself and a
failure prints it rather than a list.
-}
countOf :: Connection -> Query -> IO Int
countOf conn sql = do
    rows <- query_ conn sql
    case rows of
        [Only n] -> pure n
        _ -> fail ("expected one count row, got " <> show (length rows))

defaultAppConfig :: IO AppConfig
defaultAppConfig = case loadConfig [] Nothing of
    Right c -> pure (configApp c)
    Left e -> fail ("Config error: " <> show e)
