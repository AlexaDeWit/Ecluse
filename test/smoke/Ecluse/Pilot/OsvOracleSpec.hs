-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Smoke tier: compile the /live/ osv.dev npm export through the same
one-shot path operators script ('Ecluse.Pilot.runPilotCompile') and
sanity-check the artifact's advisory population. This is the drift alarm for
the upstream feed: a schema change at osv.dev that our parser silently drops
shows up here as a collapsed row count long before it would surface in a
production sync.

Non-gating by design (the smoke tier): osv.dev is an uncontrolled external
service, so the test pends rather than fails when it is unreachable. A red
here is a real disagreement with the live oracle worth investigating.
-}
module Ecluse.Pilot.OsvOracleSpec (spec) where

import Control.Exception (try)
import Database.SQLite.Simple (Only (..), close, open, query_)
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
                        appCfg
                        PilotCompileOptions
                            { pcoEcosystem = "npm"
                            , pcoSource = Nothing
                            , pcoOutDir = outDir
                            , pcoUpload = False
                            }
            case outcome of
                Left (e :: HttpException) ->
                    pendingWith ("osv.dev unreachable: " <> show e)
                Right dbFile -> do
                    conn <- open dbFile
                    total <- query_ conn "SELECT COUNT(*) FROM package_vulnerability_ranges" :: IO [Only Int]
                    lodash <- query_ conn "SELECT COUNT(*) FROM package_vulnerability_ranges WHERE package_name = 'lodash'" :: IO [Only Int]
                    close conn
                    -- Floors, not exact counts: the live dataset only grows.
                    -- npm carries thousands of advisories, and lodash's are
                    -- years old and permanent; dropping below either floor
                    -- means the parser and the feed have stopped agreeing.
                    map fromOnly total `shouldSatisfy` any (>= 1000)
                    map fromOnly lodash `shouldSatisfy` any (>= 1)

defaultAppConfig :: IO AppConfig
defaultAppConfig = case loadConfig [] Nothing of
    Right c -> pure (configApp c)
    Left e -> fail ("Config error: " <> show e)
