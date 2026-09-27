-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Smoke tier: compile the /live/ osv.dev npm export through the same one-shot path operators
script ('Ecluse.Pilot.runPilotCompile'), then check the artifact's advisory population. It is the
drift alarm for the upstream feed: a schema change osv.dev makes that our parser silently drops
collapses the row counts here, long before a production sync would surface it.
-}
module Ecluse.PilotSmokeSpec (spec) where

import Database.SQLite.Simple (Connection, Only (Only), Query, close, open, query_)
import Katip (Environment (..), initLogEnv)
import Network.HTTP.Client (HttpException)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import UnliftIO.Exception (try)

import Ecluse.Config (Config, loadConfig)
import Ecluse.Core.Fault.Http (isRetryableStatusCode)
import Ecluse.Core.Osv.Compile (PilotEpssRequired (perFailure))
import Ecluse.Core.Osv.Epss (EpssFeedFailure (EpssFeedStatus, EpssFeedTransport))
import Ecluse.Pilot (PilotCompileOptions (..), runPilotCompile)
import Ecluse.Runtime.Telemetry (telemetryDisabled)

spec :: Spec
spec = describe "osv.dev npm export (live oracle)" $
    it "compiles the live export into an artifact with a plausible advisory population" $ do
        le <- initLogEnv "smoke" (Environment "smoke")
        config <- defaultConfig
        withSystemTempDirectory "ecluse-osv-smoke" $ \outDir -> do
            outcome <-
                try . try $
                    runPilotCompile
                        le
                        telemetryDisabled
                        Nothing
                        config
                        PilotCompileOptions{pcoEcosystem = "npm", pcoOutDir = outDir, pcoUpload = False}
            case outcome of
                Left (e :: HttpException) ->
                    pendingWith ("osv.dev unreachable: " <> show e)
                Right (Left refusal)
                    | feedUnreachable (perFailure refusal) -> pendingWith ("the EPSS feed is unreachable: " <> displayException refusal)
                    | otherwise -> expectationFailure ("the live EPSS feed no longer decodes: " <> displayException refusal)
                Right (Right dbFile) -> do
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

-- No mount is declared, so the compile requires the feed and the artifact carries its scores.
defaultConfig :: IO Config
defaultConfig = case loadConfig [] Nothing of
    Right c -> pure c
    Left e -> fail ("Config error: " <> show e)

{- Only a feed the network could not deliver, or a status a retry could clear, is an outage. A moved
feed (404, 410) and a feed that arrived and failed are regressions. -}
feedUnreachable :: EpssFeedFailure -> Bool
feedUnreachable = \case
    EpssFeedStatus code -> isRetryableStatusCode code
    EpssFeedTransport _ -> True
    _ -> False
