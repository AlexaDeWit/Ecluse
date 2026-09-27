-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Compile temporary advisory artifacts through Pilot's compiler, and read back what they hold.
Local HTTP stubs serve the chosen OSV archive and the shared EPSS feed slice.
-}
module Ecluse.Test.OsvDb (
    epssFixtureFile,
    withFixtureOsvDb,
    withOsvZipDb,
    compileOsvZipDbTo,
    compileOsvZipDbWithFeedTo,

    -- * Reading an artifact back
    scoresOf,
    metaOf,

    -- * Pilot configuration over the stubs
    stubSourceEnv,
    denyIfEpssRules,
) where

import Data.Map.Strict qualified as Map
import Database.SQLite.Simple (Only (fromOnly), query_, withConnection)
import Network.HTTP.Types.Status (Status, status200)
import System.IO.Temp (withSystemTempDirectory)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Osv.Compile (CompileSources (..), compileOsvToSqlite)
import Ecluse.Core.Osv.Ecosystem (osvEcosystemFor)
import Ecluse.Core.Osv.Provenance (QuietTime (..))
import Ecluse.Core.Osv.Schema (EpssRequirement (EpssRequired))
import Ecluse.Test.Osv (CorpusVersion, osvCorpusZip, runOsvTestM)
import Ecluse.Test.Port (noopAdvisoryCompileMetricsPort)
import Ecluse.Test.Stub (Stub, stubBaseUrl, withStub)

-- | The shared EPSS feed slice omits some corpus aliases to cover missing scores.
epssFixtureFile :: FilePath
epssFixtureFile = "test/unit/fixtures/epss/sample-epss.csv.gz"

-- | Compile a committed corpus version into a temporary artifact through local HTTP stubs.
withFixtureOsvDb :: CorpusVersion -> (FilePath -> IO a) -> IO a
withFixtureOsvDb v use = do
    zipBytes <- osvCorpusZip v
    withOsvZipDb Npm zipBytes use

-- | Compile an archive and the shared EPSS slice into a temporary ecosystem artifact over local HTTP.
withOsvZipDb :: Ecosystem -> LByteString -> (FilePath -> IO a) -> IO a
withOsvZipDb eco zipBytes use =
    withSystemTempDirectory "ecluse-osv-fixture" (compileOsvZipDbTo eco zipBytes >=> use)

-- | Compile into the supplied directory so tests can exercise artifact replacement.
compileOsvZipDbTo :: Ecosystem -> LByteString -> FilePath -> IO FilePath
compileOsvZipDbTo eco zipBytes dir = do
    epssBytes <- readFileLBS epssFixtureFile
    compileOsvZipDbWithFeedTo eco EpssRequired (status200, epssBytes) zipBytes dir

-- | 'compileOsvZipDbTo' against a chosen feed answer, under a chosen EPSS requirement.
compileOsvZipDbWithFeedTo :: Ecosystem -> EpssRequirement -> (Status, LByteString) -> LByteString -> FilePath -> IO FilePath
compileOsvZipDbWithFeedTo eco requirement (feedStatus, epssBytes) zipBytes dir =
    withStub status200 zipBytes $ \osvStub ->
        withStub feedStatus epssBytes $ \epssStub ->
            runOsvTestM
                ( compileOsvToSqlite
                    noopAdvisoryCompileMetricsPort
                    Nothing
                    dir
                    (osvEcosystemFor eco)
                    requirement
                    CompileSources
                        { csOsvExportUrl = toString (stubBaseUrl osvStub) <> "/all.zip"
                        , csEpssFeedUrl = toString (stubBaseUrl epssStub) <> "/epss_scores-current.csv.gz"
                        }
                    fixtureQuietTime
                )

-- A century, so a committed fixture's own dates never raise the quiet-time alarm in a suite
-- that is about something else.
fixtureQuietTime :: QuietTime
fixtureQuietTime = QuietTime{qtOsv = century, qtEpss = century}
  where
    century = 100 * 365 * 86400

-- | Every range's EPSS score, in table order.
scoresOf :: FilePath -> IO [Maybe Double]
scoresOf dbFile = withConnection dbFile $ \conn ->
    map fromOnly <$> (query_ conn "SELECT epss_score FROM package_vulnerability_ranges" :: IO [Only (Maybe Double)])

-- | The artifact's @meta@ table.
metaOf :: FilePath -> IO (Map Text Text)
metaOf dbFile = withConnection dbFile $ \conn ->
    Map.fromList <$> (query_ conn "SELECT key, value FROM meta" :: IO [(Text, Text)])

-- | Configuration that points both advisory sources at stubs, so a run passes no override.
stubSourceEnv :: Stub -> Stub -> [(String, String)]
stubSourceEnv osvStub epssStub =
    [ ("ECLUSE_ADVISORIES__OSV_EXPORT_BASE_URL", toString (stubBaseUrl osvStub))
    , ("ECLUSE_ADVISORIES__EPSS_FEED_URL", toString (stubBaseUrl epssStub) <> "/epss.csv.gz")
    ]

-- | A rule set with one @DenyIfEpss@, at the threshold and alignment that still require the feed.
denyIfEpssRules :: String
denyIfEpssRules = "{\"risk\":{\"type\":\"DenyIfEpss\",\"minEpss\":1,\"onUnavailable\":\"skip\"}}"
