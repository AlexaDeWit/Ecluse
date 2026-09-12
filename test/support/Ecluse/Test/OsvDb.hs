-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Compile temporary advisory artifacts through Pilot's compiler.
Local HTTP stubs serve the chosen OSV archive and the shared EPSS feed slice.
-}
module Ecluse.Test.OsvDb (
    epssFixtureFile,
    withFixtureOsvDb,
    withOsvZipDb,
    compileOsvZipDbTo,
) where

import Network.HTTP.Types.Status (status200)
import System.IO.Temp (withSystemTempDirectory)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Osv.Compile (CompileSources (..), compileOsvToSqlite)
import Ecluse.Core.Osv.Ecosystem (osvEcosystemFor)
import Ecluse.Core.Osv.Provenance (QuietTime (..))
import Ecluse.Test.Osv (CorpusVersion, osvCorpusZip, runOsvTestM)
import Ecluse.Test.Port (noopAdvisoryCompileMetricsPort)
import Ecluse.Test.Stub (stubBaseUrl, withStub)

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
    withStub status200 zipBytes $ \osvStub ->
        withStub status200 epssBytes $ \epssStub ->
            runOsvTestM
                ( compileOsvToSqlite
                    noopAdvisoryCompileMetricsPort
                    Nothing
                    dir
                    (osvEcosystemFor eco)
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
