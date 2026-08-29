-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Compile the fixture corpus into a real @osv.db@ artifact.

Local HTTP stubs serve a corpus version and the EPSS feed slice, and Pilot's own
compiler ('Ecluse.Core.Osv.Compile.compileOsvToSqlite', in @ecluse-core@) builds
the database from them. A suite therefore exercises a genuine artifact, not a
hand-built one.
-}
module Ecluse.Test.OsvDb (
    epssFixtureFile,
    withFixtureOsvDb,
) where

import Network.HTTP.Types.Status (status200)
import System.IO.Temp (withSystemTempDirectory)

import Ecluse.Core.Osv.Compile (CompileSources (..), compileOsvToSqlite)
import Ecluse.Test.Osv (CorpusVersion, osvCorpusZip, runOsvTestM)
import Ecluse.Test.Port (noopAdvisoryCompileMetricsPort)
import Ecluse.Test.Stub (stubBaseUrl, withStub)

{- | The gzipped EPSS feed slice the fixture artifacts join against. It scores the CVE
aliases the corpus advisories carry, and deliberately omits some of them.
-}
epssFixtureFile :: FilePath
epssFixtureFile = "test/unit/fixtures/epss/sample-epss.csv.gz"

{- | Serve a corpus version and the EPSS slice through local HTTP stubs, compile them into a real
@osv.db@, and hand the artifact's path to the continuation. The harness deletes the artifact when
the continuation returns.
-}
withFixtureOsvDb :: CorpusVersion -> (FilePath -> IO a) -> IO a
withFixtureOsvDb v use = do
    zipBytes <- osvCorpusZip v
    epssBytes <- readFileLBS epssFixtureFile
    withSystemTempDirectory "ecluse-osv-fixture" $ \dir ->
        withStub status200 zipBytes $ \osvStub ->
            withStub status200 epssBytes $ \epssStub -> do
                dbFile <-
                    runOsvTestM
                        ( compileOsvToSqlite
                            noopAdvisoryCompileMetricsPort
                            Nothing
                            dir
                            "npm"
                            CompileSources
                                { csOsvExportUrl = toString (stubBaseUrl osvStub) <> "/all.zip"
                                , csEpssFeedUrl = toString (stubBaseUrl epssStub) <> "/epss_scores-current.csv.gz"
                                }
                        )
                use dbFile
