-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Compile the fixture corpus into a real @osv.db@ artifact.

A local HTTP stub serves a corpus version, and Pilot's own compiler
('Ecluse.Core.Osv.Compile.compileOsvToSqlite', in @ecluse-core@) builds the
database from it. A suite therefore exercises a genuine artifact, not a
hand-built one.
-}
module Ecluse.Test.OsvDb (
    withFixtureOsvDb,
) where

import Network.HTTP.Types.Status (status200)
import System.IO.Temp (withSystemTempDirectory)

import Ecluse.Core.Osv.Compile (compileOsvToSqlite)
import Ecluse.Test.Osv (CorpusVersion, osvCorpusZip, runOsvTestM)
import Ecluse.Test.Port (noopAdvisoryCompileMetricsPort)
import Ecluse.Test.Stub (stubBaseUrl, withStub)

{- | Serve a corpus version through a local HTTP stub, compile it into a real @osv.db@, and hand the
artifact's path to the continuation. The harness deletes the artifact when the continuation returns.
-}
withFixtureOsvDb :: CorpusVersion -> (FilePath -> IO a) -> IO a
withFixtureOsvDb v use = do
    zipBytes <- osvCorpusZip v
    withSystemTempDirectory "ecluse-osv-fixture" $ \dir ->
        withStub status200 zipBytes $ \stub -> do
            dbFile <-
                runOsvTestM
                    (compileOsvToSqlite noopAdvisoryCompileMetricsPort Nothing dir "npm" (toString (stubBaseUrl stub) <> "/all.zip"))
            use dbFile
