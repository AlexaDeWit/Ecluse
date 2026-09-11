-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Advisory-export fixtures for the nginx upstream in end-to-end tests.
Each committed corpus generation is served as its own osv.dev-shaped archive, beside the
one EPSS slice every generation joins onto.
-}
module Ecluse.E2E.Fixtures.Advisories (
    advisoryExportPath,
    advisoryEpssPath,
    buildAdvisoryFixtures,
) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

import Ecluse.Test.Osv (CorpusVersion (CorpusV1, CorpusV2), osvCorpusZip)
import Ecluse.Test.OsvDb (epssFixtureFile)

{- | The stub-relative path one advisory generation's OSV export archive is served at. Pilot's
@--source@ names it, so a scenario picks the generation it compiles.
-}
advisoryExportPath :: CorpusVersion -> Text
advisoryExportPath generation = "advisories/" <> tag <> "/all.zip"
  where
    tag = case generation of
        CorpusV1 -> "v1"
        CorpusV2 -> "v2"

-- | The stub-relative path of the EPSS feed slice Pilot joins onto every generation.
advisoryEpssPath :: Text
advisoryEpssPath = "advisories/epss_scores-current.csv.gz"

{- | Write every advisory generation's export archive and the shared EPSS slice into the stub's
document root, so Pilot compiles from a local upstream rather than the public feeds.
-}
buildAdvisoryFixtures :: FilePath -> IO ()
buildAdvisoryFixtures root = do
    for_ [minBound .. maxBound] $ \generation ->
        osvCorpusZip generation >>= writeUnder (advisoryExportPath generation)
    readFileLBS epssFixtureFile >>= writeUnder advisoryEpssPath
  where
    writeUnder relative bytes = do
        let path = root </> toString relative
        createDirectoryIfMissing True (takeDirectory path)
        writeFileLBS path bytes
