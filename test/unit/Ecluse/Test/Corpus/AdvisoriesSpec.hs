-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pin the corpus captures whose rule lookups find advisories in the compiled corpus advisories.
module Ecluse.Test.Corpus.AdvisoriesSpec (spec) where

import Test.Hspec
import UnliftIO (bracket)
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.Core.Cve (CveDb (cveDbClose, cveDbLookup), CveLookup (cveCoveredNames), openCveDb)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Osv.Schema (EpssRequirement (EpssRequired))
import Ecluse.Test.Corpus (corpusPackages, cpName, pypiCorpusPackages)
import Ecluse.Test.Corpus.Advisories (compileCorpusAdvisories)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec =
    describe "compileCorpusAdvisories" $
        it "names the corpus captures whose rule lookups find advisories" $
            for_ [(Npm, corpusPackages, ["@babel/core", "express", "lodash", "react", "request", "webpack"]), (PyPI, pypiCorpusPackages, ["numpy", "requests"])] $ \(eco, captures, advised) ->
                withSystemTempDirectory "ecluse-corpus-advisories" $ \dir -> do
                    compiled <- compileCorpusAdvisories eco dir
                    bracket (openCveDb eco EpssRequired compiled >>= expectRight) cveDbClose $ \db -> do
                        covered <- cveCoveredNames (cveDbLookup db)
                        sort (filter (`elem` covered) (map cpName captures)) `shouldBe` advised
