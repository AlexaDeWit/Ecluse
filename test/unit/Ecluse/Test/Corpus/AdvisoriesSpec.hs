-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pin the corpus captures the compiled corpus advisories cover, the rows the rules read for them, and the shipped policy.
module Ecluse.Test.Corpus.AdvisoriesSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.Config (RulePolicy (policyRules), defaultPolicy)
import Ecluse.Core.Cve (CveDb (cveDbLookup), CveLookup (cveCoveredNames))
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Package (mkPackageName)
import Ecluse.Test.Corpus (corpusPackages, cpName, cpPackage, pypiCorpusPackages)
import Ecluse.Test.Corpus.Advisories (checkCapturesServed, compileCorpusAdvisories, shippedPolicy)
import Ecluse.Test.OsvDb (withServedArtifact)

spec :: Spec
spec = do
    describe "compileCorpusAdvisories" $
        for_ [(Npm, corpusPackages, ["@babel/core", "express", "lodash", "react", "request", "webpack"]), (PyPI, pypiCorpusPackages, ["numpy", "requests"])] $ \(eco, captures, advised) ->
            it ("covers the " <> show eco <> " captures the corpus records name, and the rules read their rows") $
                withSystemTempDirectory "ecluse-corpus-advisories" $ \dir -> do
                    compiled <- compileCorpusAdvisories eco dir
                    withServedArtifact eco compiled $ \deps db -> do
                        covered <- cveCoveredNames (cveDbLookup db)
                        sort (filter (`elem` covered) (map cpName captures)) `shouldBe` advised
                        checkCapturesServed deps (map cpPackage captures) `shouldReturn` Right ()
                        checkCapturesServed deps [mkPackageName eco Nothing "ecluse-not-advised"] `shouldReturn` Left "the served advisories cover none of the captures"

    describe "shippedPolicy" $
        it "is the policy config/default.yaml ships" $
            shippedPolicy `shouldMatchList` Map.elems (policyRules defaultPolicy)
