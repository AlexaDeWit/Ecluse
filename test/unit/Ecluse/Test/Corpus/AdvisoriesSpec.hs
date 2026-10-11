-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Pin the corpus captures the compiled corpus advisories cover, the rows the rules read for them, and the shared shipped policy.
module Ecluse.Test.Corpus.AdvisoriesSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Test.Hspec
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.Config (RulePolicy (policyRules), defaultPolicy)
import Ecluse.Core.Cve (AdvisoryRange (AdvisoryRange), CveDb (cveDbLookup), CveLookup (CveLookup, cveCoveredNames))
import Ecluse.Core.Cve.Types (DbEtag (DbEtag))
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Osv.Types (UpperBound (Unbounded))
import Ecluse.Core.Package (PackageInfo (infoVersions), mkPackageName)
import Ecluse.Core.Rules (evalRules, prepare)
import Ecluse.Core.Rules.Types (DenyIfCveParams (DenyIfCveParams), EvalContext (EvalContext), FailureAlignment (FailDeny), Rule (AllowIfOlderThan, DenyIfCve), completeEvidence)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Test.Corpus (corpusPackages, cpName, cpPackage, pypiCorpusPackages)
import Ecluse.Test.Corpus.Advisories (checkCapturesServed, compileCorpusAdvisories, shippedPolicy)
import Ecluse.Test.OsvDb (withServedArtifact)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIIndex)
import Ecluse.Test.Rules (admittedBy, atDefaultPrecedence, blockedBy, inertRuleDeps, isBlockedByDefault, servingRuleDeps)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    describe "compileCorpusAdvisories" $
        for_ [(Npm, corpusPackages, ["@babel/core", "express", "lodash", "react", "request", "webpack"]), (PyPI, pypiCorpusPackages, ["numpy", "requests"])] $ \(eco, captures, advised) ->
            it ("covers the " <> show eco <> " captures the corpus records name, and the rules read their rows") $
                withSystemTempDirectory "ecluse-corpus-advisories" $ \dir -> do
                    compiled <- compileCorpusAdvisories eco dir
                    withServedArtifact eco compiled $ \deps db -> do
                        -- A pass also holds each capture's display name equal to its lookup key, which the filter below relies on.
                        checkCapturesServed deps (map cpPackage captures) `shouldReturn` Right ()
                        covered <- cveCoveredNames (cveDbLookup db)
                        sort (filter (`elem` covered) (map cpName captures)) `shouldBe` advised
                        checkCapturesServed deps [mkPackageName eco Nothing "ecluse-not-advised"] `shouldReturn` Left "the served advisories cover none of the captures"

    describe "real Requests advisory remediation" $
        it "admits the captured 2.33.0 fix during quarantine and refuses its affected predecessor" $
            withSystemTempDirectory "ecluse-real-requests-remediation" $ \dir -> do
                compiled <- compileCorpusAdvisories PyPI dir
                bytes <- readFileBS "bench/corpus/pypi/requests.simple.json"
                (info, _) <- expectRight (projectPyPIIndex defaultLimits (mkPackageName PyPI Nothing "requests") bytes)
                fixedRelease <- maybe (fail "the capture has no 2.33 release") pure (Map.lookup "2.33" (infoVersions info))
                affected <- maybe (fail "the capture has no 2.32.5 release") pure (Map.lookup "2.32.5" (infoVersions info))
                let ctx = EvalContext (UTCTime (fromGregorian 2026 3 25) 58240.382036) Nothing
                withServedArtifact PyPI compiled $ \deps _ -> do
                    enabled <- prepare deps shippedPolicy
                    admittedBy <$> evalRules ctx enabled (completeEvidence fixedRelease) `shouldReturn` Just "AllowIfRemediatesCve"
                    disabled <- prepare deps [atDefaultPrecedence (AllowIfOlderThan 604800)]
                    evalRules ctx disabled (completeEvidence fixedRelease) >>= (`shouldSatisfy` isBlockedByDefault)
                    denies <- prepare deps (shippedPolicy <> [atDefaultPrecedence (DenyIfCve (DenyIfCveParams 0 FailDeny))])
                    blockedBy <$> evalRules ctx denies (completeEvidence affected) `shouldReturn` Just "DenyIfCve"

    describe "checkCapturesServed" $ do
        let react = mkPackageName Npm Nothing "react"
        it "fails a capture the generation names when the rules read no row for it" $
            checkCapturesServed (servingRuleDeps (DbEtag "t") (CveLookup (const (pure [])) (pure ["react"]))) [react]
                `shouldReturn` Left "the served advisories return no row for react"
        it "fails when no generation is serving" $
            checkCapturesServed inertRuleDeps [react] `shouldReturn` Left "no advisory generation is serving"
        it "fails a capture whose display name differs from its lookup key, which a display-name match would drop" $ do
            let row = AdvisoryRange "CVE-2099-1" Nothing Nothing Unbounded Nothing
                keys = ["requests", "typing-extensions"]
                canonicalOnly = CveLookup (\name -> pure [row | name `elem` keys]) (pure keys)
            checkCapturesServed (servingRuleDeps (DbEtag "t") canonicalOnly) [mkPackageName PyPI Nothing "requests", mkPackageName PyPI Nothing "typing_extensions"]
                `shouldReturn` Left "the capture typing_extensions differs from its lookup key typing-extensions"

    describe "shippedPolicy" $
        it "is the shared policy config/default.yaml ships" $
            shippedPolicy `shouldMatchList` Map.elems (policyRules defaultPolicy)
