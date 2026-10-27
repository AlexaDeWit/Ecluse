-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Captured advisories retain native package coverage and the shared shipped policy.
module Ecluse.Test.Corpus.AdvisoriesSpec (spec) where

import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian)
import Test.Hspec
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.Config (RulePolicy (policyRules), decodeDefaultConfig, sourceDefaultNames)
import Ecluse.Config.DefaultConfig (defaultConfigBytes)
import Ecluse.Core.Cve (AdvisoryRange (AdvisoryRange), CveDb (cveDbLookup), CveLookup (CveLookup, cveCoveredNames))
import Ecluse.Core.Cve.Types (DbEtag (DbEtag))
import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems))
import Ecluse.Core.Osv.Types (UpperBound (Unbounded))
import Ecluse.Core.Package (PackageDetails (pkgPublishedAt), PackageInfo (infoVersions), mkPackageName)
import Ecluse.Core.Rules (RuleDeps, prepare)
import Ecluse.Core.Rules.Types (DenyIfCveParams (DenyIfCveParams), EvalContext (EvalContext), FailureAlignment (FailDeny), Rule (AllowIfOlderThan, DenyIfCve), completeEvidence)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Test.Corpus (cargoCorpusPackages, corpusPackages, cpName, cpPackage, pypiCorpusPackages, rubyGemsCorpusPackages)
import Ecluse.Test.Corpus.Advisories (AdvisoryInputs (..), checkCapturesServed, compileAdvisoryInputs, compileCorpusAdvisories, configuredShippedPolicy, corpusAdvisories, shippedPolicy)
import Ecluse.Test.Corpus.RubyGems (projectRubyGemsInfo)
import Ecluse.Test.Osv (osvZipOf)
import Ecluse.Test.OsvDb (scoresOf, withServedArtifact)
import Ecluse.Test.Registry.Npm.Metadata (projectNpmManifest)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIIndex)
import Ecluse.Test.Rules (admittedBy, atDefaultPrecedence, blockedBy, evalRules, inertRuleDeps, isBlockedByDefault, servingRuleDeps)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    describe "compileCorpusAdvisories" $
        for_ [(Npm, corpusPackages, ["@babel/core", "express", "lodash", "react", "request", "webpack"]), (PyPI, pypiCorpusPackages, ["numpy", "requests"]), (Cargo, cargoCorpusPackages, ["tokio"]), (RubyGems, rubyGemsCorpusPackages, ["rails"])] $ \(eco, captures, advised) ->
            it ("covers the " <> show eco <> " captures the corpus records name, and the rules read their rows") $
                withSystemTempDirectory "ecluse-corpus-advisories" $ \dir -> do
                    compiled <- compileCorpusAdvisories eco dir
                    withServedArtifact eco compiled $ \deps db -> do
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
                    assertCapturedRemediation deps ctx fixedRelease affected

    describe "real RubyGems advisory remediation" $
        it "admits the captured exact Rails fix during quarantine and preserves scored and unknown advisory evidence" $
            withSystemTempDirectory "ecluse-real-rails-remediation" $ \dir -> do
                compiled <- compileCorpusAdvisories RubyGems dir
                scores <- scoresOf compiled
                scores `shouldSatisfy` elem (Just 0.01034)
                scores `shouldSatisfy` elem Nothing
                bytes <- readFileBS "bench/corpus/rubygems/rails.info"
                (info, _) <- expectRight (projectRubyGemsInfo "https://rubygems.org" defaultLimits (mkPackageName RubyGems Nothing "rails") bytes)
                fixedRelease <- maybe (fail "the Rails capture has no 7.0.8.1 release") pure (Map.lookup "7.0.8.1" (infoVersions info))
                affected <- maybe (fail "the Rails capture has no 7.0.8 release") pure (Map.lookup "7.0.8" (infoVersions info))
                uploaded <- maybe (fail "the captured Rails fix has no publication time") pure (pkgPublishedAt fixedRelease)
                withServedArtifact RubyGems compiled $ \deps _ ->
                    assertCapturedRemediation deps (EvalContext (addUTCTime 3600 uploaded) Nothing) fixedRelease affected

    describe "canonical remediation controls from compiled captures" $ do
        for_ ["4.17.21", "v4.17.21"] $ \advisoryFix ->
            it ("admits captured lodash with advisory fix " <> toString advisoryFix) $
                withSystemTempDirectory "ecluse-lodash-fix-variant" $ \dir -> do
                    raw <- decodeUtf8 <$> readFileBS "bench/corpus/advisories/npm/GHSA-35jh-r3h4-6jhm.json"
                    let variant = T.replace "\"fixed\":\"4.17.21\"" ("\"fixed\":\"" <> advisoryFix <> "\"") raw
                    archive <- osvZipOf [("lodash-fix-spelling-variant.json", LBS.fromStrict (encodeUtf8 variant))]
                    inputs <- corpusAdvisories Npm
                    path <- compileAdvisoryInputs Npm dir inputs{aiOsvZip = archive}
                    bytes <- readFileBS "bench/corpus/npm/lodash.full.json"
                    (info, _) <- expectRight (projectNpmManifest defaultLimits (mkPackageName Npm Nothing "lodash") bytes)
                    fixedRelease <- maybe (fail "the lodash capture has no 4.17.21 release") pure (Map.lookup "4.17.21" (infoVersions info))
                    affected <- maybe (fail "the lodash capture has no 4.17.20 release") pure (Map.lookup "4.17.20" (infoVersions info))
                    uploaded <- maybe (fail "the captured lodash fix has no publication time") pure (pkgPublishedAt fixedRelease)
                    let ctx = EvalContext (addUTCTime 3600 uploaded) Nothing
                    withServedArtifact Npm path $ \deps _ -> do
                        assertCapturedRemediation deps ctx fixedRelease affected

        it "admits an already canonical 1.19 PyPI fix in named capture variants" $
            withSystemTempDirectory "ecluse-canonical-pypi-control-variant" $ \dir -> do
                raw <- decodeUtf8 <$> readFileBS "bench/corpus/advisories/pypi/GHSA-gc5v-m9x4-r6x2.json"
                archive <- osvZipOf [("requests-canonical-fix-variant.json", LBS.fromStrict (encodeUtf8 (T.replace "\"fixed\":\"2.33.0\"" "\"fixed\":\"1.19\"" raw)))]
                inputs <- corpusAdvisories PyPI
                path <- compileAdvisoryInputs PyPI dir inputs{aiOsvZip = archive}
                capture <- decodeUtf8 <$> readFileBS "bench/corpus/pypi/requests.simple.json"
                let variant = encodeUtf8 (T.replace "requests-2.32.5" "requests-1.18" (T.replace "requests-2.33.0" "requests-1.19" capture))
                (info, _) <- expectRight (projectPyPIIndex defaultLimits (mkPackageName PyPI Nothing "requests") variant)
                fixedRelease <- maybe (fail "the named PyPI variant has no 1.19 release") pure (Map.lookup "1.19" (infoVersions info))
                affected <- maybe (fail "the named PyPI variant has no 1.18 release") pure (Map.lookup "1.18" (infoVersions info))
                let ctx = EvalContext (UTCTime (fromGregorian 2026 3 25) 58240.382036) Nothing
                withServedArtifact PyPI path $ \deps _ -> do
                    assertCapturedRemediation deps ctx fixedRelease affected

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
        it "is the shared policy config/default.yaml ships without an advisory source" $ do
            (_, catalogue) <- expectRight (decodeDefaultConfig defaultConfigBytes)
            shippedPolicy `shouldMatchList` Map.elems (Map.withoutKeys (policyRules catalogue) sourceDefaultNames)

    describe "configuredShippedPolicy" $
        it "matches the effective shared policy with an advisory source" $ do
            (_, catalogue) <- expectRight (decodeDefaultConfig defaultConfigBytes)
            configuredShippedPolicy `shouldMatchList` Map.elems (policyRules catalogue)

assertCapturedRemediation :: RuleDeps -> EvalContext -> PackageDetails -> PackageDetails -> Expectation
assertCapturedRemediation deps ctx fixedRelease affected = do
    enabled <- prepare deps configuredShippedPolicy
    admittedBy <$> evalRules ctx enabled (completeEvidence fixedRelease) `shouldReturn` Just "AllowIfRemediatesCve"
    disabled <- prepare deps [atDefaultPrecedence (AllowIfOlderThan 604800)]
    evalRules ctx disabled (completeEvidence fixedRelease) >>= (`shouldSatisfy` isBlockedByDefault)
    denies <- prepare deps (configuredShippedPolicy <> [atDefaultPrecedence (DenyIfCve (DenyIfCveParams 0 FailDeny))])
    blockedBy <$> evalRules ctx denies (completeEvidence affected) `shouldReturn` Just "DenyIfCve"
