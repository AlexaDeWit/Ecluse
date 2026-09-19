-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Telemetry.MetricsSpec (spec) where

import Prelude hiding (universe)

import Data.Text qualified as T
import Data.Universe.Class (universe)
import OpenTelemetry.Attributes (Attribute (AttributeValue), PrimitiveAttribute (TextAttribute), lookupAttribute)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Telemetry.Metrics (
    AdvisoryCompileResult (CompileAborted, CompileCompleted),
    AdvisoryDropCause (DropMalformed, DropOversize),
    AdvisorySyncResult (AdvisoryFetchFailed, AdvisorySwapped),
    BreakerState (Closed, HalfOpen, Open),
    CacheResult (..),
    CredentialResult (..),
    Decision (..),
    Label (..),
    LabelKey,
    MirrorResult (..),
    Provider (ProviderCodeArtifact, ProviderRegistry, ProviderVerdaccio),
    ReasonClass (..),
    breakerStateCode,
    labelKey,
    labelKeyName,
    metricAttributes,
    renderLabel,
 )
import Ecluse.Test.Metrics (allLabelKeys, highCardinalityKeys)

{- | Tests the bounded-label discipline. The crux is the cardinality guard: package, version,
scope, and message must never become metric labels. The instrument catalogue's own names are
tested in "Ecluse.Core.Telemetry.CatalogueSpec".
-}
spec :: Spec
spec = do
    labelKeySpec
    boundedDomainSpec
    renderSpec

labelKeySpec :: Spec
labelKeySpec = describe "label keys (the cardinality guard)" $ do
    it "is exactly the closed bounded-enum set" $
        map labelKeyName allLabelKeys
            `shouldMatchList` [ "decision"
                              , "reason_class"
                              , "rule"
                              , "ecosystem"
                              , "mount"
                              , "upstream"
                              , "status_class"
                              , "result"
                              , "target"
                              , "provider"
                              , "cause"
                              , "source"
                              , "tier"
                              ]

    it "REJECTS high-cardinality identifiers as labels (the crux)" $
        -- No 'Label' constructor produces a high-cardinality key, and the closed key set holds none
        -- either, so nothing can attach an unbounded label.
        filter (`elem` highCardinalityKeys) (map labelKeyName allLabelKeys) `shouldBe` []

    it "files every bounded label under a key in the closed set" $
        filter (\l -> labelKey l `notElem` (allLabelKeys :: [LabelKey])) allBoundedLabels `shouldBe` []

boundedDomainSpec :: Spec
boundedDomainSpec = describe "bounded label value domains" $ do
    it "draws the whole bounded-label series space from a small, fixed product" $
        -- The operator-bounded `rule` aside, this handful is the whole label-value space: an
        -- unbounded package identifier has no Universe to enumerate.
        length allBoundedLabels `shouldSatisfy` (< 64)

    it "renders every bounded label to a non-empty value under a closed key" $
        filter
            ( \l ->
                let (key, value) = renderLabel l
                 in key `notElem` map labelKeyName allLabelKeys || T.null value
            )
            allBoundedLabels
            `shouldBe` []

    it "materialises every bounded label as the attribute renderLabel names, under that key" $
        -- An instrument reads its series back by this key, so a label that renders one way and
        -- materialises another would split the series without failing anything.
        for_ allBoundedLabels $ \label -> do
            let (key, value) = renderLabel label
            lookupAttribute (metricAttributes [label]) key `shouldBe` Just (AttributeValue (TextAttribute value))

    it "encodes breaker state as a small ordinal gauge value, not a label" $
        map breakerStateCode [Closed, HalfOpen, Open] `shouldBe` [0, 1, 2]

renderSpec :: Spec
renderSpec = describe "renderLabel" $ do
    it "renders the serve decision to admit/deny/unavailable" $ do
        renderLabel (LDecision Admit) `shouldBe` ("decision", "admit")
        renderLabel (LDecision Deny) `shouldBe` ("decision", "deny")
        renderLabel (LDecision Unavailable) `shouldBe` ("decision", "unavailable")

    it "carries the configured rule name as the one operator-bounded label" $
        renderLabel (LRule "min-age") `shouldBe` ("rule", "min-age")

    it "spells each credential provider as the configuration spells its store tag" $ do
        -- The value an operator declares a store under is the value their dashboard filters on.
        renderLabel (LProvider ProviderRegistry) `shouldBe` ("provider", "registry")
        renderLabel (LProvider ProviderCodeArtifact) `shouldBe` ("provider", "codeArtifact")
        renderLabel (LProvider ProviderVerdaccio) `shouldBe` ("provider", "verdaccio")

    it "buckets a denial reason into a bounded class, never the message" $
        renderLabel (LReasonClass ReasonMissingIntegrity) `shouldBe` ("reason_class", "missing_integrity")

    it "renders an advisory sync attempt's outcome, never the artifact it fetched" $ do
        renderLabel (LAdvisorySyncResult AdvisorySwapped) `shouldBe` ("result", "swapped")
        renderLabel (LAdvisorySyncResult AdvisoryFetchFailed) `shouldBe` ("result", "fetch_failed")

    it "renders an advisory compile's verdict and its bounded drop cause" $ do
        renderLabel (LAdvisoryCompileResult CompileCompleted) `shouldBe` ("result", "completed")
        renderLabel (LAdvisoryCompileResult CompileAborted) `shouldBe` ("result", "aborted")
        -- The dropped entry's own name and bytes stay on the log line.
        renderLabel (LAdvisoryDropCause DropOversize) `shouldBe` ("cause", "oversize")
        renderLabel (LAdvisoryDropCause DropMalformed) `shouldBe` ("cause", "malformed")

    it "shares the result key across cache/mirror/credential/advisory outcomes" $ do
        fst (renderLabel (LCacheResult Hit)) `shouldBe` "result"
        fst (renderLabel (LMirrorResult Published)) `shouldBe` "result"
        fst (renderLabel (LCredentialResult Refreshed)) `shouldBe` "result"
        fst (renderLabel (LAdvisorySyncResult AdvisorySwapped)) `shouldBe` "result"
        fst (renderLabel (LAdvisoryCompileResult CompileCompleted)) `shouldBe` "result"

-- Every label constructible from a finite value domain, the operator-bounded `rule` excepted
-- because its domain is configuration. An unbounded label could not be enumerated here.
allBoundedLabels :: [Label]
allBoundedLabels =
    concat
        [ LDecision <$> universe
        , LReasonClass <$> universe
        , LEcosystem <$> ecosystems
        , LMount <$> ecosystems
        , LUpstream <$> universe
        , LStatusClass <$> universe
        , LCacheResult <$> universe
        , LMirrorResult <$> universe
        , LSweepTarget <$> universe
        , LCredentialResult <$> universe
        , LAdvisorySyncResult <$> universe
        , LAdvisoryCompileResult <$> universe
        , LAdvisoryDropCause <$> universe
        , LProvider <$> universe
        , LCause <$> universe
        , LBreakerSource <$> universe
        , LTier <$> universe
        ]
  where
    ecosystems :: [Ecosystem]
    ecosystems = [Npm, PyPI, RubyGems]
