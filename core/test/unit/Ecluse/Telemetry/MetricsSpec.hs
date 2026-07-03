module Ecluse.Telemetry.MetricsSpec (spec) where

import Prelude hiding (universe)

import Data.Text qualified as T
import Data.Universe.Class (universe)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Telemetry.Metrics (
    BreakerState (Closed, HalfOpen, Open),
    CacheResult (..),
    CredentialResult (..),
    Decision (..),
    Label (..),
    LabelKey,
    MirrorResult (..),
    ReasonClass (..),
    StatusClass (Status2xx, Status3xx, Status4xx, Status5xx, StatusOther),
    allLabelKeys,
    allMetricNames,
    breakerStateCode,
    highCardinalityKeys,
    labelKey,
    labelKeyName,
    metricAttributes,
    metricName,
    renderLabel,
    statusClassOf,
 )

{- | Tests for the @ecluse.*@ catalogue and the __bounded-label discipline__. The
crux is the cardinality guard: high-cardinality identifiers (package\/version\/scope\/
message) must never become metric labels. These assert the catalogue's wire names, the
closed label-key set, that every label value is drawn from a small fixed domain, and --
the guard -- that no high-cardinality identifier is a label key (and, structurally, that
no 'Label' constructor produces one). Pure.
-}
spec :: Spec
spec = do
    catalogueSpec
    labelKeySpec
    boundedDomainSpec
    renderSpec

catalogueSpec :: Spec
catalogueSpec = describe "metric-name catalogue" $ do
    it "renders the ecluse.* catalogue and the HTTP semantic convention to their wire names" $ do
        let names = map metricName allMetricNames
        names
            `shouldContain` [ "ecluse.serve.decision"
                            , "ecluse.rule.denials"
                            , "ecluse.rule.eval.duration"
                            , "ecluse.rule.effectful.failures"
                            , "ecluse.rule.breaker.state"
                            , "ecluse.serve.admission.in_flight"
                            , "ecluse.upstream.fetch.duration"
                            , "ecluse.upstream.fetch.errors"
                            , "ecluse.metadata_cache.requests"
                            , "ecluse.metadata_cache.entries"
                            , "ecluse.metadata_cache.resident_bytes"
                            , "ecluse.metadata_cache.version.resident_bytes"
                            , "ecluse.metadata_cache.assembled.resident_bytes"
                            , "ecluse.mirror.enqueued"
                            , "ecluse.mirror.enqueue.failures"
                            , "ecluse.mirror.jobs.processed"
                            , "ecluse.mirror.publish.duration"
                            , "ecluse.credential.refresh"
                            , "ecluse.credential.token.ttl.seconds"
                            ]
        names `shouldContain` ["http.server.request.duration"]

    it "namespaces every metric under ecluse.* or the OTel http.* convention" $ do
        let names = map metricName allMetricNames
        all (\n -> "ecluse." `T.isPrefixOf` n || "http." `T.isPrefixOf` n) names `shouldBe` True

    it "does not re-emit cloud-native queue metrics" $
        map metricName allMetricNames
            `shouldNotContain` ["ecluse.queue.backlog", "ecluse.mirror.queue.depth", "ecluse.mirror.dlq.depth"]

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
                              , "provider"
                              , "cause"
                              , "source"
                              , "tier"
                              ]

    it "REJECTS high-cardinality identifiers as labels (the crux)" $
        -- package / version / scope / message are never label keys. There is, by
        -- construction, no Label that produces one; this asserts the closed key set
        -- contains none of them either, so an unbounded label cannot be attached.
        filter (`elem` highCardinalityKeys) (map labelKeyName allLabelKeys) `shouldBe` []

    it "files every bounded label under a key in the closed set" $
        all (\l -> labelKey l `elem` (allLabelKeys :: [LabelKey])) allBoundedLabels `shouldBe` True

boundedDomainSpec :: Spec
boundedDomainSpec = describe "bounded label value domains" $ do
    it "draws the whole bounded-label series space from a small, fixed product" $
        -- Excluding the operator-bounded `rule` (a deployment's small fixed rule set),
        -- the entire space of metric label values is this handful -- never the unbounded
        -- space of package identifiers. A label whose domain was not finite could not
        -- appear in this enumeration (it has no Universe instance to enumerate).
        length allBoundedLabels `shouldSatisfy` (< 64)

    it "renders every bounded label to a non-empty value under a closed key" $
        all
            ( \l ->
                let (key, value) = renderLabel l
                 in key `elem` map labelKeyName allLabelKeys && not (T.null value)
            )
            allBoundedLabels
            `shouldBe` True

    it
        "materialises OpenTelemetry attributes for every bounded label without error"
        (traverse_ (evaluateWHNF . metricAttributes . (: [])) allBoundedLabels :: IO ())

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

    it "buckets a denial reason into a bounded class, never the message" $
        renderLabel (LReasonClass ReasonMissingIntegrity) `shouldBe` ("reason_class", "missing_integrity")

    it "shares the result key across cache/mirror/credential outcomes" $ do
        fst (renderLabel (LCacheResult Hit)) `shouldBe` "result"
        fst (renderLabel (LMirrorResult Published)) `shouldBe` "result"
        fst (renderLabel (LCredentialResult Refreshed)) `shouldBe` "result"

    it "classifies an HTTP status into its bounded class" $ do
        statusClassOf 200 `shouldBe` Status2xx
        statusClassOf 301 `shouldBe` Status3xx
        statusClassOf 404 `shouldBe` Status4xx
        statusClassOf 503 `shouldBe` Status5xx
        statusClassOf 100 `shouldBe` StatusOther

-- The bounded-label universe: every label constructible from a finite value domain
-- (the operator-bounded `rule` excepted, since its domain is configuration, not an
-- enum). If a label's domain were unbounded it could not be enumerated here.
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
        , LCredentialResult <$> universe
        , LProvider <$> universe
        , LCause <$> universe
        , LBreakerSource <$> universe
        , LTier <$> universe
        ]
  where
    ecosystems :: [Ecosystem]
    ecosystems = [Npm, PyPI, RubyGems]
