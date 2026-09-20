-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Telemetry.CatalogueSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Telemetry.Catalogue (
    metricName,
 )
import Ecluse.Test.Metrics (allMetricNames)

-- | Tests the @ecluse.*@ instrument catalogue and the wire names it renders.
spec :: Spec
spec = catalogueSpec

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
                            , "ecluse.serve.admission.queued"
                            , "ecluse.publish.body.in_flight_bytes"
                            , "ecluse.publish.body.shed"
                            , "ecluse.registry.merge.divergence"
                            , "ecluse.upstream.fetch.duration"
                            , "ecluse.upstream.fetch.errors"
                            , "ecluse.metadata_cache.requests"
                            , "ecluse.metadata_cache.version.requests"
                            , "ecluse.metadata_cache.assembled.requests"
                            , "ecluse.metadata_cache.refused"
                            , "ecluse.metadata_cache.version.full_hits"
                            , "ecluse.metadata_cache.entries"
                            , "ecluse.metadata_cache.resident_bytes"
                            , "ecluse.metadata_cache.version.resident_bytes"
                            , "ecluse.metadata_cache.assembled.resident_bytes"
                            , "ecluse.serve.perimeter.faults"
                            , "ecluse.serve.relay.anomalies"
                            , "ecluse.mirror.enqueued"
                            , "ecluse.mirror.enqueue.failures"
                            , "ecluse.mirror.jobs.processed"
                            , "ecluse.mirror.publish.duration"
                            , "ecluse.dredger.versions"
                            , "ecluse.credential.refresh"
                            , "ecluse.credential.token.ttl.seconds"
                            , "ecluse.advisory.sync.attempts"
                            , "ecluse.advisory.sync.duration"
                            , "ecluse.advisory.database.age.seconds"
                            , "ecluse.advisory.source.age.seconds"
                            , "ecluse.advisory.compile.accepted"
                            , "ecluse.advisory.compile.dropped"
                            , "ecluse.advisory.compile.runs"
                            ]
        names `shouldContain` ["http.server.request.duration"]

    it "namespaces every metric under ecluse.* or the OTel http.* convention" $ do
        let names = map metricName allMetricNames
        all (\n -> "ecluse." `T.isPrefixOf` n || "http." `T.isPrefixOf` n) names `shouldBe` True

    it "does not re-emit cloud-native queue metrics" $
        map metricName allMetricNames
            `shouldNotContain` ["ecluse.queue.backlog", "ecluse.mirror.queue.depth", "ecluse.mirror.dlq.depth"]
