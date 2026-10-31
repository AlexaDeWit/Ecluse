-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Check service attribution and saturation arithmetic and rendering.
module Ecluse.BenchLoad.NormaliseSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.BenchLoad.Normalise (
    Attribution (
        attrOverheadFraction,
        attrOverheadMs,
        attrTotalMs,
        attrUpstreamFraction,
        attrUpstreamMs
    ),
    LiveFetch (LiveFetch, NoLiveFetch),
    NormalisedRow (NormalisedRow),
    Saturation (satQueuingDelayMs, satQueuingDominates, satQueuingFraction),
    SaturationInput (SaturationInput),
    attribute,
    deriveSaturation,
    publicLegMultiple,
    queuingDominanceThreshold,
    renderNormalised,
    renderSaturation,
 )

spec :: Spec
spec = do
    describe "publicLegMultiple" $
        it "is one round trip: concurrent fan-out plus a single-flight public leg" $
            publicLegMultiple `shouldBe` 1.0

    describe "attribute" $ do
        it "splits a latency into the upstream baseline and the Écluse overhead" $ do
            let a = attribute 150 255
            attrTotalMs a `shouldBe` 255
            attrUpstreamMs a `shouldBe` 150
            attrOverheadMs a `shouldBe` 105

        it "reports each part as a fraction of the total that sum to one" $ do
            let a = attribute 150 250
            attrUpstreamFraction a `shouldBe` (150 / 250)
            attrOverheadFraction a `shouldBe` (100 / 250)
            attrUpstreamFraction a + attrOverheadFraction a `shouldBe` 1.0

        it "caps the upstream at the total so the overhead is never negative" $ do
            let a = attribute 150 120
            attrUpstreamMs a `shouldBe` 120
            attrOverheadMs a `shouldBe` 0
            attrOverheadFraction a `shouldBe` 0

        it "attributes nothing on a zero total without dividing by zero" $ do
            let a = attribute 150 0
            attrUpstreamMs a `shouldBe` 0
            attrOverheadMs a `shouldBe` 0
            attrUpstreamFraction a `shouldBe` 0
            attrOverheadFraction a `shouldBe` 0

    describe "deriveSaturation" $ do
        it "derives the queuing delay as loaded p50 minus the concurrency-1 service p50" $ do
            let s = deriveSaturation queuingDominanceThreshold (SaturationInput "merge-cold" 42 50 (Just 255) (Just 1090))
            satQueuingDelayMs s `shouldBe` Just 835

        it "flags a scenario queuing-bound when the delay exceeds the threshold of the loaded p50" $ do
            let s = deriveSaturation 0.5 (SaturationInput "merge-cold" 42 50 (Just 255) (Just 1090))
            satQueuingFraction s `shouldBe` Just (835 / 1090)
            satQueuingDominates s `shouldBe` True

        it "does not flag a scenario whose queuing delay stays under the threshold" $ do
            let s = deriveSaturation 0.5 (SaturationInput "worker" 130 0 (Just 153) (Just 160))
            satQueuingDominates s `shouldBe` False

        it "floors the queuing delay at zero when the loaded p50 is the faster of the two" $ do
            let s = deriveSaturation 0.5 (SaturationInput "x" 10 0 (Just 200) (Just 150))
            satQueuingDelayMs s `shouldBe` Just 0
            satQueuingDominates s `shouldBe` False

        it "leaves the delay undefined and unflagged when a p50 is absent" $ do
            let s = deriveSaturation 0.5 (SaturationInput "x" 10 0 Nothing (Just 150))
            satQueuingDelayMs s `shouldBe` Nothing
            satQueuingFraction s `shouldBe` Nothing
            satQueuingDominates s `shouldBe` False

    describe "renderNormalised" $ do
        it "subtracts the injected latency, names the live fetch time, and renders the absolute and percentage split" $ do
            let rendered = renderNormalised 20 (LiveFetch 150 8) [NormalisedRow "merge-cold" (Just 255) Nothing Nothing]
            rendered `shouldSatisfy` T.isInfixOf "Baseline = the injected upstream latency, 20.0 ms"
            rendered `shouldSatisfy` T.isInfixOf "Live public fetch time: 150.0 ms, the mean of 8 live samples."
            rendered `shouldSatisfy` T.isInfixOf "| merge-cold | 255.0 ms | 20.0 ms (8%) | 235.0 ms (92%) | n/a | n/a | n/a | 385.0 ms | available |"

        it "says the live fetch time is unavailable when the probe gave none" $ do
            let rendered = renderNormalised 20 NoLiveFetch [NormalisedRow "merge-cold" Nothing Nothing Nothing]
            rendered `shouldSatisfy` T.isInfixOf "Live public fetch time: unavailable, because the probe was off or no fetch succeeded."
            T.lines rendered `shouldSatisfy` elem "| merge-cold | n/a | n/a | n/a | n/a | n/a | n/a | n/a | available |"

        it "retains private-hit totals without a public-baseline subtraction" $ do
            let rendered = renderNormalised 17 (LiveFetch 150 8) [NormalisedRow "private relay" (Just 2.5) (Just 4) (Just "no public leg")]
            T.lines rendered `shouldSatisfy` elem "| private relay | 2.5 ms | n/a | n/a | 4.0 ms | n/a | n/a | n/a | unavailable: no public leg |"

        it "retains the private-hit reason even when no successful latency exists" $ do
            let rendered = renderNormalised 17 NoLiveFetch [NormalisedRow "private relay" Nothing Nothing (Just "no public leg")]
            T.lines rendered `shouldSatisfy` elem "| private relay | n/a | n/a | n/a | n/a | n/a | n/a | n/a | unavailable: no public leg |"

    describe "renderSaturation" $ do
        it "reports no usable comparison for an empty result" $ do
            let rendered = renderSaturation 0.5 []
            T.lines rendered `shouldSatisfy` elem "No usable queuing comparison is available."
            rendered `shouldNotSatisfy` T.isInfixOf "No measured queuing fraction exceeds"
            rendered `shouldNotSatisfy` T.isInfixOf "not backlog"
            rendered `shouldNotSatisfy` T.isInfixOf "FLAG, queuing-bound:"
            rendered `shouldNotSatisfy` T.isInfixOf "Queuing comparison unavailable:"

        it "calls out the queuing-bound scenarios in a loud summary line" $ do
            let bound = deriveSaturation 0.5 (SaturationInput "merge-cold" 42 50 (Just 255) (Just 1090))
                rendered = renderSaturation 0.5 [bound]
            rendered `shouldSatisfy` T.isInfixOf "FLAG"
            rendered `shouldSatisfy` T.isInfixOf "queuing-bound: merge-cold"

        it "surfaces the best-effort deadline-abort count in the table" $ do
            let bound = deriveSaturation 0.5 (SaturationInput "merge-cold" 42 50 (Just 255) (Just 1090))
                ok = deriveSaturation 0.5 (SaturationInput "worker" 130 0 (Just 153) (Just 160))
            renderSaturation 0.5 [bound] `shouldSatisfy` T.isInfixOf "| 50 |"
            renderSaturation 0.5 [ok] `shouldSatisfy` T.isInfixOf "| 0 |"

        it "qualifies an under-threshold observation without ruling out backlog" $ do
            let ok = deriveSaturation 0.5 (SaturationInput "worker" 130 0 (Just 153) (Just 160))
                rendered = renderSaturation 0.5 [ok]
            rendered `shouldSatisfy` T.isInfixOf "No measured queuing fraction exceeds 50%"
            rendered `shouldSatisfy` T.isInfixOf "This observation does not rule out backlog."
            T.lines rendered `shouldSatisfy` elem "| worker | 130.0 req/s | 0 | 153.0 ms | 160.0 ms | 7.0 ms (4%) | ok |"

        it "marks a scenario with no successful response instead of calling it ok" $ do
            let unmeasured = deriveSaturation 0.5 (SaturationInput "merge-cold" 0 0 (Just 255) Nothing)
                rendered = renderSaturation 0.5 [unmeasured]
            satQueuingDelayMs unmeasured `shouldBe` Nothing
            satQueuingFraction unmeasured `shouldBe` Nothing
            T.lines rendered `shouldSatisfy` elem "| merge-cold | 0.0 req/s | 0 | 255.0 ms | n/a | n/a | no successes |"
            rendered `shouldSatisfy` T.isInfixOf "No usable queuing comparison is available."
            rendered `shouldSatisfy` T.isInfixOf "merge-cold (no successful response under load)"
            rendered `shouldNotSatisfy` T.isInfixOf "| ok |"

        it "retains loaded measurements when the concurrency-one latency is missing" $ do
            let unavailable = deriveSaturation 0.5 (SaturationInput "loaded-only" 42 50 Nothing (Just 1090))
                rendered = renderSaturation 0.5 [unavailable]
            T.lines rendered `shouldSatisfy` elem "| loaded-only | 42.0 req/s | 50 | n/a | 1090.0 ms | n/a | n/a |"
            rendered `shouldSatisfy` T.isInfixOf "No usable queuing comparison is available."
            rendered `shouldSatisfy` T.isInfixOf "loaded-only (concurrency-1 service p50 unavailable)"
            rendered `shouldNotSatisfy` T.isInfixOf "No measured queuing fraction exceeds"

        it "keeps a zero loaded latency without treating its undefined fraction as measured" $ do
            let unavailable = deriveSaturation 0.5 (SaturationInput "zero" 10 3 (Just 255) (Just 0))
                rendered = renderSaturation 0.5 [unavailable]
            satQueuingDelayMs unavailable `shouldBe` Just 0
            satQueuingFraction unavailable `shouldBe` Nothing
            T.lines rendered `shouldSatisfy` elem "| zero | 10.0 req/s | 3 | 255.0 ms | 0.0 ms | 0.0 ms | n/a |"
            rendered `shouldSatisfy` T.isInfixOf "No usable queuing comparison is available."
            rendered `shouldSatisfy` T.isInfixOf "zero (loaded p50 is not positive)"

        it "summarises measured rows and names every unavailable comparison" $ do
            let measured = deriveSaturation 0.5 (SaturationInput "worker" 130 0 (Just 153) (Just 160))
                missingService = deriveSaturation 0.5 (SaturationInput "missing-service" 42 50 Nothing (Just 1090))
                missingLoaded = deriveSaturation 0.5 (SaturationInput "missing-loaded" 0 9 (Just 255) Nothing)
                zero = deriveSaturation 0.5 (SaturationInput "zero" 10 3 (Just 255) (Just 0))
                rendered = renderSaturation 0.5 [measured, missingService, missingLoaded, zero]
            rendered `shouldSatisfy` T.isInfixOf "No measured queuing fraction exceeds 50%"
            rendered `shouldNotSatisfy` T.isInfixOf "No usable queuing comparison is available."
            rendered `shouldSatisfy` T.isInfixOf "Queuing comparison unavailable: missing-service (concurrency-1 service p50 unavailable), missing-loaded (no successful response under load), zero (loaded p50 is not positive)."

        it "retains a flagged observation beside an unavailable comparison" $ do
            let bound = deriveSaturation 0.5 (SaturationInput "merge-cold" 42 50 (Just 255) (Just 1090))
                unavailable = deriveSaturation 0.5 (SaturationInput "missing-service" 12 4 Nothing (Just 200))
                rendered = renderSaturation 0.5 [bound, unavailable]
            rendered `shouldSatisfy` T.isInfixOf "FLAG, queuing-bound: merge-cold."
            rendered `shouldSatisfy` T.isInfixOf "missing-service (concurrency-1 service p50 unavailable)"
            T.lines rendered `shouldSatisfy` elem "| merge-cold | 42.0 req/s | 50 | 255.0 ms | 1090.0 ms | 835.0 ms (77%) | queuing-bound |"
