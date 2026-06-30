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
    BaselineSource (InjectedFallback, MeasuredRtt),
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
        it "is one round trip -- concurrent fan-out plus single-flight public leg" $
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
            -- A path faster than the live registry (or measurement noise) below the baseline.
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
        it "names a measured baseline and renders the absolute and percentage split" $ do
            let rendered = renderNormalised (MeasuredRtt 150 8) [NormalisedRow "merge-cold" (Just 255) Nothing]
            rendered `shouldSatisfy` T.isInfixOf "measured public RTT 150.0 ms"
            rendered `shouldSatisfy` T.isInfixOf "merge-cold"
            rendered `shouldSatisfy` T.isInfixOf "105.0 ms (41%)"

        it "labels the fallback baseline when the live probe was unavailable" $ do
            let rendered = renderNormalised (InjectedFallback 5) [NormalisedRow "merge-cold" Nothing Nothing]
            rendered `shouldSatisfy` T.isInfixOf "injected fallback 5.0 ms"
            rendered `shouldSatisfy` T.isInfixOf "n/a"

    describe "renderSaturation" $ do
        it "calls out the queuing-bound scenarios in a loud summary line" $ do
            let bound = deriveSaturation 0.5 (SaturationInput "merge-cold" 42 50 (Just 255) (Just 1090))
                rendered = renderSaturation 0.5 [bound]
            rendered `shouldSatisfy` T.isInfixOf "FLAG"
            rendered `shouldSatisfy` T.isInfixOf "queuing-bound: merge-cold"

        it "surfaces the best-effort deadline-abort count in the table" $ do
            -- The count is a best-effort signal (oha's error label, nix-pinned); it reaches
            -- the operator-visible table verbatim, defaulting to a plain 0 when there is none.
            let bound = deriveSaturation 0.5 (SaturationInput "merge-cold" 42 50 (Just 255) (Just 1090))
                ok = deriveSaturation 0.5 (SaturationInput "worker" 130 0 (Just 153) (Just 160))
            renderSaturation 0.5 [bound] `shouldSatisfy` T.isInfixOf "| 50 |"
            renderSaturation 0.5 [ok] `shouldSatisfy` T.isInfixOf "| 0 |"

        it "states no scenario is queuing-bound when none dominates" $ do
            let ok = deriveSaturation 0.5 (SaturationInput "worker" 130 0 (Just 153) (Just 160))
                rendered = renderSaturation 0.5 [ok]
            rendered `shouldSatisfy` T.isInfixOf "No scenario is queuing-bound"
