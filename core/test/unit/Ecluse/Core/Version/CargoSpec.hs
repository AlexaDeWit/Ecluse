-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native SemVer boundaries and precedence, independent of npm parser behaviour.
module Ecluse.Core.Version.CargoSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Version.Cargo (cargoUniquenessKey, isCargoStable, parseCargo, renderCargo)

-- | Native Cargo SemVer grammar and comparison examples.
spec :: Spec
spec = do
    describe "parseCargo" $ do
        for_ ["0.0.0", "1.2.3", "18446744073709551615.18446744073709551615.18446744073709551615", "1.2.3-0", "1.2.3-rc.1+build.000", "1.2.3+" <> T.replicate 1000 "7"] $ \raw ->
            it ("round-trips " <> show raw) $
                renderCargo <$> parseCargo raw `shouldBe` Just raw
        for_ ["", "v1.2.3", "V1.2.3", "1", "1.2", "1.2.3.4", "01.2.3", "1.02.3", "1.2.03", "1.2.3-01", "1.2.3-", "1.2.3+", "1.2.3-a..b", "1.2.3+a..b", "1.2.3+a+b", "1.2.3-α", "1.2.3+café", "１.2.3", "1.2.3 ", " 1.2.3", "18446744073709551616.0.0", "0.18446744073709551616.0", "0.0.18446744073709551616"] $ \raw ->
            it ("rejects " <> show raw) $
                parseCargo raw `shouldBe` Nothing
        it "accepts the shared text bound and refuses one extra byte" $ do
            parseCargo ("1.2.3+" <> T.replicate 1018 "a") `shouldSatisfy` isJust
            parseCargo ("1.2.3+" <> T.replicate 1019 "a") `shouldBe` Nothing
    describe "native precedence" $ do
        let ordered = ["1.0.0-alpha", "1.0.0-alpha.1", "1.0.0-alpha.beta", "1.0.0-beta", "1.0.0-beta.2", "1.0.0-beta.11", "1.0.0-rc.1", "1.0.0"]
        for_ (zip ordered (drop 1 ordered)) $ \(lower, higher) ->
            it (show lower <> " precedes " <> show higher) $
                liftA2 compare (parseCargo lower) (parseCargo higher) `shouldBe` Just LT
        it "compares numeric prerelease identifiers beyond u64 without an integer-width bound" $ do
            let large = T.replicate 900 "9"
            liftA2 compare (parseCargo ("1.2.3-" <> large)) (parseCargo ("1.2.3-1" <> large)) `shouldBe` Just LT
            liftA2 compare (parseCargo ("1.2.3-" <> large)) (parseCargo "1.2.3-alpha") `shouldBe` Just LT
        it "distinguishes raw build spelling from uniqueness and precedence" $ do
            let raw = "1.2.3-rc.1+build.000"
            renderCargo <$> parseCargo raw `shouldBe` Just raw
            cargoUniquenessKey <$> parseCargo raw `shouldBe` Just "1.2.3-rc.1"
            parseCargo "1.2.3+build.1" `shouldBe` parseCargo "1.2.3+build.2"
        it "classifies a build-only version as stable" $ do
            isCargoStable <$> parseCargo "1.2.3+build.1" `shouldBe` Just True
            isCargoStable <$> parseCargo "1.2.3-rc.1+build.1" `shouldBe` Just False
