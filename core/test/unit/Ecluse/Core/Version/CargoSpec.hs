-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native SemVer boundaries and precedence, independent of npm parser behaviour.
module Ecluse.Core.Version.CargoSpec (spec) where

import Data.Text qualified as T
import Hedgehog qualified as H
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Version.Cargo (cargoUniquenessKey, isCargoStable, parseCargo, renderCargo)
import Ecluse.Test.Version qualified as V
import Ecluse.Test.Version.Oracle (oracleCompare, oracleSort)

-- | Native Cargo SemVer grammar and comparison examples.
spec :: Spec
spec = do
    describe "parseCargo" $ do
        for_ ["0.0.0", "1.2.3", "18446744073709551615.18446744073709551615.18446744073709551615", "1.2.3-0", "1.2.3-rc.1+build.000", "1.2.3+" <> T.replicate 1000 "7"] $ \raw ->
            it ("round-trips " <> show raw) $ do
                renderCargo <$> parseCargo raw `shouldBe` Just raw
                oracleCompare Cargo raw raw `shouldReturn` Just EQ
        for_ ["", "v1.2.3", "V1.2.3", "1", "1.2", "1.2.3.4", "01.2.3", "1.02.3", "1.2.03", "1.2.3-01", "1.2.3-", "1.2.3+", "1.2.3-a..b", "1.2.3+a..b", "1.2.3+a+b", "1.2.3-α", "1.2.3+café", "１.2.3", "1.2.3 ", " 1.2.3", "18446744073709551616.0.0", "0.18446744073709551616.0", "0.0.18446744073709551616"] $ \raw ->
            it ("rejects " <> show raw) $ do
                parseCargo raw `shouldBe` Nothing
                oracleCompare Cargo raw raw `shouldReturn` Nothing
        it "accepts the shared text bound and refuses one extra byte" $ do
            parseCargo ("1.2.3+" <> T.replicate 1018 "a") `shouldSatisfy` isJust
            parseCargo ("1.2.3+" <> T.replicate 1019 "a") `shouldBe` Nothing
    describe "native precedence" $ do
        let ordered = ["1.0.0-alpha", "1.0.0-alpha.1", "1.0.0-alpha.beta", "1.0.0-beta", "1.0.0-beta.2", "1.0.0-beta.11", "1.0.0-rc.1", "1.0.0"]
        for_ (zip ordered (drop 1 ordered)) $ \(lower, higher) ->
            it (show lower <> " precedes " <> show higher) $ do
                liftA2 compare (parseCargo lower) (parseCargo higher) `shouldBe` Just LT
                oracleCompare Cargo lower higher `shouldReturn` Just LT
        it "compares numeric prerelease identifiers beyond u64 without an integer-width bound" $ do
            let large = T.replicate 900 "9"
            liftA2 compare (parseCargo ("1.2.3-" <> large)) (parseCargo ("1.2.3-1" <> large)) `shouldBe` Just LT
            liftA2 compare (parseCargo ("1.2.3-" <> large)) (parseCargo "1.2.3-alpha") `shouldBe` Just LT
            liftA2 compare (parseCargo "1.2.3-alpha") (parseCargo ("1.2.3-" <> large)) `shouldBe` Just GT
            oracleCompare Cargo ("1.2.3-" <> large) ("1.2.3-1" <> large) `shouldReturn` Just LT
            oracleCompare Cargo ("1.2.3-" <> large) "1.2.3-alpha" `shouldReturn` Just LT
        it "places stable versions after prereleases in the reverse comparison" $
            liftA2 compare (parseCargo "1.2.3") (parseCargo "1.2.3-rc.1") `shouldBe` Just GT
        it "distinguishes raw build spelling from uniqueness and precedence" $ do
            let raw = "1.2.3-rc.1+build.000"
            renderCargo <$> parseCargo raw `shouldBe` Just raw
            cargoUniquenessKey <$> parseCargo raw `shouldBe` Just "1.2.3-rc.1"
            parseCargo "1.2.3+build.1" `shouldBe` parseCargo "1.2.3+build.2"
            oracleCompare Cargo "1.2.3+build.1" "1.2.3+build.2" `shouldReturn` Just EQ
        it "matches native precedence above JavaScript's exact integer range" $ do
            let lower = "9007199254740992.0.0"
                higher = "9007199254740993.0.0"
            liftA2 compare (parseCargo lower) (parseCargo higher) `shouldBe` Just LT
            oracleCompare Cargo lower higher `shouldReturn` Just LT
        it "sorts native versions stably while ignoring build metadata" $
            oracleSort Cargo ["1.2.3+build.2", "invalid", "1.2.2", "1.2.3+build.1"]
                `shouldReturn` Just ["1.2.2", "1.2.3+build.2", "1.2.3+build.1"]
        it "classifies a build-only version as stable" $ do
            isCargoStable <$> parseCargo "1.2.3+build.1" `shouldBe` Just True
            isCargoStable <$> parseCargo "1.2.3-rc.1+build.1" `shouldBe` Just False
    describe "generative precedence differential" $
        modifyMaxSuccess (const 100) $
            it "orders every generated pair both sides accept as the pinned oracle does" $
                hedgehog $ do
                    raw1 <- H.forAll V.genCargo
                    raw2 <- H.forAll V.genCargo
                    theirs <- H.evalIO (oracleCompare Cargo raw1 raw2)
                    let ours = liftA2 compare (parseCargo raw1) (parseCargo raw2)
                    H.cover 50 "both sides accepted" (isJust ours && isJust theirs)
                    case (ours, theirs) of
                        (Just o, Just t) -> o H.=== t
                        _ -> H.success
