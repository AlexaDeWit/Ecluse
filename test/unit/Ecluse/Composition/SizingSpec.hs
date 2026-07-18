-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.SizingSpec (spec) where

import Data.Text qualified as T
import Network.HTTP.Client (ManagerSettings (managerConnCount), defaultManagerSettings)
import Test.Hspec

import Ecluse.Composition.Sizing (
    connectionPoolSettings,
    resolvePrivateConnections,
    resolvePublicConnections,
    resolveServeAdmission,
 )

spec :: Spec
spec = connectionPoolSpec

connectionPoolSpec :: Spec
connectionPoolSpec = do
    describe "resolveServeAdmission" $ do
        it "computes the default from the capability count" $ do
            fst (resolveServeAdmission Nothing 4) `shouldBe` 40
            fst (resolveServeAdmission Nothing 16) `shouldBe` 160
            -- At 10 per capability even a single-capability pod computes above
            -- the floor of 8; the floor is a backstop should the multiplier drop.
            fst (resolveServeAdmission Nothing 1) `shouldBe` 10
            fst (resolveServeAdmission Nothing 2) `shouldBe` 20

        it "lets an explicit config value win over the computation" $
            fst (resolveServeAdmission (Just 24) 4) `shouldBe` 24

        it "names the decision's provenance in the boot line" $ do
            snd (resolveServeAdmission Nothing 4) `shouldSatisfy` T.isInfixOf "computed from 4 capabilities"
            snd (resolveServeAdmission (Just 24) 4) `shouldSatisfy` T.isInfixOf "from config"

    describe "resolvePrivateConnections" $ do
        it "computes a quarter of the file-descriptor limit within the sane band" $ do
            -- A typical container soft limit (1024) → 256, comfortably inside the band.
            fst (resolvePrivateConnections Nothing 1024) `shouldBe` 256
            fst (resolvePrivateConnections Nothing 4096) `shouldBe` 1024

        it "floors a tiny file-descriptor limit and caps an enormous one" $ do
            -- A small limit floors at 64 so a constrained pod still reuses connections.
            fst (resolvePrivateConnections Nothing 256) `shouldBe` 64
            fst (resolvePrivateConnections Nothing 64) `shouldBe` 64
            -- An enormous limit caps at 4096 rather than retaining an absurd idle cache.
            fst (resolvePrivateConnections Nothing 65536) `shouldBe` 4096

        it "is computed from a datapoint unrelated to the admission capacity" $
            -- Same capability count, different fd limit ⇒ different pool: the two knobs
            -- are computationally independent (the private hit streams outside admission).
            fst (resolvePrivateConnections Nothing 1024)
                `shouldNotBe` fst (resolvePrivateConnections Nothing 8192)

        it "lets an explicit config value win over the computation" $
            fst (resolvePrivateConnections (Just 512) 1024) `shouldBe` 512

        it "names the decision's provenance in the boot line" $ do
            snd (resolvePrivateConnections Nothing 1024) `shouldSatisfy` T.isInfixOf "computed from file-descriptor limit 1024"
            snd (resolvePrivateConnections (Just 512) 1024) `shouldSatisfy` T.isInfixOf "from config"

    describe "resolvePublicConnections" $ do
        it "computes an eighth of the file-descriptor limit within the sane band" $ do
            -- Half the private pool's share: the public leg is the transient
            -- onboarding ramp, not the steady-state workhorse.
            fst (resolvePublicConnections Nothing 1024) `shouldBe` 128
            fst (resolvePublicConnections Nothing 4096) `shouldBe` 512

        it "floors a tiny file-descriptor limit and caps an enormous one" $ do
            -- A small limit floors at 32 so a constrained pod still reuses
            -- connections across an onboarding burst.
            fst (resolvePublicConnections Nothing 128) `shouldBe` 32
            -- An enormous limit caps at 1024 rather than retaining an absurd idle
            -- cache to one public origin.
            fst (resolvePublicConnections Nothing 65536) `shouldBe` 1024

        it "lets an explicit config value win over the computation" $
            fst (resolvePublicConnections (Just 10) 65536) `shouldBe` 10

        it "names the decision's provenance in the boot line" $ do
            snd (resolvePublicConnections Nothing 1024) `shouldSatisfy` T.isInfixOf "computed from file-descriptor limit 1024"
            snd (resolvePublicConnections (Just 10) 1024) `shouldSatisfy` T.isInfixOf "from config"

    describe "connectionPoolSettings" $
        it "sets the configured per-host connection bound" $
            managerConnCount (connectionPoolSettings 23 defaultManagerSettings) `shouldBe` 23
