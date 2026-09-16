-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Server.ReadinessSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Server.Readiness (
    DatabaseRequirement (DatabaseOptional, DatabaseRequired),
    MountReadiness (MountAwaitingFirstSync, MountReady),
    Readiness (AwaitingMounts, Latched, Routable),
    allMountsReady,
    alwaysReady,
    mountReadiness,
    mountStateFor,
    routable,
 )

spec :: Spec
spec = do
    describe "mountStateFor -- one mount's state, decided from its own rules" $ do
        it "waits for the first sync where the mount's rules deny on the database" $ do
            mountStateFor DatabaseRequired False `shouldBe` MountAwaitingFirstSync
            mountStateFor DatabaseRequired True `shouldBe` MountReady

        it "is ready before any artifact loads where no rule denies on the database" $ do
            -- The fast lane abstains without a database, so the mount decides everything it
            -- can decide and never holds the listener out of rotation for Pilot.
            mountStateFor DatabaseOptional False `shouldBe` MountReady
            mountStateFor DatabaseOptional True `shouldBe` MountReady

    describe "mountReadiness -- the verdict decided from the mounts" $ do
        it "is routable with no configured mount, because nothing gates routing" $
            mountReadiness Map.empty `shouldBe` Routable Map.empty

        it "is routable while one of two ecosystems holds its advisory database" $
            -- The isolation this whole type exists for: a missing PyPI database leaves the
            -- healthy npm mount in rotation.
            mountReadiness oneOfTwo `shouldBe` Routable oneOfTwo

        it "awaits the mounts while none of them holds one" $
            mountReadiness neitherOfTwo `shouldBe` AwaitingMounts neitherOfTwo

    describe "routable -- what /readyz answers from" $ do
        it "answers for a routable verdict" $
            routable (mountReadiness oneOfTwo) `shouldBe` True

        it "refuses while every configured mount is still awaiting its first sync" $
            routable (mountReadiness neitherOfTwo) `shouldBe` False

        it "refuses under a latch, whatever the mounts hold" $
            routable Latched `shouldBe` False

    describe "allMountsReady -- the Dredger's wait condition, not the routing verdict" $ do
        it "holds only once every configured mount has its advisory database" $ do
            allMountsReady (mountReadiness oneOfTwo) `shouldBe` False
            allMountsReady (mountReadiness neitherOfTwo) `shouldBe` False
            allMountsReady (mountReadiness bothOfTwo) `shouldBe` True

        it "holds with no configured mount, so a plan without an advisory store never waits" $
            allMountsReady alwaysReady `shouldBe` True

        it "does not hold under a latch" $
            allMountsReady Latched `shouldBe` False

    describe "alwaysReady -- a role with no advisory mount to wait for" $
        it "is the empty plan's verdict" $
            alwaysReady `shouldBe` Routable Map.empty
  where
    oneOfTwo = Map.fromList [(Npm, MountReady), (PyPI, MountAwaitingFirstSync)]
    neitherOfTwo = Map.fromList [(Npm, MountAwaitingFirstSync), (PyPI, MountAwaitingFirstSync)]
    bothOfTwo = Map.fromList [(Npm, MountReady), (PyPI, MountReady)]
