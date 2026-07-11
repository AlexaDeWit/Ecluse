module Ecluse.Server.DrainSpec (spec) where

import Test.Hspec

import Ecluse.Runtime.Server.Drain (beginDrain, isDraining, neverDraining, newDrainSignal)

spec :: Spec
spec =
    describe "DrainSignal" $ do
        it "a fresh signal is not draining" $ do
            drain <- newDrainSignal
            isDraining drain `shouldReturn` False

        it "beginDrain raises it" $ do
            drain <- newDrainSignal
            beginDrain drain
            isDraining drain `shouldReturn` True

        it "raising is idempotent (a second raise keeps it raised)" $ do
            drain <- newDrainSignal
            beginDrain drain
            beginDrain drain
            isDraining drain `shouldReturn` True

        it "neverDraining stays lowered even after a raise (the inert default)" $ do
            beginDrain neverDraining
            isDraining neverDraining `shouldReturn` False
