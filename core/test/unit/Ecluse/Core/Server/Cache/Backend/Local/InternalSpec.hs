-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Aggregate capacity and STM rollback across local-store maintenance.
module Ecluse.Core.Server.Cache.Backend.Local.InternalSpec (spec) where

import System.Clock (fromNanoSecs)
import Test.Hspec

import Ecluse.Core.Server.Cache.Backend.Local.Internal

data Aborted = Aborted
    deriving stock (Show, Eq)

instance Exception Aborted

spec :: Spec
spec = describe "LocalPool" $ do
    it "tests one aggregate entry limit and byte limit" $ do
        pool <- newLocalPool 2 10
        atomically (poolFits pool 10) `shouldReturn` True
        atomically (adjustPool pool 1 7)
        atomically (poolFits pool 4) `shouldReturn` False
        atomically (poolFits pool 3) `shouldReturn` True
        atomically (adjustPool pool 1 0)
        atomically (poolFits pool 0) `shouldReturn` False

    it "rolls back occupancy if a store transaction aborts" $ do
        pool <- newLocalPoolWithClock (pure (fromNanoSecs 0)) 1 10
        runPool pool (\_ -> adjustPool pool 1 10 >> throwSTM Aborted) `shouldThrow` (== Aborted)
        atomically (poolFits pool 10) `shouldReturn` True

    it "refuses negative and sentinel weights without overflow" $ do
        pool <- newLocalPool maxBound maxBound
        map (poolAcceptsWeight pool) [-1, 0, maxBound - 1, maxBound] `shouldBe` [False, True, True, False]
