-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Pooled retention, indexed recency, expiry, and cancellation accounting.
module Ecluse.Core.Server.Cache.Backend.LocalSpec (spec) where

import Control.Exception (throw)
import System.Clock (fromNanoSecs)
import Test.Hspec
import UnliftIO (cancel, mapConcurrently_, timeout, withAsync)

import Ecluse.Core.Server.Cache.Backend (BackendStorage (LocalStorage), CacheOccupancy (..), Recency (..), RetentionOperations (..), supportsFullRetention)
import Ecluse.Core.Server.Cache.Backend.Local
import Ecluse.Core.Server.Cache.Backend.Local.Internal (newLocalPoolWithClock)
import Ecluse.Core.Server.Cache.Store (SingleFlight, lookupStore, newSingleFlightWithBackend, resolveSingleFlight)
import Ecluse.Core.Server.Cache.Types (StoreBudget (..))
import Ecluse.Test.Server.Cache (newLocalBackend)

data Weighed = Weighed
    deriving stock (Show)

instance Exception Weighed

spec :: Spec
spec = do
    describe "newLocalBackend" $
        for_ [(0, 100), (100, 0)] $ \(entries, bytes) ->
            it ("serves without weighing at bounds " <> show (entries, bytes)) $ do
                backend <- newLocalBackend 60 entries bytes (\_ -> throw Weighed)
                supportsFullRetention LocalStorage `shouldBe` False
                store <- newSingleFlightWithBackend (Just backend) :: IO (SingleFlight () Text Text)
                resolveSingleFlight (const pass) (const pass) pass store "key" (pure (Right "value")) `shouldReturn` Right "value"
                lookupStore (const pass) pass PreserveRecency store "key" `shouldReturn` Nothing

    describe "newPooledRetention" $ do
        it "lets either store consume idle capacity without retaining another store's values" $ do
            pool <- newLocalPool 3 30
            selected <- weightedStore pool (StoreBudget 0 0)
            assembled <- newPooledRetention pool 60 (StoreBudget 0 0) (const 10) :: IO (RetentionOperations Int Text)
            for_ [1 .. 3] $ \key -> putWeight selected key 10
            traverse (getWeight selected) [1 .. 3] `shouldReturn` replicate 3 (Just 10)
            refused <- newIORef (0 :: Int)
            roInsert assembled (const pass) (modifyIORef' refused (+ 1)) 1 "body"
            readIORef refused `shouldReturn` 1
            roLookup assembled (const pass) PreserveRecency 1 `shouldReturn` Nothing

        it "evicts the minimum recency only until aggregate bytes fit" $ do
            pool <- newLocalPool 10 30
            store <- weightedStore pool (StoreBudget 0 0)
            for_ [1 .. 3] $ \key -> putWeight store key 10
            roLookup store (const pass) RefreshRecency 1 `shouldReturn` Just 10
            putWeight store 4 10
            traverse (getWeight store) [1 .. 4] `shouldReturn` [Just 10, Nothing, Just 10, Just 10]
            putWeight store 5 20
            traverse (getWeight store) [1, 3, 4, 5] `shouldReturn` [Nothing, Nothing, Just 10, Just 20]

        it "preserves recency on probes and replaces one indexed weight per key" $ do
            pool <- newLocalPool 2 30
            store <- weightedStore pool (StoreBudget 0 0)
            putWeight store 1 20
            putWeight store 1 10
            putWeight store 2 20
            getWeight store 1 `shouldReturn` Just 10
            putWeight store 3 10
            traverse (getWeight store) [1 .. 3] `shouldReturn` [Nothing, Just 20, Just 10]

        for_ [StoreBudget 1 0, StoreBudget 0 10] $ \floorBudget ->
            it ("refuses admission before crossing the store floor " <> show floorBudget) $ do
                pool <- newLocalPool 3 30
                other <- weightedStore pool (StoreBudget 0 0)
                store <- weightedStore pool floorBudget
                putWeight other 1 20
                putWeight store 1 10
                refused <- newIORef (0 :: Int)
                roInsert store (const pass) (modifyIORef' refused (+ 1)) 2 10
                readIORef refused `shouldReturn` 1
                traverse (getWeight store) [1, 2] `shouldReturn` [Just 10, Nothing]

        it "reports partial eviction when a floor prevents the remaining admission" $ do
            pool <- newLocalPool 4 40
            other <- weightedStore pool (StoreBudget 0 0)
            store <- weightedStore pool (StoreBudget 1 10)
            putWeight other 1 10
            for_ [1 .. 3] $ \key -> putWeight store key 10
            observed <- newIORef (CacheOccupancy 3 30)
            refused <- newIORef (0 :: Int)
            roInsert store (writeIORef observed) (modifyIORef' refused (+ 1)) 4 25
            readIORef refused `shouldReturn` 1
            readIORef observed `shouldReturn` CacheOccupancy 1 10
            traverse (getWeight store) [1 .. 4] `shouldReturn` [Nothing, Nothing, Just 10, Nothing]

        it "refuses an oversized candidate without evicting a retained value" $ do
            pool <- newLocalPool 1 10
            store <- weightedStore pool (StoreBudget 0 0)
            putWeight store 1 10
            refused <- newIORef (0 :: Int)
            roInsert store (const pass) (modifyIORef' refused (+ 1)) 2 11
            readIORef refused `shouldReturn` 1
            getWeight store 1 `shouldReturn` Just 10

        it "reclaims expired entries in an idle store and reports its zero occupancy" $ do
            clock <- newIORef (fromNanoSecs 0)
            pool <- newLocalPoolWithClock (readIORef clock) 1 10
            idle <- weightedStore pool (StoreBudget 1 10)
            active <- weightedStore pool (StoreBudget 0 0)
            observed <- newIORef (CacheOccupancy 0 0)
            roInsert idle (writeIORef observed) pass 1 10
            writeIORef clock (fromNanoSecs 60000000001)
            putWeight active 2 10
            getWeight idle 1 `shouldReturn` Nothing
            getWeight active 2 `shouldReturn` Just 10
            readIORef observed `shouldReturn` CacheOccupancy 0 0

        it "reclaims an idle store's expiry when another store reads a fresh hit" $ do
            clock <- newIORef (fromNanoSecs 0)
            pool <- newLocalPoolWithClock (readIORef clock) 2 20
            idle <- weightedStore pool (StoreBudget 0 0)
            active <- weightedStore pool (StoreBudget 0 0)
            observed <- newIORef (CacheOccupancy 0 0)
            roInsert idle (writeIORef observed) pass 1 10
            writeIORef clock (fromNanoSecs 30000000000)
            putWeight active 2 10
            getWeight active 2 `shouldReturn` Just 10
            readIORef observed `shouldReturn` CacheOccupancy 1 10
            writeIORef clock (fromNanoSecs 60000000001)
            getWeight active 2 `shouldReturn` Just 10
            readIORef observed `shouldReturn` CacheOccupancy 0 0
            getWeight idle 1 `shouldReturn` Nothing

        it "keeps a replacement alive when its old deadline expires" $ do
            clock <- newIORef (fromNanoSecs 0)
            pool <- newLocalPoolWithClock (readIORef clock) 1 20
            store <- weightedStore pool (StoreBudget 0 0)
            putWeight store 1 10
            writeIORef clock (fromNanoSecs 30000000000)
            putWeight store 1 20
            writeIORef clock (fromNanoSecs 60000000001)
            getWeight store 1 `shouldReturn` Just 20
            writeIORef clock (fromNanoSecs 90000000001)
            getWeight store 1 `shouldReturn` Nothing
            putWeight store 2 20
            getWeight store 2 `shouldReturn` Just 20

        it "keeps concurrent stores within one aggregate count and byte bound" $ do
            pool <- newLocalPool 10 100
            left <- weightedStore pool (StoreBudget 0 0)
            right <- weightedStore pool (StoreBudget 0 0)
            leftOccupancy <- newIORef (CacheOccupancy 0 0)
            rightOccupancy <- newIORef (CacheOccupancy 0 0)
            let insert key = do
                    let (store, observed) = if even key then (left, leftOccupancy) else (right, rightOccupancy)
                    roInsert store (writeIORef observed) pass key 10
                    void (roLookup store (writeIORef observed) RefreshRecency key)
            mapConcurrently_ insert [1 .. 100]
            values <- catMaybes <$> traverse (\key -> getWeight (if even key then left else right) key) [1 .. 100]
            length values `shouldBe` 10
            sum values `shouldBe` 100
            observations <- traverse readIORef [leftOccupancy, rightOccupancy]
            sum (map occEntries observations) `shouldBe` length values
            sum (map occBytes observations) `shouldBe` sum values

        it "keeps committed indexes and accounting after cancellation during reporting" $ do
            outcome <- timeout 1000000 $ do
                pool <- newLocalPool 1 10
                store <- weightedStore pool (StoreBudget 0 0)
                entered <- newEmptyMVar
                blocked <- newEmptyMVar
                let record _ = putMVar entered () >> takeMVar blocked
                withAsync (roInsert store record pass 1 10) $ \writer -> do
                    takeMVar entered
                    cancel writer
                getWeight store 1 `shouldReturn` Just 10
                putWeight store 2 10
                traverse (getWeight store) [1, 2] `shouldReturn` [Nothing, Just 10]
            outcome `shouldBe` Just ()

weightedStore :: LocalPool -> StoreBudget -> IO (RetentionOperations Int Int)
weightedStore pool floorBudget = newPooledRetention pool 60 floorBudget id

putWeight :: RetentionOperations Int Int -> Int -> Int -> IO ()
putWeight store = roInsert store (const pass) pass

getWeight :: RetentionOperations Int Int -> Int -> IO (Maybe Int)
getWeight store = roLookup store (const pass) PreserveRecency
