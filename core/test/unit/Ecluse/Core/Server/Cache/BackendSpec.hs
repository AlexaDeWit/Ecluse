-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TupleSections #-}

-- | External retention failure, deadline, and cancellation contracts.
module Ecluse.Core.Server.Cache.BackendSpec (spec) where

import Test.Hspec
import UnliftIO (cancel, timeout, wait, withAsync)
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Server.Cache.Backend (BackendStorage (ExternalStorage), supportsFullRetention)
import Ecluse.Core.Server.Cache.Store (SingleFlight, newSingleFlightWithBackend, resolveSingleFlight)

import Ecluse.Test.Server.Cache (externalBackend)

data BackendFault = BackendFault
    deriving stock (Show)

instance Exception BackendFault

spec :: Spec
spec = do
    describe "externalBackend" $ do
        it "falls back once after a read fault and returns the fetch despite a write fault" $ do
            failed <- newIORef (0 :: Int)
            fetched <- newIORef (0 :: Int)
            let backend = externalBackend 100000 (\_ _ -> throwIO BackendFault) (\_ _ -> throwIO BackendFault)
            supportsFullRetention (ExternalStorage 100000) `shouldBe` True
            store <- newSingleFlightWithBackend (Just backend)
            run failed store (modifyIORef' fetched (+ 1) $> Right "fresh") `shouldReturn` Right "fresh"
            readIORef fetched `shouldReturn` 1
            readIORef failed `shouldReturn` 2

        it "bounds a stalled read and write without leaving pending work" $ do
            failed <- newIORef (0 :: Int)
            started <- newIORef (0 :: Int)
            release <- newEmptyMVar
            let block = modifyIORef' started (+ 1) >> readMVar release
                backend = externalBackend 10000 (\_ _ -> block $> Nothing) (\_ _ -> block)
            store <- newSingleFlightWithBackend (Just backend)
            timeout 1000000 (run failed store (pure (Right "fresh"))) `shouldReturn` Just (Right "fresh")
            readIORef started `shouldReturn` 2
            readIORef failed `shouldReturn` 2
            run failed store (pure (Right "again")) `shouldReturn` Right "again"
            readIORef started `shouldReturn` 4

        it "propagates cancellation and releases the flight for the next caller" $ do
            failed <- newIORef (0 :: Int)
            started <- newEmptyMVar
            release <- newEmptyMVar
            armed <- newIORef True
            let readValue _ _ = do
                    block <- atomicModifyIORef' armed (False,)
                    when block (putMVar started () >> readMVar release)
                    pure Nothing
                backend = externalBackend 1000000 readValue (\_ _ -> pass)
            store <- newSingleFlightWithBackend (Just backend)
            withAsync (run failed store (pure (Right "cancelled"))) $ \worker -> do
                takeMVar started
                cancel worker
            run failed store (pure (Right "fresh")) `shouldReturn` Right "fresh"
            readIORef failed `shouldReturn` 0

        it "shares an active backend read before a retained hit is available" $ do
            failed <- newIORef (0 :: Int)
            started <- newEmptyMVar
            joined <- newEmptyMVar
            release <- newEmptyMVar
            let backend = externalBackend 1000000 (\_ _ -> putMVar started () >> takeMVar release $> Just "held") (\_ _ -> pass)
            store <- newSingleFlightWithBackend (Just backend)
            withAsync (run failed store (pure (Right "wrong"))) $ \leader -> do
                takeMVar started
                withAsync (resolveSingleFlight (const (putMVar joined ())) (const pass) pass store "key" (pure (Right "wrong"))) $ \follower -> do
                    takeMVar joined
                    putMVar release ()
                    wait leader `shouldReturn` Right "held"
                    wait follower `shouldReturn` Right "held"

run :: IORef Int -> SingleFlight () Text Text -> IO (Either () Text) -> IO (Either () Text)
run failed store = resolveSingleFlight (const pass) (const pass) (modifyIORef' failed (+ 1)) store "key"
