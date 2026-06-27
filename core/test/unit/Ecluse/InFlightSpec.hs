module Ecluse.InFlightSpec (spec) where

import Test.Hspec
import UnliftIO (async, cancel, timeout, wait)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (mask, throwString, try)

import Ecluse.Core.InFlight (guardInFlight)

spec :: Spec
spec = do
    describe "guardInFlight" $ do
        it "returns the body's result and releases the slot, never touching the orphan hook, on a normal exit" $ do
            released <- newIORef (0 :: Int)
            orphaned <- newIORef (0 :: Int)
            result <-
                mask $ \restore ->
                    guardInFlight restore (\_ -> bump orphaned) (bump released) (pure (42 :: Int))
            result `shouldBe` 42
            readIORef released `shouldReturn` 1
            -- A clean run never invokes the orphan hand-off.
            readIORef orphaned `shouldReturn` 0

        it "hands the error to the orphan hook and still releases the slot on a synchronous failure" $ do
            released <- newIORef (0 :: Int)
            orphaned <- newIORef (0 :: Int)
            outcome <-
                try $
                    mask $ \restore ->
                        guardInFlight restore (\_ -> bump orphaned) (bump released) (throwString "boom" :: IO Int)
            -- The body's exception propagates to the leader unchanged.
            (outcome :: Either SomeException Int) `shouldSatisfy` isLeft
            readIORef orphaned `shouldReturn` 1
            readIORef released `shouldReturn` 1

        it "runs the orphan hook before releasing the slot" $ do
            steps <- newIORef []
            let record s = atomicModifyIORef' steps (\xs -> (xs <> [s :: Text], ()))
            _ <-
                try
                    ( mask $ \restore ->
                        guardInFlight restore (\_ -> record "orphan") (record "release") (throwString "boom" :: IO ())
                    ) ::
                    IO (Either SomeException ())
            -- Order matters: a consumer fills its result promise (orphan) before the
            -- slot is freed (release), so a follower never sees the slot gone without
            -- the result delivered.
            readIORef steps `shouldReturn` ["orphan", "release"]

        it "unblocks a waiting follower and releases the slot when the leader is cancelled mid-body (no orphan window)" $ do
            -- The hazard the guard closes: an async exception lands on the leader while
            -- it is inside the body. The orphan hand-off must fill the follower's promise
            -- and the slot must still free, or the follower parks forever. A 'timeout'
            -- turns a regression into a fast failure instead of a hung suite.
            outcome <- timeout 5_000_000 $ do
                released <- newIORef (0 :: Int)
                promise <- newEmptyTMVarIO
                running <- newEmptyMVar
                let onOrphan e = atomically $ do
                        unfilled <- isEmptyTMVar promise
                        when unfilled (putTMVar promise (Left e))
                    body = do
                        putMVar running () -- the leader is now inside the interruptible body
                        forever (threadDelay 1_000_000)
                leader <- async (mask $ \restore -> guardInFlight restore onOrphan (bump released) body)
                takeMVar running
                -- A follower blocks on the promise; it must unblock when the leader is
                -- cancelled, never park forever.
                follower <- async (atomically (readTMVar promise) :: IO (Either SomeException ()))
                threadDelay 30000
                cancel leader -- async-cancel inside the body; 'cancel' waits for the leader to die
                followed <- wait follower
                rel <- readIORef released
                pure (isLeft followed, rel)
            outcome `shouldBe` Just (True, 1)

        it "releases the slot under a no-op orphan hook when cancelled (the flag-only consumer)" $ do
            -- The credential refresher's shape: no result promise, so the orphan hook is
            -- a no-op and freeing the slot is the whole signal. A cancel mid-body must
            -- still run the release.
            outcome <- timeout 5_000_000 $ do
                released <- newIORef (0 :: Int)
                running <- newEmptyMVar
                let body = putMVar running () >> forever (threadDelay 1_000_000)
                leader <- async (mask $ \restore -> guardInFlight restore (const (pure ())) (bump released) body)
                takeMVar running
                threadDelay 30000
                cancel leader -- waits for the leader's release to run
                readIORef released
            outcome `shouldBe` Just 1
  where
    bump :: IORef Int -> IO ()
    bump ref = atomicModifyIORef' ref (\n -> (n + 1, ()))
