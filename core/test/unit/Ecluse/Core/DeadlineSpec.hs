-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A swept deadline fires no earlier than set and about one tick late, never lands once its scope
has ended on any exit path, and a timeout keeps 'System.Timeout.timeout''s contract.
-}
module Ecluse.Core.DeadlineSpec (spec) where

import GHC.Clock (getMonotonicTime, getMonotonicTimeNSec)
import Test.Hspec
import UnliftIO.Async (async, cancel, mapConcurrently, wait, waitCatch)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO, try, tryAny, uninterruptibleMask_)

import Ecluse.Core.Deadline (Deadlines, newDeadlines, sweepTick, timeout, withDeadline)
import Ecluse.Test.Deadline (withSweeper)

spec :: Spec
spec = do
    withDeadlineSpec
    timeoutSpec

withDeadlineSpec :: Spec
withDeadlineSpec = describe "withDeadline" $ do
    it "raises a due check no earlier than its deadline, from the first sweep after it" $
        withSweeper $ \deadlines -> do
            started <- getMonotonicTimeNSec
            firstDue <- newIORef Nothing
            let limit = started + 300_000_000
                check now
                    | now >= limit = Just (toException Fired) <$ modifyIORef' firstDue (<|> Just now)
                    | otherwise = pure Nothing
            outcome <- try (withDeadline deadlines check (threadDelay 5_000_000))
            arrived <- getMonotonicTimeNSec
            outcome `shouldBe` Left Fired
            arrived `shouldSatisfy` (>= limit)
            -- One tick is 100 ms. The rest is room for a loaded runner to schedule the sweeper.
            lateness <- fmap (subtract limit) <$> readIORef firstDue
            lateness `shouldSatisfy` maybe False (< 250_000_000)
            (arrived - limit) `shouldSatisfy` (< 600_000_000)

    it "never raises a throw already in flight once the scope ends first" $
        withSweeper $ \deadlines -> do
            asked <- newIORef False
            let check _ = Just (toException Fired) <$ writeIORef asked True
            -- Masked across the whole scope, the thread holds the throw back until the scope has ended.
            outcome <- try (uninterruptibleMask_ (withDeadline deadlines check (holdUntil (readIORef asked) >> threadDelay 50_000)))
            readIORef asked `shouldReturn` True
            outcome `shouldBe` Right @Fired ()
            try (threadDelay 400_000) `shouldReturn` Right @Fired ()

    describe "drops its check on every way out of the scope, and raises nothing after it" $ do
        it "after a return" $
            withSweeper $
                \deadlines -> exitsQuietly deadlines (\scoped -> scoped (threadDelay 250_000))

        it "after a synchronous throw" $
            withSweeper $ \deadlines -> exitsQuietly deadlines $ \scoped ->
                try (scoped (threadDelay 250_000 >> throwIO Escaped)) `shouldReturn` Left Escaped

        it "after the thread is cancelled" $
            withSweeper $ \deadlines -> exitsQuietly deadlines $ \scoped -> do
                running <- async (scoped (threadDelay 5_000_000))
                threadDelay 250_000
                cancel running
                waitCatch running >>= (`shouldSatisfy` isLeft)

    it "raises every due check when many threads arm at once" $
        withSweeper $ \deadlines -> do
            started <- getMonotonicTime
            outcomes <- mapConcurrently (const (timeout deadlines 200_000 (threadDelay 5_000_000))) [1 .. 200 :: Int]
            finished <- getMonotonicTime
            outcomes `shouldSatisfy` all isNothing
            (finished - started) `shouldSatisfy` (< 2)

    it "keeps every other armed check when one throws during a sweep" $ do
        deadlines <- newDeadlines
        thrown <- newIORef False
        counted <- newIORef (0 :: Int)
        release <- newEmptyMVar
        let throwsOnce _ = readIORef thrown >>= \done -> if done then pure Nothing else writeIORef thrown True >> throwIO Escaped
            counting _ = Nothing <$ modifyIORef' counted (+ 1)
            held check = async (withDeadline deadlines check (readMVar release))
        holders <- traverse held [throwsOnce, counting]
        threadDelay 50_000
        try (sweepTick deadlines) `shouldReturn` Left Escaped
        countedFirst <- readIORef counted
        sweepTick deadlines
        readIORef counted >>= (`shouldSatisfy` (> countedFirst))
        putMVar release ()
        traverse_ wait holders

timeoutSpec :: Spec
timeoutSpec = describe "timeout" $ do
    it "returns the result of an action that finishes in time" $
        withSweeper $
            \deadlines -> timeout deadlines 1_000_000 (pure 'x') `shouldReturn` Just 'x'

    it "never fires on a negative bound, and fires at once on zero" $
        withSweeper $ \deadlines -> do
            timeout deadlines (-1) (threadDelay 300_000 $> 'x') `shouldReturn` Just 'x'
            timeout deadlines 0 (pure 'x') `shouldReturn` Nothing

    it "keeps nested scopes apart in both directions" $
        withSweeper $ \deadlines -> do
            timeout deadlines 2_000_000 (timeout deadlines 200_000 (threadDelay 5_000_000)) `shouldReturn` Just Nothing
            timeout deadlines 200_000 (timeout deadlines 2_000_000 (threadDelay 5_000_000)) `shouldReturn` Nothing

    it "passes through a synchronous catch inside the action" $
        withSweeper $ \deadlines -> do
            started <- getMonotonicTime
            timeout deadlines 200_000 (tryAny (threadDelay 5_000_000)) >>= (`shouldSatisfy` isNothing)
            getMonotonicTime >>= (`shouldSatisfy` (< 2)) . subtract started

{- Run one scope through the given exit, with a check that would fire 150 ms after it ends, and
then show the sweeper stopped asking and nothing landed. -}
exitsQuietly :: Deadlines -> ((IO () -> IO ()) -> IO ()) -> IO ()
exitsQuietly deadlines exit = do
    started <- getMonotonicTimeNSec
    askCount <- newIORef (0 :: Int)
    let check now = do
            modifyIORef' askCount (+ 1)
            pure (if now >= started + 400_000_000 then Just (toException Fired) else Nothing)
    exit (withDeadline deadlines check)
    seen <- readIORef askCount
    seen `shouldSatisfy` (>= 1)
    try (threadDelay 500_000) `shouldReturn` Right @Fired ()
    readIORef askCount `shouldReturn` seen

-- Poll a condition for up to 2 s. It never throws, so it can run with exceptions masked.
holdUntil :: IO Bool -> IO ()
holdUntil done = go (400 :: Int)
  where
    go remaining = done >>= \finished -> unless (finished || remaining <= 0) (threadDelay 5_000 >> go (remaining - 1))

-- The failure a check raises.
data Fired = Fired
    deriving stock (Eq, Show)

instance Exception Fired

-- A fault that leaves a scope or a sweep.
data Escaped = Escaped
    deriving stock (Eq, Show)

instance Exception Escaped
