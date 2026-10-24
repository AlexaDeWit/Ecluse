-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A queued request times out at 0.5 s on the sweeper, the queue refuses at once past its cap, wakes
the oldest request first, and drops a cancelled request from the line and the count.
-}
module Ecluse.Core.Server.Admission.OverflowSpec (spec) where

import GHC.Clock (getMonotonicTime)
import Test.Hspec
import UnliftIO (async, cancel, timeout, wait, withAsync)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Core.Deadline (newDeadlines)
import Ecluse.Core.Server.Admission.Overflow (
    Attempt (..),
    Overflow,
    Waited (..),
    awaitPlace,
    newLine,
    newOverflow,
    newOverflowTuned,
    overflowWaiting,
    signalLine,
 )
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Port (noopMetricsPort)

-- Block until the queue holds this many requests.
awaitWaiting :: Overflow -> Int -> IO ()
awaitWaiting overflow count = do
    waiting <- overflowWaiting overflow
    unless (waiting == count) (threadDelay 1_000 >> awaitWaiting overflow count)

outcomeName :: Waited a -> Text
outcomeName = \case
    Placed _ -> "placed"
    WaitedOut -> "waited out"
    QueueFull -> "queue full"

spec :: Spec
spec = around_ (timeout 10_000_000 >=> (`shouldBe` Just ())) $ describe "awaitPlace" $ do
    it "refuses a request no place frees for once 0.5 s pass, retrying it on the sweeper's tick meanwhile" $
        withSweeper $ \deadlines -> do
            overflow <- newOverflow deadlines 1
            line <- newLine
            tries <- newIORef (0 :: Int)
            started <- getMonotonicTime
            outcome <- awaitPlace noopMetricsPort overflow line (modifyIORef' tries (+ 1) $> (TurnedAway line :: Attempt ()))
            elapsed <- subtract started <$> getMonotonicTime
            outcomeName outcome `shouldBe` "waited out"
            elapsed `shouldSatisfy` (>= 0.5)
            elapsed `shouldSatisfy` (< 1.5)
            readIORef tries >>= (`shouldSatisfy` (>= 1))
            overflowWaiting overflow `shouldReturn` 0

    it "refuses at once past its cap, and drops a cancelled request from the count" $
        withSweeper $ \deadlines -> do
            overflow <- newOverflowTuned deadlines 1 5_000_000
            line <- newLine
            let stillTurnedAway = pure (TurnedAway line :: Attempt ())
            withAsync (awaitPlace noopMetricsPort overflow line stillTurnedAway) $ \queued -> do
                awaitWaiting overflow 1
                started <- getMonotonicTime
                beyondCap <- awaitPlace noopMetricsPort overflow line stillTurnedAway
                elapsed <- subtract started <$> getMonotonicTime
                outcomeName beyondCap `shouldBe` "queue full"
                elapsed `shouldSatisfy` (< 0.1)
                cancel queued
            overflowWaiting overflow `shouldReturn` 0

    it "wakes the oldest request first on each freed place" $ do
        -- Nothing sweeps these deadlines, so only the signals below wake anyone.
        deadlines <- newDeadlines
        overflow <- newOverflowTuned deadlines 8 5_000_000
        line <- newLine
        order <- newIORef []
        let tryAs name = modifyIORef' order (name :) $> Entered name
            queue name count = async (awaitPlace noopMetricsPort overflow line (tryAs (name :: Text))) <* awaitWaiting overflow count
        waiters <- sequence [queue "first" 1, queue "second" 2, queue "third" 3]
        for_ waiters $ \_ -> do
            atomically (signalLine line)
            threadDelay 20_000
        traverse_ wait waiters
        reverse <$> readIORef order `shouldReturn` ["first", "second", "third"]

    it "gives the next wake to the request behind one that was cancelled" $ do
        deadlines <- newDeadlines
        overflow <- newOverflowTuned deadlines 8 5_000_000
        line <- newLine
        woken <- newEmptyMVar
        cancelled <- async (awaitPlace noopMetricsPort overflow line (putMVar woken "cancelled" $> Entered ()))
        awaitWaiting overflow 1
        behind <- async (awaitPlace noopMetricsPort overflow line (putMVar woken "behind" $> Entered ()))
        awaitWaiting overflow 2
        cancel cancelled
        awaitWaiting overflow 1
        atomically (signalLine line)
        takeMVar woken `shouldReturn` ("behind" :: Text)
        outcomeName <$> wait behind `shouldReturn` "placed"
        overflowWaiting overflow `shouldReturn` 0
