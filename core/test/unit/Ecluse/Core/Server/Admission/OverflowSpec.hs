-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A queued request times out at 0.5 s on the sweeper, the queue refuses at once past its cap, wakes
the oldest request first, and drops a cancelled request from the line and the count.
-}
module Ecluse.Core.Server.Admission.OverflowSpec (spec) where

import GHC.Clock (getMonotonicTime)
import GHC.Conc (BlockReason (BlockedOnSTM), ThreadStatus (ThreadBlocked), getNumCapabilities, setNumCapabilities, threadStatus)
import Test.Hspec
import UnliftIO (Async, async, asyncOn, asyncThreadId, cancel, timeout, wait, withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (bracket_, throwIO, tryAny)

import Ecluse.Core.Deadline (newDeadlines, sweepTick)
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
import Ecluse.Core.Telemetry.Record (MetricsPort (..))
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Poll (awaitUntil)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

-- Block until the queue holds this many requests.
awaitWaiting :: Overflow -> Int -> IO ()
awaitWaiting overflow count = do
    waiting <- overflowWaiting overflow
    unless (waiting == count) (threadDelay 1_000 >> awaitWaiting overflow count)

awaitLine :: Async a -> IO ()
awaitLine worker =
    awaitUntil 1_000_000 1_000 ((== ThreadBlocked BlockedOnSTM) <$> threadStatus (asyncThreadId worker))
        `shouldReturn` True

outcomeName :: Waited a -> Text
outcomeName = \case
    Placed _ -> "placed"
    WaitedOut _ -> "waited out"
    QueueFull _ -> "queue full"

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

    it "keeps the cap while all woken retries pause and releases cancelled permits" $ do
        deadlines <- newDeadlines
        overflow <- newOverflow deadlines 1
        line <- newLine
        entered <- newEmptyMVar
        release <- newEmptyMVar
        let retry = putMVar entered () >> takeMVar release $> (TurnedAway line :: Attempt ())
        workers <- forM [1 .. 8] $ \count -> do
            worker <- async (awaitPlace noopMetricsPort overflow line retry)
            awaitWaiting overflow count
            pure worker
        for_ workers $ \_ -> atomically (signalLine line) >> takeMVar entered
        overflowWaiting overflow `shouldReturn` 0
        outcomeName <$> awaitPlace noopMetricsPort overflow line retry `shouldReturn` "queue full"
        traverse_ cancel workers
        overflowWaiting overflow `shouldReturn` 0
        withAsync (awaitPlace noopMetricsPort overflow line (pure (Entered ()))) $ \worker -> do
            awaitWaiting overflow 1
            atomically (signalLine line)
            outcomeName <$> wait worker `shouldReturn` "placed"

    it "orders cross-line returns by immutable arrival, even when retries return backwards" $ do
        deadlines <- newDeadlines
        overflow <- newOverflowTuned deadlines 8 5_000_000
        source <- newLine
        destination <- newLine
        olderStarted <- newEmptyMVar
        youngerStarted <- newEmptyMVar
        olderRelease <- newEmptyMVar
        youngerRelease <- newEmptyMVar
        order <- newEmptyMVar
        let attempts name started release = do
                initialAttempt <- newIORef True
                pure $
                    atomicModifyIORef' initialAttempt (False,) >>= \case
                        True -> putMVar started () >> takeMVar release $> TurnedAway destination
                        False -> putMVar order name $> Entered ()
        olderTry <- attempts ("older" :: Text) olderStarted olderRelease
        youngerTry <- attempts "younger" youngerStarted youngerRelease
        withAsync (awaitPlace noopMetricsPort overflow source olderTry) $ \older -> do
            awaitWaiting overflow 1
            withAsync (awaitPlace noopMetricsPort overflow source youngerTry) $ \younger -> do
                awaitWaiting overflow 2
                atomically (signalLine source)
                takeMVar olderStarted
                atomically (signalLine source)
                takeMVar youngerStarted
                putMVar youngerRelease ()
                awaitLine younger
                putMVar olderRelease ()
                awaitLine older
                atomically (signalLine destination)
                takeMVar order `shouldReturn` "older"
                outcomeName <$> wait older `shouldReturn` "placed"
                atomically (signalLine destination)
                takeMVar order `shouldReturn` "younger"
                outcomeName <$> wait younger `shouldReturn` "placed"

    it "does not retry a wake delivered after the absolute deadline without a sweep" $ do
        deadlines <- newDeadlines
        overflow <- newOverflowTuned deadlines 1 20_000
        line <- newLine
        tries <- newIORef (0 :: Int)
        withAsync (awaitPlace noopMetricsPort overflow line (modifyIORef' tries (+ 1) $> Entered ())) $ \worker -> do
            awaitWaiting overflow 1
            threadDelay 40_000
            atomically (signalLine line)
            outcomeName <$> wait worker `shouldReturn` "waited out"
        readIORef tries `shouldReturn` 0
        overflowWaiting overflow `shouldReturn` 0

    it "does not renew overflow after a retry crosses its deadline" $ do
        deadlines <- newDeadlines
        overflow <- newOverflowTuned deadlines 1 20_000
        line <- newLine
        started <- newEmptyMVar
        release <- newEmptyMVar
        withAsync (awaitPlace noopMetricsPort overflow line (putMVar started () >> takeMVar release $> (TurnedAway line :: Attempt ()))) $ \worker -> do
            awaitWaiting overflow 1
            atomically (signalLine line)
            takeMVar started
            threadDelay 40_000
            putMVar release ()
            outcomeName <$> wait worker `shouldReturn` "waited out"
        overflowWaiting overflow `shouldReturn` 0

    it "counts a room expiry as admitted and a tick alone retries an unsignalled line" $ do
        deadlines <- newDeadlines
        overflow <- newOverflowTuned deadlines 1 5_000_000
        line <- newLine
        admitted <- newIORef (0 :: Int)
        shed <- newIORef (0 :: Int)
        let metrics = noopMetricsPort{mpOverflowAdmitted = modifyIORef' admitted (+ 1), mpOverflowShed = modifyIORef' shed (+ 1)}
        withAsync (awaitPlace metrics overflow line (pure (Expired line :: Attempt ()))) $ \worker -> do
            awaitWaiting overflow 1
            sweepTick deadlines
            outcomeName <$> wait worker `shouldReturn` "waited out"
        readIORef admitted `shouldReturn` 1
        readIORef shed `shouldReturn` 0
        overflowWaiting overflow `shouldReturn` 0

    it "passes an expired oldest wake to a younger live waiter without a sweep" $ do
        deadlines <- newDeadlines
        overflow <- newOverflowTuned deadlines 2 100_000
        line <- newLine
        tries <- newIORef ([] :: [Text])
        withAsync (awaitPlace noopMetricsPort overflow line (modifyIORef' tries ("old" :) $> Entered ())) $ \older -> do
            awaitWaiting overflow 1
            threadDelay 150_000
            withAsync (awaitPlace noopMetricsPort overflow line (modifyIORef' tries ("young" :) $> Entered ())) $ \younger -> do
                awaitWaiting overflow 2
                atomically (signalLine line)
                outcomeName <$> wait older `shouldReturn` "waited out"
                outcomeName <$> wait younger `shouldReturn` "placed"
        readIORef tries `shouldReturn` ["young"]
        overflowWaiting overflow `shouldReturn` 0

    it "returns its permit when an admission metric throws" $ do
        deadlines <- newDeadlines
        overflow <- newOverflowTuned deadlines 1 5_000_000
        line <- newLine
        let metrics = noopMetricsPort{mpOverflowAdmitted = throwIO (TestContractEscape "metric failed")}
        withAsync (tryAny (awaitPlace metrics overflow line (pure (Entered ())))) $ \worker -> do
            awaitWaiting overflow 1
            atomically (signalLine line)
            isLeft <$> wait worker `shouldReturn` True
        overflowWaiting overflow `shouldReturn` 0

    it "wakes only one request per line in a sweep across capabilities" $ do
        capabilities <- getNumCapabilities
        bracket_ (setNumCapabilities (max 2 capabilities)) (setNumCapabilities capabilities) $ do
            deadlines <- newDeadlines
            overflow <- newOverflowTuned deadlines 2 5_000_000
            line <- newLine
            tries <- newTVarIO (0 :: Int)
            release <- newEmptyMVar
            let retry = atomically (modifyTVar' tries (+ 1)) >> takeMVar release $> (TurnedAway line :: Attempt ())
            older <- asyncOn 0 (awaitPlace noopMetricsPort overflow line retry)
            awaitWaiting overflow 1
            younger <- asyncOn 1 (awaitPlace noopMetricsPort overflow line retry)
            awaitWaiting overflow 2
            awaitLine older
            awaitLine younger
            sweepTick deadlines
            awaitUntil 1_000_000 1_000 ((>= 1) <$> readTVarIO tries) `shouldReturn` True
            awaitLine younger
            readTVarIO tries `shouldReturn` 1
            cancel older
            cancel younger
            overflowWaiting overflow `shouldReturn` 0

    it "recovers on the next tick when a consumed wake's retry is cancelled" $ do
        deadlines <- newDeadlines
        overflow <- newOverflowTuned deadlines 2 5_000_000
        line <- newLine
        started <- newEmptyMVar
        release <- newEmptyMVar
        withAsync (awaitPlace noopMetricsPort overflow line (putMVar started () >> takeMVar release $> Entered ())) $ \older -> do
            awaitWaiting overflow 1
            withAsync (awaitPlace noopMetricsPort overflow line (pure (Entered ()))) $ \younger -> do
                awaitWaiting overflow 2
                atomically (signalLine line)
                takeMVar started
                cancel older
                overflowWaiting overflow `shouldReturn` 1
                sweepTick deadlines
                outcomeName <$> wait younger `shouldReturn` "placed"
        overflowWaiting overflow `shouldReturn` 0
