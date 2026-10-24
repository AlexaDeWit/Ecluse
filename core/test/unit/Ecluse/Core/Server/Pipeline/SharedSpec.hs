-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Metadata admission queues a request a full room turns away, holding nothing while it waits,
refuses at once with @Connection: close@ past the queue's cap, and without a queue refuses a full
room at once as before.
-}
module Ecluse.Core.Server.Pipeline.SharedSpec (spec) where

import GHC.Clock (getMonotonicTime)
import GHC.Conc (BlockReason (BlockedOnSTM), ThreadStatus (ThreadBlocked), threadStatus)
import Network.HTTP.Types (ResponseHeaders, hConnection)
import Test.Hspec
import UnliftIO (async, asyncThreadId, cancel, timeout, wait, withAsync)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Core.Deadline (Deadlines, newDeadlines)
import Ecluse.Core.Server.Admission (ServeAdmission, newServeAdmissionTuned, serveAdmissionLine, withOverflow, withServeAdmission)
import Ecluse.Core.Server.Admission.Meter (MeterSettings (..), meterLine, meterSnapshot, newMemoryMeter, withMemoryEntry)
import Ecluse.Core.Server.Admission.Overflow (Attempt (Entered), Overflow, Waited (Placed), awaitPlace, newOverflowTuned, overflowWaiting)
import Ecluse.Core.Server.Admission.Types (MeterSnapshot (snChargedBytes, snWaiting))
import Ecluse.Core.Server.Context (ServeRuntime (srMemoryMeter))
import Ecluse.Core.Server.Pipeline.Shared (hRetryAfter, withMetadataAdmission)
import Ecluse.Core.Telemetry.Record (MetricsPort (..))
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Poll (awaitUntil)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Server.Handler (testServeRuntimeWith)

-- One slot and no waiting room, so a second request finds the room full at once.
oneSlot :: Deadlines -> IO ServeAdmission
oneSlot deadlines = newServeAdmissionTuned deadlines 1 0 0

-- A request through metadata admission: the headers its shed got, or what its answer saw.
request :: ServeRuntime -> IO Text -> IO (Either ResponseHeaders Text)
request runtime work = withMetadataAdmission runtime (pure . Left) (const work) (pure . Right)

-- Start a request that holds the one slot until the returned action lets it finish.
holdSlot :: ServeRuntime -> IO (IO (Either ResponseHeaders Text))
holdSlot runtime = do
    entered <- newEmptyMVar
    release <- newEmptyMVar
    holder <- async (request runtime (putMVar entered () >> takeMVar release $> "holder"))
    takeMVar entered
    pure (putMVar release () >> wait holder)

awaitQueued :: Overflow -> IO ()
awaitQueued overflow = overflowWaiting overflow >>= \waiting -> when (waiting < 1) (threadDelay 1_000 >> awaitQueued overflow)

spec :: Spec
spec = around_ (timeout 10_000_000 >=> (`shouldBe` Just ())) $ describe "withMetadataAdmission" $ do
    it "queues without a charge or slot and wakes on a freed slot without a sweeper" $ do
        deadlines <- newDeadlines
        overflow <- newOverflowTuned deadlines 8 5_000_000
        admission <- (`withOverflow` overflow) <$> oneSlot deadlines
        runtime <- testServeRuntimeWith deadlines admission noopMetricsPort
        finishHolder <- holdSlot runtime
        holderCharge <- snChargedBytes <$> meterSnapshot (srMemoryMeter runtime)
        queued <- async (request runtime (pure "queued"))
        awaitQueued overflow
        -- The queued request holds no entry step, so the meter still holds the holder's alone.
        snChargedBytes <$> meterSnapshot (srMemoryMeter runtime) `shouldReturn` holderCharge
        finishHolder `shouldReturn` Right "holder"
        wait queued `shouldReturn` Right "queued"
        overflowWaiting overflow `shouldReturn` 0

    it "refuses at once with Retry-After and Connection: close past the queue's cap" $
        withSweeper $ \deadlines -> do
            overflow <- newOverflowTuned deadlines 0 5_000_000
            admission <- (`withOverflow` overflow) <$> oneSlot deadlines
            runtime <- testServeRuntimeWith deadlines admission noopMetricsPort
            finishHolder <- holdSlot runtime
            started <- getMonotonicTime
            refused <- request runtime (pure "never")
            elapsed <- subtract started <$> getMonotonicTime
            refused `shouldBe` Left [(hRetryAfter, "1"), (hConnection, "close")]
            elapsed `shouldSatisfy` (< 0.1)
            finishHolder `shouldReturn` Right "holder"

    it "refuses with Retry-After alone once the queue's wait runs out" $
        withSweeper $ \deadlines -> do
            overflow <- newOverflowTuned deadlines 8 200_000
            admission <- (`withOverflow` overflow) <$> oneSlot deadlines
            runtime <- testServeRuntimeWith deadlines admission noopMetricsPort
            finishHolder <- holdSlot runtime
            started <- getMonotonicTime
            refused <- request runtime (pure "never")
            elapsed <- subtract started <$> getMonotonicTime
            refused `shouldBe` Left [(hRetryAfter, "1")]
            elapsed `shouldSatisfy` (>= 0.2)
            finishHolder `shouldReturn` Right "holder"

    it "refuses a full room at once with Retry-After alone when no queue is on" $
        withSweeper $ \deadlines -> do
            runtime <- (\admission -> testServeRuntimeWith deadlines admission noopMetricsPort) =<< oneSlot deadlines
            finishHolder <- holdSlot runtime
            started <- getMonotonicTime
            refused <- request runtime (pure "never")
            elapsed <- subtract started <$> getMonotonicTime
            refused `shouldBe` Left [(hRetryAfter, "1")]
            elapsed `shouldSatisfy` (< 0.1)
            finishHolder `shouldReturn` Right "holder"

    it "records no memory shed after recovery on a sweeper tick" $
        withSweeper $ \deadlines -> do
            overflow <- newOverflowTuned deadlines 8 5_000_000
            admission <- (`withOverflow` overflow) <$> oneSlot deadlines
            memoryShed <- newIORef (0 :: Int)
            let metrics = noopMetricsPort{mpMemoryAdmissionShed = modifyIORef' memoryShed (+ 1)}
            original <- testServeRuntimeWith deadlines admission metrics
            meter <- newMemoryMeter deadlines (MeterSettings 1 1 0 1_000_000)
            let runtime = original{srMemoryMeter = meter}
            finishHolder <- holdSlot runtime
            queued <- async (request runtime (pure "queued"))
            awaitQueued overflow
            snChargedBytes <$> meterSnapshot meter `shouldReturn` 1
            readIORef memoryShed `shouldReturn` 0
            finishHolder `shouldReturn` Right "holder"
            wait queued `shouldReturn` Right "queued"
            readIORef memoryShed `shouldReturn` 0
            overflowWaiting overflow `shouldReturn` 0

    it "counts a memory room expiry once without a sweeper" $ do
        deadlines <- newDeadlines
        overflow <- newOverflowTuned deadlines 8 5_000_000
        admission <- (`withOverflow` overflow) <$> oneSlot deadlines
        memoryShed <- newIORef (0 :: Int)
        let metrics = noopMetricsPort{mpMemoryAdmissionShed = modifyIORef' memoryShed (+ 1)}
        original <- testServeRuntimeWith deadlines admission metrics
        meter <- newMemoryMeter deadlines (MeterSettings 1 1 1 0)
        let runtime = original{srMemoryMeter = meter}
        finishHolder <- holdSlot runtime
        -- Zero wait pins a room expiry and its final memory refusal without a sweep.
        request runtime (pure "never") `shouldReturn` Left [(hRetryAfter, "1")]
        readIORef memoryShed `shouldReturn` 1
        finishHolder `shouldReturn` Right "holder"

    it "records one final memory shed after repeated overflow retries" $
        withSweeper $ \deadlines -> do
            overflow <- newOverflowTuned deadlines 8 250_000
            admission <- (`withOverflow` overflow) <$> oneSlot deadlines
            memoryShed <- newIORef (0 :: Int)
            let metrics = noopMetricsPort{mpMemoryAdmissionShed = modifyIORef' memoryShed (+ 1)}
            original <- testServeRuntimeWith deadlines admission metrics
            meter <- newMemoryMeter deadlines (MeterSettings 1 1 0 0)
            let runtime = original{srMemoryMeter = meter}
            finishHolder <- holdSlot runtime
            request runtime (pure "never") `shouldReturn` Left [(hRetryAfter, "1")]
            readIORef memoryShed `shouldReturn` 1
            finishHolder `shouldReturn` Right "holder"

    it "wakes the memory line when a room waiter cancels without a sweeper" $ do
        deadlines <- newDeadlines
        overflow <- newOverflowTuned deadlines 8 5_000_000
        meter <- newMemoryMeter deadlines (MeterSettings 1 1 1 5_000_000)
        release <- newEmptyMVar
        entered <- newEmptyMVar
        withAsync (withMemoryEntry noopMetricsPort meter (\_ -> putMVar entered () >> takeMVar release)) $ \_ -> do
            takeMVar entered
            withAsync (withMemoryEntry noopMetricsPort meter (\_ -> pure ())) $ \room -> do
                awaitUntil 1_000_000 1_000 ((== 1) . snWaiting <$> meterSnapshot meter) `shouldReturn` True
                withAsync (awaitPlace noopMetricsPort overflow (meterLine meter) (pure (Entered ()))) $ \queued -> do
                    awaitQueued overflow
                    cancel room
                    wait queued >>= \case
                        Placed () -> pass
                        _ -> expectationFailure "memory room departure did not wake overflow"

    it "wakes the serve line when a room waiter cancels without a sweeper" $ do
        deadlines <- newDeadlines
        overflow <- newOverflowTuned deadlines 8 5_000_000
        admission <- newServeAdmissionTuned deadlines 1 1 5_000_000
        runtime <- testServeRuntimeWith deadlines admission noopMetricsPort
        finishHolder <- holdSlot runtime
        room <- async (withServeAdmission noopMetricsPort admission (pure ()))
        awaitUntil 1_000_000 1_000 ((== ThreadBlocked BlockedOnSTM) <$> threadStatus (asyncThreadId room)) `shouldReturn` True
        withAsync (awaitPlace noopMetricsPort overflow (serveAdmissionLine admission) (pure (Entered ()))) $ \queued -> do
            awaitQueued overflow
            cancel room
            wait queued >>= \case
                Placed () -> pass
                _ -> expectationFailure "serve room departure did not wake overflow"
        finishHolder `shouldReturn` Right "holder"

    it "does not count memory shed when a retry later finds the serve gate full" $
        withSweeper $ \deadlines -> do
            overflow <- newOverflowTuned deadlines 8 350_000
            admission <- (`withOverflow` overflow) <$> oneSlot deadlines
            memoryShed <- newIORef (0 :: Int)
            let metrics = noopMetricsPort{mpMemoryAdmissionShed = modifyIORef' memoryShed (+ 1)}
            original <- testServeRuntimeWith deadlines admission metrics
            meter <- newMemoryMeter deadlines (MeterSettings 1 1 0 0)
            let runtime = original{srMemoryMeter = meter}
            memoryEntered <- newEmptyMVar
            memoryRelease <- newEmptyMVar
            serveEntered <- newEmptyMVar
            serveRelease <- newEmptyMVar
            withAsync (withMemoryEntry noopMetricsPort meter (\_ -> putMVar memoryEntered () >> takeMVar memoryRelease)) $ \memoryHolder -> do
                takeMVar memoryEntered
                withAsync (withServeAdmission noopMetricsPort admission (putMVar serveEntered () >> takeMVar serveRelease)) $ \_ -> do
                    takeMVar serveEntered
                    queued <- async (request runtime (pure "never"))
                    awaitQueued overflow
                    putMVar memoryRelease ()
                    wait memoryHolder `shouldReturn` Right ()
                    wait queued `shouldReturn` Left [(hRetryAfter, "1")]
                    readIORef memoryShed `shouldReturn` 0
