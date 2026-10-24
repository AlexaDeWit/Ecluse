-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Metadata admission queues a request a full room turns away, holding nothing while it waits,
refuses at once with @Connection: close@ past the queue's cap, and without a queue refuses a full
room at once as before.
-}
module Ecluse.Core.Server.Pipeline.SharedSpec (spec) where

import GHC.Clock (getMonotonicTime)
import Network.HTTP.Types (ResponseHeaders, hConnection)
import Test.Hspec
import UnliftIO (async, timeout, wait)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Server.Admission (ServeAdmission, newServeAdmissionTuned, withOverflow)
import Ecluse.Core.Server.Admission.Meter (meterSnapshot)
import Ecluse.Core.Server.Admission.Overflow (Overflow, newOverflowTuned, overflowWaiting)
import Ecluse.Core.Server.Admission.Types (MeterSnapshot (snChargedBytes))
import Ecluse.Core.Server.Context (ServeRuntime (srMemoryMeter))
import Ecluse.Core.Server.Pipeline.Shared (hRetryAfter, withMetadataAdmission)
import Ecluse.Test.Deadline (withSweeper)
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
    it "queues a request a full room turns away, holding no charge or slot, and admits it when the slot frees" $
        withSweeper $ \deadlines -> do
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
