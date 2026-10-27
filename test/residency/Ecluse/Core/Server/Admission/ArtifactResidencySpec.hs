-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Verified-read byte reservations outlive delivery and release queued readers after cancellation.
module Ecluse.Core.Server.Admission.ArtifactResidencySpec (spec) where

import Control.Concurrent.MVar qualified as MVar
import Control.Concurrent.STM (check)
import Test.Hspec
import UnliftIO.Async (Async, cancel, race, wait, waitCatch, withAsync)

import Ecluse.Core.Server.Admission.Artifact (VerifiedArtifactBudget, newVerifiedArtifactBudget, verifiedArtifactWaiting, withVerifiedArtifactAdmission)
import Ecluse.Core.Telemetry.Record (MetricsPort (..))
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "verified artifact reservation lifetime" $ do
    it "refuses overweight reads without clamping or entering the body action" $
        withBudget $ \budget metrics held -> do
            entered <- newIORef False
            withVerifiedArtifactAdmission metrics budget (capacity + 1) (writeIORef entered True) `shouldReturn` Nothing
            readIORef entered `shouldReturn` False
            readIORef held `shouldReturn` 0
    it "cancels an observed queued read without retaining bytes or consuming capacity" $
        withBudget $ \budget metrics held -> do
            entered <- MVar.newEmptyMVar
            release <- MVar.newEmptyMVar
            calls <- newIORef (0 :: Int)
            withAsync (withVerifiedArtifactAdmission metrics budget capacity (MVar.putMVar entered () >> MVar.takeMVar release)) $ \holder -> do
                MVar.takeMVar entered
                readIORef held `shouldReturn` capacity
                withAsync (withVerifiedArtifactAdmission metrics budget capacity (modifyIORef' calls (+ 1))) $ \queued -> do
                    awaitQueued budget queued
                    readIORef calls `shouldReturn` 0
                    readIORef held `shouldReturn` capacity
                    cancel queued
                    waitCatch queued >>= (`shouldSatisfy` isLeft)
                    atomically (verifiedArtifactWaiting budget) `shouldReturn` 0
                    readIORef held `shouldReturn` capacity
                MVar.putMVar release ()
                wait holder `shouldReturn` Just ()
            readIORef held `shouldReturn` 0
            withVerifiedArtifactAdmission metrics budget capacity (modifyIORef' calls (+ 1)) `shouldReturn` Just ()
            readIORef calls `shouldReturn` 1
    it "holds bytes through a blocked delivery and releases them when the client is cancelled" $
        withBudget $ \budget metrics held -> do
            delivery <- MVar.newEmptyMVar
            blocked <- MVar.newEmptyMVar :: IO (MVar.MVar ())
            resumed <- newIORef False
            withAsync (withVerifiedArtifactAdmission metrics budget capacity (MVar.putMVar delivery () >> MVar.takeMVar blocked)) $ \client -> do
                MVar.takeMVar delivery
                readIORef held `shouldReturn` capacity
                withAsync (withVerifiedArtifactAdmission metrics budget capacity (writeIORef resumed True)) $ \queued -> do
                    awaitQueued budget queued
                    readIORef resumed `shouldReturn` False
                    cancel client
                    waitCatch client >>= (`shouldSatisfy` isLeft)
                    wait queued `shouldReturn` Just ()
            readIORef resumed `shouldReturn` True
            readIORef held `shouldReturn` 0
            atomically (verifiedArtifactWaiting budget) `shouldReturn` 0

capacity :: Int
capacity = 64 * 1024

withBudget :: (VerifiedArtifactBudget -> MetricsPort -> IORef Int -> IO a) -> IO a
withBudget use = withSweeper $ \deadlines -> do
    budget <- newVerifiedArtifactBudget deadlines capacity capacity >>= expectRight
    held <- newIORef 0
    let metrics = noopMetricsPort{mpArtifactBodyInFlightBytes = \delta -> atomicModifyIORef' held (\before -> (before + delta, ()))}
    use budget metrics held

awaitQueued :: VerifiedArtifactBudget -> Async a -> IO ()
awaitQueued budget worker =
    race (atomically (verifiedArtifactWaiting budget >>= check . (== 1))) (wait worker) >>= \case
        Left () -> pure ()
        Right _ -> expectationFailure "verified reader ended before entering the actual waiting room"
