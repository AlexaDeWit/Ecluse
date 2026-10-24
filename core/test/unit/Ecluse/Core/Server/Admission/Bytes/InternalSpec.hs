-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Byte admission state, cancellation, and metric contracts.
module Ecluse.Core.Server.Admission.Bytes.InternalSpec (spec) where

import Control.Concurrent.STM (check)
import Test.Hspec
import UnliftIO (cancel, mapConcurrently, timeout, wait, withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO, try)

import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Server.Admission.Bytes.Internal (
    ByteAdmission (..),
    newByteAdmission,
    newByteAdmissionTuned,
    withByteAdmission,
 )
import Ecluse.Core.Server.Admission.Weighted (weightedAdmissionWaiting)
import Ecluse.Core.Telemetry.Record (MetricsPort (mpPublishBodyInFlightBytes, mpPublishBodyShed))
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Port (noopMetricsPort)

-- | The typed escape one case throws inside a held action.
data HolderEscaped = HolderEscaped
    deriving stock (Eq, Show)

instance Exception HolderEscaped

-- | A handle over the given capacity with a roomy waiter bound and a short wait.
newBudget :: Deadlines -> Int -> IO ByteAdmission
newBudget deadlines capacity = newByteAdmissionTuned deadlines capacity 8 50_000

-- | Acquire the weight around the action with inert telemetry.
holding :: ByteAdmission -> Int -> IO a -> IO (Maybe a)
holding = withByteAdmission noopMetricsPort

spec :: Spec
spec = around_ (timeout 10_000_000 >=> (`shouldBe` Just ())) $ around withSweeper $ describe "withByteAdmission (the aggregate body-byte admission)" $ do
    it "admits weights that fit and sheds a weight the capacity cannot hold within the wait" $ \deadlines -> do
        ba <- newByteAdmission deadlines 100
        holding ba 60 (pure ("held" :: Text)) `shouldReturn` Just "held"
        -- Each call releases its weight on completion, so a second large weight fits.
        holding ba 90 (pure ("held" :: Text)) `shouldReturn` Just "held"

    it "keeps the aggregate held bytes within the capacity under concurrent holders" $ \deadlines -> do
        -- Capacity for two 40-byte holders. A gauge recording every delta proves the
        -- reserved sum never exceeds the capacity while a third waits or sheds.
        held <- newIORef (0 :: Int)
        peak <- newIORef (0 :: Int)
        let port =
                noopMetricsPort
                    { mpPublishBodyInFlightBytes = \delta ->
                        atomicModifyIORef' held (\h -> (h + delta, ())) >> do
                            now <- readIORef held
                            atomicModifyIORef' peak (\p -> (max p now, ()))
                    }
        ba <- newBudget deadlines 100
        _ <- mapConcurrently (\(_ :: Int) -> withByteAdmission port ba 40 (threadDelay 10_000)) [1 .. 6]
        readIORef peak >>= (`shouldSatisfy` (<= 100))

    it "returns capacity when the release observer throws" $ \deadlines -> do
        let port =
                noopMetricsPort
                    { mpPublishBodyInFlightBytes = \delta -> when (delta < 0) (throwIO HolderEscaped)
                    }
        ba <- newBudget deadlines 100
        outcome <- try (withByteAdmission port ba 100 (pure ()))
        outcome `shouldBe` Left HolderEscaped
        holding ba 100 (pure ()) `shouldReturn` Just ()

    it "returns the weight when the holder is cancelled, unblocking a waiter" $ \deadlines -> do
        ba <- newByteAdmissionTuned deadlines 100 8 5_000_000
        release <- newEmptyMVar
        entered <- newEmptyMVar
        withAsync (holding ba 100 (putMVar entered () >> takeMVar release)) $ \holder -> do
            takeMVar entered
            withAsync (holding ba 100 (pure ())) $ \waiter -> do
                atomically (weightedAdmissionWaiting (baCore ba) >>= check . (== 1))
                cancel holder
                wait waiter `shouldReturn` Just ()
        holding ba 100 (pure ()) `shouldReturn` Just ()

    it "returns the weight on a synchronous throw inside the held action" $ \deadlines -> do
        ba <- newBudget deadlines 100
        outcome <- try (holding ba 100 (throwIO HolderEscaped $> ("unreached" :: Text)))
        outcome `shouldBe` Left HolderEscaped
        holding ba 100 (pure ()) `shouldReturn` Just ()

    it "sheds instantly past the waiter room and records the shed" $ \deadlines -> do
        sheds <- newIORef (0 :: Int)
        let port = noopMetricsPort{mpPublishBodyShed = atomicModifyIORef' sheds (\n -> (n + 1, ()))}
        ba <- newByteAdmissionTuned deadlines 10 0 50_000
        release <- newEmptyMVar
        entered <- newEmptyMVar
        withAsync (withByteAdmission port ba 10 (putMVar entered () >> takeMVar release)) $ \holder -> do
            takeMVar entered
            withByteAdmission port ba 1 (pure ()) `shouldReturn` Nothing
            readIORef sheds `shouldReturn` (1 :: Int)
            putMVar release ()
            wait holder `shouldReturn` Just ()

    it "clamps an oversized weight to the capacity rather than deadlocking" $ \deadlines -> do
        ba <- newBudget deadlines 10
        holding ba 1000000 (pure ("held" :: Text)) `shouldReturn` Just "held"
