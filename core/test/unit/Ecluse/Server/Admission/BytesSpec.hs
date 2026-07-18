-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Server.Admission.BytesSpec (spec) where

import Test.Hspec
import UnliftIO (async, cancel, mapConcurrently, wait)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO, try)

import Ecluse.Core.Server.Admission.Bytes (
    ByteAdmission,
    newByteAdmissionTuned,
    withByteAdmission,
 )
import Ecluse.Core.Telemetry.Record (MetricsPort (mpPublishBodyInFlightBytes, mpPublishBodyShed))
import Ecluse.Test.Port (noopMetricsPort)

-- | The typed escape one case throws inside a held action.
data HolderEscaped = HolderEscaped
    deriving stock (Eq, Show)

instance Exception HolderEscaped

-- | A handle over the given capacity with a roomy waiter bound and a short wait.
newBudget :: Int -> IO ByteAdmission
newBudget capacity = newByteAdmissionTuned capacity 8 50_000

-- | Acquire the weight around the action with inert telemetry.
holding :: ByteAdmission -> Int -> IO a -> IO (Maybe a)
holding = withByteAdmission noopMetricsPort

spec :: Spec
spec = describe "withByteAdmission (the aggregate body-byte admission)" $ do
    it "admits weights that fit and sheds a weight the capacity cannot hold within the wait" $ do
        ba <- newBudget 100
        holding ba 60 (pure ("held" :: Text)) `shouldReturn` Just "held"
        -- Nothing is held between calls (released on completion), so a second
        -- large weight fits again.
        holding ba 90 (pure ("held" :: Text)) `shouldReturn` Just "held"

    it "keeps the aggregate held bytes within the capacity under concurrent holders" $ do
        -- Capacity for two 40-byte holders; a gauge recording every delta proves
        -- the reserved sum never exceeds the capacity while a third waits or sheds.
        held <- newIORef (0 :: Int)
        peak <- newIORef (0 :: Int)
        let port =
                noopMetricsPort
                    { mpPublishBodyInFlightBytes = \delta ->
                        atomicModifyIORef' held (\h -> (h + delta, ())) >> do
                            now <- readIORef held
                            atomicModifyIORef' peak (\p -> (max p now, ()))
                    }
        ba <- newBudget 100
        _ <- mapConcurrently (\(_ :: Int) -> withByteAdmission port ba 40 (threadDelay 10_000)) [1 .. 6]
        readIORef peak >>= (`shouldSatisfy` (<= 100))

    it "returns the weight when the holder is cancelled, unblocking a waiter" $ do
        ba <- newBudget 100
        release <- newEmptyMVar
        holder <- async (holding ba 100 (takeMVar release))
        -- Give the holder the whole capacity, then cancel it mid-hold.
        threadDelay 10_000
        cancel holder
        -- The full capacity is back: an immediate full-weight acquire admits.
        holding ba 100 (pure ()) `shouldReturn` Just ()

    it "returns the weight on a synchronous throw inside the held action" $ do
        ba <- newBudget 100
        outcome <- try (holding ba 100 (throwIO HolderEscaped $> ("unreached" :: Text)))
        outcome `shouldBe` Left HolderEscaped
        holding ba 100 (pure ()) `shouldReturn` Just ()

    it "sheds instantly past the waiter room and records the shed" $ do
        sheds <- newIORef (0 :: Int)
        let port = noopMetricsPort{mpPublishBodyShed = atomicModifyIORef' sheds (\n -> (n + 1, ()))}
        ba <- newByteAdmissionTuned 10 0 50_000
        release <- newEmptyMVar
        holder <- async (withByteAdmission port ba 10 (takeMVar release))
        threadDelay 10_000
        -- The capacity is held and the room is zero: an arrival sheds at the door.
        withByteAdmission port ba 1 (pure ()) `shouldReturn` Nothing
        readIORef sheds `shouldReturn` (1 :: Int)
        putMVar release ()
        wait holder >>= (`shouldBe` Just ())

    it "clamps an oversized weight to the capacity rather than deadlocking" $ do
        ba <- newBudget 10
        holding ba 1000000 (pure ("held" :: Text)) `shouldReturn` Just "held"
