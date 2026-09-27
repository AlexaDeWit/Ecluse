-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Server.Admission.MeterSpec (spec) where

import Test.Hspec
import UnliftIO (Async, async, cancel, poll, wait)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO, try)

import Ecluse.Core.Server.Admission.Meter (
    MemoryMeter,
    MemoryTicket,
    MeterSettings (..),
    MeterSnapshot (..),
    chargeOnce,
    chargeRead,
    meterSnapshot,
    newMemoryMeter,
    unmeteredTicket,
    withMemoryEntry,
 )
import Ecluse.Test.Port (noopMetricsPort)

-- | The typed escape one case throws inside a metered request.
data RequestEscaped = RequestEscaped
    deriving stock (Eq, Show)

instance Exception RequestEscaped

-- | A meter with a 10-byte step, a room of two and a short wait.
newMeter :: Int -> IO MemoryMeter
newMeter budget =
    newMemoryMeter MeterSettings{msBudgetBytes = budget, msStepBytes = 10, msEntryRoom = 2, msEntryWaitMicros = 50_000}

entering :: MemoryMeter -> (MemoryTicket -> IO a) -> IO (Maybe a)
entering = withMemoryEntry noopMetricsPort

charged :: MemoryMeter -> IO Int
charged meter = snChargedBytes <$> meterSnapshot meter

spec :: Spec
spec = describe "Ecluse.Core.Server.Admission.Meter" $ do
    it "charges the entry step, pays small reads from it, and returns everything at the end" $ do
        meter <- newMeter 100
        inside <- entering meter $ \ticket -> do
            chargeRead ticket 4
            chargeRead ticket 6
            charged meter
        inside `shouldBe` Just 10
        charged meter `shouldReturn` 0

    it "takes whole steps for reads past the headroom" $ do
        meter <- newMeter 100
        inside <- entering meter $ \ticket -> do
            chargeRead ticket 25
            charged meter
        inside `shouldBe` Just 30
        charged meter `shouldReturn` 0

    it "sheds a new request, as a value, when the entry step does not fit within the wait" $ do
        meter <- newMeter 10
        held <- entering meter $ \_ -> entering meter (\_ -> pure ())
        held `shouldBe` Just Nothing
        snEntryShed <$> meterSnapshot meter `shouldReturn` 1

    it "returns the charge when the request throws" $ do
        meter <- newMeter 100
        outcome <- try (entering meter (\ticket -> chargeOnce ticket 40 >> throwIO RequestEscaped))
        outcome `shouldBe` (Left RequestEscaped :: Either RequestEscaped (Maybe ()))
        charged meter `shouldReturn` 0

    it "pauses a read while another holds the overdraw token, and resumes when that read ends" $ do
        meter <- newMeter 30
        (paused, go) <- enteredReader meter (\ticket -> chargeRead ticket 15 >> chargeRead ticket 0)
        (endRead, holder) <- startTokenHolder meter
        go
        threadDelay 20_000
        (isNothing <$> poll paused) `shouldReturn` True
        snPausedReads <$> meterSnapshot meter `shouldReturn` 1
        endRead
        wait holder `shouldReturn` Just ()
        wait paused `shouldReturn` Just ()
        charged meter `shouldReturn` 0

    it "lets the oldest paused read overdraw so a full meter still moves" $ do
        meter <- newMeter 20
        overdrawer <- async . entering meter $ \ticket -> chargeRead ticket 60 >> chargeRead ticket 0
        wait overdrawer `shouldReturn` Just ()
        snOverdraws <$> meterSnapshot meter `shouldNotReturn` 0
        snPeakChargedBytes <$> meterSnapshot meter `shouldReturn` 60
        charged meter `shouldReturn` 0

    it "cancels a paused read without leaking its charge" $ do
        meter <- newMeter 30
        (paused, go) <- enteredReader meter (`chargeRead` 15)
        (endRead, holder) <- startTokenHolder meter
        go
        threadDelay 20_000
        cancel paused
        snPausedReads <$> meterSnapshot meter `shouldReturn` 0
        endRead
        wait holder `shouldReturn` Just ()
        charged meter `shouldReturn` 0

    it "charges nothing through an unmetered ticket" $ do
        chargeRead unmeteredTicket 1_000_000 `shouldReturn` ()
        chargeOnce unmeteredTicket 1_000_000 `shouldReturn` ()

-- A request that has taken its entry step and runs its body only on the returned signal.
enteredReader :: MemoryMeter -> (MemoryTicket -> IO ()) -> IO (Async (Maybe ()), IO ())
enteredReader meter body = do
    entered <- newEmptyMVar
    go <- newEmptyMVar
    started <- async . entering meter $ \ticket -> putMVar entered () >> takeMVar go >> body ticket
    takeMVar entered
    pure (started, putMVar go ())

-- A request that overdraws a 30-byte meter to 60 with the token, and holds it until its read ends.
startTokenHolder :: MemoryMeter -> IO (IO (), Async (Maybe ()))
startTokenHolder meter = do
    overdrawn <- newEmptyMVar
    readEnds <- newEmptyMVar
    holder <- async . entering meter $ \ticket -> do
        chargeRead ticket 45
        putMVar overdrawn ()
        takeMVar readEnds
        chargeRead ticket 0
    takeMVar overdrawn
    pure (putMVar readEnds (), holder)
