-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Server.Admission.MeterSpec (spec) where

import Test.Hspec
import UnliftIO (Async, async, cancel, poll, wait)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO, try)

import Ecluse.Core.Server.Admission.Brake (BrakeLevel (Braking, Calm))
import Ecluse.Core.Server.Admission.Meter (
    MemoryMeter,
    MemoryTicket,
    MeterSettings (..),
    MeterSnapshot (..),
    chargeOnce,
    chargeRead,
    meterSnapshot,
    newMemoryMeter,
    steerMeter,
    withMemoryEntry,
 )
import Ecluse.Core.Telemetry.Record (MetricsPort (..))
import Ecluse.Test.Port (noopMetricsPort)

-- | The typed escape one case throws inside a metered request.
data RequestEscaped = RequestEscaped
    deriving stock (Eq, Show)

instance Exception RequestEscaped

-- | What the meter reported through its metrics port.
data Counted = Counted
    { cQueued :: Int
    , cShed :: Int
    , cPaused :: Int
    , cOverdraws :: Int
    }
    deriving stock (Eq, Show)

-- | A meter with a 10-byte step, a room of two and a short wait, and what it reports.
newMeter :: Int -> IO (MemoryMeter, MetricsPort, IO Counted)
newMeter budget = do
    meter <- newMemoryMeter MeterSettings{msBudgetBytes = budget, msStepBytes = 10, msEntryRoom = 2, msEntryWaitMicros = 50_000}
    counts <- newIORef (Counted 0 0 0 0)
    let bump f = atomicModifyIORef' counts (\c -> (f c, ()))
        port =
            noopMetricsPort
                { mpMemoryAdmissionQueued = bump (\c -> c{cQueued = cQueued c + 1})
                , mpMemoryAdmissionShed = bump (\c -> c{cShed = cShed c + 1})
                , mpMemoryAdmissionPaused = bump (\c -> c{cPaused = cPaused c + 1})
                , mpMemoryAdmissionOverdraw = bump (\c -> c{cOverdraws = cOverdraws c + 1})
                }
    pure (meter, port, readIORef counts)

charged :: MemoryMeter -> IO Int
charged meter = snChargedBytes <$> meterSnapshot meter

spec :: Spec
spec = describe "Ecluse.Core.Server.Admission.Meter" $ do
    it "charges the entry step, pays small reads from it, and returns everything at the end" $ do
        (meter, port, _) <- newMeter 100
        inside <- withMemoryEntry port meter $ \ticket -> do
            chargeRead ticket 4
            chargeRead ticket 6
            charged meter
        inside `shouldBe` Just 10
        charged meter `shouldReturn` 0

    it "takes whole steps for reads past the headroom" $ do
        (meter, port, _) <- newMeter 100
        inside <- withMemoryEntry port meter $ \ticket -> do
            chargeRead ticket 25
            charged meter
        inside `shouldBe` Just 30
        charged meter `shouldReturn` 0

    it "sheds a new request, as a value, when the entry step does not fit within the wait" $ do
        (meter, port, counted) <- newMeter 10
        held <- withMemoryEntry port meter $ \_ -> withMemoryEntry port meter (\_ -> pure ())
        held `shouldBe` Just Nothing
        counted `shouldReturn` Counted{cQueued = 0, cShed = 1, cPaused = 0, cOverdraws = 0}

    it "returns the charge when the request throws" $ do
        (meter, port, _) <- newMeter 100
        outcome <- try (withMemoryEntry port meter (\ticket -> chargeOnce ticket 40 >> throwIO RequestEscaped))
        outcome `shouldBe` (Left RequestEscaped :: Either RequestEscaped (Maybe ()))
        charged meter `shouldReturn` 0

    it "pauses a read while another holds the overdraw token, and resumes when that read ends" $ do
        (meter, port, counted) <- newMeter 30
        (paused, go) <- enteredReader port meter (\ticket -> chargeRead ticket 15 >> chargeRead ticket 0)
        (endRead, holder) <- startTokenHolder port meter
        go
        threadDelay 20_000
        (isNothing <$> poll paused) `shouldReturn` True
        cPaused <$> counted `shouldReturn` 1
        endRead
        wait holder `shouldReturn` Just ()
        wait paused `shouldReturn` Just ()
        charged meter `shouldReturn` 0

    it "lets the oldest paused read overdraw so a full meter still moves" $ do
        (meter, port, counted) <- newMeter 20
        peak <- withMemoryEntry port meter $ \ticket -> chargeRead ticket 60 >> charged meter <* chargeRead ticket 0
        peak `shouldBe` Just 60
        cOverdraws <$> counted `shouldReturn` 1
        charged meter `shouldReturn` 0

    it "lets a queued request in once the sampler grows the budget" $ do
        (meter, port, counted) <- newMeter 10
        release <- newEmptyMVar
        holder <- async . withMemoryEntry port meter $ \_ -> takeMVar release
        threadDelay 10_000
        entrant <- async (withMemoryEntry port meter (\_ -> pure ()))
        threadDelay 10_000
        steerMeter meter 20 Calm
        wait entrant `shouldReturn` Just ()
        cQueued <$> counted `shouldReturn` 1
        putMVar release ()
        wait holder `shouldReturn` Just ()

    it "reports the steered budget and the brake level that moved it" $ do
        (meter, _, _) <- newMeter 100
        meterSnapshot meter `shouldReturn` MeterSnapshot{snBudgetBytes = 100, snChargedBytes = 0, snBrakeLevel = Calm}
        steerMeter meter 50 Braking
        meterSnapshot meter `shouldReturn` MeterSnapshot{snBudgetBytes = 50, snChargedBytes = 0, snBrakeLevel = Braking}

    it "cancels a paused read without leaking its charge" $ do
        (meter, port, _) <- newMeter 30
        (paused, go) <- enteredReader port meter (`chargeRead` 15)
        (endRead, holder) <- startTokenHolder port meter
        go
        threadDelay 20_000
        cancel paused
        endRead
        wait holder `shouldReturn` Just ()
        charged meter `shouldReturn` 0

-- A request that has taken its entry step and runs its body only on the returned signal.
enteredReader :: MetricsPort -> MemoryMeter -> (MemoryTicket -> IO ()) -> IO (Async (Maybe ()), IO ())
enteredReader port meter body = do
    entered <- newEmptyMVar
    go <- newEmptyMVar
    started <- async . withMemoryEntry port meter $ \ticket -> putMVar entered () >> takeMVar go >> body ticket
    takeMVar entered
    pure (started, putMVar go ())

-- A request that overdraws a 30-byte meter to 60 with the token, and holds it until its read ends.
startTokenHolder :: MetricsPort -> MemoryMeter -> IO (IO (), Async (Maybe ()))
startTokenHolder port meter = do
    overdrawn <- newEmptyMVar
    readEnds <- newEmptyMVar
    holder <- async . withMemoryEntry port meter $ \ticket -> do
        chargeRead ticket 45
        putMVar overdrawn ()
        takeMVar readEnds
        chargeRead ticket 0
    takeMVar overdrawn
    pure (putMVar readEnds (), holder)
