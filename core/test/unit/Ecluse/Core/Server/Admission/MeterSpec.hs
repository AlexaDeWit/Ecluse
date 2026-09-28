-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Server.Admission.MeterSpec (spec) where

import Control.Concurrent.STM (check)
import Test.Hspec
import UnliftIO (Async, async, cancel, concurrently_, poll, wait, waitCatch)
import UnliftIO.Exception (throwIO, try)

import Ecluse.Core.Server.Admission.Meter (
    MemoryMeter,
    MemoryTicket,
    MeterSettings (..),
    awaitingFlight,
    charge,
    meterFigures,
    meterSnapshot,
    newMemoryMeter,
    servingFlight,
    steerMeter,
    takeLargestCharge,
    withMemoryEntry,
 )
import Ecluse.Core.Server.Admission.Types (BrakeLevel (Braking, Calm), FlightKey (FlightKey), MeterSnapshot (..))
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
    , cPauses :: Int
    , cOverdraws :: Int
    }
    deriving stock (Eq, Show)

-- | A meter with a 10-byte step, a room of two and a long wait, and what it reports.
newMeter :: Int -> IO (MemoryMeter, MetricsPort, IO Counted)
newMeter = newMeterWaiting 10_000_000

newMeterWaiting :: Int -> Int -> IO (MemoryMeter, MetricsPort, IO Counted)
newMeterWaiting waitMicros budget = do
    meter <- newMemoryMeter MeterSettings{msBudgetBytes = budget, msStepBytes = 10, msEntryRoom = 2, msEntryWaitMicros = waitMicros}
    counts <- newIORef (Counted 0 0 0 0)
    let bump f = atomicModifyIORef' counts (\c -> (f c, ()))
        port =
            noopMetricsPort
                { mpMemoryAdmissionQueued = bump (\c -> c{cQueued = cQueued c + 1})
                , mpMemoryAdmissionShed = bump (\c -> c{cShed = cShed c + 1})
                , mpMemoryAdmissionPause = bump (\c -> c{cPauses = cPauses c + 1})
                , mpMemoryAdmissionOverdraw = bump (\c -> c{cOverdraws = cOverdraws c + 1})
                }
    pure (meter, port, readIORef counts)

charged :: MemoryMeter -> IO Int
charged meter = snChargedBytes <$> meterSnapshot meter

-- Block until the meter's figures satisfy the condition.
awaitFigures :: MemoryMeter -> (MeterSnapshot -> Bool) -> IO ()
awaitFigures meter holds = atomically (meterFigures meter >>= check . holds)

spec :: Spec
spec = describe "Ecluse.Core.Server.Admission.Meter" $ do
    it "charges the entry step, pays small charges from it, and returns everything at the end" $ do
        (meter, port, _) <- newMeter 100
        inside <- withMemoryEntry port meter $ \ticket -> do
            charge ticket 4
            charge ticket 6
            charged meter
        inside `shouldBe` Just 10
        charged meter `shouldReturn` 0

    it "takes whole steps for charges past the headroom" $ do
        (meter, port, _) <- newMeter 100
        inside <- withMemoryEntry port meter $ \ticket -> do
            charge ticket 25
            charged meter
        inside `shouldBe` Just 30
        charged meter `shouldReturn` 0

    it "sheds a new request, as a value, when the entry step does not fit within the wait" $ do
        (meter, port, counted) <- newMeterWaiting 0 10
        held <- withMemoryEntry port meter $ \_ -> withMemoryEntry port meter (\_ -> pure ())
        held `shouldBe` Just Nothing
        counted `shouldReturn` Counted{cQueued = 0, cShed = 1, cPauses = 0, cOverdraws = 0}

    it "frees a queued request's place when its wait expires, without running it" $ do
        (meter, port, _) <- newMeterWaiting 20_000 10
        (release, holder) <- holdEntry port meter
        withMemoryEntry port meter (\_ -> throwIO RequestEscaped) `shouldReturn` (Nothing :: Maybe ())
        snWaiting <$> meterSnapshot meter `shouldReturn` 0
        release
        wait holder `shouldReturn` Just ()
        withMemoryEntry port meter (\_ -> pure ()) `shouldReturn` Just ()

    it "frees a queued request's place when it is cancelled" $ do
        (meter, port, _) <- newMeter 10
        (release, holder) <- holdEntry port meter
        cancelled <- async (withMemoryEntry port meter (\_ -> pure ()))
        awaitFigures meter ((== 1) . snWaiting)
        cancel cancelled
        awaitFigures meter ((== 0) . snWaiting)
        release
        wait holder `shouldReturn` Just ()
        withMemoryEntry port meter (\_ -> pure ()) `shouldReturn` Just ()

    it "returns the charge when the request throws" $ do
        (meter, port, _) <- newMeter 100
        outcome <- try (withMemoryEntry port meter (\ticket -> charge ticket 40 >> throwIO RequestEscaped))
        outcome `shouldBe` (Left RequestEscaped :: Either RequestEscaped (Maybe ()))
        charged meter `shouldReturn` 0

    it "lets the oldest paused request overdraw so a full meter still moves" $ do
        (meter, port, counted) <- newMeter 20
        peak <- withMemoryEntry port meter $ \ticket -> charge ticket 60 >> charged meter
        peak `shouldBe` Just 60
        cOverdraws <$> counted `shouldReturn` 1
        charged meter `shouldReturn` 0

    it "lets one of three requests on a full meter overdraw, and pauses the other two" $ do
        (meter, port, counted) <- newMeter 30
        done <- newTVarIO (0 :: Int)
        release <- newEmptyMVar
        chargers <- replicateM 3 . enteredCharger port meter $ \ticket -> do
            charge ticket 15
            atomically (modifyTVar' done (+ 1))
            readMVar release
        traverse_ snd chargers
        awaitFigures meter ((== 2) . snPaused)
        atomically (readTVar done >>= check . (== 1))
        counted >>= \c -> (cOverdraws c, cPauses c) `shouldBe` (1, 2)
        putMVar release ()
        traverse_ (\(running, _) -> wait running `shouldReturn` Just ()) chargers
        charged meter `shouldReturn` 0

    it "keeps a second request paused until the token holder's request ends, not its read" $ do
        (meter, port, counted) <- newMeter 30
        (paused, go) <- enteredCharger port meter (`charge` 15)
        (finish, holder) <- startTokenHolder port meter
        go
        awaitFigures meter ((== 1) . snPaused)
        (isNothing <$> poll paused) `shouldReturn` True
        cPauses <$> counted `shouldReturn` 1
        finish
        wait holder `shouldReturn` Just ()
        wait paused `shouldReturn` Just ()
        charged meter `shouldReturn` 0

    it "moves shared work the token holder waits on after its own read ends early" $ do
        (meter, port, _) <- newMeter 30
        let flight = FlightKey "typescript"
        (served, goServed) <- enteredCharger port meter (\ticket -> charge (servingFlight flight ticket) 15)
        (unserved, goUnserved) <- enteredCharger port meter (\ticket -> charge (servingFlight (FlightKey "react") ticket) 15)
        holderWaits <- newEmptyMVar
        (finish, holder) <- startTokenHolderWith port meter $ \ticket -> awaitingFlight ticket flight (takeMVar holderWaits)
        goServed
        goUnserved
        wait served `shouldReturn` Just ()
        awaitFigures meter ((== 1) . snPaused)
        (isNothing <$> poll unserved) `shouldReturn` True
        putMVar holderWaits ()
        finish
        wait holder `shouldReturn` Just ()
        wait unserved `shouldReturn` Just ()
        charged meter `shouldReturn` 0

    it "lets a queued request in once the sampler grows the budget" $ do
        (meter, port, counted) <- newMeter 10
        (release, holder) <- holdEntry port meter
        entrant <- async (withMemoryEntry port meter (\_ -> pure ()))
        awaitFigures meter ((== 1) . snWaiting)
        steerMeter meter 20 Calm
        wait entrant `shouldReturn` Just ()
        cQueued <$> counted `shouldReturn` 1
        release
        wait holder `shouldReturn` Just ()

    it "reports the steered budget and the brake level that moved it" $ do
        (meter, _, _) <- newMeter 100
        meterSnapshot meter `shouldReturn` MeterSnapshot{snBudgetBytes = 100, snChargedBytes = 0, snWaiting = 0, snPaused = 0, snBrakeLevel = Calm}
        steerMeter meter 50 Braking
        meterSnapshot meter `shouldReturn` MeterSnapshot{snBudgetBytes = 50, snChargedBytes = 0, snWaiting = 0, snPaused = 0, snBrakeLevel = Braking}

    it "reports the largest total one request reached, once" $ do
        (meter, port, _) <- newMeter 100
        _ <- withMemoryEntry port meter (`charge` 35)
        _ <- withMemoryEntry port meter (`charge` 5)
        takeLargestCharge meter `shouldReturn` 40
        takeLargestCharge meter `shouldReturn` 0

    it "cancels a paused request without leaking its charge" $ do
        (meter, port, _) <- newMeter 30
        (paused, go) <- enteredCharger port meter (`charge` 15)
        (finish, holder) <- startTokenHolder port meter
        go
        awaitFigures meter ((== 1) . snPaused)
        cancel paused
        void (waitCatch paused)
        awaitFigures meter ((== 0) . snPaused)
        finish
        wait holder `shouldReturn` Just ()
        charged meter `shouldReturn` 0

-- A request that holds its entry step, filling a 10-byte meter, until the returned signal.
holdEntry :: MetricsPort -> MemoryMeter -> IO (IO (), Async (Maybe ()))
holdEntry port meter = do
    release <- newEmptyMVar
    holder <- async . withMemoryEntry port meter $ \_ -> takeMVar release
    awaitFigures meter ((>= 10) . snChargedBytes)
    pure (putMVar release (), holder)

-- A request that has taken its entry step and runs its body only on the returned signal.
enteredCharger :: MetricsPort -> MemoryMeter -> (MemoryTicket -> IO ()) -> IO (Async (Maybe ()), IO ())
enteredCharger port meter body = do
    entered <- newEmptyMVar
    go <- newEmptyMVar
    started <- async . withMemoryEntry port meter $ \ticket -> putMVar entered () >> takeMVar go >> body ticket
    takeMVar entered
    pure (started, putMVar go ())

-- A request that overdraws a 30-byte meter, taking the token, and ends on the returned signal.
startTokenHolder :: MetricsPort -> MemoryMeter -> IO (IO (), Async (Maybe ()))
startTokenHolder port meter = startTokenHolderWith port meter (const pass)

-- As 'startTokenHolder', running @afterwards@ once it holds the token and its read has ended.
startTokenHolderWith :: MetricsPort -> MemoryMeter -> (MemoryTicket -> IO ()) -> IO (IO (), Async (Maybe ()))
startTokenHolderWith port meter afterwards = do
    overdrawn <- newEmptyMVar
    ends <- newEmptyMVar
    holder <- async . withMemoryEntry port meter $ \ticket -> do
        charge ticket 45
        putMVar overdrawn ()
        concurrently_ (afterwards ticket) (takeMVar ends)
    takeMVar overdrawn
    pure (putMVar ends (), holder)
