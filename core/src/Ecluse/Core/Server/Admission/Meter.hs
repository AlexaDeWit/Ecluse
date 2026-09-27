-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The pay-as-you-read memory meter: one byte budget that requests pay into as they read and build.

A request takes a small entry step before its CPU slot and is shed, as a value, when that step does
not fit within the admission wait. A started request pays for what it reads one step at a time, and
pauses instead of failing when the budget is spent. One ticket at a time may overdraw, so a pause
always leaves a request that moves. Everything a ticket paid returns when its request ends.
-}
module Ecluse.Core.Server.Admission.Meter (
    -- * The meter
    MemoryMeter,
    MeterSettings (..),
    newMemoryMeter,
    setMeterBudget,
    setMeterBrakeLevel,
    MeterSnapshot (..),
    meterSnapshot,

    -- * A request's ticket
    MemoryTicket,
    unmeteredTicket,
    withMemoryEntry,
    chargeRead,
    chargeOnce,
) where

import Control.Concurrent.STM (retry)
import Data.IntSet qualified as IntSet
import GHC.Conc (registerDelay)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception qualified as UE

import Ecluse.Core.Server.Admission.Budget (
    EntryGate (..),
    GrowthGate (..),
    MeterView (..),
    entryDecision,
    entryReady,
    growthDecision,
    roundUpToStep,
 )
import Ecluse.Core.Telemetry.Record (MetricsPort (..))

-- | The boot-time shape of a meter.
data MeterSettings = MeterSettings
    { msBudgetBytes :: Int
    -- ^ The starting budget. The sampler may move it later.
    , msStepBytes :: Int
    -- ^ The entry step, and the unit growth is paid in.
    , msEntryRoom :: Int
    -- ^ How many new requests may wait at the door at once.
    , msEntryWaitMicros :: Int
    -- ^ How long a new request waits for its entry step before it is shed.
    }
    deriving stock (Eq, Show)

-- | The process-wide meter. The constructor stays hidden so only the checked operations change it.
data MemoryMeter = MemoryMeter
    { mmBudget :: TVar Int
    , mmCharged :: TVar Int
    , mmPeakCharged :: TVar Int
    , mmWaiters :: TVar IntSet.IntSet
    , mmToken :: TVar (Maybe Int)
    , mmEntryWaiting :: TVar Int
    , mmBrakeLevel :: TVar Int
    , mmNextTicket :: IORef Int
    , mmCounters :: Counters
    , mmStepBytes :: Int
    , mmEntryRoom :: Int
    , mmEntryWaitMicros :: Int
    }

data Counters = Counters
    { cEntryQueued :: IORef Int
    , cEntryShed :: IORef Int
    , cGrowthPaused :: IORef Int
    , cOverdraws :: IORef Int
    }

-- | What the meter holds and has done, for gauges and the load harness.
data MeterSnapshot = MeterSnapshot
    { snBudgetBytes :: Int
    , snChargedBytes :: Int
    , snPeakChargedBytes :: Int
    , snPausedReads :: Int
    , snEntryWaiting :: Int
    , snEntryQueued :: Int
    , snEntryShed :: Int
    , snGrowthPaused :: Int
    , snOverdraws :: Int
    , snBrakeLevel :: Int
    }
    deriving stock (Eq, Show)

-- | Build a meter. The step and the wait floor at sensible minimums.
newMemoryMeter :: MeterSettings -> IO MemoryMeter
newMemoryMeter settings = do
    budget <- newTVarIO (max 0 (msBudgetBytes settings))
    charged <- newTVarIO 0
    peak <- newTVarIO 0
    waiters <- newTVarIO IntSet.empty
    token <- newTVarIO Nothing
    entryWaiting <- newTVarIO 0
    brake <- newTVarIO 0
    next <- newIORef 0
    counters <- Counters <$> newIORef 0 <*> newIORef 0 <*> newIORef 0 <*> newIORef 0
    pure
        MemoryMeter
            { mmBudget = budget
            , mmCharged = charged
            , mmPeakCharged = peak
            , mmWaiters = waiters
            , mmToken = token
            , mmEntryWaiting = entryWaiting
            , mmBrakeLevel = brake
            , mmNextTicket = next
            , mmCounters = counters
            , mmStepBytes = max 1 (msStepBytes settings)
            , mmEntryRoom = max 0 (msEntryRoom settings)
            , mmEntryWaitMicros = max 0 (msEntryWaitMicros settings)
            }

-- | Move the budget. Paused reads and queued entries retry against the new value at once.
setMeterBudget :: MemoryMeter -> Int -> IO ()
setMeterBudget meter = atomically . writeTVar (mmBudget meter) . max 0

-- | Record the sampler's brake level for the gauges.
setMeterBrakeLevel :: MemoryMeter -> Int -> IO ()
setMeterBrakeLevel meter = atomically . writeTVar (mmBrakeLevel meter)

-- | Read every figure the meter keeps, without blocking a request.
meterSnapshot :: MemoryMeter -> IO MeterSnapshot
meterSnapshot meter = do
    (budget, charged, peak, paused, waiting, brake) <-
        atomically $
            (,,,,,)
                <$> readTVar (mmBudget meter)
                <*> readTVar (mmCharged meter)
                <*> readTVar (mmPeakCharged meter)
                <*> (IntSet.size <$> readTVar (mmWaiters meter))
                <*> readTVar (mmEntryWaiting meter)
                <*> readTVar (mmBrakeLevel meter)
    let counters = mmCounters meter
    MeterSnapshot budget charged peak paused waiting
        <$> readIORef (cEntryQueued counters)
        <*> readIORef (cEntryShed counters)
        <*> readIORef (cGrowthPaused counters)
        <*> readIORef (cOverdraws counters)
        <*> pure brake

-- | One request's account with the meter, or no account at all.
data MemoryTicket
    = Unmetered
    | Metered Ticket

data Ticket = Ticket
    { tMeter :: MemoryMeter
    , tId :: Int
    , tCharged :: TVar Int
    -- ^ Everything this ticket took from the budget, returned in one transaction at the end.
    , tHeadroom :: IORef Int
    -- ^ Paid but not yet used, so most chunks touch no shared state.
    , tMetrics :: MetricsPort
    }

-- | A ticket that pays nothing, for work outside the metered serve paths.
unmeteredTicket :: MemoryTicket
unmeteredTicket = Unmetered

{- | Run a request under the meter. 'Nothing' is a shed: the waiting room was full, or the entry
step did not fit within the wait. Everything the ticket paid returns on every exit path.
-}
withMemoryEntry :: (MonadUnliftIO m) => MetricsPort -> MemoryMeter -> (MemoryTicket -> m a) -> m (Maybe a)
withMemoryEntry metrics meter body =
    UE.mask $ \restore -> do
        ticket <- liftIO (newTicket metrics meter)
        entered <- liftIO (enter ticket)
        case entered of
            Nothing -> liftIO (countShed ticket) $> Nothing
            Just waited ->
                Just
                    <$> ( restore (liftIO (when waited (countQueued ticket)) >> body (Metered ticket))
                            `UE.finally` liftIO (releaseTicket ticket)
                        )

newTicket :: MetricsPort -> MemoryMeter -> IO Ticket
newTicket metrics meter = do
    ticketId <- atomicModifyIORef' (mmNextTicket meter) (\n -> (n + 1, n))
    charged <- newTVarIO 0
    headroom <- newIORef 0
    pure Ticket{tMeter = meter, tId = ticketId, tCharged = charged, tHeadroom = headroom, tMetrics = metrics}

-- The door. 'Just' carries whether the request had to wait. A blocked wait stays
-- interruptible, and a cancellation there takes nothing.
enter :: Ticket -> IO (Maybe Bool)
enter ticket = do
    gate <- atomically $ do
        view <- readView meter
        waiting <- readTVar (mmEntryWaiting meter)
        case entryDecision view waiting (mmEntryRoom meter) step of
            EntryAdmit -> takeBytes ticket step $> EntryAdmit
            EntryQueue -> writeTVar (mmEntryWaiting meter) (waiting + 1) $> EntryQueue
            EntryRefuse -> pure EntryRefuse
    case gate of
        EntryAdmit -> openHeadroom $> Just False
        EntryRefuse -> pure Nothing
        EntryQueue -> do
            deadline <- registerDelay (mmEntryWaitMicros meter)
            admitted <-
                atomically (entryOrExpire deadline)
                    `UE.finally` atomically (modifyTVar' (mmEntryWaiting meter) (subtract 1))
            if admitted then openHeadroom $> Just True else pure Nothing
  where
    meter = tMeter ticket
    step = mmStepBytes meter
    openHeadroom = writeIORef (tHeadroom ticket) step
    entryOrExpire deadline = do
        view <- readView meter
        if entryReady view step
            then takeBytes ticket step $> True
            else do
                expired <- readTVar deadline
                if expired then pure False else retry

readView :: MemoryMeter -> STM MeterView
readView meter =
    MeterView
        <$> readTVar (mmBudget meter)
        <*> readTVar (mmCharged meter)
        <*> readTVar (mmToken meter)
        <*> (fmap fst . IntSet.minView <$> readTVar (mmWaiters meter))

-- Take bytes onto both the meter and the ticket in one step, so a release can never miss them.
takeBytes :: Ticket -> Int -> STM ()
takeBytes ticket bytes = do
    let meter = tMeter ticket
    charged <- (+ bytes) <$> readTVar (mmCharged meter)
    writeTVar (mmCharged meter) charged
    modifyTVar' (mmPeakCharged meter) (max charged)
    modifyTVar' (tCharged ticket) (+ bytes)

releaseTicket :: Ticket -> IO ()
releaseTicket ticket = atomically $ do
    let meter = tMeter ticket
    held <- readTVar (tCharged ticket)
    writeTVar (tCharged ticket) 0
    modifyTVar' (mmCharged meter) (subtract held)
    dropToken ticket

dropToken :: Ticket -> STM ()
dropToken ticket = do
    let token = mmToken (tMeter ticket)
    holder <- readTVar token
    when (holder == Just (tId ticket)) (writeTVar token Nothing)

-- Whether an overdraw keeps the token until the read ends, or uses it for one step only.
data TokenUse = KeepToken | SingleUse
    deriving stock (Eq)

{- | Pay for bytes a read hands on, already scaled to their charge. Zero marks the end of a read,
which hands the overdraw token back if this ticket held it.
-}
chargeRead :: MemoryTicket -> Int -> IO ()
chargeRead Unmetered _ = pass
chargeRead (Metered ticket) cost
    | cost <= 0 = atomically (dropToken ticket)
    | otherwise = pay ticket KeepToken cost

-- | Pay once for bytes a request is about to build, already scaled to their charge.
chargeOnce :: MemoryTicket -> Int -> IO ()
chargeOnce Unmetered _ = pass
chargeOnce (Metered ticket) cost = when (cost > 0) (pay ticket SingleUse cost)

pay :: Ticket -> TokenUse -> Int -> IO ()
pay ticket use cost = do
    shortfall <- atomicModifyIORef' (tHeadroom ticket) $ \headroom ->
        if cost <= headroom then (headroom - cost, 0) else (0, cost - headroom)
    when (shortfall > 0) $ do
        let want = roundUpToStep (mmStepBytes (tMeter ticket)) shortfall
        grow ticket use want
        atomicModifyIORef' (tHeadroom ticket) (\headroom -> (headroom + want - shortfall, ()))

-- Take a growth step, pausing while it neither fits nor may overdraw. A pause registers the
-- ticket as a waiter, which holds new entries back and names the oldest claimant.
grow :: Ticket -> TokenUse -> Int -> IO ()
grow ticket use want = do
    immediate <- atomically (attempt ticket use want)
    overdrew <- case immediate of
        Just overdrew -> pure overdrew
        Nothing -> do
            countPaused ticket
            (atomically (modifyTVar' waiters (IntSet.insert (tId ticket))) >> atomically (attempt ticket use want >>= maybe retry pure))
                `UE.finally` atomically (modifyTVar' waiters (IntSet.delete (tId ticket)))
    when overdrew (countOverdraw ticket)
  where
    waiters = mmWaiters (tMeter ticket)

-- One growth attempt: 'Just' whether it overdrew, or 'Nothing' to pause.
attempt :: Ticket -> TokenUse -> Int -> STM (Maybe Bool)
attempt ticket use want = do
    view <- readView (tMeter ticket)
    case growthDecision view (tId ticket) want of
        GrowWithin -> takeBytes ticket want $> Just False
        GrowOverdraw -> do
            when (use == KeepToken) (writeTVar (mmToken (tMeter ticket)) (Just (tId ticket)))
            takeBytes ticket want $> Just True
        GrowWait -> pure Nothing

countQueued, countShed, countPaused, countOverdraw :: Ticket -> IO ()
countQueued ticket = bump cEntryQueued ticket >> mpMemoryEntryQueued (tMetrics ticket)
countShed ticket = bump cEntryShed ticket >> mpMemoryEntryShed (tMetrics ticket)
countPaused ticket = bump cGrowthPaused ticket >> mpMemoryGrowthPaused (tMetrics ticket)
countOverdraw ticket = bump cOverdraws ticket >> mpMemoryOverdraw (tMetrics ticket)

bump :: (Counters -> IORef Int) -> Ticket -> IO ()
bump field ticket = atomicModifyIORef' (field (mmCounters (tMeter ticket))) (\n -> (n + 1, ()))
