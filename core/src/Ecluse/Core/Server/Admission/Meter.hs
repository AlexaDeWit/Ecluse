-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The metadata memory gate's meter: one byte budget that requests pay into as they read and build.

A request takes a small entry step before its CPU slot and is shed, as a value, when that step does
not fit within the admission wait. A started request pays for what it reads one step at a time, and
pauses instead of failing when the budget is spent. One ticket at a time holds the overdraw token
until its request ends, so the budget holds within one request's worth. Work another request waits
on, a shared fetch or render, moves with the priority of that request, so a pause never deadlocks.
-}
module Ecluse.Core.Server.Admission.Meter (
    -- * The meter
    MemoryMeter,
    MeterSettings (..),
    newMemoryMeter,
    steerMeter,
    meterSnapshot,
    meterFigures,
    takeLargestCharge,

    -- * A request's ticket
    MemoryTicket,
    withMemoryEntry,
    charge,

    -- * Shared work
    awaitingFlight,
    servingFlight,
) where

import Control.Concurrent.STM (retry, stateTVar)
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
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
import Ecluse.Core.Server.Admission.Types (BrakeLevel (Calm), FlightKey, MeterSnapshot (..))
import Ecluse.Core.Telemetry.Record (MetricsPort (..))

-- | The boot-time shape of a meter.
data MeterSettings = MeterSettings
    { msBudgetBytes :: Int
    -- ^ The starting budget. The sampler may move it later.
    , msStepBytes :: Int
    -- ^ The entry step, and the unit growth is paid in.
    , msEntryRoom :: Int
    -- ^ How many new requests may wait at the gate at once.
    , msEntryWaitMicros :: Int
    -- ^ How long a new request waits for its entry step before it is shed.
    }
    deriving stock (Eq, Show)

-- | The process-wide meter. The constructor stays hidden so only the checked operations change it.
data MemoryMeter = MemoryMeter
    { mmBudget :: TVar Int
    , mmCharged :: TVar Int
    , mmWaiters :: TVar (Map Int (Maybe FlightKey))
    -- ^ Paused tickets, each with the shared work its paused charge serves.
    , mmInterest :: TVar (Map FlightKey IntSet)
    -- ^ The tickets waiting on each unit of shared work.
    , mmToken :: TVar (Maybe Int)
    , mmEntryWaiting :: TVar Int
    , mmBrakeLevel :: TVar BrakeLevel
    , mmLargest :: TVar Int
    -- ^ The largest total any one ticket reached since the sampler last read it.
    , mmNextTicket :: IORef Int
    , mmStepBytes :: Int
    , mmEntryRoom :: Int
    , mmEntryWaitMicros :: Int
    }

-- | Build a meter. The step floors at one byte, and the room and the wait at zero.
newMemoryMeter :: MeterSettings -> IO MemoryMeter
newMemoryMeter settings =
    MemoryMeter
        <$> newTVarIO (max 0 (msBudgetBytes settings))
        <*> newTVarIO 0
        <*> newTVarIO Map.empty
        <*> newTVarIO Map.empty
        <*> newTVarIO Nothing
        <*> newTVarIO 0
        <*> newTVarIO Calm
        <*> newTVarIO 0
        <*> newIORef 0
        <*> pure (max 1 (msStepBytes settings))
        <*> pure (max 0 (msEntryRoom settings))
        <*> pure (max 0 (msEntryWaitMicros settings))

-- | Move the budget and record the brake level that moved it. Paused reads and queued entries retry at once.
steerMeter :: MemoryMeter -> Int -> BrakeLevel -> IO ()
steerMeter meter budget level = atomically $ do
    writeTVar (mmBudget meter) (max 0 budget)
    writeTVar (mmBrakeLevel meter) level

-- | Read the meter's figures without blocking a request.
meterSnapshot :: MemoryMeter -> IO MeterSnapshot
meterSnapshot = atomically . meterFigures

-- | The meter's figures inside a transaction, so a caller can wait for them to change.
meterFigures :: MemoryMeter -> STM MeterSnapshot
meterFigures meter =
    MeterSnapshot
        <$> readTVar (mmBudget meter)
        <*> readTVar (mmCharged meter)
        <*> readTVar (mmEntryWaiting meter)
        <*> (Map.size <$> readTVar (mmWaiters meter))
        <*> readTVar (mmBrakeLevel meter)

-- | The largest total one ticket reached since the last call, for the sampler alone.
takeLargestCharge :: MemoryMeter -> IO Int
takeLargestCharge meter = atomically (stateTVar (mmLargest meter) (,0))

-- | One request's account with the meter, or a view of it that pays for shared work.
data MemoryTicket = MemoryTicket
    { tMeter :: MemoryMeter
    , tId :: Int
    , tCharged :: TVar Int
    -- ^ Everything this ticket took from the budget, returned in one transaction at the end.
    , tHeadroom :: IORef Int
    -- ^ Paid but not yet used, so most chunks touch no shared state.
    , tServes :: Maybe FlightKey
    -- ^ The shared work this view pays for, whose waiters lend it their priority.
    , tMetrics :: MetricsPort
    }

{- | Run a request under the meter. 'Nothing' is a shed: the waiting room was full, or the entry
step did not fit within the wait. Everything the ticket paid returns on every exit path.
-}
withMemoryEntry :: (MonadUnliftIO m) => MetricsPort -> MemoryMeter -> (MemoryTicket -> m a) -> m (Maybe a)
withMemoryEntry metrics meter body =
    UE.mask $ \restore -> do
        ticket <- liftIO (newTicket metrics meter)
        entered <- liftIO (enter ticket)
        case entered of
            Nothing -> liftIO (mpMemoryAdmissionShed metrics) $> Nothing
            Just waited ->
                Just
                    <$> ( restore (liftIO (when waited (mpMemoryAdmissionQueued metrics)) >> body ticket)
                            `UE.finally` liftIO (releaseTicket ticket)
                        )

newTicket :: MetricsPort -> MemoryMeter -> IO MemoryTicket
newTicket metrics meter = do
    ticketId <- atomicModifyIORef' (mmNextTicket meter) (\n -> (n + 1, n))
    charged <- newTVarIO 0
    headroom <- newIORef 0
    pure MemoryTicket{tMeter = meter, tId = ticketId, tCharged = charged, tHeadroom = headroom, tServes = Nothing, tMetrics = metrics}

-- The gate. 'Just' carries whether the request had to wait. It runs under the caller's mask, and
-- the queued count taken in the deciding transaction is returned before any interruptible step.
enter :: MemoryTicket -> IO (Maybe Bool)
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
            admitted <-
                (registerDelay (mmEntryWaitMicros meter) >>= atomically . entryOrExpire)
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

-- The oldest waiter counts the tickets waiting on its shared work, so it may carry their priority.
readView :: MemoryMeter -> STM MeterView
readView meter = do
    interest <- readTVar (mmInterest meter)
    waiters <- readTVar (mmWaiters meter)
    MeterView
        <$> readTVar (mmBudget meter)
        <*> readTVar (mmCharged meter)
        <*> readTVar (mmToken meter)
        <*> pure (smallest (IntSet.fromList [oldestOf interest waiter serves | (waiter, serves) <- Map.toList waiters]))
  where
    oldestOf interest waiter serves = maybe waiter (min waiter) (smallest (waitingOn interest serves))

smallest :: IntSet -> Maybe Int
smallest = fmap fst . IntSet.minView

waitingOn :: Map FlightKey IntSet -> Maybe FlightKey -> IntSet
waitingOn interest = maybe IntSet.empty (\key -> Map.findWithDefault IntSet.empty key interest)

-- Take bytes onto both the meter and the ticket in one step, so a release can never miss them.
takeBytes :: MemoryTicket -> Int -> STM ()
takeBytes ticket bytes = do
    let meter = tMeter ticket
    modifyTVar' (mmCharged meter) (+ bytes)
    total <- (+ bytes) <$> readTVar (tCharged ticket)
    writeTVar (tCharged ticket) total
    modifyTVar' (mmLargest meter) (max total)

releaseTicket :: MemoryTicket -> IO ()
releaseTicket ticket = atomically $ do
    let meter = tMeter ticket
    held <- readTVar (tCharged ticket)
    writeTVar (tCharged ticket) 0
    modifyTVar' (mmCharged meter) (subtract held)
    holder <- readTVar (mmToken meter)
    when (holder == Just (tId ticket)) (writeTVar (mmToken meter) Nothing)

{- | Mark this request as waiting on a unit of shared work for the length of the action, so the
request that does that work may pause and overdraw with this request's priority.
-}
awaitingFlight :: (MonadUnliftIO m) => MemoryTicket -> FlightKey -> m a -> m a
awaitingFlight ticket key =
    UE.bracket_ (liftIO (atomically (update (IntSet.insert (tId ticket))))) (liftIO (atomically (update (IntSet.delete (tId ticket)))))
  where
    interest = mmInterest (tMeter ticket)
    update change = modifyTVar' interest (Map.alter (nonEmptySet . change . fromMaybe IntSet.empty) key)
    nonEmptySet set = if IntSet.null set then Nothing else Just set

-- | The same ticket, paying for shared work: its charges carry the priority of every request waiting on it.
servingFlight :: FlightKey -> MemoryTicket -> MemoryTicket
servingFlight key ticket = ticket{tServes = Just key}

-- | Pay for bytes the request is about to hold, already scaled to their charge.
charge :: MemoryTicket -> Int -> IO ()
charge ticket cost = when (cost > 0) $ do
    shortfall <- atomicModifyIORef' (tHeadroom ticket) $ \headroom ->
        if cost <= headroom then (headroom - cost, 0) else (0, cost - headroom)
    when (shortfall > 0) $ do
        let want = roundUpToStep (mmStepBytes (tMeter ticket)) shortfall
        grow ticket want
        atomicModifyIORef' (tHeadroom ticket) (\headroom -> (headroom + want - shortfall, ()))

-- Take a growth step, pausing while it neither fits nor may overdraw. A pause registers the
-- ticket as a waiter, which holds new entries back and names the oldest claimant.
grow :: MemoryTicket -> Int -> IO ()
grow ticket want = do
    immediate <- atomically (attempt ticket want)
    overdrew <- case immediate of
        Just overdrew -> pure overdrew
        Nothing -> do
            mpMemoryAdmissionPause (tMetrics ticket)
            (atomically (modifyTVar' waiters (Map.insert (tId ticket) (tServes ticket))) >> atomically (attempt ticket want >>= maybe retry pure))
                `UE.finally` atomically (modifyTVar' waiters (Map.delete (tId ticket)))
    when overdrew (mpMemoryAdmissionOverdraw (tMetrics ticket))
  where
    waiters = mmWaiters (tMeter ticket)

-- One growth attempt: 'Just' whether it overdrew, or 'Nothing' to pause.
attempt :: MemoryTicket -> Int -> STM (Maybe Bool)
attempt ticket want = do
    let meter = tMeter ticket
    view <- readView meter
    served <- IntSet.insert (tId ticket) . (`waitingOn` tServes ticket) <$> readTVar (mmInterest meter)
    case growthDecision view served want of
        GrowWithin -> takeBytes ticket want $> Just False
        GrowOnToken -> takeBytes ticket want $> Just True
        GrowTakeToken -> do
            writeTVar (mmToken meter) (Just (tId ticket))
            takeBytes ticket want $> Just True
        GrowWait -> pure Nothing
