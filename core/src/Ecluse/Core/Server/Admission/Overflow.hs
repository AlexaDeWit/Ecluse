-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Bounded overflow ahead of the metadata gates. Each request keeps its permit and arrival
order through retries. Gate signals and sweeper ticks wake requests without request timers.
-}
module Ecluse.Core.Server.Admission.Overflow (
    -- * A gate's line
    Line,
    newLine,
    signalLine,

    -- * The queue
    Overflow,
    newOverflow,
    newOverflowTuned,
    overflowWaiting,

    -- * Waiting for a place
    Attempt (..),
    attemptAfter,
    Waited (..),
    awaitPlace,
) where

import Control.Concurrent.STM (check, orElse)
import Data.Map.Strict qualified as Map
import GHC.Clock (getMonotonicTimeNSec)
import UnliftIO.Exception (finally, mask)

import Ecluse.Core.Deadline (Check, Deadlines, withDeadline, withDeadlineFlag)
import Ecluse.Core.Server.Admission.Types (Shed (..))
import Ecluse.Core.Telemetry.Record (MetricsPort (..))

-- | Requests one gate turned away, oldest first. Equality identifies the gate of a final refusal.
data Line = Line (TVar (Map Integer (TVar Bool))) (TVar Word64)
    deriving stock (Eq)

-- | An empty gate line, with one periodic wake allowed per sweep.
newLine :: IO Line
newLine = Line <$> newTVarIO Map.empty <*> newTVarIO 0

-- | Wake the oldest request on the line. A gate runs it in the transaction that frees a place.
signalLine :: Line -> STM ()
signalLine (Line entries _) =
    readTVar entries >>= \queue -> case Map.minView queue of
        Nothing -> pass
        Just (oldest, rest) -> writeTVar entries rest >> writeTVar oldest True

-- | The queue in front of both gates: how many requests wait, how many may, and for how long.
data Overflow = Overflow
    { ovDeadlines :: Deadlines
    , ovWaiting :: TVar Int
    , ovReserved :: TVar Int
    , ovNext :: TVar Integer
    , ovCap :: Int
    , ovWaitMicros :: Int
    }

-- | A queue for the serve slots: eight places for each, and a 0.5 s wait. The deadlines must be swept.
newOverflow :: Deadlines -> Int -> IO Overflow
newOverflow deadlines slots = newOverflowTuned deadlines (8 * max 1 slots) 500_000

-- | Set the cap and the microsecond wait explicitly. Both floor at zero.
newOverflowTuned :: Deadlines -> Int -> Int -> IO Overflow
newOverflowTuned deadlines cap waitMicros = do
    waiting <- newTVarIO 0
    reserved <- newTVarIO 0
    next <- newTVarIO 0
    pure Overflow{ovDeadlines = deadlines, ovWaiting = waiting, ovReserved = reserved, ovNext = next, ovCap = max 0 cap, ovWaitMicros = max 0 waitMicros}

-- | Requests parked on the gate lines, excluding retries and admitted callbacks.
overflowWaiting :: Overflow -> IO Int
overflowWaiting = readTVarIO . ovWaiting

-- | What one try at the gates came to.
data Attempt a
    = -- | It got a place, and its action ran.
      Entered a
    | -- | A gate's room was full, so the request waits on that gate's line.
      TurnedAway Line
    | -- | It took a room place, and the room's wait ran out first.
      Expired Line

-- | Read a gate's answer, where the line is that gate's.
attemptAfter :: Line -> Either Shed a -> Attempt a
attemptAfter line = \case
    Right result -> Entered result
    Left RoomFull -> TurnedAway line
    Left WaitExpired -> Expired line

-- | How a queued request ended.
data Waited a
    = -- | It got a place after waiting, and its action ran.
      Placed a
    | -- | No place freed within the wait, or the room it reached ran out of time.
      WaitedOut Line
    | -- | The queue already held its cap, so the request never joined it.
      QueueFull Line

{- | Queue a request the gate behind the line turned away, and run @try@ each time it wakes, until it
gets a place or the wait runs out. Past the cap it answers 'QueueFull' at once.
-}
awaitPlace :: MetricsPort -> Overflow -> Line -> IO (Attempt a) -> IO (Waited a)
awaitPlace metrics overflow line try = mask $ \restore -> do
    start <- getMonotonicTimeNSec
    entry <- newTVarIO False
    joined <- atomically $ do
        reserved <- readTVar (ovReserved overflow)
        if reserved >= ovCap overflow
            then pure Nothing
            else do
                age <- readTVar (ovNext overflow)
                writeTVar (ovNext overflow) (age + 1)
                writeTVar (ovReserved overflow) (reserved + 1)
                modifyTVar' (ovWaiting overflow) (+ 1)
                insertLine line age entry
                pure (Just age)
    let remaining = (\now -> ovWaitMicros overflow - fromIntegral ((now - start) `div` 1_000)) <$> getMonotonicTimeNSec
        shed waitingOn = mpOverflowShed metrics $> WaitedOut waitingOn
        queued age waitingOn waiting = do
            micros <- remaining
            woken <- waitInLine overflow waitingOn age waiting micros
            left <- remaining
            if not woken || left <= 0
                then do
                    when woken (atomically (signalLine waitingOn))
                    shed waitingOn
                else
                    restore try >>= \case
                        Entered result -> mpOverflowAdmitted metrics $> Placed result
                        Expired refusedAt -> mpOverflowAdmitted metrics $> WaitedOut refusedAt
                        TurnedAway next -> do
                            leftAfterTry <- remaining
                            if leftAfterTry <= 0
                                then shed next
                                else do
                                    fresh <- newTVarIO False
                                    atomically (modifyTVar' (ovWaiting overflow) (+ 1) >> insertLine next age fresh)
                                    queued age next fresh
    case joined of
        Nothing -> mpOverflowFull metrics $> QueueFull line
        Just age -> queued age line entry `finally` atomically (modifyTVar' (ovReserved overflow) (subtract 1))

-- Line membership ends before retrying. The permit survives until the whole request exits.
waitInLine :: Overflow -> Line -> Integer -> TVar Bool -> Int -> IO Bool
waitInLine overflow line age entry micros =
    withDeadlineFlag (ovDeadlines overflow) micros wait
        `finally` atomically (leaveLine line age entry >> modifyTVar' (ovWaiting overflow) (subtract 1))
  where
    wait due = withDeadline (ovDeadlines overflow) (wakeIfOldest line age) $ do
        woken <- atomically (wokenOrDue entry due)
        when woken (atomically (writeTVar entry False))
        pure woken

wokenOrDue :: TVar Bool -> TVar Bool -> STM Bool
wokenOrDue entry due = ((readTVar due >>= check) $> False) `orElse` ((readTVar entry >>= check) $> True)

insertLine :: Line -> Integer -> TVar Bool -> STM ()
insertLine (Line entries _) age entry = modifyTVar' entries (Map.insert age entry)

-- A late wake passes on when cancellation or expiry removes its recipient.
leaveLine :: Line -> Integer -> TVar Bool -> STM ()
leaveLine line@(Line entries _) age entry = do
    woken <- readTVar entry
    modifyTVar' entries (Map.delete age)
    when woken (signalLine line)

wakeIfOldest :: Line -> Integer -> Check
wakeIfOldest line@(Line entries lastSweep) age now = do
    atomically $ do
        queue <- readTVar entries
        previous <- readTVar lastSweep
        when (previous /= now && fmap fst (Map.lookupMin queue) == Just age) $ do
            writeTVar lastSweep now
            signalLine line
    pure Nothing
