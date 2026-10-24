-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The queue a metadata request joins when a gate's waiting room is full. It waits there for up to
0.5 s, holding no slot, room place or memory charge. Each gate wakes the oldest request on its own
line when it frees a place, and the sweeper wakes the front request every tick in case a place freed
without a signal. A woken request tries the door again by the normal rule, so it never passes a
request already in a room. The waits rely on the sweeper's ticks, so the composition root, which
supervises the sweeper, turns the queue on.
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
import Data.Sequence qualified as Seq
import GHC.Clock (getMonotonicTimeNSec)
import UnliftIO.Exception (mask, onException)

import Ecluse.Core.Deadline (Check, Deadlines, withDeadline, withDeadlineFlag)
import Ecluse.Core.Server.Admission.Types (Shed (..))
import Ecluse.Core.Telemetry.Record (MetricsPort (..))

-- | Requests one gate turned away, oldest first. Waking a request takes it off the line.
newtype Line = Line (TVar (Seq (TVar Bool)))

newLine :: IO Line
newLine = Line <$> newTVarIO Seq.empty

-- | Wake the oldest request on the line. A gate runs it in the transaction that frees a place.
signalLine :: Line -> STM ()
signalLine (Line entries) =
    readTVar entries >>= \queue -> case Seq.viewl queue of
        Seq.EmptyL -> pass
        oldest Seq.:< rest -> writeTVar entries rest >> writeTVar oldest True

-- | The queue in front of both gates: how many requests wait, how many may, and for how long.
data Overflow = Overflow
    { ovDeadlines :: Deadlines
    , ovWaiting :: TVar Int
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
    pure Overflow{ovDeadlines = deadlines, ovWaiting = waiting, ovCap = max 0 cap, ovWaitMicros = max 0 waitMicros}

-- | How many requests wait in the queue now.
overflowWaiting :: Overflow -> IO Int
overflowWaiting = readTVarIO . ovWaiting

-- | What one try at the gates came to.
data Attempt a
    = -- | It got a place, and its action ran.
      Entered a
    | -- | A gate's room was full, so the request waits on that gate's line.
      TurnedAway Line
    | -- | It took a room place, and the room's wait ran out first.
      Expired

-- | Read a gate's answer, where the line is that gate's.
attemptAfter :: Line -> Either Shed a -> Attempt a
attemptAfter line = \case
    Right result -> Entered result
    Left RoomFull -> TurnedAway line
    Left WaitExpired -> Expired

-- | How a queued request ended.
data Waited a
    = -- | It got a place after waiting, and its action ran.
      Placed a
    | -- | No place freed within the wait, or the room it reached ran out of time.
      WaitedOut
    | -- | The queue already held its cap, so the request never joined it.
      QueueFull

{- | Queue a request the gate behind the line turned away, and run @try@ each time it wakes, until it
gets a place or the wait runs out. Past the cap it answers 'QueueFull' at once.
-}
awaitPlace :: MetricsPort -> Overflow -> Line -> IO (Attempt a) -> IO (Waited a)
awaitPlace metrics overflow line try = mask $ \restore -> do
    start <- getMonotonicTimeNSec
    entry <- newTVarIO False
    joined <- atomically (joinLine overflow line entry)
    let remaining = (\now -> ovWaitMicros overflow - fromIntegral ((now - start) `div` 1_000)) <$> getMonotonicTimeNSec
        queued waitingOn waiting = do
            woken <- waitInLine overflow waitingOn waiting =<< remaining
            if not woken
                then mpOverflowShed metrics $> WaitedOut
                else
                    restore try >>= \case
                        Entered result -> mpOverflowAdmitted metrics $> Placed result
                        Expired -> mpOverflowAdmitted metrics $> WaitedOut
                        TurnedAway next -> do
                            fresh <- newTVarIO False
                            atomically (rejoinLine overflow next fresh)
                            queued next fresh
    if joined then queued line entry else mpOverflowFull metrics $> QueueFull

-- True once woken, and False once the wait ran out. Either way the request has left the line and the count.
waitInLine :: Overflow -> Line -> TVar Bool -> Int -> IO Bool
waitInLine overflow line entry micros =
    withDeadlineFlag (ovDeadlines overflow) micros $ \due ->
        withDeadline (ovDeadlines overflow) (wakeIfOldest line entry) $ do
            woken <- atomically (wokenOrDue entry due) `onException` atomically (leaveLine overflow line entry)
            atomically (if woken then modifyTVar' (ovWaiting overflow) (subtract 1) else leaveLine overflow line entry)
            pure woken

wokenOrDue :: TVar Bool -> TVar Bool -> STM Bool
wokenOrDue entry due = ((readTVar entry >>= check) $> True) `orElse` ((readTVar due >>= check) $> False)

joinLine :: Overflow -> Line -> TVar Bool -> STM Bool
joinLine overflow (Line entries) entry = do
    waiting <- readTVar (ovWaiting overflow)
    let room = waiting < ovCap overflow
    when room $ do
        writeTVar (ovWaiting overflow) (waiting + 1)
        modifyTVar' entries (Seq.|> entry)
    pure room

-- Back to the front after a try that found no place: it is still the oldest, and it already holds a place in the count.
rejoinLine :: Overflow -> Line -> TVar Bool -> STM ()
rejoinLine overflow (Line entries) entry = do
    modifyTVar' (ovWaiting overflow) (+ 1)
    modifyTVar' entries (entry Seq.<|)

-- A wake that came after the request stopped waiting goes to the next request on the line.
leaveLine :: Overflow -> Line -> TVar Bool -> STM ()
leaveLine overflow line@(Line entries) entry = do
    woken <- readTVar entry
    if woken then signalLine line else modifyTVar' entries (Seq.filter (/= entry))
    modifyTVar' (ovWaiting overflow) (subtract 1)

-- The sweeper asks every queued request each tick, and only the oldest on its line wakes.
wakeIfOldest :: Line -> TVar Bool -> Check
wakeIfOldest line@(Line entries) entry _ = do
    queue <- readTVarIO entries
    when (Seq.lookup 0 queue == Just entry) (atomically (wakeEntry line entry))
    pure Nothing

wakeEntry :: Line -> TVar Bool -> STM ()
wakeEntry (Line entries) entry =
    readTVar entries >>= \queue -> case Seq.viewl queue of
        oldest Seq.:< rest | oldest == entry -> writeTVar entries rest >> writeTVar entry True
        _ -> pass
