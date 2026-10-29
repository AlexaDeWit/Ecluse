-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Deadlines that one sweeper thread checks every 100 ms, so arming one never waits on another
thread. GHC's timers share one process-wide queue, and every arm and disarm edits it in turn. Here
arming adds to a list owned by the arming thread's capability, and a deadline fires about one tick
late. A due check is thrown from a forked thread, as 'System.Timeout.timeout' does, so a disarm can
stop a throw still in flight. A flag scope raises a 'TVar' instead, for a transaction that waits.
-}
module Ecluse.Core.Deadline (
    -- * The sweeper
    Deadlines,
    newDeadlines,
    sweepTick,

    -- * Armed scopes
    Check,
    withDeadline,
    withEndableDeadline,
    withDeadlineFlag,
    timeout,
    inspectionDeadline,
) where

import Control.Exception (asyncExceptionFromException, asyncExceptionToException, handleJust, throwTo)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, sizeofSmallArray, smallArrayFromList)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getNumCapabilities, threadCapability)
import GHC.IORef qualified as IORef
import Text.Show (showString, showsPrec)
import UnliftIO.Concurrent (ThreadId, forkIOWithUnmask, killThread, myThreadId, threadDelay)
import UnliftIO.Exception (bracket, onException, uninterruptibleMask_)
import UnliftIO.MVar (isEmptyMVar)

import Ecluse.Core.Security.Limits (requestTimeoutSeconds)

-- | The process's armed checks, one list for each capability.
newtype Deadlines = Deadlines (SmallArray (IORef [Armed]))

{- | Asked at each sweep with the monotonic time in nanoseconds. A 'Just' is raised in the armed
thread, and ends the check.
-}
type Check = Word64 -> IO (Maybe SomeException)

-- The lock is empty while the scope is open. Disarming fills it with 'Nothing', and a thrower with its own id.
data Armed = Armed ThreadId (MVar (Maybe ThreadId)) Check

-- | One list for each capability the process runs now. Create it once the capability count is final.
newDeadlines :: IO Deadlines
newDeadlines = do
    count <- getNumCapabilities
    Deadlines . smallArrayFromList <$> replicateM (max 1 count) (newIORef [])

-- | Wait one tick, then raise every due check. The composition root repeats it under supervision.
sweepTick :: Deadlines -> IO ()
sweepTick (Deadlines cells) = do
    threadDelay tickMicros
    now <- getMonotonicTimeNSec
    traverse_ (sweepCell now) cells

tickMicros :: Int
tickMicros = 100_000

-- A fault part way through puts every entry back. One that fired already leaves at the next sweep.
sweepCell :: Word64 -> IORef [Armed] -> IO ()
sweepCell now cell = do
    entries <- IORef.atomicSwapIORef cell []
    kept <- filterM (stillArmed now) entries `onException` putBack entries
    unless (null kept) (putBack kept)
  where
    -- Consing never forces the list underneath, so an arming thread never waits on the sweeper.
    putBack entries = void (IORef.atomicModifyIORef'_ cell (entries <>))

stillArmed :: Word64 -> Armed -> IO Bool
stillArmed now (Armed owner lock check) =
    isEmptyMVar lock >>= \case
        False -> pure False
        True ->
            check now >>= \case
                Nothing -> pure True
                Just failure -> False <$ forkIOWithUnmask (\unmask -> unmask (throwOnce owner lock failure))

-- Base 'throwTo' delivers the exception as given. UnliftIO's would wrap a synchronous one as asynchronous.
throwOnce :: ThreadId -> MVar (Maybe ThreadId) -> SomeException -> IO ()
throwOnce owner lock failure = do
    self <- myThreadId
    claimed <- tryPutMVar lock (Just self)
    when claimed (throwTo owner failure)

{- | Run an action under a check the sweeper asks every tick. A failure it returns is raised in this
thread while the action runs, and never after it ends.
-}
withDeadline :: Deadlines -> Check -> IO a -> IO a
withDeadline deadlines check = withEndableDeadline deadlines check . const

-- | 'withDeadline' whose action can end the check early. Once the end returns, nothing the check raised lands.
withEndableDeadline :: Deadlines -> Check -> (IO () -> IO a) -> IO a
withEndableDeadline deadlines check action = bracket (arm deadlines check) disarm (action . disarm)

arm :: Deadlines -> Check -> IO (MVar (Maybe ThreadId))
arm (Deadlines cells) check = do
    owner <- myThreadId
    lock <- newEmptyMVar
    (capability, _) <- threadCapability owner
    let cell = indexSmallArray cells (capability `mod` sizeofSmallArray cells)
    void (IORef.atomicModifyIORef'_ cell (Armed owner lock check :))
    pure lock

-- A thrower that claimed the lock first dies before its throw lands, as this thread is uninterruptible here. A second disarm does nothing.
disarm :: MVar (Maybe ThreadId) -> IO ()
disarm lock = uninterruptibleMask_ $ do
    disarmed <- tryPutMVar lock Nothing
    unless disarmed (readMVar lock >>= traverse_ killThread)

{- | Run an action with a flag the first sweep after the microseconds raises, so a transaction can wait
for the deadline without a GHC timer. A bound of zero or less starts raised and needs no sweep.
-}
withDeadlineFlag :: Deadlines -> Int -> (TVar Bool -> IO a) -> IO a
withDeadlineFlag deadlines micros action
    | micros <= 0 = newTVarIO True >>= action
    | otherwise = do
        flag <- newTVarIO False
        limit <- (`nanosAfter` micros) <$> getMonotonicTimeNSec
        withDeadline deadlines (raiseFrom limit flag) (action flag)

-- Raising throws nothing, so the check stays until its scope ends, and it writes the flag once.
raiseFrom :: Word64 -> TVar Bool -> Check
raiseFrom limit flag now = Nothing <$ when (now >= limit) (atomically (readTVar flag >>= \raised -> unless raised (writeTVar flag True)))

{- | 'System.Timeout.timeout' on the sweeper: 'Nothing' once the action outlives the microseconds,
about a tick late. A negative bound never fires, and zero fires at once.
-}
timeout :: Deadlines -> Int -> IO a -> IO (Maybe a)
timeout deadlines micros action
    | micros < 0 = Just <$> action
    | micros == 0 = pure Nothing
    | otherwise = do
        expiry <- Expired <$> newIORef ()
        limit <- (`nanosAfter` micros) <$> getMonotonicTimeNSec
        let due now = pure (if now >= limit then Just (toException expiry) else Nothing)
        -- Base 'handleJust', because the unliftio one never catches an asynchronous exception.
        handleJust (\caught -> if caught == expiry then Just () else Nothing) (\() -> pure Nothing) $
            Just <$> withDeadline deadlines due action

-- | Return the refusal once an archive or metadata inspection outlives the request timeout, on the sweeper.
inspectionDeadline :: Deadlines -> e -> IO (Either e a) -> IO (Either e a)
inspectionDeadline deadlines refusal action = fromMaybe (Left refusal) <$> timeout deadlines (requestTimeoutSeconds * 1_000_000) action

-- The monotonic nanoseconds this many microseconds after an instant, saturating rather than wrapping.
nanosAfter :: Word64 -> Int -> Word64
nanosAfter start micros = fromInteger (min (toInteger (maxBound :: Word64)) (toInteger start + toInteger micros * 1_000))

-- Asynchronous, as 'System.Timeout.timeout''s own is, so a synchronous catch inside the action passes it on.
-- Each scope's reference is its identity, so a nested scope never takes another's.
newtype Expired = Expired (IORef ())
    deriving newtype (Eq)

-- Shown as 'System.Timeout.timeout''s own, which a client span may record.
instance Show Expired where
    showsPrec _ _ = showString "<<timeout>>"

instance Exception Expired where
    toException = asyncExceptionToException
    fromException = asyncExceptionFromException
