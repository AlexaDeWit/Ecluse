-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Bound registry transfers by body progress during upstream waits.
Consumer time and waits for response headers do not count.
-}
module Ecluse.Core.Registry.Progress (
    -- * The watchdog
    Watch,
    watched,
    watchedRaising,
    isProgressFloorFailure,

    -- * Metered transfers
    meteredReader,
    meteredUpload,
    invTimerMode,
    armDeadline,
    ServeCapExceeded (..),
) where

import Control.Concurrent (forkIO, forkIOWithUnmask, killThread)
import Control.Concurrent.MVar qualified as InvMVar
import GHC.Conc (getNumCapabilities, threadCapability)
import UnliftIO.Exception (uninterruptibleMask_)

import Data.Unique qualified as InvUnique
import GHC.Event qualified as InvEvent
import System.Environment qualified as InvEnv
import System.IO.Unsafe qualified as InvUnsafe

import Control.Exception (throwTo)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as LBS
import GHC.Clock (getMonotonicTimeNSec)
import GHC.IORef qualified as IORef
import Network.HTTP.Client (BodyReader, GivesPopper, Popper, RequestBody (..))
import UnliftIO (bracket, try, withAsync)
import UnliftIO.Concurrent (ThreadId, myThreadId, threadDelay)

import Ecluse.Core.Registry.Progress.Internal (Window, beginAt, endAt, freshWindow, pause, waitedBy)
import Ecluse.Core.Security (ProgressFloor, floorWindowMicros)

-- | One transfer's progress in the current window, shared by its meters and its watchdog.
data Watch = Watch ProgressFloor (IORef Window)

-- Raised in a transfer's thread as a synchronous failure of the transfer, as a socket error would be.
data BelowProgressFloor = BelowProgressFloor
    deriving stock (Show)

instance Exception BelowProgressFloor

-- | Identify the watchdog's transport-timeout interruption without exposing its constructor.
isProgressFloorFailure :: SomeException -> Bool
isProgressFloorFailure fault = case fromException fault of
    Just BelowProgressFloor -> True
    Nothing -> False

-- | Return 'Nothing' below the floor. The catch outlives the watchdog, so its interruption cannot escape later.
watched :: ProgressFloor -> (Watch -> IO a) -> IO (Maybe a)
watched progress transfer =
    try (watchedRaising progress transfer) <&> \case
        Left BelowProgressFloor -> Nothing
        Right result -> Just result

-- | Raise a floor failure so an already committed response tears down.
watchedRaising :: ProgressFloor -> (Watch -> IO a) -> IO a
watchedRaising progress transfer = do
    owner <- myThreadId
    watch <- Watch progress <$> newIORef freshWindow
    case invTimerMode of
        "ship" -> withAsync (watchdog watch owner) (\_ -> transfer watch)
        "none" -> transfer watch
        "one" -> invTimers 1 (transfer watch)
        "tm4" -> invTimers 4 (transfer watch)
        "uniq3" -> replicateM_ 3 (void InvUnique.newUnique) >> transfer watch
        "tm4u3" -> replicateM_ 3 (void InvUnique.newUnique) >> invTimers 4 (transfer watch)
        "sweep" -> bracket (armDeadline (watchDue watch)) id (\_ -> transfer watch)
        _ -> withAsync (watchdog watch owner) (\_ -> transfer watch)

-- The watchdog's verdict at one sweep: the floor's interruption once the window's waits exceed its budget.
watchDue :: Watch -> Word64 -> IO (Maybe SomeException)
watchDue (Watch progress window) now = do
    current <- readIORef window
    pure (if waitedBy now current >= fromIntegral (floorWindowMicros progress) * 1_000 then Just (toException BelowProgressFloor) else Nothing)

-- | The serve-path cap's interruption, delivered by the sweeper.
data ServeCapExceeded = ServeCapExceeded
    deriving stock (Show)

instance Exception ServeCapExceeded

data ArmState = ArmRunning | ArmFiring (InvMVar.MVar ThreadId) | ArmDone

{- Prototype (#1835): one process-wide sweeper thread checks every armed deadline each tick. Arming
conses onto the arming capability's list, so no request touches the GHC timer manager. -}
{-# NOINLINE invSweeper #-}
invSweeper :: [IORef [Word64 -> IO Bool]]
invSweeper = InvUnsafe.unsafePerformIO $ do
    count <- getNumCapabilities
    cells <- replicateM (max 1 count) (newIORef [])
    _ <- forkIO (forever (threadDelay invTickMicros >> traverse_ sweepCell cells))
    pure cells
  where
    sweepCell cell = do
        entries <- IORef.atomicSwapIORef cell []
        now <- getMonotonicTimeNSec
        kept <- filterM (fmap not . ($ now)) entries
        unless (null kept) (void (IORef.atomicModifyIORef'_ cell (kept <>)))

invTickMicros :: Int
invTickMicros = InvUnsafe.unsafePerformIO (maybe 100_000 (* 1000) . (readMaybe =<<) <$> InvEnv.lookupEnv "INV_SWEEP_TICK_MS")
{-# NOINLINE invTickMicros #-}

{- | Arm a check for the calling thread. At each tick the sweeper asks @due@ whether to interrupt; a
due exception is thrown from a forked thread, as 'System.Timeout' does. The result disarms, and must
run before the caller leaves the scope that expects the exception.
-}
armDeadline :: (Word64 -> IO (Maybe SomeException)) -> IO (IO ())
armDeadline due = do
    owner <- myThreadId
    armed <- newIORef ArmRunning
    let check now =
            readIORef armed >>= \case
                ArmRunning ->
                    due now >>= \case
                        Nothing -> pure False
                        Just failure -> do
                            firing <- InvMVar.newEmptyMVar
                            won <- atomicModifyIORef' armed $ \case
                                ArmRunning -> (ArmFiring firing, True)
                                other -> (other, False)
                            when won (forkIOWithUnmask (\unmask -> unmask (throwTo owner failure)) >>= InvMVar.putMVar firing)
                            pure True
                _ -> pure True
    (capability, _) <- threadCapability owner
    for_ (invSweeper !!? (capability `mod` length invSweeper)) $ \cell -> void (IORef.atomicModifyIORef'_ cell (check :))
    pure $ do
        previous <- atomicModifyIORef' armed (ArmDone,)
        case previous of
            ArmFiring firing -> InvMVar.readMVar firing >>= uninterruptibleMask_ . killThread
            _ -> pass

-- Diagnostic only: the shipped timer shape is chosen by INV_TIMER_MODE.
invTimerMode :: String
invTimerMode = InvUnsafe.unsafePerformIO (fromMaybe "sweep" <$> InvEnv.lookupEnv "INV_TIMER_MODE")
{-# NOINLINE invTimerMode #-}

-- Register n far timers on the system timer manager for the length of the action.
invTimers :: Int -> IO a -> IO a
invTimers n action = do
    manager <- InvEvent.getSystemTimerManager
    bracket
        (replicateM n (InvEvent.registerTimeout manager 600_000_000 pass))
        (traverse_ (InvEvent.unregisterTimeout manager))
        (const action)

{- Base 'throwTo' delivers the plain exception, which the synchronous catch in 'watched' sees.
UnliftIO's would wrap it as asynchronous. -}
watchdog :: Watch -> ThreadId -> IO ()
watchdog (Watch progress window) owner = go
  where
    budget = fromIntegral (floorWindowMicros progress) * 1_000
    go = do
        now <- getMonotonicTimeNSec
        current <- readIORef window
        let spent = waitedBy now current
        if spent >= budget
            then throwTo owner BelowProgressFloor
            else threadDelay (fromIntegral (pause budget spent current `div` 1_000) + 1) >> go

-- | A reader whose every read counts as a wait on the upstream, and its chunk as progress.
meteredReader :: Watch -> BodyReader -> BodyReader
meteredReader watch readChunk = do
    beginWait watch
    chunk <- readChunk
    endWait watch (BS.length chunk)
    pure chunk

-- | Count each slice once written. An empty body has no upload to watch.
meteredUpload :: Watch -> RequestBody -> RequestBody
meteredUpload watch body = case body of
    RequestBodyLBS bytes | not (LBS.null bytes) -> fromChunks (LBS.length bytes) (LBS.toChunks bytes)
    RequestBodyBS bytes | not (BS.null bytes) -> fromChunks (fromIntegral (BS.length bytes)) [bytes]
    RequestBodyBuilder size builder | size > 0 -> fromChunks size (LBS.toChunks (toLazyByteString builder))
    RequestBodyStream size gives -> RequestBodyStream size (meteredGives watch gives)
    RequestBodyStreamChunked gives -> RequestBodyStreamChunked (meteredGives watch gives)
    RequestBodyIO io -> RequestBodyIO (meteredUpload watch <$> io)
    _ -> body
  where
    fromChunks size chunks = RequestBodyStream size (meteredGives watch (givesChunks chunks))

givesChunks :: [ByteString] -> GivesPopper ()
givesChunks chunks needsPopper = do
    remaining <- newIORef chunks
    needsPopper . atomicModifyIORef' remaining $ \case
        [] -> ([], BS.empty)
        chunk : rest -> (rest, chunk)

{- The connection asks for the next slice only once it has written the last, so each call closes
the wait that slice opened. Slicing keeps a large chunk from counting only when it all lands. -}
meteredGives :: Watch -> GivesPopper () -> GivesPopper ()
meteredGives watch gives needsPopper = gives $ \popper -> do
    handed <- newIORef 0
    pending <- newIORef BS.empty
    needsPopper $ do
        readIORef handed >>= \size -> when (size > 0) (endWait watch size)
        slice <- nextSlice pending popper
        writeIORef handed (BS.length slice)
        unless (BS.null slice) (beginWait watch)
        pure slice

nextSlice :: IORef ByteString -> Popper -> IO ByteString
nextSlice pending popper = do
    held <- readIORef pending
    chunk <- if BS.null held then popper else pure held
    let (slice, rest) = BS.splitAt uploadSliceBytes chunk
    writeIORef pending rest
    pure slice

uploadSliceBytes :: Int
uploadSliceBytes = 64 * 1024

-- GHC.IORef's atomicModifyIORef'_ builds no pair per update, unlike relude's namesake.
beginWait :: Watch -> IO ()
beginWait (Watch _ window) = do
    now <- getMonotonicTimeNSec
    void (IORef.atomicModifyIORef'_ window (beginAt now))

endWait :: Watch -> Int -> IO ()
endWait (Watch progress window) bytes = do
    now <- getMonotonicTimeNSec
    void (IORef.atomicModifyIORef'_ window (endAt progress now bytes))
