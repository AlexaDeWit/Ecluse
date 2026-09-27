-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The progress watchdog a registry transfer runs under. It counts the request-body bytes
handed to the connection and the response-body bytes read from it, and the time spent blocked on
the upstream in either direction. A transfer that waits a whole 'ProgressFloor' window without
moving the floor's bytes is stopped. The consumer's own work between reads, and the wait for the
status line and headers, never count.
-}
module Ecluse.Core.Registry.Progress (
    -- * The watchdog
    Watch,
    watched,
    watchedRaising,

    -- * Metered transfers
    meteredReader,
    meteredUpload,
) where

import Control.Exception (throwTo)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as LBS
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client (BodyReader, GivesPopper, Popper, RequestBody (..))
import UnliftIO (try, withAsync)
import UnliftIO.Concurrent (ThreadId, myThreadId, threadDelay)

import Ecluse.Core.Security (ProgressFloor, floorMinBytes, floorWindowMicros)

-- | One transfer's progress in the current window, shared by its meters and its watchdog.
data Watch = Watch ProgressFloor (IORef Window)

-- When the current wait on the upstream began, the nanoseconds waited before it, and the bytes moved.
data Window = Window (Maybe Word64) Word64 Int

-- Raised in a transfer's thread as a synchronous failure of the transfer, as a socket error would be.
data BelowProgressFloor = BelowProgressFloor
    deriving stock (Show)

instance Exception BelowProgressFloor

{- | Run a transfer under the watchdog. 'Nothing' means the transfer fell below the floor and was
stopped. The catch sits outside the watchdog's lifetime, so a raise can never land after it.
-}
watched :: ProgressFloor -> (Watch -> IO a) -> IO (Maybe a)
watched progress transfer =
    try (watchedRaising progress transfer) <&> \case
        Left BelowProgressFloor -> Nothing
        Right result -> Just result

{- | 'watched' for a transfer already committed to a client, which a floor miss can only abort. The
failure propagates as an exception, so the response tears down instead of ending cleanly.
-}
watchedRaising :: ProgressFloor -> (Watch -> IO a) -> IO a
watchedRaising progress transfer = do
    owner <- myThreadId
    watch <- Watch progress <$> newIORef (Window Nothing 0 0)
    withAsync (watchdog watch owner) (\_ -> transfer watch)

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

{- With a wait open the watchdog wakes as its budget runs out. With none open the waited time cannot
grow, so it sleeps at least a sixteenth of the window instead of spinning on a near-spent budget. -}
pause :: Word64 -> Word64 -> Window -> Word64
pause budget spent = \case
    Window (Just _) _ _ -> budget - spent
    Window Nothing _ _ -> max (budget `div` 16) (budget - spent)

-- | A reader whose every read counts as a wait on the upstream, and its chunk as progress.
meteredReader :: Watch -> BodyReader -> BodyReader
meteredReader watch readChunk = do
    beginWait watch
    chunk <- readChunk
    endWait watch (BS.length chunk)
    pure chunk

{- | The same body, handed over in slices the watch counts once each is written. A body with no
bytes has no upload to watch and stays as it is.
-}
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

beginWait :: Watch -> IO ()
beginWait (Watch _ window) = do
    now <- getMonotonicTimeNSec
    atomicModifyIORef' window (\(Window _ waited moved) -> (Window (Just now) waited moved, ()))

endWait :: Watch -> Int -> IO ()
endWait (Watch progress window) bytes = do
    now <- getMonotonicTimeNSec
    atomicModifyIORef' window $ \current@(Window _ _ moved) ->
        if moved + bytes >= floorMinBytes progress
            then (Window Nothing 0 0, ())
            else (Window Nothing (waitedBy now current) (moved + bytes), ())

-- The watchdog can read its clock just before the transfer stamps a later wait, so the gap floors at zero.
waitedBy :: Word64 -> Window -> Word64
waitedBy now (Window since waited _) = waited + maybe 0 (\began -> if now > began then now - began else 0) since
