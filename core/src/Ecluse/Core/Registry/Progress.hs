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
) where

import Control.Exception (throwTo)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as LBS
import GHC.Clock (getMonotonicTimeNSec)
import GHC.IORef qualified as IORef
import Network.HTTP.Client (BodyReader, GivesPopper, Popper, RequestBody (..))
import UnliftIO (try, withAsync)
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
