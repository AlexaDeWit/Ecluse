-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Paced clients constrain receive buffers and wait one second after refusal or transport failure.
module Ecluse.BenchLoad.SlowReader (
    slowReaders,
    slowReaderDescription,
) where

import Control.Concurrent (threadDelay)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import GHC.Clock (getMonotonicTime)
import Network.HTTP.Client qualified as HTTP
import Network.Socket (AddrInfo (..), Family (AF_INET), Socket, SocketOption (MaxSegment, RecvBuffer), SocketType (Stream), close, connect, defaultHints, defaultProtocol, getAddrInfo, setSocketOption, socket)
import Network.Socket.ByteString (recv, sendAll)
import UnliftIO (bracket, bracketOnError, timeout, tryAny)
import UnliftIO.Async (forConcurrently_)

import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Harness (Companion (Companion))
import Ecluse.BenchLoad.Latency (isRefusalStatus, isSuccessStatus)
import Ecluse.BenchLoad.Oha (OhaReport (OhaReport))
import Ecluse.Core.Server.Admission.Weighted (admissionWaitMicros)

-- | This many clients reading the URLs in turn, each from its own place in the list.
slowReaders :: Int -> [Text] -> Companion
slowReaders clients urls = Companion "slow readers" clients (readFor clients urls)

-- | A slow-reader scenario's description, from its listing load and the cold scenario that load repeats.
slowReaderDescription :: Text -> Text -> Text
slowReaderDescription listing cold =
    listing
        <> " with public cache TTL 0, as "
        <> cold
        <> " does. The figures are that load's, measured while the same number of further clients request the same listings and read each response at "
        <> show (slowReadBytesPerSecond `div` 1024)
        <> " KiB/s over "
        <> show ethernetSegmentBytes
        <> "-byte segments. After a refusal or a failure, a slow reader waits one second before it asks again."

-- The rate every client reads at: a slow CI link, which takes two seconds over a 2 MiB listing.
slowReadBytesPerSecond :: Int
slowReadBytesPerSecond = 1_048_576

-- | One request's outcome: its status and the seconds from connecting to the last byte, or why it failed.
data SlowRead
    = Answered Int Double
    | Failed Text
    deriving stock (Eq, Show)

-- Where one URL's request goes, and its bytes on the wire.
data Target = Target [AddrInfo] ByteString

target :: Text -> IO Target
target url = do
    request <- HTTP.parseRequest (toString url)
    let host = HTTP.host request
        port = HTTP.port request
        wire = "GET " <> HTTP.path request <> HTTP.queryString request <> " HTTP/1.1\r\nHost: " <> host <> ":" <> show port <> "\r\nConnection: close\r\n\r\n"
    -- The measured proxy and its stubs listen on IPv4 loopback.
    addresses <- getAddrInfo (Just defaultHints{addrFamily = AF_INET, addrSocketType = Stream}) (Just (BS8.unpack host)) (Just (show port))
    pure (Target addresses wire)

readTarget :: Int -> Target -> IO SlowRead
readTarget rate (Target addresses wire) = case addresses of
    [] -> pure (Failed "no IPv4 address")
    address : _ -> do
        start <- getMonotonicTime
        kept <- bracket (open address) close $ \sock -> sendAll sock wire >> readPaced rate sock
        end <- getMonotonicTime
        pure (maybe (Failed "no status line") (`Answered` (end - start)) (statusOf kept))

-- The segment size and receive buffer are set before the handshake, which is when they take effect.
open :: AddrInfo -> IO Socket
open address =
    bracketOnError (socket (addrFamily address) Stream defaultProtocol) close $ \sock -> do
        setSocketOption sock MaxSegment ethernetSegmentBytes
        setSocketOption sock RecvBuffer receiveBufferBytes
        connect sock (addrAddress address) $> sock

-- Read to the server's close, never ahead of the rate from the first byte, keeping the status line.
readPaced :: Int -> Socket -> IO ByteString
readPaced rate sock = do
    opening <- recv sock recvBytes
    if BS.null opening then pure opening else getMonotonicTime >>= \start -> go start (BS.length opening) (BS.take statusBytes opening)
  where
    go start total kept = do
        now <- getMonotonicTime
        let due = start + fromIntegral total / fromIntegral rate
        when (due > now) (threadDelay (ceiling ((due - now) * 1_000_000)))
        chunk <- recv sock recvBytes
        if BS.null chunk
            then pure kept
            else go start (total + BS.length chunk) (if BS.length kept < statusBytes then BS.take statusBytes (kept <> chunk) else kept)

statusOf :: ByteString -> Maybe Int
statusOf kept = case BS8.words (BS8.takeWhile (/= '\r') kept) of
    _version : code : _ -> readMaybe (BS8.unpack code)
    _ -> Nothing

-- Run every client until the deadline. A read the deadline cuts short counts as oha counts its own.
readFor :: Int -> [Text] -> Int -> IO OhaReport
readFor clients urls seconds = do
    targets <- maybe (benchFail "slow readers: no URL to read") (traverse target) (nonEmpty urls)
    tally <- newIORef (Tally 0 mempty mempty [])
    start <- getMonotonicTime
    void (timeout (seconds * 1_000_000) (forConcurrently_ [0 .. clients - 1] (\i -> readLoop tally (NE.drop (i `mod` length targets) (NE.cycle targets)))))
    end <- getMonotonicTime
    reportOf (end - start) <$> readIORef tally

data Tally = Tally
    { tStarted :: Int
    , tStatuses :: Map Int Int
    , tErrors :: Map Text Int
    , tSuccessSeconds :: [Double]
    }

readLoop :: IORef Tally -> [Target] -> IO ()
readLoop tally = \case
    [] -> pass
    next : rest -> do
        atomicModifyIORef' tally (\t -> (t{tStarted = tStarted t + 1}, ()))
        outcome <- either (Failed . show) id <$> tryAny (readTarget slowReadBytesPerSecond next)
        atomicModifyIORef' tally (\t -> (record outcome t, ()))
        when (waitsAfter outcome) (threadDelay admissionWaitMicros)
        readLoop tally rest

-- A failure waits too, so readers of a proxy that is down do not spin on refused connections.
waitsAfter :: SlowRead -> Bool
waitsAfter = \case
    Answered status _ -> isRefusalStatus status
    Failed _ -> True

record :: SlowRead -> Tally -> Tally
record outcome t = case outcome of
    Answered status seconds ->
        t
            { tStatuses = Map.insertWith (+) status 1 (tStatuses t)
            , tSuccessSeconds = if isSuccessStatus status then seconds : tSuccessSeconds t else tSuccessSeconds t
            }
    Failed reason -> t{tErrors = Map.insertWith (+) reason 1 (tErrors t)}

reportOf :: Double -> Tally -> OhaReport
reportOf elapsed t = OhaReport elapsed (Map.mapKeys show (tStatuses t)) errors Nothing (tSuccessSeconds t) Nothing
  where
    unfinished = tStarted t - sum (tStatuses t) - sum (tErrors t)
    errors = if unfinished > 0 then Map.insert "aborted due to deadline" unfinished (tErrors t) else tErrors t

ethernetSegmentBytes :: Int
ethernetSegmentBytes = 1448

receiveBufferBytes :: Int
receiveBufferBytes = 65_536

recvBytes :: Int
recvBytes = 16_384

statusBytes :: Int
statusBytes = 64
