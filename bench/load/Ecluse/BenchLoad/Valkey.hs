-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Bounded loopback RESP client for the raw-metadata experiment.
Connections are pooled. Synchronous writes finish before a miss returns.
-}
module Ecluse.BenchLoad.Valkey (
    Valkey,
    ValkeyConfig (..),
    withValkey,
    externalFetch,
    responseLength,
) where

import Control.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import Data.Aeson (encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket (Family (AF_INET), SockAddr (SockAddrInet), SocketType (Stream), close, connect, defaultProtocol, socket, socketToHandle, tupleToHostAddress)
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose)
import UnliftIO (timeout)
import UnliftIO.Exception (bracket, bracketOnError, bracket_, finally, mask, onException, throwIO, tryAny)
import UnliftIO.MVar (modifyMVar, modifyMVar_, withMVar)

import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Registry (FetchFault, RegistryResponse (..))
import Ecluse.Core.Registry.Npm (fetchMetadataFormBounded)
import Ecluse.Core.Registry.Npm.Request (MetadataForm (Full))
import Ecluse.Core.Registry.Origin (OriginClient (ocLimits, ocToken), originBaseUrl)
import Ecluse.Core.Security (maxMetadataBytes)
import Ecluse.Core.Text (displayExceptionT)
import Ecluse.Test.Package (hexSha256Of)

-- | Every experiment has its own namespace and anonymous source. Timeouts include pool waits.
data ValkeyConfig = ValkeyConfig
    { vcPort :: Int
    , vcConnections :: Int
    , vcTimeoutMicros :: Int
    , vcTtlMillis :: Int
    , vcNamespace :: Text
    , vcPublicSource :: Text
    , vcEvents :: FilePath
    , vcRecordEvents :: Bool
    }

-- | The handle owns idle sockets but never retains metadata values.
data Valkey = Valkey
    { valkeyConfig :: ValkeyConfig
    , valkeyIdle :: MVar [Handle]
    , valkeySlots :: QSem
    , valkeyEventLock :: MVar ()
    , valkeyCounters :: IORef (Map Text Integer)
    }

newtype ProtocolFailure = ProtocolFailure Text
    deriving stock (Show)
instance Exception ProtocolFailure

-- | Close all pooled sockets when the proxy exits, including a failed measurement.
withValkey :: ValkeyConfig -> (Valkey -> IO a) -> IO a
withValkey config = bracket allocate release
  where
    allocate = do
        when (vcConnections config < 1 || vcTimeoutMicros config < 1 || vcTtlMillis config < 1 || vcPort config < 1 || vcPort config > 65535) $
            throwIO (ProtocolFailure "invalid Valkey experiment bounds")
        Valkey config <$> newMVar [] <*> newQSem (vcConnections config) <*> newMVar () <*> newIORef Map.empty
    release client =
        (readIORef (valkeyCounters client) >>= LBS.writeFile (takeDirectory (vcEvents config) </> "valkey-summary.json") . encode)
            `finally` (readMVar (valkeyIdle client) >>= traverse_ hClose)

-- | Only the configured anonymous public source enters this cache. Other origins bypass it.
externalFetch :: Valkey -> OriginClient -> PackageName -> IO (Either FetchFault RegistryResponse)
externalFetch client origin name
    | originBaseUrl origin /= vcPublicSource config || isJust (ocToken origin) = fetchMetadataFormBounded origin Full name
    | otherwise = do
        result <- command client "get" ["GET", key] limit
        case result of
            Just (Just bytes) -> pure (Right (RegistryResponse 200 (BS.length bytes) bytes))
            _ -> do
                increment client "originFetches" 1
                fetched <- fetchMetadataFormBounded origin Full name
                for_ (rightToMaybe fetched) $ \response ->
                    when (responseStatusCode response == 200) $
                        void (command client "set" ["SET", key, responseBody response, "PX", encodeUtf8 (show (vcTtlMillis config) :: Text)] 2)
                pure fetched
  where
    config = valkeyConfig client
    limit = maxMetadataBytes (ocLimits origin)
    key = encodeUtf8 (vcNamespace config <> ":npm:" <> hexSha256Of (encodeUtf8 (originBaseUrl origin <> "\x1f" <> renderPackageName name)))

command :: Valkey -> Text -> [ByteString] -> Int -> IO (Maybe (Maybe ByteString))
command client operation arguments limit = do
    start <- getMonotonicTimeNSec
    result <- timeout (vcTimeoutMicros (valkeyConfig client)) (tryAny (withConnection client (exchange arguments limit)))
    end <- getMonotonicTimeNSec
    let value = result >>= rightToMaybe
        outcome = case result of
            Nothing -> "timeout" :: Text
            Just (Left _) -> "failure"
            Just (Right Nothing) -> "miss"
            Just (Right (Just _)) -> "success"
        failure = case result of
            Just (Left err) -> Just (displayExceptionT err)
            _ -> Nothing
    increment client (operation <> "." <> outcome) 1
    increment client "commandPayloadBytes" (fromIntegral (sum (map BS.length arguments)))
    increment client "receivedCompletePayloadBytes" (fromIntegral (maybe 0 (maybe 0 BS.length) value))
    when (vcRecordEvents (valkeyConfig client)) $ withMVar (valkeyEventLock client) $ \() ->
        LBS.appendFile (vcEvents (valkeyConfig client)) $
            encode (object ["operation" .= operation, "outcome" .= outcome, "failure" .= failure, "startNs" .= start, "endNs" .= end, "elapsedNs" .= (end - start), "commandPayloadBytes" .= sum (map BS.length arguments), "receivedCompletePayloadBytes" .= maybe 0 (maybe 0 BS.length) value]) <> "\n"
    pure value

increment :: Valkey -> Text -> Integer -> IO ()
increment client key amount = atomicModifyIORef' (valkeyCounters client) (\counts -> (Map.insertWith (+) key amount counts, ()))

withConnection :: Valkey -> (Handle -> IO a) -> IO a
withConnection client action = bracket_ (waitQSem (valkeySlots client)) (signalQSem (valkeySlots client)) $
    mask $ \restore -> do
        handle <- modifyMVar (valkeyIdle client) $ \case
            [] -> ([],) <$> openConnection (vcPort (valkeyConfig client))
            available : remaining -> pure (remaining, available)
        ( do
                result <- restore (action handle)
                modifyMVar_ (valkeyIdle client) (pure . (handle :))
                pure result
            )
            `onException` hClose handle

openConnection :: Int -> IO Handle
openConnection port =
    bracketOnError (socket AF_INET Stream defaultProtocol) close $ \connection -> do
        connect connection (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))
        handle <- socketToHandle connection ReadWriteMode
        hSetBuffering handle NoBuffering
        pure handle

exchange :: [ByteString] -> Int -> Handle -> IO (Maybe ByteString)
exchange arguments limit handle = do
    BS.hPut handle ("*" <> decimal (length arguments) <> "\r\n")
    for_ arguments $ \argument -> do
        BS.hPut handle ("$" <> decimal (BS.length argument) <> "\r\n")
        BS.hPut handle argument
        BS.hPut handle "\r\n"
    header <- readHeader handle
    case header of
        "+OK" -> pure (Just "OK")
        "$-1" -> pure Nothing
        _ -> do
            size <- either (throwIO . ProtocolFailure) pure (responseLength limit header)
            bytes <- BS.hGet handle size
            ending <- BS.hGet handle 2
            unless (BS.length bytes == size && ending == "\r\n") (throwIO (ProtocolFailure "truncated bulk response"))
            pure (Just bytes)
  where
    decimal :: Int -> ByteString
    decimal = encodeUtf8 . (show :: Int -> Text)

readHeader :: Handle -> IO ByteString
readHeader handle = go [] 0
  where
    go bytes count
        | count >= (128 :: Int) = throwIO (ProtocolFailure "response header exceeds 128 bytes")
        | otherwise = do
            byte <- BS.hGet handle 1
            if BS.null byte
                then throwIO (ProtocolFailure "truncated response header")
                else
                    if byte == "\n"
                        then case bytes of
                            "\r" : rest -> pure (BS.concat (reverse rest))
                            _ -> throwIO (ProtocolFailure "response header lacks CRLF")
                        else go (byte : bytes) (count + 1)

-- | Reject errors and oversized lengths before allocating a response body.
responseLength :: Int -> ByteString -> Either Text Int
responseLength limit header = case BS.uncons header of
    Just (36, digits) -> case readMaybe (toString (decodeUtf8 digits :: Text)) of
        Just size | size >= 0 && size <= limit -> Right size
        _ -> Left "invalid or oversized bulk response"
    _ -> Left ("unexpected RESP reply: " <> decodeUtf8 header)
