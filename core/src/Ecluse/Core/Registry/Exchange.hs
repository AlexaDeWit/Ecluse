-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Bounded registry exchanges and their transport-fault classification.
Read exchanges retain explicit access refusals before reading an error body. Every exchange runs
under the "Ecluse.Core.Registry.Progress" watchdog: one that moves fewer than the floor's body
bytes, up or down, in a window of waiting fails with the transport timeout, and its connection
closes rather than returning to the pool. The serve path also wraps its exchanges in
'withinServeCap', which ends each one before the request timeout.
-}
module Ecluse.Core.Registry.Exchange (
    -- * The bounded exchange
    boundedExchange,
    runExchange,
    singleAttemptSettings,
    newSingleAttemptManager,
    withSuccessBody,
    boundedRelay,

    -- * The serve-path cap
    withinServeCap,

    -- * Source digests
    digestingRead,

    -- * Metered reads
    chargedRead,
    chargedReadWith,
    accumulatingCharge,
    accumulatingChargeFrom,
    boundedChargedRead,

    -- * Request formation
    formThen,
) where

import Crypto.Hash (hashInit, hashUpdate)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Client (
    BodyReader,
    Manager,
    ManagerSettings (managerRetryableException),
    Request (requestBody),
    Response (responseStatus),
    brRead,
    newManager,
    responseBody,
    withResponse,
 )
import Network.HTTP.Types.Status (statusCode)
import UnliftIO (try)
import UnliftIO.Timeout (timeout)

import Ecluse.Core.Fault (TransportCause (TransportTimeout), transportFault)
import Ecluse.Core.Fault.Http (classifyTransport, typedTlsManagerSettings)
import Ecluse.Core.Registry (
    BodyOutcome (SuccessBody, UnreadStatus),
    FetchFault (FetchBoundExceeded, FetchTransport),
    PublishRelayResponse (..),
    UrlFormationError,
    isSuccessStatus,
 )
import Ecluse.Core.Registry.Progress (meteredReader, meteredUpload, watched)
import Ecluse.Core.Security (
    BodyLimit,
    LimitError,
    ProgressFloor,
    boundedRead,
    floorMinBytes,
    floorServeCapMicros,
    floorWindowMicros,
 )
import Ecluse.Core.Server.MemoryModel (bufferedChunkCharge)
import Ecluse.Core.Snapshot (ContentDigest, digestFromContext)

-- | Destructive clients must return uncertain transport failures for reassessment before retry.
singleAttemptSettings :: ManagerSettings -> ManagerSettings
singleAttemptSettings settings = settings{managerRetryableException = const False}

-- | A 'typedTlsManagerSettings' manager under 'singleAttemptSettings'. @newTlsManagerWith@ would replace its retry check and restore the replay on a reused connection.
newSingleAttemptManager :: IO Manager
newSingleAttemptManager = newManager (singleAttemptSettings typedTlsManagerSettings)

-- | Project status, decompressed byte count, and body. Transport failures retain their typed cause.
boundedExchange :: (Int -> Int -> ByteString -> a) -> Manager -> ProgressFloor -> BodyLimit -> Request -> IO (Either FetchFault a)
boundedExchange project manager progress limits request =
    runExchange manager progress request (readBounded project limits)

-- | Consume a response under the progress watchdog, classifying transport and body-limit failures.
runExchange :: Manager -> ProgressFloor -> Request -> (Response BodyReader -> IO (Either LimitError a)) -> IO (Either FetchFault a)
runExchange manager progress request readResponse =
    watched progress (\watch -> try (withResponse (metered watch) manager (readResponse . fmap (meteredReader watch))))
        <&> \case
            Nothing -> Left (belowFloor progress)
            Just (Left httpErr) -> Left (FetchTransport (classifyTransport httpErr))
            Just (Right (Left limitErr)) -> Left (FetchBoundExceeded limitErr)
            Just (Right (Right projected)) -> Right projected
  where
    metered watch = request{requestBody = meteredUpload watch (requestBody request)}

-- | Fail an action that outlives the floor's serve-path cap with the transport timeout.
withinServeCap :: ProgressFloor -> (FetchFault -> e) -> IO (Either e a) -> IO (Either e a)
withinServeCap progress inject action =
    fromMaybe (Left (inject capExceeded)) <$> timeout (floorServeCapMicros progress) action
  where
    capExceeded = FetchTransport (transportFault TransportTimeout ("the upstream exchange outlived its " <> seconds (floorServeCapMicros progress) <> "-second serve-path cap"))

belowFloor :: ProgressFloor -> FetchFault
belowFloor progress =
    FetchTransport . transportFault TransportTimeout $
        "the exchange moved fewer than "
            <> show (floorMinBytes progress)
            <> " body bytes, counting both directions together, in a "
            <> seconds (floorWindowMicros progress)
            <> "-second progress window"

-- Whole seconds as an operator wrote them, and a fraction only where one exists.
seconds :: Int -> Text
seconds micros = case micros `divMod` 1_000_000 of
    (whole, 0) -> show whole
    _ -> show (fromIntegral micros / 1_000_000 :: Double)

-- | The exchange keeping the answered status alongside the body, for the first-party relay.
boundedRelay :: Manager -> ProgressFloor -> BodyLimit -> Request -> IO (Either FetchFault PublishRelayResponse)
boundedRelay =
    boundedExchange $ \status _ body ->
        PublishRelayResponse{relayStatus = status, relayBody = LBS.fromStrict body}

-- | Report request-formation and exchange failures through the same error channel.
formThen ::
    (UrlFormationError -> fault) ->
    (Request -> IO (Either fault a)) ->
    Either UrlFormationError Request ->
    IO (Either fault a)
formThen unformable = either (pure . Left . unformable)

readBounded :: (Int -> Int -> ByteString -> a) -> BodyLimit -> Response BodyReader -> IO (Either LimitError a)
readBounded project limits response =
    fmap (uncurry (project (statusCode (responseStatus response))))
        <$> boundedRead limits (brRead (responseBody response))

-- | Consume a 2xx body within the response lifetime. A status outside 2xx never reaches the consumer.
withSuccessBody :: Manager -> ProgressFloor -> (IO ByteString -> IO (Either LimitError a)) -> Request -> IO (Either FetchFault (BodyOutcome a))
withSuccessBody manager progress consume request = runExchange manager progress request $ \response -> do
    let code = statusCode (responseStatus response)
    if isSuccessStatus code
        then fmap (SuccessBody code) <$> consume (brRead (responseBody response))
        else pure (Right (UnreadStatus code))

-- | Run a consumer over a source that hashes each chunk it passes on. A success carries the digest of every chunk read.
digestingRead :: (IO ByteString -> IO (Either e a)) -> IO ByteString -> IO (Either e (a, ContentDigest))
digestingRead consume readChunk = do
    context <- newIORef hashInit
    let next = do
            chunk <- readChunk
            modifyIORef' context (`hashUpdate` chunk)
            pure chunk
    consume next >>= traverse (\result -> (result,) . digestFromContext <$> readIORef context)

-- | Pay for each chunk's length before the consumer sees it.
chargedRead :: (Int -> IO ()) -> IO ByteString -> IO ByteString
chargedRead charge = chargedReadWith (charge . BS.length)

-- | Pay for the observed chunk before a parser or buffer retains it.
chargedReadWith :: (ByteString -> IO ()) -> IO ByteString -> IO ByteString
chargedReadWith charge readChunk = do
    chunk <- readChunk
    charge chunk
    pure chunk

-- | Convert per-chunk costs to an absolute requirement within one read operation.
accumulatingCharge :: (Int -> IO ()) -> IO (Int -> IO ())
accumulatingCharge charge = do
    (add, _) <- accumulatingChargeFrom 0 charge
    pure $ \bytes -> when (bytes > 0) (add bytes)

{- | Start from an existing reservation and report the saturated total, including after callback failure.
Every addition invokes the callback, including zero. Construction does not charge the seed.
-}
accumulatingChargeFrom :: Integer -> (Int -> IO ()) -> IO (Int -> IO (), IO Int)
accumulatingChargeFrom initial charge = do
    held <- newIORef initial
    let total = fromInteger . min (toInteger (maxBound :: Int))
        add bytes = do
            required <- atomicModifyIORef' held (\previous -> let next = previous + toInteger bytes in (next, next))
            charge (total required)
    pure (add, total <$> readIORef held)

-- | Reserve buffered chunks and their final strict copy before the bounded reader retains them.
boundedChargedRead :: (Int -> IO ()) -> BodyLimit -> IO ByteString -> IO (Either LimitError (Int, ByteString))
boundedChargedRead charge limits readChunk = do
    retain <- accumulatingCharge charge
    boundedRead limits (chargedRead (retain . bufferedChunkCharge) readChunk)
