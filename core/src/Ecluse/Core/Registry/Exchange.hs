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
    singleAttemptSettings,
    boundedFetch,
    boundedJsonFetch,
    withSuccessBody,
    boundedRelay,

    -- * The serve-path cap
    withinServeCap,

    -- * Source digests
    digestingRead,

    -- * Request formation
    formThen,
) where

import Crypto.Hash (hashInit, hashUpdate)
import Data.ByteString.Lazy qualified as LBS
import Data.JsonStream.Parser qualified as J
import Network.HTTP.Client (
    BodyReader,
    Manager,
    ManagerSettings (managerRetryableException),
    Request (requestBody),
    Response (responseStatus),
    brRead,
    responseBody,
    withResponse,
 )
import Network.HTTP.Types.Status (statusCode)
import UnliftIO (try)
import UnliftIO.Timeout (timeout)

import Ecluse.Core.Fault (TransportCause (TransportTimeout), transportFault)
import Ecluse.Core.Fault.Http (classifyTransport)
import Ecluse.Core.Registry (
    BodyOutcome (SuccessBody, UnreadStatus),
    FetchFault (FetchBoundExceeded, FetchTransport),
    PublishRelayResponse (..),
    RegistryResponse (RegistryResponse),
    UrlFormationError,
    isAuthorisationFailure,
    isSuccessStatus,
 )
import Ecluse.Core.Registry.JsonStream (StreamResult, readJsonStream)
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
import Ecluse.Core.Snapshot (ContentDigest, digestFromContext)

-- | Destructive clients must return uncertain transport failures for reassessment before retry.
singleAttemptSettings :: ManagerSettings -> ManagerSettings
singleAttemptSettings settings = settings{managerRetryableException = const False}

-- | Project status, decompressed byte count, and body. Transport failures retain their typed cause.
boundedExchange :: (Int -> Int -> ByteString -> a) -> Manager -> ProgressFloor -> BodyLimit -> Request -> IO (Either FetchFault a)
boundedExchange project manager progress limits request =
    runExchange manager progress request (readBounded project limits)

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

{- | Fail an action that outlives the floor's serve-path cap with the transport timeout. The serve
path wraps its exchanges in it, and the mirror worker and the Dredger do not.
-}
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

-- | Preserve explicit auth refusals without reading their untrusted error bodies.
boundedFetch :: Manager -> ProgressFloor -> BodyLimit -> Request -> IO (Either FetchFault RegistryResponse)
boundedFetch manager progress limits request = runExchange manager progress request $ \response ->
    let code = statusCode (responseStatus response)
     in if isAuthorisationFailure code
            then pure (Right (RegistryResponse code 0 ""))
            else readBounded RegistryResponse limits response

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

-- | Extract selected values from a 2xx body within the response lifetime. Other statuses are never parsed.
boundedJsonFetch :: Manager -> ProgressFloor -> BodyLimit -> J.Parser a -> (s -> a -> Either LimitError s) -> s -> Request -> IO (Either FetchFault (BodyOutcome (StreamResult s)))
boundedJsonFetch manager progress limits parser step initial = withSuccessBody manager progress (readJsonStream limits parser step initial)

-- | Consume a 2xx body within the response lifetime. A status outside 2xx never reaches the consumer.
withSuccessBody :: Manager -> ProgressFloor -> (IO ByteString -> IO (Either LimitError a)) -> Request -> IO (Either FetchFault (BodyOutcome a))
withSuccessBody manager progress consume request = runExchange manager progress request $ \response -> do
    let code = statusCode (responseStatus response)
    if isSuccessStatus code
        then fmap (SuccessBody code) <$> consume (brRead (responseBody response))
        else pure (Right (UnreadStatus code))

{- | Run a consumer over a source that hashes each chunk it passes on. A successful result carries
the digest of every chunk the consumer read.
-}
digestingRead :: (IO ByteString -> IO (Either e a)) -> IO ByteString -> IO (Either e (a, ContentDigest))
digestingRead consume readChunk = do
    context <- newIORef hashInit
    let next = do
            chunk <- readChunk
            modifyIORef' context (`hashUpdate` chunk)
            pure chunk
    consume next >>= traverse (\result -> (result,) . digestFromContext <$> readIORef context)
