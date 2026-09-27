-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Bounded registry exchanges and their transport-fault classification.
Read exchanges retain explicit access refusals before reading an error body. Every exchange is
held to an 'ExchangeDeadline': a body read that waits out the idle interval, or an exchange that
outlives the cap, fails with the transport timeout the response-header timeout also raises.
-}
module Ecluse.Core.Registry.Exchange (
    -- * The bounded exchange
    boundedExchange,
    singleAttemptSettings,
    boundedFetch,
    boundedJsonFetch,
    withSuccessBody,
    boundedRelay,

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
    HttpException (HttpExceptionRequest),
    HttpExceptionContent (ResponseTimeout),
    Manager,
    ManagerSettings (managerRetryableException),
    Request,
    Response (responseStatus),
    brRead,
    responseBody,
    withResponse,
 )
import Network.HTTP.Types.Status (statusCode)
import UnliftIO (throwIO, try)
import UnliftIO.Timeout (timeout)

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
import Ecluse.Core.Security (
    BodyLimit,
    ExchangeDeadline,
    LimitError,
    boundedRead,
    deadlineExchangeMicros,
    deadlineIdleMicros,
 )
import Ecluse.Core.Snapshot (ContentDigest, digestFromContext)

-- | Destructive clients must return uncertain transport failures for reassessment before retry.
singleAttemptSettings :: ManagerSettings -> ManagerSettings
singleAttemptSettings settings = settings{managerRetryableException = const False}

-- | Project status, decompressed byte count, and body. Transport failures retain their typed cause.
boundedExchange :: (Int -> Int -> ByteString -> a) -> Manager -> ExchangeDeadline -> BodyLimit -> Request -> IO (Either FetchFault a)
boundedExchange project manager deadline limits request =
    runExchange manager deadline request (readBounded project limits)

runExchange :: Manager -> ExchangeDeadline -> Request -> (Response BodyReader -> IO (Either LimitError a)) -> IO (Either FetchFault a)
runExchange manager deadline request readResponse =
    timeout (deadlineExchangeMicros deadline) (try (withResponse request manager (readResponse . fmap idleBounded)))
        <&> \case
            Nothing -> Left (FetchTransport (classifyTransport timedOut))
            Just (Left httpErr) -> Left (FetchTransport (classifyTransport httpErr))
            Just (Right (Left limitErr)) -> Left (FetchBoundExceeded limitErr)
            Just (Right (Right projected)) -> Right projected
  where
    timedOut = HttpExceptionRequest request ResponseTimeout
    idleBounded = failSilentRead (deadlineIdleMicros deadline) timedOut

{- A body reader that throws once one read waits out the interval. It throws because a reader
fails on http-client's exception channel, the one a mid-body connection reset already takes. -}
failSilentRead :: Int -> HttpException -> BodyReader -> BodyReader
failSilentRead micros expired readChunk = timeout micros readChunk >>= maybe (throwIO expired) pure

-- | Preserve explicit auth refusals without reading their untrusted error bodies.
boundedFetch :: Manager -> ExchangeDeadline -> BodyLimit -> Request -> IO (Either FetchFault RegistryResponse)
boundedFetch manager deadline limits request = runExchange manager deadline request $ \response ->
    let code = statusCode (responseStatus response)
     in if isAuthorisationFailure code
            then pure (Right (RegistryResponse code 0 ""))
            else readBounded RegistryResponse limits response

-- | The exchange keeping the answered status alongside the body, for the first-party relay.
boundedRelay :: Manager -> ExchangeDeadline -> BodyLimit -> Request -> IO (Either FetchFault PublishRelayResponse)
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
boundedJsonFetch :: Manager -> ExchangeDeadline -> BodyLimit -> J.Parser a -> (s -> a -> Either LimitError s) -> s -> Request -> IO (Either FetchFault (BodyOutcome (StreamResult s)))
boundedJsonFetch manager deadline limits parser step initial = withSuccessBody manager deadline (readJsonStream limits parser step initial)

-- | Consume a 2xx body within the response lifetime. A status outside 2xx never reaches the consumer.
withSuccessBody :: Manager -> ExchangeDeadline -> (IO ByteString -> IO (Either LimitError a)) -> Request -> IO (Either FetchFault (BodyOutcome a))
withSuccessBody manager deadline consume request = runExchange manager deadline request $ \response -> do
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
