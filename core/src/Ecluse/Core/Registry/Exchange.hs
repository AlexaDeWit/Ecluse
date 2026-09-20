-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Bounded registry exchanges and their transport-fault classification.
Read exchanges retain explicit access refusals before reading an error body.
-}
module Ecluse.Core.Registry.Exchange (
    -- * The bounded exchange
    boundedExchange,
    singleAttemptSettings,
    boundedFetch,
    boundedRelay,

    -- * Request formation
    formThen,
) where

import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Client (
    BodyReader,
    Manager,
    ManagerSettings (managerRetryableException),
    Request,
    Response (responseStatus),
    brRead,
    responseBody,
    withResponse,
 )
import Network.HTTP.Types.Status (statusCode)
import UnliftIO (try)

import Ecluse.Core.Fault.Http (classifyTransport)
import Ecluse.Core.Registry (
    FetchFault (FetchBoundExceeded, FetchTransport),
    PublishRelayResponse (..),
    RegistryResponse (RegistryResponse),
    UrlFormationError,
    isAuthorisationFailure,
 )
import Ecluse.Core.Security (BodyLimit, LimitError, boundedRead)

-- | Destructive clients must return uncertain transport failures for reassessment before retry.
singleAttemptSettings :: ManagerSettings -> ManagerSettings
singleAttemptSettings settings = settings{managerRetryableException = const False}

-- | Project status, decompressed byte count, and body. Transport failures retain their typed cause.
boundedExchange :: (Int -> Int -> ByteString -> a) -> Manager -> BodyLimit -> Request -> IO (Either FetchFault a)
boundedExchange project manager limits request =
    runExchange manager request (readBounded project limits)

runExchange :: Manager -> Request -> (Response BodyReader -> IO (Either LimitError a)) -> IO (Either FetchFault a)
runExchange manager request readResponse =
    try (withResponse request manager readResponse)
        <&> \case
            Left httpErr -> Left (FetchTransport (classifyTransport httpErr))
            Right (Left limitErr) -> Left (FetchBoundExceeded limitErr)
            Right (Right projected) -> Right projected

-- | Preserve explicit auth refusals without reading their untrusted error bodies.
boundedFetch :: Manager -> BodyLimit -> Request -> IO (Either FetchFault RegistryResponse)
boundedFetch manager limits request = runExchange manager request $ \response ->
    let code = statusCode (responseStatus response)
     in if isAuthorisationFailure code
            then pure (Right (RegistryResponse code 0 ""))
            else readBounded RegistryResponse limits response

-- | The exchange keeping the answered status alongside the body, for the first-party relay.
boundedRelay :: Manager -> BodyLimit -> Request -> IO (Either FetchFault PublishRelayResponse)
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
