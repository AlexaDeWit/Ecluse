-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The shared bounded registry exchange: run one formed request and read its response
under a response-bound budget. One implementation serves every whole-buffered exchange the
proxy runs, so callers differ only in what they project out of the answered status and the
bounded body. Every failure, a bound breach and a transport fault included, comes back as a
typed 'FetchFault' __value__, and this is the one place a registry path classifies an
@http-client@ exception. 'formThen' folds a caller's own 'UrlFormationError' into that same
channel. Ecosystem-agnostic: this forms no request and speaks no registry protocol.
-}
module Ecluse.Core.Registry.Exchange (
    -- * The bounded exchange
    boundedExchange,
    boundedFetch,
    boundedRelay,

    -- * Request formation
    formThen,
) where

import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Client (
    BodyReader,
    Manager,
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
 )
import Ecluse.Core.Security (LimitError, Limits, boundedRead)

{- | Run a formed request and project its status and bounded body onto the caller's result.
The transport wrap covers the body read, so a connection lost mid-body is a typed fault.
-}
boundedExchange :: (Int -> ByteString -> a) -> Manager -> Limits -> Request -> IO (Either FetchFault a)
boundedExchange project manager limits request =
    try (withResponse request manager (readBounded project limits))
        <&> \case
            Left httpErr -> Left (FetchTransport (classifyTransport httpErr))
            Right (Left limitErr) -> Left (FetchBoundExceeded limitErr)
            Right (Right projected) -> Right projected

-- | The exchange keeping the body alone, as a 'RegistryResponse'.
boundedFetch :: Manager -> Limits -> Request -> IO (Either FetchFault RegistryResponse)
boundedFetch = boundedExchange (\_status body -> RegistryResponse body)

-- | The exchange keeping the answered status alongside the body, for the first-party relay.
boundedRelay :: Manager -> Limits -> Request -> IO (Either FetchFault PublishRelayResponse)
boundedRelay =
    boundedExchange $ \status body ->
        PublishRelayResponse{relayStatus = status, relayBody = LBS.fromStrict body}

{- | Run an exchange over a formed request, or fold the formation failure into the same
channel, so formation and exchange reach the caller as one 'Either'.
-}
formThen ::
    (UrlFormationError -> fault) ->
    (Request -> IO (Either fault a)) ->
    Either UrlFormationError Request ->
    IO (Either fault a)
formThen unformable = either (pure . Left . unformable)

{- An overstep yields the 'LimitError' as a value, never a truncated body. -}
readBounded :: (Int -> ByteString -> a) -> Limits -> Response BodyReader -> IO (Either LimitError a)
readBounded project limits response =
    fmap (project (statusCode (responseStatus response)))
        <$> boundedRead limits (brRead (responseBody response))
