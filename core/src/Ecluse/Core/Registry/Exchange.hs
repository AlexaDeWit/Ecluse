-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The shared bounded registry exchanges: run one formed 'Request' and read its
response under a response-bound budget. Every failure folds into the typed channel at
this edge. Two shapes live here, one per whole-buffered response the proxy reads.
'boundedFetch' returns the body as a 'RegistryResponse'. 'boundedRelay' pairs the
bounded body with the status the target answered as a 'PublishRelayResponse'.

The npm read data plane ("Ecluse.Core.Registry.Npm") and the mirror-write transport
("Ecluse.Core.Registry.Publish") fetch through 'boundedFetch'. The first-party
publish relay ("Ecluse.Core.Registry.Npm") relays through 'boundedRelay'. Each
performs the identical fail-closed exchange: run the request, then read the body
chunk-by-chunk against the budget. A bound breach or a transport fault comes back as
a typed __value__, never an exception. Both live here once, so a hardening change to
the response bound touches one implementation, not copies that can drift.

Ecosystem-agnostic: this forms no request and speaks no registry protocol, only the
bounded read of a 'Request' the caller has already shaped. Request formation, and its
own typed fault, stays with the caller.
-}
module Ecluse.Core.Registry.Exchange (
    -- * Bounded response fetch
    boundedFetch,

    -- * Bounded publish relay
    boundedRelay,
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
    PublishRelayFault (RelayBoundExceeded, RelayTransport),
    PublishRelayResponse (..),
    RegistryResponse (RegistryResponse),
 )
import Ecluse.Core.Security (LimitError, Limits, boundedRead)

{- | Run a formed 'Request' over the manager and read its response body bounded against the
budget. The transport wrap covers the whole exchange, the bounded body read included, so a
connection lost mid-body is a typed 'FetchFault', never a half-read response.
-}
boundedFetch :: Manager -> Limits -> Request -> IO (Either FetchFault RegistryResponse)
boundedFetch manager limits request =
    try (withResponse request manager (readBoundedBody limits . responseBody))
        <&> \case
            Left httpErr -> Left (FetchTransport (classifyTransport httpErr))
            Right (Left limitErr) -> Left (FetchBoundExceeded limitErr)
            Right (Right response) -> Right response

{- | Run a formed publish 'Request' and buffer the target's response bounded against the budget.
The transport wrap covers the whole exchange, the bounded body read included. A connection lost
mid-body is therefore a typed 'PublishRelayFault', never a half-relayed response.
-}
boundedRelay :: Manager -> Limits -> Request -> IO (Either PublishRelayFault PublishRelayResponse)
boundedRelay manager limits request =
    try (withResponse request manager (readRelayResponse limits))
        <&> \case
            Left httpErr -> Left (RelayTransport (classifyTransport httpErr))
            Right relayed -> relayed

{- An overstep yields the 'LimitError' as a value, never a truncated body. -}
readBoundedBody :: Limits -> BodyReader -> IO (Either LimitError RegistryResponse)
readBoundedBody limits bodyReader =
    fmap RegistryResponse <$> boundedRead limits (brRead bodyReader)

readRelayResponse :: Limits -> Response BodyReader -> IO (Either PublishRelayFault PublishRelayResponse)
readRelayResponse limits response =
    readBoundedBody limits (responseBody response) <&> \case
        Left limitErr -> Left (RelayBoundExceeded limitErr)
        Right (RegistryResponse body) ->
            Right
                PublishRelayResponse
                    { relayStatus = statusCode (responseStatus response)
                    , relayBody = LBS.fromStrict body
                    }
