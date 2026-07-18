-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The shared bounded-fetch exchange: run one formed 'Request' and read its
response body under a response-bound budget, every failure folded into the typed
channel at this edge.

The npm read data plane ("Ecluse.Core.Registry.Npm") and the mirror-write
transport ("Ecluse.Core.Registry.Publish") both perform the identical fail-closed
exchange: run the request, read the body chunk-by-chunk against the budget, and
report a bound breach or a transport fault as a 'FetchFault' __value__ rather than
an exception. It lives here once so a hardening change to the response bound
touches one implementation, not two copies that can drift.

Ecosystem-agnostic: this forms no request and speaks no registry protocol, only
the bounded read of a 'Request' the caller has already shaped. Request formation,
and its own typed fault, stays with the caller.
-}
module Ecluse.Core.Registry.Exchange (
    -- * Bounded response fetch
    boundedFetch,
    readBoundedBody,
) where

import Network.HTTP.Client (
    BodyReader,
    Manager,
    Request,
    brRead,
    responseBody,
    withResponse,
 )
import UnliftIO (try)

import Ecluse.Core.Fault.Http (classifyTransport)
import Ecluse.Core.Registry (
    FetchFault (FetchBoundExceeded, FetchTransport),
    RegistryResponse (RegistryResponse),
 )
import Ecluse.Core.Security (LimitError, Limits, boundedRead)

{- | Run a formed 'Request' over the manager and read its response body bounded
against the budget, folding every failure into the typed 'FetchFault' channel: a
thrown transport exception through 'classifyTransport' as 'FetchTransport', an
over-cap body as 'FetchBoundExceeded'. The transport wrap covers the __whole__
exchange, the bounded body read included, so a connection lost mid-body is a
pre-commit fault with a value representation, never a half-read response.
-}
boundedFetch :: Manager -> Limits -> Request -> IO (Either FetchFault RegistryResponse)
boundedFetch manager limits request =
    try (withResponse request manager (readBoundedBody limits . responseBody))
        <&> \case
            Left httpErr -> Left (FetchTransport (classifyTransport httpErr))
            Right (Left limitErr) -> Left (FetchBoundExceeded limitErr)
            Right (Right response) -> Right response

{- | Read a response body chunk-by-chunk through 'boundedRead' against the budget,
returning the whole body as a 'RegistryResponse' when within the cap, or the
'LimitError' as a __value__ when the body crosses the budget (never a truncated
body). Returning the breach lets a caller thread it as a value; a consumer behind
an exception-shaped boundary wraps it there.
-}
readBoundedBody :: Limits -> BodyReader -> IO (Either LimitError RegistryResponse)
readBoundedBody limits bodyReader =
    fmap RegistryResponse <$> boundedRead limits (brRead bodyReader)
