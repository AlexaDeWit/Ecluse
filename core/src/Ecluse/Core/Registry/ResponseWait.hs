-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Open an upstream response under the sweeper's deadlines rather than http-client's own timers.
A connection gets 30 seconds, and the status line and headers get what the connection left of
them, counted from the end of the request body. That is http-client's default response timeout,
with the same 'ConnectionTimeout' and 'ResponseTimeout' faults, about one sweep late. Every
registry exchange opens its response here. Amazonka's S3 calls and the OSV and EPSS fetches keep
their own clients' timers.
-}
module Ecluse.Core.Registry.ResponseWait (
    openResponse,
    withResponse,
) where

import Control.Exception qualified as Exception
import Network.HTTP.Client (BodyReader, Request, Response, responseClose)

import Ecluse.Core.Registry.Exchange.Types (ExchangeClient (ecTrace))
import Ecluse.Core.Registry.ResponseWait.Internal (openResponseAfter)

-- | 'Network.HTTP.Client.responseOpen' under the response wait. Close the response with 'responseClose'.
openResponse :: ExchangeClient -> Request -> IO (Response BodyReader)
openResponse client request = ecTrace client request (openResponseAfter 30_000_000_000 client)

-- | 'Network.HTTP.Client.withResponse' under the response wait.
withResponse :: ExchangeClient -> Request -> (Response BodyReader -> IO a) -> IO a
-- Base 'Exception.bracket', as http-client's own uses, so closing a connection stays interruptible.
withResponse client request = Exception.bracket (openResponse client request) responseClose
