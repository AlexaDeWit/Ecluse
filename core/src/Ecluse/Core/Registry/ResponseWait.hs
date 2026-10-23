-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Open an upstream response for a registry exchange, under http-client's own connect and response
timeouts. Every registry exchange opens its response here. Amazonka's S3 calls and the OSV and EPSS
fetches use their own clients.
-}
module Ecluse.Core.Registry.ResponseWait (
    openResponse,
    withResponse,
) where

import Network.HTTP.Client (BodyReader, Request, Response, responseOpen)
import Network.HTTP.Client qualified as HTTP

import Ecluse.Core.Registry.Exchange.Types (ExchangeClient (ecManager))

-- | 'responseOpen' over the client's pool. Close the response with 'Network.HTTP.Client.responseClose'.
openResponse :: ExchangeClient -> Request -> IO (Response BodyReader)
openResponse client request = responseOpen request (ecManager client)

-- | 'HTTP.withResponse' over the client's pool.
withResponse :: ExchangeClient -> Request -> (Response BodyReader -> IO a) -> IO a
withResponse client request = HTTP.withResponse request (ecManager client)
