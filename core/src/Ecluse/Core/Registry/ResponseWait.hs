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

import Control.Exception qualified as Exception
import Network.HTTP.Client (BodyReader, Request, Response, responseClose, responseOpen)

import Ecluse.Core.Registry.Exchange.Types (ExchangeClient (ecManager, ecTrace))

-- | 'responseOpen' over the client's pool, under its tracing. Close the response with 'responseClose'.
openResponse :: ExchangeClient -> Request -> IO (Response BodyReader)
openResponse client request = ecTrace client request (`responseOpen` ecManager client)

-- | 'Network.HTTP.Client.withResponse' over the client's pool, under its tracing.
withResponse :: ExchangeClient -> Request -> (Response BodyReader -> IO a) -> IO a
-- Base 'Exception.bracket', as http-client's own uses, so closing a connection stays interruptible.
withResponse client request = Exception.bracket (openResponse client request) responseClose
