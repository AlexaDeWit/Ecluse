-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The handle every upstream exchange runs through, shared by the composition root that builds it and the exchanges that read it.
module Ecluse.Core.Registry.Exchange.Types (
    ExchangeClient (..),
    TraceExchange,
    untracedExchange,
) where

import Network.HTTP.Client (BodyReader, Manager, Request, Response)

import Ecluse.Core.Deadline (Deadlines)

{- | One connection pool, the process's deadline sweeper, and the tracing of each exchange, built
once at startup for each pool. Every exchange needs all three, so they travel together.
-}
data ExchangeClient = ExchangeClient
    { ecManager :: Manager
    -- ^ The @http-client@ pool the exchange's connection comes from.
    , ecDeadlines :: Deadlines
    -- ^ The process's sweeper: the progress floor, the serve-path cap, the request timeout and the response wait.
    , ecTrace :: TraceExchange
    -- ^ Runs once around each exchange, from the request to the response headers.
    }

-- | Open one response through the opener, handing it the request with any headers the tracing adds.
type TraceExchange = Request -> (Request -> IO (Response BodyReader)) -> IO (Response BodyReader)

-- | Open the response with no client span, for a store the Dredger reads or a process without telemetry.
untracedExchange :: TraceExchange
untracedExchange request open = open request
