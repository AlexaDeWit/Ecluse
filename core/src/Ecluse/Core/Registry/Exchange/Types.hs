-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The handle every upstream exchange runs through, shared by the composition root that builds it and the exchanges that read it.
module Ecluse.Core.Registry.Exchange.Types (
    ExchangeClient (..),
) where

import Network.HTTP.Client (Manager)

import Ecluse.Core.Deadline (Deadlines)

{- | One connection pool and the process's deadline sweeper, built once at startup for each pool.
Every exchange needs both, so they travel together wherever an exchange starts.
-}
data ExchangeClient = ExchangeClient
    { ecManager :: Manager
    -- ^ The @http-client@ pool the exchange's connection comes from.
    , ecDeadlines :: Deadlines
    -- ^ The process's sweeper, which keeps the progress floor, the serve-path cap and the request timeout.
    }
