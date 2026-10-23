-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Deadline sweepers for tests: one running for a scope, as the composition root keeps one for a process.
module Ecluse.Test.Deadline (
    withSweeper,
    withExchangeClient,
    unsweptClient,
) where

import Network.HTTP.Client (Manager)
import UnliftIO.Async (withAsync)

import Ecluse.Core.Deadline (Deadlines, newDeadlines, sweepTick)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient (ExchangeClient))

-- | Sweep a fresh set of deadlines for the length of the action.
withSweeper :: (Deadlines -> IO a) -> IO a
withSweeper action = do
    deadlines <- newDeadlines
    withAsync (forever (sweepTick deadlines)) (\_ -> action deadlines)

-- | A client over the manager whose deadlines are swept for the length of the action.
withExchangeClient :: IO Manager -> (ExchangeClient -> IO a) -> IO a
withExchangeClient manager action = manager >>= \pool -> withSweeper (action . ExchangeClient pool)

{- | A client whose deadlines nothing sweeps, for a fixture builder with no scope whose cases never
wait a deadline out. A case that does takes 'withExchangeClient'.
-}
unsweptClient :: Manager -> IO ExchangeClient
unsweptClient manager = ExchangeClient manager <$> newDeadlines
