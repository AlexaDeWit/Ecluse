-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Deadline sweepers for tests: one running for a scope, as the composition root keeps one for a process.
module Ecluse.Test.Deadline (
    withSweeper,
    withExchangeClient,
) where

import Network.HTTP.Client (Manager)
import UnliftIO.Async (link, withAsync)

import Ecluse.Core.Deadline (Deadlines, newDeadlines, sweepTick)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient (ExchangeClient))

-- | Sweep a fresh set of deadlines for the length of the action. A check that throws fails the action, as it fails a role.
withSweeper :: (Deadlines -> IO a) -> IO a
withSweeper action = do
    deadlines <- newDeadlines
    withAsync (forever (sweepTick deadlines)) (\sweeper -> link sweeper >> action deadlines)

-- | A client over the manager whose deadlines are swept for the length of the action.
withExchangeClient :: IO Manager -> (ExchangeClient -> IO a) -> IO a
withExchangeClient manager action = manager >>= \pool -> withSweeper (action . ExchangeClient pool)
