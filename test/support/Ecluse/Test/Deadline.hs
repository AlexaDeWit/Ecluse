-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | A running deadline sweeper for a test, as the composition root keeps one for a process.
module Ecluse.Test.Deadline (
    withSweeper,
    withExchangeClient,
    newSweptDeadlines,
    newSweptClient,
) where

import Network.HTTP.Client (Manager)
import UnliftIO.Async (withAsync)
import UnliftIO.Concurrent (forkIO)

import Ecluse.Core.Deadline (Deadlines, newDeadlines, sweepTick)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient (ExchangeClient))

-- | Sweep a fresh set of deadlines for the length of the action.
withSweeper :: (Deadlines -> IO a) -> IO a
withSweeper action = do
    deadlines <- newDeadlines
    withAsync (forever (sweepTick deadlines)) (\_ -> action deadlines)

-- | A client over this manager whose deadlines are swept for the length of the action.
withExchangeClient :: Manager -> (ExchangeClient -> IO a) -> IO a
withExchangeClient manager action = withSweeper (action . ExchangeClient manager)

-- | Deadlines swept for the rest of the test process, for a fixture builder with no scope to hold a sweeper.
newSweptDeadlines :: IO Deadlines
newSweptDeadlines = do
    deadlines <- newDeadlines
    _ <- forkIO (forever (sweepTick deadlines))
    pure deadlines

-- | 'newSweptDeadlines' under a client over this manager.
newSweptClient :: Manager -> IO ExchangeClient
newSweptClient manager = ExchangeClient manager <$> newSweptDeadlines
