-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Observe HTTP attempts without taking ownership of their responses.
Callers admit local work first and report headers before consuming the body.
Attempt notification precedes header writes and does not prove remote receipt.
Connection failures can occur before that notification.
The injected recorder must not throw.
-}
module Ecluse.Core.Registry.Exchange.Observe (
    ExchangeOutcome (..),
    observeExchange,
) where

import Control.Exception qualified as Exception
import Network.HTTP.Client (HttpException, Request (requestBody), RequestBody (RequestBodyIO))
import Network.HTTP.Types (Status, statusCode)

import Ecluse.Core.Registry.Progress (isProgressFloorFailure)
import Ecluse.Core.Telemetry.Upstream (ExchangeOutcome (..))

data Attempt = Unstarted | Pending | Answered
    deriving stock (Eq)

-- | Keep response ownership in the continuation. Report its headers before reading its body.
observeExchange :: (ExchangeOutcome -> IO ()) -> Request -> (Request -> (Status -> IO ()) -> IO a) -> IO a
observeExchange record request action = Exception.mask $ \restore -> do
    attemptPhase <- newIORef Unstarted
    let settle outcome = Exception.mask_ $ do
            previous <- atomicModifyIORef' attemptPhase (Answered,)
            unless (previous == Answered) (record outcome)
        starting = Exception.mask_ $ do
            previous <- atomicModifyIORef' attemptPhase (Pending,)
            when (previous == Pending) (record TransportFailure)
        observed = request{requestBody = RequestBodyIO (starting $> requestBody request)}
        headers = settle . ResponseStatus . statusCode
        failed (fault :: Exception.SomeException) = do
            current <- readIORef attemptPhase
            case (Exception.fromException fault :: Maybe Exception.SomeAsyncException, Exception.fromException fault :: Maybe HttpException) of
                (Just _, _) -> when (current == Pending) (settle Cancelled)
                (_, Just _) -> settle TransportFailure
                _ | isProgressFloorFailure fault -> settle TransportFailure
                _ -> pass
            Exception.throwIO fault
    outcome <- Exception.try (restore (action observed headers))
    either failed pure outcome
