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

-- | The raw outcome of an attempt, independent of metric attributes and origin scope.
data ExchangeOutcome
    = ResponseStatus Int
    | TransportFailure
    | Cancelled
    deriving stock (Eq, Show)

data Attempt = Unstarted | Pending | Answered
    deriving stock (Eq)

-- | Keep response ownership in the continuation. Report its headers before reading its body.
observeExchange :: (ExchangeOutcome -> IO ()) -> Request -> (Request -> (Status -> IO ()) -> IO a) -> IO a
observeExchange record request action = Exception.mask $ \restore -> do
    state <- newIORef Unstarted
    let settle outcome = Exception.mask_ $ do
            previous <- atomicModifyIORef' state (Answered,)
            unless (previous == Answered) (record outcome)
        starting = Exception.mask_ $ do
            previous <- atomicModifyIORef' state (Pending,)
            when (previous == Pending) (record TransportFailure)
        observed = request{requestBody = RequestBodyIO (starting $> requestBody request)}
        headers = settle . ResponseStatus . statusCode
        failed (fault :: Exception.SomeException) = do
            current <- readIORef state
            case (Exception.fromException fault :: Maybe Exception.SomeAsyncException, Exception.fromException fault :: Maybe HttpException) of
                (Just _, _) -> when (current == Pending) (settle Cancelled)
                (_, Just _) -> settle TransportFailure
                _ -> pass
            Exception.throwIO fault
    outcome <- Exception.try (restore (action observed headers))
    either failed pure outcome
