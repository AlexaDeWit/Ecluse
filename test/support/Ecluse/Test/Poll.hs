-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Bounded fixture polling. Callers must check the final observation after exhaustion.
module Ecluse.Test.Poll (
    pollUntil,
    awaitUntil,
    retryingIO,
    runUntil,
) where

import Control.Retry (constantDelay, limitRetries, retrying)
import UnliftIO (MonadUnliftIO, race, throwIO, timeout, tryAny)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

{- | Run @act@ up to @attempts@ times, @delayMicros@ apart, stopping at the first outcome
'accept' holds for. The final outcome comes back whether or not it was accepted.
-}
pollUntil :: (MonadIO m) => Int -> Int -> (a -> Bool) -> m a -> m a
pollUntil attempts delayMicros accept act =
    retrying
        (constantDelay delayMicros <> limitRetries (max 0 (attempts - 1)))
        (const (pure . not . accept))
        (const act)

{- | Run @check@ every @delayMicros@ until it holds or @budgetMicros@ of wall clock elapses.
'False' is the budget expiring, which the caller reports in its own words.
-}
awaitUntil :: (MonadUnliftIO m) => Int -> Int -> m Bool -> m Bool
awaitUntil budgetMicros delayMicros check = fromMaybe False <$> timeout budgetMicros loop
  where
    loop = check >>= \held -> if held then pure True else threadDelay delayMicros >> loop

{- | 'pollUntil' over an action that signals failure by throwing: the last attempt's
exception propagates. 'tryAny' leaves an async exception alone, so a cancel still travels.
-}
retryingIO :: (MonadUnliftIO m) => Int -> Int -> m a -> m a
retryingIO attempts delayMicros act =
    pollUntil attempts delayMicros isRight (tryAny act) >>= either throwIO pure

-- | Cancel the worker only after progress. Expiry and an early worker exit fail the fixture.
runUntil :: Int -> Int -> IO () -> IO Bool -> IO ()
runUntil budgetMicros delayMicros worker ready =
    race worker (awaitUntil budgetMicros delayMicros ready) >>= \case
        Right True -> pass
        Right False -> throwIO (TestContractEscape "worker progress timed out")
        Left () -> throwIO (TestContractEscape "worker stopped before progress")
