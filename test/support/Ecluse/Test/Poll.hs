-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Bounded polling for the container-backed test tiers.

A spec that waits on a container, a queue, or an asynchronous log line needs a bound, so a
broken dependency fails the run instead of hanging it. 'pollUntil' and 'retryingIO' take that
bound as an attempt count and a delay, over a "Control.Retry" policy. 'awaitUntil' takes it as
wall clock instead, which is the budget a spec wants when the work it waits on runs in the
background and the poll itself costs nothing. The two bounds differ on a loaded runner, so a
caller picks the one its case was written against. None of the three decides what an exhausted
budget means: 'pollUntil' hands back the last outcome it saw, 'awaitUntil' answers 'False', and
'retryingIO' re-throws the last exception.
-}
module Ecluse.Test.Poll (
    pollUntil,
    awaitUntil,
    retryingIO,
) where

import Control.Retry (constantDelay, limitRetries, retrying)
import UnliftIO (MonadUnliftIO, throwIO, timeout, tryAny)
import UnliftIO.Concurrent (threadDelay)

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
