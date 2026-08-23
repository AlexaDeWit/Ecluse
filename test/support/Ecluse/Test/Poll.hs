-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Bounded polling for the container-backed test tiers.

A spec that waits on a container, a queue, or an asynchronous log line needs a bound, so a
broken dependency fails the run instead of hanging it. Both helpers take that bound as an
attempt count and a delay, over a "Control.Retry" policy. Neither decides what an exhausted
budget means: 'pollUntil' hands back the last outcome it saw, and 'retryingIO' re-throws
the last exception.
-}
module Ecluse.Test.Poll (
    pollUntil,
    retryingIO,
) where

import Control.Retry (constantDelay, limitRetries, retrying)
import UnliftIO (MonadUnliftIO, throwIO, tryAny)

{- | Run @act@ up to @attempts@ times, @delayMicros@ apart, stopping at the first outcome
'accept' holds for. The final outcome comes back whether or not it was accepted.
-}
pollUntil :: (MonadIO m) => Int -> Int -> (a -> Bool) -> m a -> m a
pollUntil attempts delayMicros accept act =
    retrying
        (constantDelay delayMicros <> limitRetries (max 0 (attempts - 1)))
        (const (pure . not . accept))
        (const act)

{- | 'pollUntil' over an action that signals failure by throwing: the last attempt's
exception propagates. 'tryAny' leaves an async exception alone, so a cancel still travels.
-}
retryingIO :: (MonadUnliftIO m) => Int -> Int -> m a -> m a
retryingIO attempts delayMicros act =
    pollUntil attempts delayMicros isRight (tryAny act) >>= either throwIO pure
