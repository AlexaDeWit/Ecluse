-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- |
Module      : Ecluse.Core.Osv.Retry

Backoff for Pilot's periodic osv.dev fetch.

Pilot pulls the npm advisory export from osv.dev on a schedule. When that upstream
is unreachable, throttling us, or returning 5xx, a naive retry-immediately loop
would hammer it from a single egress (NAT) address and invite an aggressive
rate-limit or an outright ban. So a transient fetch failure is retried under a
/truncated exponential backoff with full jitter/: each wait grows exponentially
from a base delay, is capped so it cannot run away (the "truncated" part), is
randomised across the interval @[0, cap]@ so many Pilots do not resynchronise onto
the upstream at once, and the number of retries is bounded so the loop always
terminates and hands control back to the outer sync-interval loop rather than
spinning.

Only /transient/ faults are retried: connection failures, timeouts, and 5xx (plus
the throttling 408 and 429) responses. A clean 4xx is a permanent client-side
error and a corrupt archive is a parse fault; retrying neither helps, so both fail
fast.
-}
module Ecluse.Core.Osv.Retry (
    -- * Policy
    defaultOsvRetryPolicy,

    -- * Classifying a fetch failure
    isRetryableHttpException,
    isRetryableStatusCode,

    -- * Running a fetch under the policy
    withOsvRetry,

    -- * Log lines
    transientMessage,
) where

import Control.Monad.Catch (Handler (Handler), MonadMask)
import Control.Retry (
    RetryPolicyM,
    RetryStatus (rsIterNumber),
    capDelay,
    fullJitterBackoff,
    limitRetries,
    recovering,
 )
import Katip (KatipContext, Severity (WarningS), logFM, ls)
import Network.HTTP.Client (
    HttpException (..),
    HttpExceptionContent (..),
    responseStatus,
 )
import Network.HTTP.Types.Status (statusCode)

{- | The shipped osv.dev fetch backoff: full-jitter exponential backoff, capped per
attempt and bounded in count. The knobs (microseconds, the unit "Control.Retry"
speaks) are a 1s base doubling to a 60s ceiling, over five retries (at most six
attempts). 'limitRetries' supplies the stop, the policy monoid short-circuits to
@Nothing@ once the budget is spent, so the loop is finite and the worst case adds
under two minutes of waiting before the fetch gives up to the outer sync loop.
Inspect the schedule without sleeping using 'Control.Retry.simulatePolicy'.
-}
defaultOsvRetryPolicy :: (MonadIO m) => RetryPolicyM m
defaultOsvRetryPolicy = limitRetries 5 <> capDelay 60_000_000 (fullJitterBackoff 1_000_000)

{- | Is this HTTP status worth retrying? A 5xx is a server-side fault that may
clear, and 408 (request timeout) and 429 (too many requests) are explicit "back
off and come back" signals. Every other code, in particular a 4xx that is not
408\/429, is a permanent client-side error a retry cannot fix.
-}
isRetryableStatusCode :: Int -> Bool
isRetryableStatusCode code = code >= 500 || code == 408 || code == 429

{- | Should a fetch that threw this 'HttpException' be retried? Connection failures
and timeouts are transient by nature; a status-code rejection defers to
'isRetryableStatusCode'; a malformed URL is a configuration fault no retry can
mend. Anything not positively known to be transient is treated as permanent, so
Pilot fails fast rather than hammering the upstream on a guess.
-}
isRetryableHttpException :: HttpException -> Bool
isRetryableHttpException = \case
    InvalidUrlException{} -> False
    HttpExceptionRequest _ content -> case content of
        StatusCodeException response _ -> isRetryableStatusCode (statusCode (responseStatus response))
        ConnectionFailure{} -> True
        ConnectionTimeout -> True
        ResponseTimeout -> True
        NoResponseDataReceived -> True
        ConnectionClosed -> True
        _ -> False

{- | Run an osv.dev fetch under a "Control.Retry" policy. A transient
'HttpException' (see 'isRetryableHttpException') is retried with backoff until it
either succeeds or the retry budget is spent; a permanent one is not retried.
'recovering' re-throws the original exception on exhaustion or when the handler
declines, so the caller's own handler (the export loop, which logs and then waits
the full sync interval) still sees it. A non-'HttpException' fault, for example a
corrupt-archive parse error, is not caught here and propagates unretried.
-}
withOsvRetry :: (MonadMask m, KatipContext m) => RetryPolicyM m -> m a -> m a
withOsvRetry policy fetch =
    recovering policy [retryHandler] (const fetch)

-- Log-and-retry a transient 'HttpException'; decline a permanent one so
-- 'recovering' re-throws it. It closes over nothing in 'withOsvRetry', so it is a
-- top-level binding rather than a 'where' helper (STYLE section 9.5).
retryHandler :: (KatipContext m) => RetryStatus -> Handler m Bool
retryHandler status = Handler $ \e ->
    if isRetryableHttpException e
        then logFM WarningS (ls (transientMessage status e)) >> pure True
        else pure False

{- | The warning logged before a transient fetch failure is retried. Reports the
1-based attempt number ('rsIterNumber' counts retries from zero) and the cause, so
an operator reading the logs can watch the backoff engage. It depends only on its
arguments, so it can be exercised in isolation.
-}
transientMessage :: RetryStatus -> HttpException -> String
transientMessage status err =
    "osv.dev fetch failed transiently on attempt "
        <> show (1 + rsIterNumber status)
        <> "; backing off before the next retry. Cause: "
        <> show err
