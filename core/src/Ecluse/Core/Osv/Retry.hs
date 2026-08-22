-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Backoff for Pilot's periodic osv.dev fetch.

Pilot pulls the npm advisory export from osv.dev on a schedule. That upstream can be
unreachable, throttle the caller, or return 5xx. A naive retry-immediately loop would
then hammer it from a single egress (NAT) address and invite an aggressive rate-limit
or an outright ban. A transient fetch failure therefore retries under a /truncated
exponential backoff with full jitter/. Each wait grows exponentially from a base
delay. A cap stops it running away, the "truncated" part. Randomising the wait across
the interval @[0, cap]@ keeps many Pilots from resynchronising onto the upstream at
once. A bound on the number of retries makes the loop terminate and hand control back
to the outer sync-interval loop rather than spinning.

Only /transient/ faults retry. A response that carries a status code decides on the
code: 5xx and the throttling 408 and 429 retry. Every other 'HttpException' folds into
the shared transport vocabulary ("Ecluse.Core.Fault"), and 'transportRetryable' decides
it. A clean 4xx is a permanent client-side error and a corrupt archive is a parse
fault. Retrying neither helps, so both fail fast.
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
    HttpException (HttpExceptionRequest),
    HttpExceptionContent (StatusCodeException),
    responseStatus,
 )
import Network.HTTP.Types.Status (statusCode)

import Ecluse.Core.Fault (TransportFault (tfCause), transportRetryable)
import Ecluse.Core.Fault.Http (classifyTransport)

{- | The shipped osv.dev fetch backoff: full jitter from a 1s base to a 60s ceiling, over
five retries (six attempts at most). The loop is finite, and the worst case waits under
two minutes before the fetch gives up to the outer sync loop.
-}
defaultOsvRetryPolicy :: (MonadIO m) => RetryPolicyM m
defaultOsvRetryPolicy = limitRetries 5 <> capDelay 60_000_000 (fullJitterBackoff 1_000_000)

{- | Is this HTTP status worth retrying? A 5xx may clear, and 408 and 429 are explicit
"back off and come back" signals. Every other code is permanent, so a retry cannot fix it.
-}
isRetryableStatusCode :: Int -> Bool
isRetryableStatusCode code = code >= 500 || code == 408 || code == 429

{- | Should a fetch that threw this 'HttpException' retry? A status-code failure asks
'isRetryableStatusCode'. Every other exception folds into the shared transport
vocabulary, where 'transportRetryable' owns the decision for every caller.
-}
isRetryableHttpException :: HttpException -> Bool
isRetryableHttpException = \case
    HttpExceptionRequest _ (StatusCodeException response _) ->
        isRetryableStatusCode (statusCode (responseStatus response))
    other -> transportRetryable (tfCause (classifyTransport other))

{- | Run an osv.dev fetch under a "Control.Retry" policy. A transient 'HttpException'
retries until the budget is spent, then the original exception is re-thrown to the
caller. Any other fault, a corrupt-archive parse error for example, propagates unretried.
-}
withOsvRetry :: (MonadMask m, KatipContext m) => RetryPolicyM m -> m a -> m a
withOsvRetry policy fetch =
    recovering policy [retryHandler] (const fetch)

-- Declining a permanent 'HttpException' makes 'recovering' re-throw it.
retryHandler :: (KatipContext m) => RetryStatus -> Handler m Bool
retryHandler status = Handler $ \e ->
    if isRetryableHttpException e
        then logFM WarningS (ls (transientMessage status e)) >> pure True
        else pure False

{- | The warning logged before a retry of a transient fetch failure. The attempt number is
1-based, because 'rsIterNumber' counts retries from zero.
-}
transientMessage :: RetryStatus -> HttpException -> String
transientMessage status err =
    "osv.dev fetch failed transiently on attempt "
        <> show (1 + rsIterNumber status)
        <> "; backing off before the next retry. Cause: "
        <> show err
