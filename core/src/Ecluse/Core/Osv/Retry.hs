-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Backoff for Pilot's periodic advisory-source fetches.
Bounded exponential waits with full jitter limit repeated load during upstream failures.
"Ecluse.Core.Fault.Http" classifies HTTP failures shared with registry adapters.
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
import Ecluse.Core.Fault.Http (classifyTransport, isRetryableStatusCode)

-- | Full jitter from a 1s base to a 60s ceiling, with five retries (six attempts).
defaultOsvRetryPolicy :: (MonadIO m) => RetryPolicyM m
defaultOsvRetryPolicy = limitRetries 5 <> capDelay 60_000_000 (fullJitterBackoff 1_000_000)

-- | Classify status failures by HTTP code and other exceptions by transport cause.
isRetryableHttpException :: HttpException -> Bool
isRetryableHttpException = \case
    HttpExceptionRequest _ (StatusCodeException response _) ->
        isRetryableStatusCode (statusCode (responseStatus response))
    other -> transportRetryable (tfCause (classifyTransport other))

-- | Retry temporary HTTP failures within the policy budget. Other failures propagate immediately.
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
    "advisory-source fetch failed transiently on attempt "
        <> show (1 + rsIterNumber status)
        <> "; backing off before the next retry. Cause: "
        <> show err
