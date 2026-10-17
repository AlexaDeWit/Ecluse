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
import Network.HTTP.Client (HttpException)

import Ecluse.Core.Fault (HttpFault (HttpStatus, HttpTransport), TransportFault (tfCause), renderHttpFault, transportRetryable)
import Ecluse.Core.Fault.Http (httpFault, isRetryableStatusCode)

-- | Full jitter from a 1s base to a 60s ceiling, with five retries (six attempts).
defaultOsvRetryPolicy :: (MonadIO m) => RetryPolicyM m
defaultOsvRetryPolicy = limitRetries 5 <> capDelay 60_000_000 (fullJitterBackoff 1_000_000)

-- | Classify status failures by HTTP code and other exceptions by transport cause.
isRetryableHttpException :: HttpException -> Bool
isRetryableHttpException err = case httpFault err of
    HttpStatus code -> isRetryableStatusCode code
    HttpTransport fault -> transportRetryable (tfCause fault)

-- | Retry temporary HTTP failures within the policy budget, warning before each retry. Other failures propagate immediately.
withOsvRetry :: (MonadMask m, KatipContext m) => Text -> RetryPolicyM m -> m a -> m a
withOsvRetry feed policy fetch =
    recovering policy [retryHandler feed] (const fetch)

-- Declining a permanent 'HttpException' makes 'recovering' re-throw it.
retryHandler :: (KatipContext m) => Text -> RetryStatus -> Handler m Bool
retryHandler feed status = Handler $ \e ->
    if isRetryableHttpException e
        then logFM WarningS (ls (transientMessage feed status e)) >> pure True
        else pure False

-- | The warning before a retry, which names a status by the feed's @host:port@. The attempt number is 1-based, as 'rsIterNumber' counts from zero.
transientMessage :: Text -> RetryStatus -> HttpException -> Text
transientMessage feed status err =
    "advisory-source fetch failed transiently on attempt "
        <> show (1 + rsIterNumber status)
        <> ", backing off before the next retry: "
        <> renderHttpFault feed (httpFault err)
