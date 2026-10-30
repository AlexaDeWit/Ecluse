-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Public-read admission, HTTP attempt observation, and response-driven cooldown policy.
module Ecluse.Core.Registry.Public (
    PublicControl,
    newPublicControl,
    publicControlWith,
    ReadControl (..),
    withReadControl,
    classifyPublicResponse,
) where

import Control.Retry (RetryPolicyM (getRetryPolicyM), defaultRetryStatus, fullJitterBackoff)
import Data.List (lookup)
import Data.Time (UTCTime, getCurrentTime)
import Network.HTTP.Client (Request (host, port))
import Network.HTTP.Types (ResponseHeaders, Status, statusCode)

import Ecluse.Core.Fault (RetryAfter (RetryAfter))
import Ecluse.Core.Fault.Http (hostPortLabel)
import Ecluse.Core.Fault.RetryAfter (parseRetryAfter, upstreamRetryCap)
import Ecluse.Core.Registry (FetchFault (FetchCooldown, FetchUrlUnformable), UrlFormationError (UnparseableUrl))
import Ecluse.Core.Registry.Backoff (CooldownEvent, PublicBackoff, PublicResponse (..), newPublicBackoff, publicRetryAfter, recordPublicResponse, reportingBackoff, withPublicAttempt)
import Ecluse.Core.Registry.Exchange.Observe (observeExchange)
import Ecluse.Core.Security.Authority (HostPort (hpHost), hostPortAddress)
import Ecluse.Core.Telemetry.Upstream (ExchangeOutcome, PublicReadKind)

-- | One process shares the cooldown handle across its proxy and worker reads.
data PublicControl = PublicControl
    { pcBackoff :: PublicBackoff
    , pcClock :: IO UTCTime
    , pcRecord :: Text -> PublicReadKind -> ExchangeOutcome -> IO ()
    }

-- | Create the fixed public policy with full jitter above its positive fallback floor.
newPublicControl :: (Text -> PublicReadKind -> ExchangeOutcome -> IO ()) -> (HostPort -> CooldownEvent -> IO ()) -> IO PublicControl
newPublicControl record report = do
    backoff <- newPublicBackoff getCurrentTime jitter
    pure (publicControlWith (reportingBackoff report backoff) getCurrentTime record)
  where
    jitter (RetryAfter floorDelay) (RetryAfter ceilingDelay) = do
        sample <- getRetryPolicyM (fullJitterBackoff (ceilingDelay - floorDelay)) defaultRetryStatus
        pure (RetryAfter (floorDelay + fromMaybe 0 sample))

-- | Supply the same clock used by an existing cooldown handle, with a non-throwing recorder.
publicControlWith :: PublicBackoff -> IO UTCTime -> (Text -> PublicReadKind -> ExchangeOutcome -> IO ()) -> PublicControl
publicControlWith = PublicControl

-- | Private and write exchanges bypass public admission and the public request counter.
data ReadControl = Uncontrolled | Controlled PublicControl PublicReadKind

-- | Observe actual HTTP work after admission. The header callback records status before applying cooldown policy.
withReadControl :: ReadControl -> Request -> (Request -> (Status -> ResponseHeaders -> IO (Maybe RetryAfter)) -> IO a) -> IO (Either FetchFault a)
withReadControl Uncontrolled request action = Right <$> action request (\_ _ -> pure Nothing)
withReadControl (Controlled control kind) request action = case hostPortAddress authority of
    Nothing -> pure (Left (FetchUrlUnformable (UnparseableUrl authority)))
    Just target -> fmap (first FetchCooldown) $ withPublicAttempt (pcBackoff control) target $ \attempt ->
        observeExchange (pcRecord control (hpHost target) kind) request $ \observed recordHeaders ->
            action observed $ \status headers -> do
                recordHeaders status
                now <- pcClock control
                let response = classifyPublicResponse now status headers
                recordPublicResponse attempt response
                case response of
                    NonThrottling -> pure Nothing
                    Throttled _ -> publicRetryAfter (pcBackoff control) target
  where
    authority = hostPortLabel (decodeUtf8 (host request)) (port request)

-- | Every 429 cools down. A 503 does so only with a valid positive hint, capped at ten minutes.
classifyPublicResponse :: UTCTime -> Status -> ResponseHeaders -> PublicResponse
classifyPublicResponse now status headers =
    case (statusCode status, parseRetryAfter now upstreamRetryCap (lookup "Retry-After" headers)) of
        (429, hint) -> Throttled hint
        (503, Just hint) -> Throttled (Just hint)
        _ -> NonThrottling
