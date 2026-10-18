-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Process-local public authority cooldowns with one owned recovery request.
module Ecluse.Core.Registry.Backoff (
    BackoffPolicy,
    mkBackoffPolicy,
    PublicBackoff,
    newPublicBackoff,
    PublicAttempt,
    withPublicAttempt,
    PublicResponse (..),
    recordPublicResponse,
) where

import Data.Map.Strict qualified as Map
import Data.Time (UTCTime, diffUTCTime)
import UnliftIO.Exception (mask)

import Ecluse.Core.Breaker (Breaker, BreakerPhase (..))
import Ecluse.Core.Breaker qualified as Breaker
import Ecluse.Core.Fault (RetryAfter (RetryAfter))
import Ecluse.Core.InFlight (guardInFlight)
import Ecluse.Core.Security.Authority (HostPort)

-- | Caller-selected fallback bounds and the retry hint while a probe owns recovery.
data BackoffPolicy = BackoffPolicy
    { bpFloor :: RetryAfter
    , bpCap :: RetryAfter
    , bpProbeHint :: RetryAfter
    }
    deriving stock (Eq, Show)

-- | Require positive whole seconds and a fallback floor no greater than its cap.
mkBackoffPolicy :: RetryAfter -> RetryAfter -> RetryAfter -> Maybe BackoffPolicy
mkBackoffPolicy floorDelay cap probeHint = do
    guard (floorDelay > RetryAfter 0 && floorDelay <= cap && probeHint > RetryAfter 0)
    pure (BackoffPolicy floorDelay cap probeHint)

-- | Share one handle across all public readers. Only pass authorities approved by the public egress policy.
data PublicBackoff = PublicBackoff
    { pbPolicy :: BackoffPolicy
    , pbClock :: IO UTCTime
    , pbJitter :: RetryAfter -> RetryAfter -> IO RetryAfter
    , pbHosts :: TVar (Map HostPort HostState)
    }

data HostState = HostState
    { hsBreaker :: Breaker
    , hsCeiling :: RetryAfter
    }

-- | Supply a fresh clock and a jitter selector. Its output is clamped to the supplied fallback interval.
newPublicBackoff :: BackoffPolicy -> IO UTCTime -> (RetryAfter -> RetryAfter -> IO RetryAfter) -> IO PublicBackoff
newPublicBackoff policy clock jitter = PublicBackoff policy clock jitter <$> newTVarIO Map.empty

-- | An admitted request may settle once. Its enclosing callback owns its lifetime.
data PublicAttempt = PublicAttempt
    { paBackoff :: PublicBackoff
    , paAuthority :: HostPort
    , paTicket :: Breaker.Attempt
    , paFinished :: TVar Bool
    }

-- | Refuse a cooling authority without running the callback. Every exit releases an unsettled recovery probe.
withPublicAttempt :: PublicBackoff -> HostPort -> (PublicAttempt -> IO a) -> IO (Either RetryAfter a)
withPublicAttempt backoff authority action = mask $ \restore -> do
    now <- pbClock backoff
    admitted <- atomically (claim backoff authority now)
    case admitted of
        Left delay -> pure (Left delay)
        Right attempt -> Right <$> guardInFlight restore (const pass) (releaseAttempt attempt) (action attempt)

-- | The caller classifies status and parses any retry hint before reporting the response headers.
data PublicResponse
    = NonThrottling
    | -- | A supplied hint must already respect the caller's honoured-header cap. Missing hints use fallback.
      Throttled (Maybe RetryAfter)
    deriving stock (Eq, Show)

-- | Commit response headers before consuming the body. Repeated calls and calls after the callback ends leave state unchanged.
recordPublicResponse :: PublicAttempt -> PublicResponse -> IO ()
recordPublicResponse attempt = \case
    NonThrottling -> atomically (settle attempt (acceptResponse policy (paTicket attempt)))
    Throttled hint -> do
        delay <- responseDelay attempt hint
        now <- pbClock backoff
        atomically (settle attempt (throttleResponse policy (paTicket attempt) now delay))
  where
    backoff = paBackoff attempt
    policy = pbPolicy backoff

initialHost :: BackoffPolicy -> HostState
initialHost policy = HostState Breaker.initialBreaker (bpFloor policy)

claim :: PublicBackoff -> HostPort -> UTCTime -> STM (Either RetryAfter PublicAttempt)
claim backoff authority now = do
    hosts <- readTVar (pbHosts backoff)
    let state = Map.findWithDefault (initialHost (pbPolicy backoff)) authority hosts
        (ticket, breaker) = Breaker.admit now (hsBreaker state)
    case ticket of
        Nothing -> pure (Left (remainingDelay (pbPolicy backoff) now (hsBreaker state)))
        Just owned -> do
            writeTVar (pbHosts backoff) (Map.insert authority state{hsBreaker = breaker} hosts)
            finished <- newTVar False
            pure (Right (PublicAttempt backoff authority owned finished))

remainingDelay :: BackoffPolicy -> UTCTime -> Breaker -> RetryAfter
remainingDelay policy now breaker = case Breaker.breakerPhase breaker of
    Open until' -> RetryAfter (fromInteger (max 1 (min (toInteger (maxBound :: Int)) (ceiling (diffUTCTime until' now)))))
    _ -> bpProbeHint policy

releaseAttempt :: PublicAttempt -> IO ()
releaseAttempt attempt = atomically (settle attempt (\state -> state{hsBreaker = Breaker.release (paTicket attempt) (hsBreaker state)}))

settle :: PublicAttempt -> (HostState -> HostState) -> STM ()
settle attempt step = do
    finished <- readTVar (paFinished attempt)
    unless finished $ do
        modifyTVar' (pbHosts (paBackoff attempt)) (Map.adjust step (paAuthority attempt))
        writeTVar (paFinished attempt) True

acceptResponse :: BackoffPolicy -> Breaker.Attempt -> HostState -> HostState
acceptResponse policy ticket state = case Breaker.breakerPhase (hsBreaker state) of
    HalfOpen
        | recovered /= hsBreaker state -> state{hsBreaker = recovered, hsCeiling = bpFloor policy}
    _ -> state
  where
    recovered = Breaker.recordSuccess ticket (hsBreaker state)

responseDelay :: PublicAttempt -> Maybe RetryAfter -> IO RetryAfter
responseDelay _ (Just hint) | hint > RetryAfter 0 = pure hint
responseDelay attempt _ = do
    hosts <- readTVarIO (pbHosts backoff)
    let ceilingDelay = maybe (bpFloor policy) hsCeiling (Map.lookup (paAuthority attempt) hosts)
    selected <- pbJitter backoff (bpFloor policy) ceilingDelay
    pure (max (bpFloor policy) (min ceilingDelay selected))
  where
    backoff = paBackoff attempt
    policy = pbPolicy backoff

throttleResponse :: BackoffPolicy -> Breaker.Attempt -> UTCTime -> RetryAfter -> HostState -> HostState
throttleResponse policy ticket now (RetryAfter seconds) state
    | next == old = state
    | otherwise = state{hsBreaker = next, hsCeiling = nextCeiling}
  where
    old = hsBreaker state
    duration = case Breaker.breakerPhase old of
        Open until' -> max (fromIntegral seconds) (diffUTCTime until' now)
        _ -> fromIntegral seconds
    next = Breaker.recordFailure 1 duration now ticket old
    nextCeiling = case Breaker.breakerPhase old of
        Open{} -> hsCeiling state
        _ -> growFallback policy (hsCeiling state)

-- Integer arithmetic prevents caller-supplied bounds from overflowing before the cap applies.
growFallback :: BackoffPolicy -> RetryAfter -> RetryAfter
growFallback policy (RetryAfter current) = case bpCap policy of
    RetryAfter cap -> RetryAfter (fromInteger (min (toInteger cap) (2 * toInteger current)))
