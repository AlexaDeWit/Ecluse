-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Process-local public authority cooldowns with one owned recovery request.
module Ecluse.Core.Registry.Backoff (
    reportingBackoff,
    CooldownEvent (..),
    publicRetryAfter,
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

data BackoffPolicy = BackoffPolicy
    { bpFloor :: RetryAfter
    , bpCap :: RetryAfter
    , bpProbeHint :: RetryAfter
    }
    deriving stock (Eq, Show)

fixedBackoffPolicy :: BackoffPolicy
fixedBackoffPolicy = BackoffPolicy (RetryAfter 60) (RetryAfter 600) (RetryAfter 1)

-- | A cooldown opens or a response restores the authority to healthy service.
data CooldownEvent = CoolingDown | Recovered
    deriving stock (Eq, Show)

-- | Attach a non-throwing observer. Notifications run after the state transaction commits.
reportingBackoff :: (HostPort -> CooldownEvent -> IO ()) -> PublicBackoff -> PublicBackoff
reportingBackoff report backoff = backoff{pbReport = report}

-- | Read the remaining refusal hint without acquiring a recovery probe.
publicRetryAfter :: PublicBackoff -> HostPort -> IO (Maybe RetryAfter)
publicRetryAfter backoff authority = do
    now <- pbClock backoff
    hosts <- readTVarIO (pbHosts backoff)
    pure $
        Map.lookup authority hosts >>= \hostState -> case Breaker.breakerPhase (hsBreaker hostState) of
            Closed{} -> Nothing
            RecoveryReady -> Nothing
            _ -> Just (remainingDelay (pbPolicy backoff) now (hsBreaker hostState))

-- | Share one handle across all public readers. Only pass authorities approved by the public egress policy.
data PublicBackoff = PublicBackoff
    { pbPolicy :: BackoffPolicy
    , pbClock :: IO UTCTime
    , pbJitter :: RetryAfter -> RetryAfter -> IO RetryAfter
    , pbHosts :: TVar (Map HostPort HostState)
    , pbReport :: HostPort -> CooldownEvent -> IO ()
    }

data HostState = HostState
    { hsBreaker :: Breaker
    , hsCeiling :: RetryAfter
    }

-- | Use the fixed public policy with a fresh clock and jitter selector, clamped to the current fallback interval.
newPublicBackoff :: IO UTCTime -> (RetryAfter -> RetryAfter -> IO RetryAfter) -> IO PublicBackoff
newPublicBackoff clock jitter = do
    hosts <- newTVarIO Map.empty
    pure (PublicBackoff fixedBackoffPolicy clock jitter hosts (\_ _ -> pass))

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
    NonThrottling -> commit (acceptResponse policy (paTicket attempt))
    Throttled hint -> do
        delay <- responseDelay attempt hint
        now <- pbClock backoff
        commit (throttleResponse policy (paTicket attempt) now delay)
  where
    backoff = paBackoff attempt
    policy = pbPolicy backoff
    commit step = atomically (settle attempt step) >>= traverse_ (pbReport backoff (paAuthority attempt))

initialHost :: BackoffPolicy -> HostState
initialHost policy = HostState Breaker.initialBreaker (bpFloor policy)

claim :: PublicBackoff -> HostPort -> UTCTime -> STM (Either RetryAfter PublicAttempt)
claim backoff authority now = do
    hosts <- readTVar (pbHosts backoff)
    let hostState = Map.findWithDefault (initialHost (pbPolicy backoff)) authority hosts
        (ticket, breaker) = Breaker.admit now (hsBreaker hostState)
    case ticket of
        Nothing -> pure (Left (remainingDelay (pbPolicy backoff) now (hsBreaker hostState)))
        Just owned -> do
            writeTVar (pbHosts backoff) (Map.insert authority hostState{hsBreaker = breaker} hosts)
            finished <- newTVar False
            pure (Right (PublicAttempt backoff authority owned finished))

remainingDelay :: BackoffPolicy -> UTCTime -> Breaker -> RetryAfter
remainingDelay policy now breaker = case Breaker.breakerPhase breaker of
    Open until' -> RetryAfter (fromInteger (max 1 (min (toInteger (maxBound :: Int)) (ceiling (diffUTCTime until' now)))))
    _ -> bpProbeHint policy

releaseAttempt :: PublicAttempt -> IO ()
releaseAttempt attempt = void (atomically (settle attempt (\hostState -> hostState{hsBreaker = Breaker.release (paTicket attempt) (hsBreaker hostState)})))

settle :: PublicAttempt -> (HostState -> HostState) -> STM (Maybe CooldownEvent)
settle attempt step = do
    finished <- readTVar (paFinished attempt)
    if finished
        then pure Nothing
        else do
            let hostsVar = pbHosts (paBackoff attempt)
            hosts <- readTVar hostsVar
            let old = Map.lookup (paAuthority attempt) hosts
                event = old >>= \hostState -> transition (hsBreaker hostState) (hsBreaker (step hostState))
            writeTVar hostsVar (Map.adjust step (paAuthority attempt) hosts)
            writeTVar (paFinished attempt) True
            pure event

acceptResponse :: BackoffPolicy -> Breaker.Attempt -> HostState -> HostState
acceptResponse policy ticket hostState = case Breaker.breakerPhase (hsBreaker hostState) of
    HalfOpen
        | recovered /= hsBreaker hostState -> hostState{hsBreaker = recovered, hsCeiling = bpFloor policy}
    _ -> hostState
  where
    recovered = Breaker.recordSuccess ticket (hsBreaker hostState)

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
throttleResponse policy ticket now (RetryAfter seconds) hostState
    | next == old = hostState
    | otherwise = hostState{hsBreaker = next, hsCeiling = nextCeiling}
  where
    old = hsBreaker hostState
    duration = case Breaker.breakerPhase old of
        Open until' -> max (fromIntegral seconds) (diffUTCTime until' now)
        _ -> fromIntegral seconds
    next = Breaker.recordFailure 1 duration now ticket old
    nextCeiling = case Breaker.breakerPhase old of
        Open{} -> hsCeiling hostState
        _ -> growFallback policy (hsCeiling hostState)

growFallback :: BackoffPolicy -> RetryAfter -> RetryAfter
growFallback policy (RetryAfter current) = case bpCap policy of
    RetryAfter cap -> RetryAfter (fromInteger (min (toInteger cap) (2 * toInteger current)))

transition :: Breaker -> Breaker -> Maybe CooldownEvent
transition old new = case (Breaker.breakerPhase old, Breaker.breakerPhase new) of
    (Open{}, Open{}) -> Nothing
    (_, Open{}) -> Just CoolingDown
    (HalfOpen, Closed{}) -> Just Recovered
    _ -> Nothing
