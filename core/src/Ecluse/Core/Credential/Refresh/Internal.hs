-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The implementation behind 'Ecluse.Core.Credential.Refresh'. This module exposes
the provider's innards that the curated public module keeps hidden, including the
'refreshingProviderWith' test hook. Importing it opts out of the module's stability
promises, the same convention @text@ and @bytestring@ use for their @.Internal@
modules. Production code imports 'Ecluse.Core.Credential.Refresh' instead. The public
module's header documents the policy itself.
-}
module Ecluse.Core.Credential.Refresh.Internal (
    -- * Configuration
    RefreshConfig (..),
    defaultRefreshConfig,

    -- * The refreshing provider
    refreshingProvider,
    refreshingProviderWith,

    -- * Telemetry reporters
    RefreshReporter (..),
    noRefreshReporter,
    CredentialReporters (..),
    noCredentialReporters,

    -- * Failure
    CredentialError (..),

    -- * State and pure\/transition helpers (exposed for direct testing)
    CacheState (..),
    ServeAction (..),
    decide,
    refreshDueAt,
    onMintSuccess,
    onMintFailure,
    releaseSingleFlight,
) where

import Control.Concurrent.STM (retry)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import UnliftIO (asyncWithUnmask, throwIO, try)
import UnliftIO.Exception (mask)

import Ecluse.Core.Breaker (
    Breaker,
    BreakerReporter,
    admit,
    initialBreaker,
    noBreakerReporter,
    recordFailure,
    recordSuccess,
    reportBreakerChange,
 )
import Ecluse.Core.Credential (AuthToken (..), CredentialProvider (..))
import Ecluse.Core.InFlight (guardInFlight)

{- | A failure the credential-refresh layer surfaces. 'BreakerOpen' can affect a client
serve only where a provider sits on the private-upstream read, never under the default
@passthrough@ strategy.
-}
data CredentialError
    = {- | The token has expired and the mint circuit breaker is open, so the
      provider does not attempt a mint. The caller must back off and retry later.
      -}
      BreakerOpen
    | {- | A caller used a 'RefreshConfig' built from 'defaultRefreshConfig'
      without supplying the named effectful leaf ('rcMint' or 'rcClock'). A wiring
      fault, not a runtime token condition.
      -}
      Unconfigured Text
    | {- | The minted token is already expired. This usually means severe clock
      skew between the local machine and the cloud provider, or a misconfigured
      backend. The policy treats it as a mint failure.
      -}
      MintedTokenAlreadyExpired
    deriving stock (Eq, Show)

instance Exception CredentialError

{- | An observer of a refresh attempt's outcome, so the refresh policy does not depend on
telemetry. A failed mint reports the still-cached token's remaining lifetime ('Nothing'
when the token has no expiry), so a sustained outage shows the gauge decaying.
-}
data RefreshReporter = RefreshReporter
    { onRefreshSucceeded :: Maybe Int -> IO ()
    -- ^ A mint succeeded, with the new token's remaining lifetime in whole seconds.
    , onRefreshFailed :: Maybe Int -> IO ()
    -- ^ A mint failed, with the still-cached token's remaining lifetime in whole seconds.
    }

-- | The inert refresh reporter: records nothing on either outcome.
noRefreshReporter :: RefreshReporter
noRefreshReporter = RefreshReporter (const pass) (const pass)

{- | The telemetry observers a refreshing provider records through, bundled so the
composition root passes one value to the provider constructors.
-}
data CredentialReporters = CredentialReporters
    { crBreakerReporter :: BreakerReporter
    -- ^ Observes the mint breaker's state transitions (@ecluse.rule.breaker.state@).
    , crRefreshReporter :: RefreshReporter
    -- ^ Observes each refresh outcome (@ecluse.credential.refresh@ \/ @.token.ttl@).
    }

-- | The inert pair: a provider built with it records nothing on either signal.
noCredentialReporters :: CredentialReporters
noCredentialReporters = CredentialReporters noBreakerReporter noRefreshReporter

{- | How a 'refreshingProvider' mints, times, and protects its token. The caller injects
the effectful leaves ('rcMint', 'rcClock', 'rcJitter') so the policy is deterministic
under test, and 'defaultRefreshConfig' supplies the rest.
-}
data RefreshConfig = RefreshConfig
    { rcMint :: IO AuthToken
    {- ^ The per-cloud token mint, the __only__ part that touches a network. A
    backend supplies just this leaf. Everything else is cloud-agnostic.
    -}
    , rcClock :: IO UTCTime
    {- ^ The clock the policy reads. Injected so a test can drive refresh timing
    without real time passing.
    -}
    , rcJitter :: IO Double
    {- ^ A jitter fraction in @[0, 1)@, sampled once per token, that pulls the refresh
    instant /earlier/. It desynchronises a cohort of instances.
    -}
    , rcRefreshAt :: Double
    {- ^ The fraction of a token's lifetime at which to refresh, before jitter
    (the ~80% point). Clamped into @[0, 1]@.
    -}
    , rcRefreshFloor :: NominalDiffTime
    {- ^ A hard floor: never schedule the refresh later than this many seconds before
    expiry. A short-lived token then still refreshes ahead of its deadline.
    -}
    , rcBreakerThreshold :: Int
    -- ^ Consecutive mint failures that trip the circuit breaker.
    , rcBreakerCooldown :: NominalDiffTime
    {- ^ How long the breaker stays open, fast-failing mints, before a single
    half-open probe tests recovery.
    -}
    , rcReporters :: CredentialReporters
    {- ^ The observers the breaker and the refresh policy report through. Inert by
    default ('noCredentialReporters'). The composition root installs the live pair.
    -}
    }

{- | Sensible defaults for the policy knobs. 'rcMint' and 'rcClock' default to leaves
that throw 'Unconfigured', so a provider built without wiring them fails loudly rather
than silently serving nothing.
-}
defaultRefreshConfig :: RefreshConfig
defaultRefreshConfig =
    RefreshConfig
        { rcMint = unconfigured "rcMint"
        , rcClock = unconfigured "rcClock"
        , rcJitter = pure 0
        , rcRefreshAt = 0.8
        , rcRefreshFloor = 30
        , rcBreakerThreshold = 5
        , rcBreakerCooldown = 60
        , rcReporters = noCredentialReporters
        }
  where
    unconfigured :: Text -> IO a
    unconfigured field = throwIO (Unconfigured field)

-- | The mutable state of a refreshing provider.
data CacheState = CacheState
    { csToken :: AuthToken
    -- ^ The token currently served.
    , csRefreshDue :: Maybe UTCTime
    {- ^ When a proactive background refresh should fire. 'Nothing' for a token
    with no expiry, which never refreshes.
    -}
    , csRefreshing :: Bool
    -- ^ Whether a mint is in flight (the single-flight flag).
    , csBreaker :: Breaker
    -- ^ The circuit-breaker state.
    }

{- | Build a 'CredentialProvider' that caches a token and refreshes it under the
'RefreshConfig' policy. It mints once eagerly, so a provider that cannot mint at all
fails here at construction rather than on the first request.
-}
refreshingProvider :: RefreshConfig -> IO CredentialProvider
refreshingProvider = refreshingProviderWith (pure ())

{- | As 'refreshingProvider', but with a hook the serving thread runs between the
single-flight claim and the mint runner, so a test can park a thread in that window and
cancel it there. Production passes @pure ()@ through 'refreshingProvider'.
-}
refreshingProviderWith :: IO () -> RefreshConfig -> IO CredentialProvider
refreshingProviderWith afterClaim cfg = do
    now <- rcClock cfg
    token <- rcMint cfg
    due <- refreshDueAt cfg now token
    stateVar <- newTVarIO (CacheState token due False initialBreaker)
    pure CredentialProvider{currentToken = serve afterClaim cfg stateVar}

{- An async exception between the single-flight claim and the run that releases it would
wedge every later expired caller on the 'decide' 'retry', so both stay in one masked scope. -}
serve :: IO () -> RefreshConfig -> TVar CacheState -> IO AuthToken
serve afterClaim cfg stateVar = mask $ \restore -> do
    now <- rcClock cfg
    action <- atomically (decide stateVar now)
    case action of
        ServeCached token -> pure token
        ServeAndRefresh token -> do
            -- Fire-and-forget: 'backgroundRefresh' catches its own failures, so the discarded
            -- 'Async' can never surface one. Forking is not interruptible, so the child that
            -- releases the single-flight flag is in place before this thread can be interrupted.
            _ <-
                asyncWithUnmask $ \unmask ->
                    guardInFlight unmask noWaiter (releaseSingleFlight stateVar) (afterClaim >> backgroundRefresh cfg stateVar)
            pure token
        MintNow ->
            -- The flag was claimed under 'mask'. 'guardInFlight' releases it on every
            -- exit and runs the synchronous mint under @restore@ so it stays cancellable.
            guardInFlight restore noWaiter (releaseSingleFlight stateVar) (afterClaim >> mintSynchronously cfg stateVar)
  where
    -- Waiters re-decide against the freed flag (the 'decide' STM 'retry'), not on a result
    -- promise, so the orphan hand-off has nothing to unblock.
    noWaiter :: SomeException -> IO ()
    noWaiter = const pass

{- | The single-flight decision over the current cache state. The flag claim happens
inside this transaction, so at most one mint is ever launched, and the caller that claims
it must release it (see 'releaseSingleFlight').
-}
decide :: TVar CacheState -> UTCTime -> STM ServeAction
decide stateVar now = do
    st <- readTVar stateVar
    if tokenValid now (csToken st)
        then
            if refreshNeeded now st && not (csRefreshing st)
                then do
                    writeTVar stateVar st{csRefreshing = True}
                    pure (ServeAndRefresh (csToken st))
                else pure (ServeCached (csToken st))
        else -- Expired. If a refresh is already in flight, wait for it (STM
        -- retry) rather than launching a second mint, then re-decide.
            if csRefreshing st
                then retry
                else do
                    writeTVar stateVar st{csRefreshing = True}
                    pure MintNow

-- | What a 'serve'\/'decide' decision resolves to.
data ServeAction
    = -- | The cached token is valid and no refresh is due: serve it.
      ServeCached AuthToken
    | -- | Valid but past the refresh threshold: serve it, refresh in background.
      ServeAndRefresh AuthToken
    | -- | Expired: the caller must mint synchronously (the slow path).
      MintNow
    deriving stock (Eq, Show)

-- What one gated mint attempt concluded, after its outcome is folded into the cache.
data MintOutcome
    = BreakerRefused
    | Minted AuthToken
    | MintedExpired
    | MintThrew SomeException

{- Gate one mint on the breaker, run it, re-clock it, and fold the outcome into the cache.
It never throws, so each caller decides for itself what an outcome surfaces as. -}
attemptMint :: RefreshConfig -> TVar CacheState -> IO MintOutcome
attemptMint cfg stateVar = do
    now <- rcClock cfg
    permitted <- gatedMint cfg stateVar now
    if permitted
        then do
            result <- try (rcMint cfg)
            now' <- rcClock cfg
            case result of
                Right token | tokenValid now' token -> do
                    recordMintSuccess cfg stateVar now' token
                    pure (Minted token)
                Right _ -> do
                    recordMintFailure cfg stateVar now'
                    pure MintedExpired
                Left (e :: SomeException) -> do
                    recordMintFailure cfg stateVar now'
                    pure (MintThrew e)
        else pure BreakerRefused

{- The background refresh: it discards the outcome, so a failed mint never reaches a caller.
'serve' wraps this run in the 'guardInFlight' that releases the single-flight flag. -}
backgroundRefresh :: RefreshConfig -> TVar CacheState -> IO ()
backgroundRefresh cfg stateVar = void (attemptMint cfg stateVar)

{- The synchronous (expired-token) path, the one case where a mint failure surfaces to the
caller. It rethrows the mint's own exception, so a caller can dispatch on the cause. -}
mintSynchronously :: RefreshConfig -> TVar CacheState -> IO AuthToken
mintSynchronously cfg stateVar =
    attemptMint cfg stateVar >>= \case
        Minted token -> pure token
        BreakerRefused -> throwIO BreakerOpen
        MintedExpired -> throwIO MintedTokenAlreadyExpired
        MintThrew e -> throwIO e

{- | Release the single-flight flag. 'serve' runs it under 'guardInFlight' inside the masked
scope that claimed the flag, so the flag clears on every exit, an async cancel included.
-}
releaseSingleFlight :: TVar CacheState -> IO ()
releaseSingleFlight stateVar =
    atomically (modifyTVar' stateVar (\st -> st{csRefreshing = False}))

{- Commit what 'Ecluse.Core.Breaker.admit' decides, returning the old and new states so
'gatedMint' can report the transition. -}
admitMintTxn :: TVar CacheState -> UTCTime -> STM (Bool, Breaker, Breaker)
admitMintTxn stateVar now = do
    st <- readTVar stateVar
    let old = csBreaker st
        (permitted, new) = admit now old
    writeTVar stateVar st{csBreaker = new}
    pure (permitted, old, new)

-- The admission gate plus its breaker-state report, which never blocks or throws.
gatedMint :: RefreshConfig -> TVar CacheState -> UTCTime -> IO Bool
gatedMint cfg stateVar now = do
    (permitted, old, new) <- atomically (admitMintTxn stateVar now)
    reportBreakerChange (crBreakerReporter (rcReporters cfg)) old new
    pure permitted

-- Report the breaker reset and the new token's remaining lifetime, after the cache fold.
recordMintSuccess :: RefreshConfig -> TVar CacheState -> UTCTime -> AuthToken -> IO ()
recordMintSuccess cfg stateVar now' token = do
    due <- refreshDueAt cfg now' token
    commitBreakerFold cfg stateVar (onMintSuccess token due)
    onRefreshSucceeded (crRefreshReporter (rcReporters cfg)) (ttlSecondsOf now' token)

-- Report any breaker trip and the still-cached token's lifetime, after the cache fold.
recordMintFailure :: RefreshConfig -> TVar CacheState -> UTCTime -> IO ()
recordMintFailure cfg stateVar now' = do
    cached <- csToken <$> readTVarIO stateVar
    commitBreakerFold cfg stateVar (onMintFailure cfg now')
    onRefreshFailed (crRefreshReporter (rcReporters cfg)) (ttlSecondsOf now' cached)

{- Reads the breaker before and after in one transaction, so the report reflects exactly
the transition it committed. -}
commitBreakerFold :: RefreshConfig -> TVar CacheState -> (CacheState -> CacheState) -> IO ()
commitBreakerFold cfg stateVar step = do
    (old, new) <- atomically $ do
        st <- readTVar stateVar
        let st' = step st
        writeTVar stateVar st'
        pure (csBreaker st, csBreaker st')
    reportBreakerChange (crBreakerReporter (rcReporters cfg)) old new

-- Remaining lifetime in whole seconds floored at zero, 'Nothing' for a token with no expiry.
ttlSecondsOf :: UTCTime -> AuthToken -> Maybe Int
ttlSecondsOf now token = case authExpiresAt token of
    Nothing -> Nothing
    Just expiry -> Just (max 0 (floor (diffUTCTime expiry now)))

{- | Fold a successful mint into the cache. 'guardInFlight' releases the single-flight
flag around the mint, not this fold, so the flag clears even on an async exception.
-}
onMintSuccess :: AuthToken -> Maybe UTCTime -> CacheState -> CacheState
onMintSuccess token due st =
    st
        { csToken = token
        , csRefreshDue = due
        , csBreaker = recordSuccess (csBreaker st)
        }

{- | Fold a failed mint into the cache. The cached token stays in place and the breaker
advances under the configured threshold and cooldown.
-}
onMintFailure :: RefreshConfig -> UTCTime -> CacheState -> CacheState
onMintFailure cfg now st =
    st{csBreaker = recordFailure (rcBreakerThreshold cfg) (rcBreakerCooldown cfg) now (csBreaker st)}

tokenValid :: UTCTime -> AuthToken -> Bool
tokenValid now token = case authExpiresAt token of
    Nothing -> True
    Just expiry -> now < expiry

refreshNeeded :: UTCTime -> CacheState -> Bool
refreshNeeded now st = case csRefreshDue st of
    Nothing -> False
    Just due -> now >= due

{- | When a freshly minted token's proactive refresh should fire. Jitter only pulls the
'rcRefreshAt' fraction of the token's lifetime earlier, never later.
-}
refreshDueAt :: RefreshConfig -> UTCTime -> AuthToken -> IO (Maybe UTCTime)
refreshDueAt cfg issuedAt token = case authExpiresAt token of
    Nothing -> pure Nothing
    Just expiry -> do
        jitter <- rcJitter cfg
        let lifetime = realToFrac (diffUTCTime expiry issuedAt) :: Double
            frac = clamp01 (rcRefreshAt cfg - clamp01 jitter)
            byFraction = addUTCTime (realToFrac (frac * lifetime)) issuedAt
            floorInstant = addUTCTime (negate (rcRefreshFloor cfg)) expiry
            -- Never later than the floor before expiry, never before issue.
            due = max issuedAt (min byFraction floorInstant)
        pure (Just due)
  where
    clamp01 :: Double -> Double
    clamp01 = max 0 . min 1
