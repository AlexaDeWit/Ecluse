-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The implementation behind "Ecluse.Core.Credential.Refresh", which documents the policy
and re-exports the curated surface. Importing this module opts out of that stability promise,
the convention @text@ and @bytestring@ use, so production code imports the public one.
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
import Data.Ord (clamp)
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

-- | A failure from credential minting or refresh policy.
data CredentialError
    = -- | The token expired with the mint breaker open, so no mint was attempted.
      BreakerOpen
    | -- | An effectful leaf still holds its 'defaultRefreshConfig' placeholder.
      Unconfigured Text
    | -- | An already-expired mint is treated as a mint failure.
      MintedTokenAlreadyExpired
    deriving stock (Eq, Show)

instance Exception CredentialError

-- | Observe refresh outcomes with the active token's absolute expiry, absent for non-expiring tokens.
data RefreshReporter = RefreshReporter
    { onRefreshSucceeded :: Maybe UTCTime -> IO ()
    -- ^ A mint succeeded, with the new token's absolute expiry.
    , onRefreshFailed :: Maybe UTCTime -> IO ()
    -- ^ A mint failed, with the still-cached token's absolute expiry.
    }

-- | The inert refresh reporter: records nothing on either outcome.
noRefreshReporter :: RefreshReporter
noRefreshReporter = RefreshReporter (const pass) (const pass)

-- | The telemetry observers a refreshing provider records through, bundled into one value.
data CredentialReporters = CredentialReporters
    { crBreakerReporter :: BreakerReporter
    -- ^ Observes the mint breaker's state transitions (@ecluse.rule.breaker.state@).
    , crRefreshReporter :: RefreshReporter
    -- ^ Observes each refresh outcome (@ecluse.credential.refresh@ \/ @.token.ttl@).
    }

-- | The inert pair: a provider built with it records nothing on either signal.
noCredentialReporters :: CredentialReporters
noCredentialReporters = CredentialReporters noBreakerReporter noRefreshReporter

-- | Refresh policy with injected mint, clock, jitter and observers.
data RefreshConfig = RefreshConfig
    { rcMint :: IO AuthToken
    -- ^ The per-cloud token mint, the __only__ part that touches a network.
    , rcClock :: IO UTCTime
    -- ^ Injected so a test drives refresh timing without real time passing.
    , rcJitter :: IO Double
    {- ^ A fraction in @[0, 1)@, sampled once per token, pulling the refresh instant
    /earlier/. It desynchronises a cohort of instances.
    -}
    , rcRefreshAt :: Double
    -- ^ The fraction of a token's lifetime to refresh at, before jitter. Clamped to @[0, 1]@.
    , rcRefreshFloor :: NominalDiffTime
    {- ^ Seconds before expiry the refresh may never be scheduled past, so a short-lived token
    still refreshes ahead of its deadline.
    -}
    , rcBreakerThreshold :: Int
    -- ^ Consecutive mint failures that trip the circuit breaker.
    , rcBreakerCooldown :: NominalDiffTime
    {- ^ How long the breaker stays open, fast-failing mints, before one half-open probe tests
    recovery.
    -}
    , rcReporters :: CredentialReporters
    -- ^ Inert by default. The composition root installs the live pair.
    }

-- | Default policy knobs. Unwired 'rcMint' and 'rcClock' throw 'Unconfigured'.
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
    -- ^ When the background refresh fires. 'Nothing' for a token with no expiry.
    , csRefreshing :: Bool
    -- ^ Whether a mint is in flight (the single-flight flag).
    , csBreaker :: Breaker
    -- ^ The circuit-breaker state.
    }

-- | Build a cached provider, minting eagerly so an initial mint failure aborts construction.
refreshingProvider :: RefreshConfig -> IO CredentialProvider
refreshingProvider = refreshingProviderWith (pure ())

-- | Add a test hook between the single-flight claim and the mint runner.
refreshingProviderWith :: IO () -> RefreshConfig -> IO CredentialProvider
refreshingProviderWith afterClaim cfg = do
    now <- rcClock cfg
    token <- rcMint cfg
    due <- refreshDueAt cfg now token
    stateVar <- newTVarIO (CacheState token due False initialBreaker)
    pure CredentialProvider{currentToken = serve afterClaim cfg stateVar}

-- | What a 'serve'\/'decide' decision resolves to.
data ServeAction
    = -- | The cached token is valid and no refresh is due: serve it.
      ServeCached AuthToken
    | -- | Valid but past the refresh threshold: serve it, refresh in background.
      ServeAndRefresh AuthToken
    | -- | Expired: the caller must mint synchronously (the slow path).
      MintNow
    deriving stock (Eq, Show)

{- An async exception between the single-flight claim and the run that releases it would
wedge every later expired caller on the 'decide' 'retry', so both stay in one masked scope. -}
serve :: IO () -> RefreshConfig -> TVar CacheState -> IO AuthToken
serve afterClaim cfg stateVar = mask $ \restore -> do
    now <- rcClock cfg
    atomically (decide stateVar now) >>= \case
        ServeCached token -> pure token
        ServeAndRefresh token -> token <$ forkRefresh afterClaim cfg stateVar
        MintNow ->
            -- The flag was claimed under 'mask'. 'guardInFlight' releases it on every
            -- exit and runs the synchronous mint under @restore@ so it stays cancellable.
            guardInFlight restore noWaiter (releaseSingleFlight stateVar) (afterClaim >> mintSynchronously cfg stateVar)

{- The masked fork installs the child's flag release before the parent can receive an
interruption, and 'backgroundRefresh' catches the mint's own failures. -}
forkRefresh :: IO () -> RefreshConfig -> TVar CacheState -> IO ()
forkRefresh afterClaim cfg stateVar =
    void $
        asyncWithUnmask $ \unmask ->
            guardInFlight unmask noWaiter (releaseSingleFlight stateVar) (afterClaim >> backgroundRefresh cfg stateVar)

-- Waiters re-decide against the freed flag (the 'decide' STM 'retry'), not on a result
-- promise, so the orphan hand-off has nothing to unblock.
noWaiter :: SomeException -> IO ()
noWaiter = const pass

{- | Claim a mint atomically when one is due, or block until an in-flight refresh frees the
flag. The caller must release a claim with 'releaseSingleFlight'.
-}
decide :: TVar CacheState -> UTCTime -> STM ServeAction
decide stateVar now = readTVar stateVar >>= decideFrom stateVar now

-- An expired token with a mint already in flight waits for it (the STM 'retry') and
-- re-decides, rather than launching a second one.
decideFrom :: TVar CacheState -> UTCTime -> CacheState -> STM ServeAction
decideFrom stateVar now st
    | not (tokenValid now (csToken st)) =
        if csRefreshing st then retry else claimSingleFlight stateVar st MintNow
    | refreshNeeded now st && not (csRefreshing st) =
        claimSingleFlight stateVar st (ServeAndRefresh (csToken st))
    | otherwise = pure (ServeCached (csToken st))

claimSingleFlight :: TVar CacheState -> CacheState -> ServeAction -> STM ServeAction
claimSingleFlight stateVar st action = action <$ writeTVar stateVar st{csRefreshing = True}

-- What one gated mint attempt concluded, after its outcome is folded into the cache.
data MintOutcome
    = BreakerRefused
    | Minted AuthToken
    | MintedExpired
    | MintThrew SomeException

-- It never throws, so each caller decides for itself what an outcome surfaces as.
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

-- It discards the outcome, so a failed background mint never reaches a caller.
backgroundRefresh :: RefreshConfig -> TVar CacheState -> IO ()
backgroundRefresh cfg stateVar = void (attemptMint cfg stateVar)

{- The one path where a mint failure surfaces to the caller. It rethrows the mint's own
exception, so a caller can dispatch on the cause. -}
mintSynchronously :: RefreshConfig -> TVar CacheState -> IO AuthToken
mintSynchronously cfg stateVar =
    attemptMint cfg stateVar >>= \case
        Minted token -> pure token
        BreakerRefused -> throwIO BreakerOpen
        MintedExpired -> throwIO MintedTokenAlreadyExpired
        MintThrew e -> throwIO e

{- | Release the single-flight flag. 'serve' runs it under 'guardInFlight' inside the masked
scope that claimed it, so the flag clears on every exit, an async cancel included.
-}
releaseSingleFlight :: TVar CacheState -> IO ()
releaseSingleFlight stateVar =
    atomically (modifyTVar' stateVar (\st -> st{csRefreshing = False}))

-- Returns the old and new breaker states so 'gatedMint' can report the transition.
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

-- Report the breaker reset and the new token's expiry, after the cache fold.
recordMintSuccess :: RefreshConfig -> TVar CacheState -> UTCTime -> AuthToken -> IO ()
recordMintSuccess cfg stateVar now' token = do
    due <- refreshDueAt cfg now' token
    commitBreakerFold cfg stateVar (onMintSuccess token due)
    onRefreshSucceeded (crRefreshReporter (rcReporters cfg)) (authExpiresAt token)

-- Report any breaker trip and the still-cached token's expiry, after the cache fold.
recordMintFailure :: RefreshConfig -> TVar CacheState -> UTCTime -> IO ()
recordMintFailure cfg stateVar now' = do
    cached <- csToken <$> readTVarIO stateVar
    commitBreakerFold cfg stateVar (onMintFailure cfg now')
    onRefreshFailed (crRefreshReporter (rcReporters cfg)) (authExpiresAt cached)

-- One transaction reads the breaker before and after, so the report reflects exactly the
-- transition it committed.
commitBreakerFold :: RefreshConfig -> TVar CacheState -> (CacheState -> CacheState) -> IO ()
commitBreakerFold cfg stateVar step = do
    (old, new) <- atomically $ do
        st <- readTVar stateVar
        let st' = step st
        writeTVar stateVar st'
        pure (csBreaker st, csBreaker st')
    reportBreakerChange (crBreakerReporter (rcReporters cfg)) old new

{- | Fold a successful mint into the cache. 'guardInFlight' releases the single-flight flag
around the mint, not this fold, so the flag clears even on an async exception.
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

{- | When a freshly minted token's refresh should fire. Jitter only pulls the 'rcRefreshAt'
fraction of the token's lifetime earlier, never later.
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
    clamp01 = clamp (0, 1)
