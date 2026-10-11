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
    MintClaim,
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
import UnliftIO.Exception (mask, onException)

import Ecluse.Core.Breaker (
    Attempt,
    Breaker,
    BreakerPhase (HalfOpen),
    BreakerReporter,
    admit,
    breakerPhase,
    initialBreaker,
    noBreakerReporter,
    recordFailure,
    recordSuccess,
    release,
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
    -- ^ A fraction in @[0, 1)@, sampled per token to advance refresh.
    , rcRefreshAt :: Double
    -- ^ The fraction of a token's lifetime to refresh at, before jitter. Clamped to @[0, 1]@.
    , rcRefreshFloor :: NominalDiffTime
    -- ^ Seconds before expiry that bound the latest refresh instant.
    , rcBreakerThreshold :: Int
    -- ^ Consecutive mint failures that trip the circuit breaker.
    , rcBreakerCooldown :: NominalDiffTime
    -- ^ Cooldown in seconds before one owned recovery mint.
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
    , csMintClaim :: Maybe MintClaim
    -- ^ The current mint owner, absent when idle.
    , csClaimCounter :: Integer
    -- ^ Retained across cleanup to fence older mint owners.
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
    stateVar <- newTVarIO (CacheState token due Nothing 0 initialBreaker)
    pure CredentialProvider{currentToken = serve afterClaim cfg stateVar}

-- | A mint identity and its atomically admitted breaker ticket.
data MintClaim = MintClaim Integer Attempt Breaker Breaker
    deriving stock (Eq, Show)

-- | Whether a caller serves the cache, owns a mint, or receives a breaker refusal.
data ServeAction
    = -- | Serve a valid cached token.
      ServeCached AuthToken
    | -- | Serve the token and transfer the claim to a background worker.
      ServeAndRefresh AuthToken MintClaim
    | -- | Run the claimed mint synchronously for an expired token.
      MintNow MintClaim
    | -- | Refuse an expired caller during cooldown or owned recovery.
      RefuseMint
    deriving stock (Eq, Show)

serve :: IO () -> RefreshConfig -> TVar CacheState -> IO AuthToken
serve afterClaim cfg stateVar = mask $ \restore -> do
    now <- rcClock cfg
    atomically (decide stateVar now) >>= \case
        ServeCached token -> pure token
        ServeAndRefresh token claim -> token <$ forkRefresh afterClaim cfg stateVar claim
        MintNow claim ->
            guardInFlight restore (const pass) (releaseSingleFlight stateVar claim) $
                startMint afterClaim cfg claim >> mintSynchronously cfg stateVar claim
        RefuseMint -> throwIO BreakerOpen

forkRefresh :: IO () -> RefreshConfig -> TVar CacheState -> MintClaim -> IO ()
forkRefresh afterClaim cfg stateVar claim =
    void
        ( asyncWithUnmask $ \unmask ->
            guardInFlight unmask (const pass) (releaseSingleFlight stateVar claim) $
                startMint afterClaim cfg claim >> void (attemptMint cfg stateVar claim)
        )
        `onException` releaseSingleFlight stateVar claim

startMint :: IO () -> RefreshConfig -> MintClaim -> IO ()
startMint afterClaim cfg (MintClaim _ _ old new) = do
    afterClaim
    reportBreakerChange (crBreakerReporter (rcReporters cfg)) old new

-- | Claim mint and breaker ownership together. Healthy expired followers wait, recovery followers refuse.
decide :: TVar CacheState -> UTCTime -> STM ServeAction
decide stateVar now = do
    st <- readTVar stateVar
    if not (tokenValid now (csToken st))
        then case csMintClaim st of
            Just _ | breakerPhase (csBreaker st) == HalfOpen -> pure RefuseMint
            Just _ -> retry
            Nothing -> claimMint stateVar now st MintNow RefuseMint
        else
            if refreshNeeded now st && isNothing (csMintClaim st)
                then claimMint stateVar now st (ServeAndRefresh (csToken st)) (ServeCached (csToken st))
                else pure (ServeCached (csToken st))

claimMint :: TVar CacheState -> UTCTime -> CacheState -> (MintClaim -> ServeAction) -> ServeAction -> STM ServeAction
claimMint stateVar now st claimed refused = case admit now (csBreaker st) of
    (Nothing, _) -> pure refused
    (Just ticket, breaker) -> do
        let next = csClaimCounter st + 1
            claim = MintClaim next ticket (csBreaker st) breaker
        writeTVar stateVar st{csMintClaim = Just claim, csClaimCounter = next, csBreaker = breaker}
        pure (claimed claim)

data MintOutcome
    = Minted AuthToken
    | MintedExpired
    | MintThrew SomeException

attemptMint :: RefreshConfig -> TVar CacheState -> MintClaim -> IO MintOutcome
attemptMint cfg stateVar claim = do
    result <- try (rcMint cfg)
    now <- rcClock cfg
    case result of
        Right token | tokenValid now token -> do
            due <- refreshDueAt cfg now token
            committed <- commitBreakerFold cfg stateVar claim (onMintSuccess claim token due)
            when committed (onRefreshSucceeded (crRefreshReporter (rcReporters cfg)) (authExpiresAt token))
            pure (Minted token)
        Right _ -> do
            recordMintFailure cfg stateVar claim now
            pure MintedExpired
        Left (e :: SomeException) -> do
            recordMintFailure cfg stateVar claim now
            pure (MintThrew e)

mintSynchronously :: RefreshConfig -> TVar CacheState -> MintClaim -> IO AuthToken
mintSynchronously cfg stateVar claim =
    attemptMint cfg stateVar claim >>= \case
        Minted token -> pure token
        MintedExpired -> throwIO MintedTokenAlreadyExpired
        MintThrew e -> throwIO e

-- | Release only this mint's ownership and its unsettled recovery ticket, on every exit.
releaseSingleFlight :: TVar CacheState -> MintClaim -> IO ()
releaseSingleFlight stateVar claim@(MintClaim _ ticket _ _) = atomically $
    modifyTVar' stateVar $ \st ->
        if csMintClaim st == Just claim
            then st{csMintClaim = Nothing, csBreaker = release ticket (csBreaker st)}
            else st

recordMintFailure :: RefreshConfig -> TVar CacheState -> MintClaim -> UTCTime -> IO ()
recordMintFailure cfg stateVar claim now = do
    cached <- csToken <$> readTVarIO stateVar
    committed <- commitBreakerFold cfg stateVar claim (onMintFailure claim cfg now)
    when committed (onRefreshFailed (crRefreshReporter (rcReporters cfg)) (authExpiresAt cached))

commitBreakerFold :: RefreshConfig -> TVar CacheState -> MintClaim -> (CacheState -> CacheState) -> IO Bool
commitBreakerFold cfg stateVar claim step = do
    (committed, old, new) <- atomically $ do
        st <- readTVar stateVar
        let matches = csMintClaim st == Just claim
            st' = if matches then step st else st
        writeTVar stateVar st'
        pure (matches, csBreaker st, csBreaker st')
    reportBreakerChange (crBreakerReporter (rcReporters cfg)) old new
    pure committed

-- | Install a matching owner's token and settle its breaker. Cleanup releases the mint claim.
onMintSuccess :: MintClaim -> AuthToken -> Maybe UTCTime -> CacheState -> CacheState
onMintSuccess claim@(MintClaim _ ticket _ _) token due st
    | csMintClaim st == Just claim =
        st{csToken = token, csRefreshDue = due, csBreaker = recordSuccess ticket (csBreaker st)}
    | otherwise = st

-- | Count a matching owner's genuine failure without replacing the cached token.
onMintFailure :: MintClaim -> RefreshConfig -> UTCTime -> CacheState -> CacheState
onMintFailure claim@(MintClaim _ ticket _ _) cfg now st
    | csMintClaim st == Just claim =
        st{csBreaker = recordFailure (rcBreakerThreshold cfg) (rcBreakerCooldown cfg) now ticket (csBreaker st)}
    | otherwise = st

tokenValid :: UTCTime -> AuthToken -> Bool
tokenValid now token = case authExpiresAt token of
    Nothing -> True
    Just expiry -> now < expiry

refreshNeeded :: UTCTime -> CacheState -> Bool
refreshNeeded now st = case csRefreshDue st of
    Nothing -> False
    Just due -> now >= due

-- | Jitter advances the refresh fraction. The floor bounds the latest instant before expiry.
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
