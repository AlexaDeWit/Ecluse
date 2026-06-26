{- | The implementation behind 'Ecluse.Credential.Refresh'. This module exposes
the provider's innards — including the 'refreshingProviderWith' test hook — that
the curated public module deliberately keeps hidden. Importing it opts out of the
module's stability promises (the same convention @text@ and @bytestring@ use for
their @.Internal@ modules); production code imports 'Ecluse.Credential.Refresh'
instead. The policy itself is documented on the public module's header.
-}
module Ecluse.Credential.Refresh.Internal (
    -- * Configuration
    RefreshConfig (..),
    defaultRefreshConfig,

    -- * The refreshing provider
    refreshingProvider,
    refreshingProviderWith,

    -- * Failure
    CredentialError (..),

    -- * State and pure\/transition helpers (exposed for direct testing)
    CacheState (..),
    ServeAction (..),
    decide,
    refreshDueAt,
    onMintSuccess,
    onMintFailure,
    admitMint,
    releaseSingleFlight,
) where

import Control.Concurrent.STM (retry)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import UnliftIO (asyncWithUnmask, throwIO, try)
import UnliftIO.Exception (finally, mask)

import Ecluse.Breaker (Breaker, admit, initialBreaker, recordFailure, recordSuccess)
import Ecluse.Credential (AuthToken (..), CredentialProvider (..))

{- | A failure surfaced from the credential-refresh layer.

The runtime case is 'BreakerOpen': there is no valid token to serve and a fresh
mint is unavailable. A still-valid token is always served instead (the refresh
fails silently in the background), so this is reached only on the expired-token
path. Whether reaching it can affect a client serve depends on what the credential
backs: never under the default @passthrough@ strategy (mirror-write only), but it
can where a provider sits on the private-upstream read (see the module header).

The degenerate case is 'Unconfigured': a 'RefreshConfig' from
'defaultRefreshConfig' was used without supplying an effectful leaf, a wiring
fault the default raises loudly rather than silently serving nothing.
-}
data CredentialError
    = {- | The token has expired and the mint circuit breaker is open, so no mint
      is attempted; the caller must back off and retry later.
      -}
      BreakerOpen
    | {- | A 'RefreshConfig' built from 'defaultRefreshConfig' was used without
      supplying the named effectful leaf ('rcMint' or 'rcClock'). A wiring fault,
      not a runtime token condition.
      -}
      Unconfigured Text
    deriving stock (Eq, Show)

instance Exception CredentialError

{- | How a 'refreshingProvider' mints, times, and protects its token. The two
effectful leaves ('rcMint', 'rcClock') and the jitter source ('rcJitter') are
injected so the whole policy is deterministic under test; the rest are policy
knobs with sensible defaults in 'defaultRefreshConfig'.
-}
data RefreshConfig = RefreshConfig
    { rcMint :: IO AuthToken
    {- ^ The per-cloud token mint — the __only__ part that touches a network. A
    backend supplies just this leaf; everything else is cloud-agnostic.
    -}
    , rcClock :: IO UTCTime
    {- ^ The clock the policy reads. Injected so refresh timing is testable
    without real time passing.
    -}
    , rcJitter :: IO Double
    {- ^ A jitter fraction in @[0, 1)@, sampled once per token, that pulls the
    refresh instant /earlier/ to desynchronise a cohort of instances so they
    do not all refresh at the same moment.
    -}
    , rcRefreshAt :: Double
    {- ^ The fraction of a token's lifetime at which to refresh, before jitter
    (the ~80% point). Clamped into @[0, 1]@.
    -}
    , rcRefreshFloor :: NominalDiffTime
    {- ^ A hard floor: never schedule the refresh later than this many seconds
    before expiry, so a token with a very short lifetime is still refreshed
    ahead of its deadline rather than served right up to it.
    -}
    , rcBreakerThreshold :: Int
    -- ^ Consecutive mint failures that trip the circuit breaker.
    , rcBreakerCooldown :: NominalDiffTime
    {- ^ How long the breaker stays open (fast-failing mints) before a single
    half-open probe is allowed to test recovery.
    -}
    }

{- | Sensible defaults for the policy knobs. The caller must still supply the
effectful leaves — 'rcMint' and 'rcClock' default to a mint\/clock that always
fails, so a provider built without wiring them up fails loudly rather than
silently serving nothing.

* refresh at 80% of lifetime (no jitter by default; 'rcJitter' may pull it earlier);
* a 30-second floor before expiry;
* breaker trips after 5 consecutive failures, cooling down for 60 seconds.
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
        }
  where
    unconfigured :: Text -> IO a
    unconfigured field = throwIO (Unconfigured field)

{- | The mutable state of a refreshing provider: the cached token, when its
proactive refresh is due, the single-flight flag, and the breaker.
-}
data CacheState = CacheState
    { csToken :: AuthToken
    -- ^ The token currently served.
    , csRefreshDue :: Maybe UTCTime
    {- ^ When a proactive background refresh should fire; 'Nothing' for a token
    with no expiry (it never refreshes).
    -}
    , csRefreshing :: Bool
    -- ^ Whether a mint is in flight (the single-flight flag).
    , csBreaker :: Breaker
    -- ^ The circuit-breaker state.
    }

{- | Build a 'CredentialProvider' that caches a token and refreshes it per the
'RefreshConfig' policy (see the module header). Mints once eagerly to seed the
cache, so a provider that cannot mint at all fails here at construction rather
than on the first request; thereafter 'currentToken' serves the cache and
refreshes behind it.
-}
refreshingProvider :: RefreshConfig -> IO CredentialProvider
refreshingProvider = refreshingProviderWith (pure ())

{- | As 'refreshingProvider', but with a hook run on the serving thread at the
single-flight claim → mint-runner handoff: the interruptible window between the
STM transaction committing the claim and the mint runner installing the scope
that releases it. It exists only so a test can deterministically park a serving
thread in that window and cancel it there; production always passes @pure ()@ via
'refreshingProvider'.
-}
refreshingProviderWith :: IO () -> RefreshConfig -> IO CredentialProvider
refreshingProviderWith afterClaim cfg = do
    now <- rcClock cfg
    token <- rcMint cfg
    due <- refreshDueAt cfg now token
    stateVar <- newTVarIO (CacheState token due False initialBreaker)
    pure CredentialProvider{currentToken = serve afterClaim cfg stateVar}

{- Serve the current token, scheduling a background refresh or — only when the
token has expired — minting synchronously. The decision is made in one STM
transaction so single-flight holds across a concurrent cohort.

The claim of the single-flight flag (inside 'decide') and the installation of the
scope that releases it (the background 'Async' for a proactive refresh; this
function's own 'finally' for a synchronous mint) are kept in __one masked
scope__: 'mask' holds async exceptions off the pure handoff between the STM
commit and that release scope, so a cancellation \/ timeout cannot land in the
gap and orphan the flag (which would wedge every later expired caller on the
'decide' 'retry'). The mint work itself runs under @restore@\/unmasked, so it
stays interruptible — the flag is simply guaranteed to have an owner first. The
@afterClaim@ hook marks exactly this window for a test (see
'refreshingProviderWith'); it is @pure ()@ in production.
-}
serve :: IO () -> RefreshConfig -> TVar CacheState -> IO AuthToken
serve afterClaim cfg stateVar = mask $ \restore -> do
    now <- rcClock cfg
    action <- atomically (decide stateVar now)
    case action of
        ServeCached token -> pure token
        ServeAndRefresh token -> do
            -- Fire-and-forget: the refresh runs in the background and the caller
            -- gets the still-valid cached token immediately. The refresh catches
            -- its own failures, so the discarded 'Async' can never surface one.
            -- The flag was claimed under 'mask'; forking is not interruptible, so
            -- the releasing child ('backgroundRefresh' owns the 'finally') is
            -- always installed before this thread can be interrupted again. The
            -- child runs unmasked, so the background mint stays cancellable.
            _ <- asyncWithUnmask (\unmask -> unmask (afterClaim >> backgroundRefresh cfg stateVar))
            pure token
        MintNow ->
            -- The flag was claimed under 'mask'; install its release 'finally'
            -- before anything interruptible runs, then do the mint work under
            -- @restore@ so the synchronous mint stays cancellable.
            restore (afterClaim >> mintSynchronously cfg stateVar)
                `finally` releaseSingleFlight stateVar

{- | The single-flight decision over the current cache state, made atomically so it
holds across a concurrent cohort: serve the still-valid token, claim the flag and
route to a background refresh when one is due, or — when the token has expired —
either claim the flag and mint synchronously or, if a mint is already in flight,
'retry' (block) until it lands rather than launching a second. The flag claim
happens here, in the transaction, so at most one mint is ever launched; the
claiming caller is responsible for releasing it (see 'serve' \/ 'releaseSingleFlight').
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

{- The background refresh: if the breaker admits a mint, attempt it and fold
the result into the cache; otherwise (breaker open) skip it. Never throws — a
failure leaves the still-valid token in place and advances the breaker, and a
suppressed refresh just keeps serving the cached token, so the request hot path is
unaffected either way.
-}
backgroundRefresh :: RefreshConfig -> TVar CacheState -> IO ()
backgroundRefresh cfg stateVar = refresh `finally` releaseSingleFlight stateVar
  where
    refresh = do
        now <- rcClock cfg
        permitted <- atomically (admitMint stateVar now)
        when permitted $ do
            result <- try (rcMint cfg)
            now' <- rcClock cfg
            case result of
                Right token -> do
                    due <- refreshDueAt cfg now' token
                    atomically (modifyTVar' stateVar (onMintSuccess token due))
                Left (_ :: SomeException) ->
                    atomically (modifyTVar' stateVar (onMintFailure cfg now'))

{- The synchronous (expired-token) path: the caller blocks on a mint because
there is no valid token to serve. The breaker gates it — when open and still in
cooldown the call fast-fails with 'BreakerOpen' without minting; otherwise it
mints, and an expired token plus a failing mint is the one case that surfaces to
the caller. The single-flight flag is released by 'serve's 'finally' around this
call (claimed and released in one masked scope), not here.
-}
mintSynchronously :: RefreshConfig -> TVar CacheState -> IO AuthToken
mintSynchronously cfg stateVar = do
    now <- rcClock cfg
    permitted <- atomically (admitMint stateVar now)
    if not permitted
        then throwIO BreakerOpen
        else do
            result <- try (rcMint cfg)
            now' <- rcClock cfg
            case result of
                Right token -> do
                    due <- refreshDueAt cfg now' token
                    atomically (modifyTVar' stateVar (onMintSuccess token due))
                    pure token
                Left (e :: SomeException) -> do
                    atomically (modifyTVar' stateVar (onMintFailure cfg now'))
                    throwIO e

{- | Release the single-flight flag. It is run in a 'finally' that is installed in
the __same masked scope__ that claimed the flag — 'serve's own 'finally' for the
synchronous mint, the background 'Async' for a proactive refresh — so the flag is
cleared on __every__ exit: success, a synchronous mint failure, or an
__asynchronous__ exception (cancellation \/ timeout) at any point from the claim
onward, including the handoff between the STM commit and the mint runner. Without
this an orphaned flag would wedge every later expired caller on the STM 'retry'.
The flag is held for the whole operation, so no concurrent mint can re-claim it
mid-flight — an unconditional release here therefore cannot clobber another
operation's claim.
-}
releaseSingleFlight :: TVar CacheState -> IO ()
releaseSingleFlight stateVar =
    atomically (modifyTVar' stateVar (\st -> st{csRefreshing = False}))

{- | The circuit-breaker admission gate, shared by the background and synchronous
mint paths. Defers the decision to 'Ecluse.Breaker.admit' and commits the breaker
state it returns: while open and cooling down it denies (fast-fail); once the
cooldown elapses it moves to half-open and admits a single probe; a closed or
half-open breaker always admits.
-}
admitMint :: TVar CacheState -> UTCTime -> STM Bool
admitMint stateVar now = do
    st <- readTVar stateVar
    let (permitted, breaker') = admit now (csBreaker st)
    writeTVar stateVar st{csBreaker = breaker'}
    pure permitted

{- | Fold a successful mint into the cache: install the token and reset the
breaker. The single-flight flag is released by 'releaseSingleFlight' in the
'finally' around the mint (not here), so it clears even on an async exception.
-}
onMintSuccess :: AuthToken -> Maybe UTCTime -> CacheState -> CacheState
onMintSuccess token due st =
    st
        { csToken = token
        , csRefreshDue = due
        , csBreaker = recordSuccess (csBreaker st)
        }

{- | Fold a failed mint into the cache: keep the still-cached token and advance the
breaker per the configured threshold and cooldown ('Ecluse.Breaker.recordFailure').
The single-flight flag is released separately by 'releaseSingleFlight' (see
'onMintSuccess').
-}
onMintFailure :: RefreshConfig -> UTCTime -> CacheState -> CacheState
onMintFailure cfg now st =
    st{csBreaker = recordFailure (rcBreakerThreshold cfg) (rcBreakerCooldown cfg) now (csBreaker st)}

{- Whether a token is still usable at the given instant. A token with no
expiry ('Nothing') is always valid.
-}
tokenValid :: UTCTime -> AuthToken -> Bool
tokenValid now token = case authExpiresAt token of
    Nothing -> True
    Just expiry -> now < expiry

{- Whether a proactive refresh is due: the token has a scheduled refresh
instant and the clock has reached it.
-}
refreshNeeded :: UTCTime -> CacheState -> Bool
refreshNeeded now st = case csRefreshDue st of
    Nothing -> False
    Just due -> now >= due

{- | Compute when a freshly minted token's proactive refresh should fire: the
'rcRefreshAt' fraction of its lifetime, pulled earlier by a per-token jitter
sample and capped at 'rcRefreshFloor' before expiry. A token with no expiry never
refreshes ('Nothing').
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
