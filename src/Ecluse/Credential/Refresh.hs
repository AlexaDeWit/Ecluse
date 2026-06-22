{- | The refresh / cache / expiry / concurrency policy behind a
'Ecluse.Credential.CredentialProvider'.

The interesting part of outbound auth is not the cloud call but the /policy/
around it: serve a cached token, refresh it proactively before it expires, never
stampede the token API, and stay up across a transient mint outage. That policy
is identical for every cloud, so it lives here once, parameterised over a tiny
per-cloud 'rcMint' leaf (CodeArtifact's @GetAuthorizationToken@, an ADC OAuth2
token, …) and an injected 'rcClock'. Only 'rcMint' touches a network; everything
else is deterministic, so the whole policy is unit-tested with a fake clock and a
fake mint (see @docs\/architecture\/cloud-backends.md@ → "Credential Provider").

== The policy

* __Proactive, background refresh.__ A token is refreshed when the clock passes a
  fraction ('rcRefreshAt', ~80%) of its lifetime, with 'rcJitter' to desynchronise
  a cohort of instances, plus a hard floor near expiry. Because the current token
  stays valid during the refresh, the request hot path __never blocks on a mint__
  in the common case — the refresh runs in the background and swaps the token in
  when it lands.

* __Single-flight.__ At most one mint is ever in flight per provider (an STM flag),
  so a cohort of callers crossing the threshold together never stampedes the cloud
  token API; the rest serve the still-valid cached token.

* __Serve-stale on failure, behind a circuit breaker.__ A failing mint does not
  fail the caller while the cached token is still valid — the wrapper keeps serving
  it and retries later. Repeated failures __trip a circuit breaker__ that fast-fails
  further mints for a cooldown ('rcBreakerCooldown') before a single half-open
  probe tests recovery, so a sustained outage neither hammers the token API nor
  adds latency. Only an __expired__ token together with a still-failing mint
  surfaces as an exception to the caller (the breaker shares its shape with the
  effectful-rule tier — see
  @docs\/architecture\/rules-engine.md@ → "Effectful-rule failure").

Because a 'CredentialProvider' is __mirror-write only__, even a fully failed
refresh never touches the client serve path — only the mirror publish (see
@docs\/architecture\/registry-model.md@ → "Credential flow and authority").
-}
module Ecluse.Credential.Refresh (
    -- * Configuration
    RefreshConfig (..),
    defaultRefreshConfig,

    -- * The refreshing provider
    refreshingProvider,

    -- * Failure
    CredentialError (..),
) where

import Control.Concurrent.STM (retry)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import UnliftIO (async, throwIO, try)
import UnliftIO.Exception (finally, stringException)

import Ecluse.Credential (AuthToken (..), CredentialProvider (..))

{- | The one failure a 'refreshingProvider' surfaces to its caller: there is no
valid token to serve and a fresh mint is unavailable. A still-valid token is
always served instead (the refresh fails silently in the background), so this is
reached only on the expired-token path — and, because credentials are
mirror-write only, never on the client serve path (see the module header).
-}
data CredentialError
    = {- | The token has expired and the mint circuit breaker is open, so no mint
      is attempted; the caller must back off and retry later.
      -}
      BreakerOpen
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
    unconfigured field =
        throwIO (stringException ("Ecluse.Credential.Refresh: " <> toString field <> " is not configured"))

-- The circuit breaker's state, gating whether a mint may be attempted.
data Breaker
    = -- Healthy: track consecutive failures up to the trip threshold.
      Closed Int
    | -- Tripped until the given instant: mints fast-fail until then.
      Open UTCTime
    | -- Cooldown elapsed: one probe mint is allowed through to test recovery.
      HalfOpen
    deriving stock (Eq, Show)

{- The mutable state of a refreshing provider: the cached token, when its
proactive refresh is due, the single-flight flag, and the breaker.
-}
data CacheState = CacheState
    { -- The token currently served.
      csToken :: AuthToken
    , -- When a proactive background refresh should fire; 'Nothing' for a token
      -- with no expiry (it never refreshes).
      csRefreshDue :: Maybe UTCTime
    , -- Whether a mint is in flight (the single-flight flag).
      csRefreshing :: Bool
    , -- The circuit-breaker state.
      csBreaker :: Breaker
    }

{- | Build a 'CredentialProvider' that caches a token and refreshes it per the
'RefreshConfig' policy (see the module header). Mints once eagerly to seed the
cache, so a provider that cannot mint at all fails here at construction rather
than on the first request; thereafter 'currentToken' serves the cache and
refreshes behind it.
-}
refreshingProvider :: RefreshConfig -> IO CredentialProvider
refreshingProvider cfg = do
    now <- rcClock cfg
    token <- rcMint cfg
    due <- refreshDueAt cfg now token
    stateVar <- newTVarIO (CacheState token due False (Closed 0))
    pure CredentialProvider{currentToken = serve cfg stateVar}

{- Serve the current token, scheduling a background refresh or — only when the
token has expired — minting synchronously. The decision is made in one STM
transaction so single-flight holds across a concurrent cohort.
-}
serve :: RefreshConfig -> TVar CacheState -> IO AuthToken
serve cfg stateVar = do
    now <- rcClock cfg
    action <- atomically (decide now)
    case action of
        ServeCached token -> pure token
        ServeAndRefresh token -> do
            -- Fire-and-forget: the refresh runs in the background and the caller
            -- gets the still-valid cached token immediately. The refresh catches
            -- its own failures, so the discarded 'Async' can never surface one.
            _ <- async (backgroundRefresh cfg stateVar)
            pure token
        MintNow -> mintSynchronously cfg stateVar
  where
    -- One atomic decision over the current state. Claims the single-flight flag
    -- (or routes to the blocking path) so at most one mint is ever launched.
    decide :: UTCTime -> STM ServeAction
    decide now = do
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

-- What a 'serve' decision resolves to.
data ServeAction
    = -- The cached token is valid and no refresh is due: serve it.
      ServeCached AuthToken
    | -- Valid but past the refresh threshold: serve it, refresh in background.
      ServeAndRefresh AuthToken
    | -- Expired: the caller must mint synchronously (the slow path).
      MintNow

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
the caller.
-}
mintSynchronously :: RefreshConfig -> TVar CacheState -> IO AuthToken
mintSynchronously cfg stateVar = mint `finally` releaseSingleFlight stateVar
  where
    mint = do
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

{- Release the single-flight flag, run in a 'finally' around every mint attempt
so it is cleared on __every__ exit — success, a synchronous mint failure, or an
__asynchronous__ exception (cancellation \/ timeout) landing between claiming the
flag and folding the result. Without this, an async exception would leave the flag
set and wedge every later expired caller on the STM 'retry'. The flag is held for
the whole operation, so no concurrent mint can re-claim it mid-flight — an
unconditional release here therefore cannot clobber another operation's claim.
-}
releaseSingleFlight :: TVar CacheState -> IO ()
releaseSingleFlight stateVar =
    atomically (modifyTVar' stateVar (\st -> st{csRefreshing = False}))

{- The circuit-breaker admission gate, shared by the background and synchronous
mint paths. While the breaker is 'Open' and the cooldown has not elapsed, deny
(fast-fail); once it elapses, move to 'HalfOpen' and admit a single probe; a
'Closed' or 'HalfOpen' breaker always admits.
-}
admitMint :: TVar CacheState -> UTCTime -> STM Bool
admitMint stateVar now = do
    st <- readTVar stateVar
    case csBreaker st of
        Open until'
            | now < until' -> pure False
            | otherwise -> do
                writeTVar stateVar st{csBreaker = HalfOpen}
                pure True
        _ -> pure True

{- Fold a successful mint into the cache: install the token and reset the
breaker. The single-flight flag is released by 'releaseSingleFlight' in the
'finally' around the mint (not here), so it clears even on an async exception.
-}
onMintSuccess :: AuthToken -> Maybe UTCTime -> CacheState -> CacheState
onMintSuccess token due st =
    st
        { csToken = token
        , csRefreshDue = due
        , csBreaker = Closed 0
        }

{- Fold a failed mint into the cache: keep the still-cached token and advance the
breaker — counting up in 'Closed' until the threshold trips it 'Open', and
re-opening when a half-open probe fails. (A mint is never attempted while the
breaker is already 'Open', so that case does not arise here; folding it in with the
half-open re-open keeps the function total.) The single-flight flag is released
separately by 'releaseSingleFlight' (see 'onMintSuccess').
-}
onMintFailure :: RefreshConfig -> UTCTime -> CacheState -> CacheState
onMintFailure cfg now st = st{csBreaker = advance (csBreaker st)}
  where
    tripped :: Breaker
    tripped = Open (addUTCTime (rcBreakerCooldown cfg) now)

    advance :: Breaker -> Breaker
    advance = \case
        Closed n
            | n + 1 >= rcBreakerThreshold cfg -> tripped
            | otherwise -> Closed (n + 1)
        _ -> tripped

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

{- Compute when a freshly minted token's proactive refresh should fire: the
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
