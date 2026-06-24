{-# LANGUAGE TupleSections #-}

module Ecluse.CredentialSpec (spec) where

import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime (..), addUTCTime, diffUTCTime, fromGregorian)
import Hedgehog (
    Callback (Ensure, Update),
    Command (Command),
    FunctorB (..),
    TraversableB (..),
    annotateShow,
    (===),
 )
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)
import UnliftIO (async, cancel, timeout, wait)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwString, try)

import Ecluse.Credential
import Ecluse.Credential.Refresh
import Ecluse.Credential.Refresh.Internal (refreshingProviderWith)

-- | A fixed expiry instant for the static-provider test.
anExpiry :: UTCTime
anExpiry = UTCTime (fromGregorian 2026 6 21) 0

-- | An arbitrary "epoch" the refresh tests advance their injected clock from.
t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 6 22) 0

-- | Build a token that expires the given number of seconds after 't0'.
tokenExpiringIn :: Text -> NominalDiffTime -> AuthToken
tokenExpiringIn s ttl =
    AuthToken{authSecret = mkSecret s, authExpiresAt = Just (addUTCTime ttl t0)}

{- | A controllable clock: a mutable instant the test sets, returned as the
@IO UTCTime@ the wrapper reads plus a setter.
-}
newClock :: UTCTime -> IO (IO UTCTime, UTCTime -> IO ())
newClock start = do
    ref <- newIORef start
    pure (readIORef ref, writeIORef ref)

{- | A 'RefreshConfig' wired to an injected clock and mint, with zero jitter and
a small breaker so the breaker behaviour is easy to drive in a test. Refresh
fires at 80% of token lifetime.
-}
testConfig :: IO UTCTime -> IO AuthToken -> RefreshConfig
testConfig clock mint =
    defaultRefreshConfig
        { rcClock = clock
        , rcMint = mint
        , rcJitter = pure 0
        , rcRefreshAt = 0.8
        , rcBreakerThreshold = 3
        , rcBreakerCooldown = 30
        }

{- | Poll a boolean action until it holds or a generous timeout elapses,
returning whether it became true. Used to await a background refresh
deterministically without sleeping for a fixed (and flaky) duration.
-}
waitUntil :: IO Bool -> IO Bool
waitUntil check = fromMaybe False <$> timeout 2_000_000 loop
  where
    loop = do
        ok <- check
        if ok then pure True else threadDelay 1_000 >> loop

{- | Spin until a counter reaches at least @n@ (or a timeout), so a test can
synchronise on a background mint having started before asserting on it.
-}
waitForCount :: IORef Int -> Int -> IO Bool
waitForCount ref n = waitUntil ((>= n) <$> readIORef ref)

spec :: Spec
spec = do
    describe "Secret" $ do
        it "redacts its contents in Show" $
            -- Load-bearing: a token must never reach a log, error, or any other
            -- 'Show'-derived signal (see observability.md). The literal secret
            -- text must not appear anywhere in the rendered form.
            show (mkSecret "super-secret-token") `shouldNotContain` "super-secret-token"

        it "renders a fixed redaction placeholder regardless of contents" $ do
            show (mkSecret "alpha") `shouldBe` ("Secret <REDACTED>" :: String)
            show (mkSecret "beta") `shouldBe` ("Secret <REDACTED>" :: String)

        it "still exposes the real secret via unSecret" $
            -- Redaction is a display concern only; the value must remain usable.
            unSecret (mkSecret "the-token") `shouldBe` "the-token"

        it "compares equal exactly when the underlying token text is equal" $ do
            -- The redacted 'Show' is identical for every secret, so equality must
            -- come from the wrapped text, not the rendered form: two secrets are
            -- equal iff their tokens are, and differ when the tokens differ.
            mkSecret "x" `shouldBe` mkSecret "x"
            mkSecret "x" `shouldNotBe` mkSecret "y"

        it "never leaks the secret even when embedded in an AuthToken's Show" $ do
            let tok = AuthToken{authSecret = mkSecret "leak-me", authExpiresAt = Just anExpiry}
            T.pack (show tok) `shouldSatisfy` (not . T.isInfixOf "leak-me")

    describe "staticProvider" $ do
        it "currentToken returns the configured token" $ do
            let tok = AuthToken{authSecret = mkSecret "static-token", authExpiresAt = Nothing}
            got <- currentToken (staticProvider tok)
            unSecret (authSecret got) `shouldBe` "static-token"

        it "currentToken returns the same token every call (no expiry, no refresh)" $ do
            let tok = AuthToken{authSecret = mkSecret "static-token", authExpiresAt = Just anExpiry}
                provider = staticProvider tok
            tok1 <- currentToken provider
            tok2 <- currentToken provider
            authExpiresAt tok1 `shouldBe` authExpiresAt tok2
            unSecret (authSecret tok1) `shouldBe` unSecret (authSecret tok2)

    describe "refreshingProvider" $ do
        it "mints once at construction and serves that token while well inside its life" $ do
            (clock, _setClock) <- newClock t0
            mintCount <- newIORef (0 :: Int)
            let mint = atomicModifyIORef' mintCount (\n -> (n + 1, ())) >> pure (tokenExpiringIn "tok-1" 3600)
            provider <- refreshingProvider (testConfig clock mint)
            -- One mint seeded the cache at construction.
            readIORef mintCount `shouldReturn` 1
            got <- currentToken provider
            unSecret (authSecret got) `shouldBe` "tok-1"
            -- Still inside the refresh threshold: no extra mint, no background refresh.
            _ <- currentToken provider
            _ <- currentToken provider
            readIORef mintCount `shouldReturn` 1

        it "refreshes proactively in the background once past the refresh threshold" $ do
            (clock, setClock) <- newClock t0
            tokenRef <- newIORef (tokenExpiringIn "tok-1" 1000)
            mintCount <- newIORef (0 :: Int)
            let mint = do
                    _ <- atomicModifyIORef' mintCount (\n -> (n + 1, ()))
                    readIORef tokenRef
            provider <- refreshingProvider (testConfig clock mint)
            readIORef mintCount `shouldReturn` 1
            -- The token the background refresh will pick up next.
            writeIORef tokenRef (tokenExpiringIn "tok-2" 1000)
            -- Cross the 80% threshold (refresh due at 800s; token still valid).
            setClock (addUTCTime 850 t0)
            stale <- currentToken provider
            -- The caller is never blocked: it still gets the valid (old) token.
            unSecret (authSecret stale) `shouldBe` "tok-1"
            -- The background refresh eventually swaps in the new token.
            waitUntil ((== "tok-2") . unSecret . authSecret <$> currentToken provider)
                `shouldReturn` True
            -- Exactly one background mint fired on top of the seeding mint.
            readIORef mintCount `shouldReturn` 2

        it "is single-flight: a cohort past the threshold triggers at most one refresh mint" $ do
            (clock, setClock) <- newClock t0
            -- The refresh mint blocks on an (initially empty) gate until released,
            -- so it is demonstrably in flight while the cohort piles in. The seed
            -- mint (call #1) does not block; only the refresh (call #2+) waits.
            gate <- newEmptyTMVarIO
            mintCount <- newIORef (0 :: Int)
            let mint = do
                    n <- atomicModifyIORef' mintCount (\c -> (c + 1, c + 1))
                    when (n >= 2) (atomically (takeTMVar gate))
                    pure (tokenExpiringIn "tok-2" 1000)
            provider <- refreshingProvider (testConfig clock mint)
            readIORef mintCount `shouldReturn` 1
            setClock (addUTCTime 850 t0)
            -- A whole cohort calls currentToken while a refresh is in flight.
            replicateM_ 5 (void (currentToken provider))
            -- Wait for the (single) refresh mint to have started.
            waitForCount mintCount 2 `shouldReturn` True
            -- Give any erroneous extra mints a chance to register, then assert
            -- only one refresh is ever in flight.
            threadDelay 20_000
            readIORef mintCount `shouldReturn` 2
            -- Release the in-flight refresh and confirm it lands.
            atomically (putTMVar gate ())
            waitUntil ((== "tok-2") . unSecret . authSecret <$> currentToken provider)
                `shouldReturn` True

        it "releases the single-flight flag when a mint is cancelled mid-flight (no wedge)" $ do
            -- Regression: an async exception (cancellation / timeout) landing between
            -- claiming the single-flight flag and folding the mint result must still
            -- release the flag, or every later expired caller wedges on the STM retry.
            (clock, setClock) <- newClock t0
            started <- newEmptyTMVarIO
            gate <- newEmptyTMVarIO
            mintCount <- newIORef (0 :: Int)
            let mint = do
                    n <- atomicModifyIORef' mintCount (\c -> (c + 1, c + 1))
                    case n of
                        1 -> pure (tokenExpiringIn "seed" 1000)
                        2 -> do
                            -- In the mint (flag claimed); block so the caller can be
                            -- cancelled here, mid-flight.
                            atomically (putTMVar started ())
                            (atomically (takeTMVar gate) :: IO ())
                            pure (tokenExpiringIn "unreached" 1000)
                        _ -> pure (tokenExpiringIn "recovered" 1000)
            provider <- refreshingProvider (testConfig clock mint)
            setClock (addUTCTime 2000 t0) -- expired: the next serve mints synchronously
            blocked <- async (currentToken provider)
            atomically (takeTMVar started) -- the caller holds the flag and is in the mint
            cancel blocked -- async-cancel mid-mint; the finally must release the flag
            -- A fresh caller must not wedge: with the flag released it mints (call #3).
            result <- timeout 1_000_000 (currentToken provider)
            (unSecret . authSecret <$> result) `shouldBe` Just "recovered"

        it "releases the single-flight flag when the serving thread is cancelled at the claim handoff (no wedge)" $ do
            -- Regression for the parent-side gap the mid-flight test above does not
            -- cover: the flag is claimed in the serve transaction but released only
            -- once the mint runner has installed its handler. An async exception in
            -- that handoff — before the runner takes ownership — must still release
            -- the flag, or every later expired caller wedges on the STM retry.
            --
            -- The window is otherwise a seam-less, interruptible point in pure
            -- dispatch (an empirical ~0.15% of naive cancels land there), so it is
            -- driven deterministically through the 'afterClaim' hook, which runs on
            -- the serving thread at exactly that handoff: it signals it has reached
            -- the window, then parks, so the test can cancel the thread there.
            (clock, setClock) <- newClock t0
            reached <- newEmptyTMVarIO
            release <- newEmptyTMVarIO
            armed <- newIORef True -- only the first claim parks; recovery runs free
            mintCount <- newIORef (0 :: Int)
            let mint = do
                    n <- atomicModifyIORef' mintCount (\c -> (c + 1, c + 1))
                    if n == 1
                        then pure (tokenExpiringIn "seed" 1000)
                        else pure (tokenExpiringIn "recovered" 1000)
                -- Run on the serving thread in the claim -> mint-runner window. On the
                -- first (to-be-cancelled) claim only, mark that the flag is claimed and
                -- the thread is parked here, then block (interruptibly) so the cancel
                -- lands inside this window; later claims pass straight through.
                afterClaim = do
                    wasArmed <- atomicModifyIORef' armed (False,)
                    when wasArmed $ do
                        atomically (putTMVar reached ())
                        (atomically (takeTMVar release) :: IO ())
            provider <- refreshingProviderWith afterClaim (testConfig clock mint)
            setClock (addUTCTime 2000 t0) -- expired: the next serve mints synchronously
            blocked <- async (currentToken provider)
            atomically (takeTMVar reached) -- flag claimed; thread parked at the handoff
            cancel blocked -- cancel in the handoff window; the flag must still release
            -- A fresh expired caller must not wedge: with the flag released it mints.
            result <- timeout 1_000_000 (currentToken provider)
            (unSecret . authSecret <$> result) `shouldBe` Just "recovered"

        it "keeps serving the still-valid token when a background mint fails" $ do
            (clock, setClock) <- newClock t0
            failRef <- newIORef False
            let mint = do
                    bad <- readIORef failRef
                    if bad then throwString "mint boom" else pure (tokenExpiringIn "tok-1" 1000)
            provider <- refreshingProvider (testConfig clock mint)
            -- From now on every mint fails.
            writeIORef failRef True
            setClock (addUTCTime 850 t0)
            -- Background refresh fires and fails; the caller still gets the valid token.
            replicateM_ 3 (void (currentToken provider))
            _ <- waitUntil (pure True)
            tok <- currentToken provider
            unSecret (authSecret tok) `shouldBe` "tok-1"

        it "surfaces failure to the caller only once the token has expired and mint still fails" $ do
            (clock, setClock) <- newClock t0
            failRef <- newIORef False
            let mint = do
                    bad <- readIORef failRef
                    if bad then throwString "mint boom" else pure (tokenExpiringIn "tok-1" 1000)
            provider <- refreshingProvider (testConfig clock mint)
            writeIORef failRef True
            -- Past expiry: no valid token left to serve, and mint fails.
            setClock (addUTCTime 2000 t0)
            currentToken provider `shouldThrow` anyException

        it "trips the breaker after repeated failures, then recovers on a half-open probe" $ do
            (clock, setClock) <- newClock t0
            failRef <- newIORef False
            mintCount <- newIORef (0 :: Int)
            let mint = do
                    _ <- atomicModifyIORef' mintCount (\n -> (n + 1, ()))
                    bad <- readIORef failRef
                    if bad then throwString "mint boom" else pure (tokenExpiringIn "tok-2" 1000)
            -- Seed succeeds; failures start afterwards.
            provider <- refreshingProvider (testConfig clock mint)
            readIORef mintCount `shouldReturn` 1
            writeIORef failRef True
            -- Expire the token so every currentToken must mint synchronously.
            setClock (addUTCTime 2000 t0)
            -- Drive enough synchronous failures to trip the breaker (threshold = 3).
            replicateM_ 3 (currentToken provider `shouldThrow` anyException)
            afterTrip <- readIORef mintCount
            afterTrip `shouldBe` 4 -- seed + 3 failing mints
            -- While the breaker is open, a call fast-fails (with 'BreakerOpen')
            -- without minting.
            currentToken provider `shouldThrow` (== BreakerOpen)
            readIORef mintCount `shouldReturn` afterTrip
            -- After the cooldown elapses, the breaker half-opens: one probe mint
            -- is allowed, it succeeds, and the provider recovers.
            writeIORef failRef False
            setClock (addUTCTime 60 (addUTCTime 2000 t0))
            recovered <- currentToken provider
            unSecret (authSecret recovered) `shouldBe` "tok-2"
            readIORef mintCount `shouldReturn` (afterTrip + 1)

        it "re-opens the breaker when the half-open probe also fails" $ do
            (clock, setClock) <- newClock t0
            failRef <- newIORef False
            mintCount <- newIORef (0 :: Int)
            let mint = do
                    _ <- atomicModifyIORef' mintCount (\n -> (n + 1, ()))
                    bad <- readIORef failRef
                    if bad then throwString "mint boom" else pure (tokenExpiringIn "tok-1" 1000)
            provider <- refreshingProvider (testConfig clock mint)
            writeIORef failRef True
            setClock (addUTCTime 2000 t0)
            -- Trip the breaker.
            replicateM_ 3 (currentToken provider `shouldThrow` anyException)
            -- Cooldown elapses: the next call is admitted as a half-open probe. It
            -- fails, so the breaker re-opens (no recovery) and a further immediate
            -- call fast-fails again without minting.
            setClock (addUTCTime 60 (addUTCTime 2000 t0))
            currentToken provider `shouldThrow` anyException
            afterProbe <- readIORef mintCount
            currentToken provider `shouldThrow` (== BreakerOpen)
            readIORef mintCount `shouldReturn` afterProbe

        it "waits for an in-flight refresh rather than launching a second mint when expired" $ do
            (clock, setClock) <- newClock t0
            gate <- newEmptyTMVarIO
            mintCount <- newIORef (0 :: Int)
            -- The refresh mint (call #2) blocks on the gate; the token expires
            -- while it is in flight, so a concurrent caller hits the expired path
            -- and must wait for the in-flight mint, not start its own.
            -- The seed token (call #1) is short-lived so a refresh is soon due;
            -- the refreshed token (call #2) lives well past the waiter's clock so
            -- it is genuinely valid once installed.
            let mint = do
                    n <- atomicModifyIORef' mintCount (\c -> (c + 1, c + 1))
                    if n >= 2
                        then do
                            atomically (takeTMVar gate)
                            pure (tokenExpiringIn "tok-2" 5000)
                        else pure (tokenExpiringIn "tok-1" 1000)
            provider <- refreshingProvider (testConfig clock mint)
            -- Cross the threshold to launch the background refresh.
            setClock (addUTCTime 850 t0)
            _ <- currentToken provider
            waitForCount mintCount 2 `shouldReturn` True
            -- Now jump past expiry while the refresh is still gated.
            setClock (addUTCTime 1200 t0)
            waiter <- async (currentToken provider)
            -- Give the waiter a chance to (wrongly) start a second mint.
            threadDelay 20_000
            readIORef mintCount `shouldReturn` 2
            -- Release the in-flight refresh; the waiter is served its result.
            atomically (putTMVar gate ())
            served <- wait waiter
            unSecret (authSecret served) `shouldBe` "tok-2"
            readIORef mintCount `shouldReturn` 2

        it "stops hammering the mint once repeated background refreshes trip the breaker" $ do
            (clock, setClock) <- newClock t0
            -- The token stays valid throughout (expires at t0+10000) but sits past
            -- its refresh threshold, so every request wants to refresh; the seed
            -- mint succeeds and every later mint fails. The breaker must cap the
            -- failing background mints rather than retry one per request.
            seeded <- newIORef True
            mintCount <- newIORef (0 :: Int)
            let mint = do
                    _ <- atomicModifyIORef' mintCount (\n -> (n + 1, ()))
                    firstTime <- readIORef seeded
                    if firstTime
                        then writeIORef seeded False >> pure (tokenExpiringIn "tok-1" 10000)
                        else throwString "mint boom"
            provider <- refreshingProvider (testConfig clock mint)
            -- Past the refresh threshold (0.8 * 10000 = 8000), token still valid.
            setClock (addUTCTime 8500 t0)
            -- Drive a burst of requests; each tries to refresh in the background.
            replicateM_ 8 (currentToken provider >> threadDelay 5_000)
            -- Let things settle, then confirm the breaker capped the failing mints
            -- (seed + at most the threshold) rather than one per request, and the
            -- still-valid token is what callers got throughout.
            threadDelay 30_000
            final <- readIORef mintCount
            final `shouldSatisfy` (<= 4) -- seed + at most threshold (3) failures
            tok <- currentToken provider
            unSecret (authSecret tok) `shouldBe` "tok-1"

        it "drives the default policy knobs (jitter, refresh fraction, breaker) end to end" $ do
            -- defaultRefreshConfig with only the effectful leaves wired: exercises
            -- the shipped defaults rather than test overrides.
            (clock, setClock) <- newClock t0
            tokenRef <- newIORef (tokenExpiringIn "tok-1" 1000)
            mintCount <- newIORef (0 :: Int)
            let mint = atomicModifyIORef' mintCount (\n -> (n + 1, ())) >> readIORef tokenRef
                cfg = defaultRefreshConfig{rcClock = clock, rcMint = mint}
            provider <- refreshingProvider cfg
            readIORef mintCount `shouldReturn` 1
            writeIORef tokenRef (tokenExpiringIn "tok-2" 1000)
            -- Default refresh fraction is 0.8 (jitter only pulls it earlier), so
            -- by 95% of life a background refresh is certainly due.
            setClock (addUTCTime 950 t0)
            _ <- currentToken provider
            waitUntil ((== "tok-2") . unSecret . authSecret <$> currentToken provider)
                `shouldReturn` True

        it "never refreshes a token that has no expiry" $ do
            (clock, setClock) <- newClock t0
            mintCount <- newIORef (0 :: Int)
            let mint = do
                    _ <- atomicModifyIORef' mintCount (\n -> (n + 1, ()))
                    pure AuthToken{authSecret = mkSecret "forever", authExpiresAt = Nothing}
            provider <- refreshingProvider (testConfig clock mint)
            readIORef mintCount `shouldReturn` 1
            -- No expiry: no refresh instant is ever scheduled, so advancing the
            -- clock arbitrarily far still serves the original token, never minting.
            setClock (addUTCTime 1_000_000 t0)
            tok <- currentToken provider
            unSecret (authSecret tok) `shouldBe` "forever"
            readIORef mintCount `shouldReturn` 1

        it "fails loudly when built from defaults without wiring the mint and clock" $ do
            -- defaultRefreshConfig leaves rcMint/rcClock unconfigured so a provider
            -- assembled without them fails at construction, not silently — whether
            -- it is the clock or the mint leaf that is left unwired.
            refreshingProvider defaultRefreshConfig `shouldThrow` anyException
            (clock, _setClock) <- newClock t0
            refreshingProvider defaultRefreshConfig{rcClock = clock} `shouldThrow` anyException

        it "trips at the default breaker threshold and cooldown" $ do
            (clock, setClock) <- newClock t0
            seeded <- newIORef True
            let mint = do
                    firstTime <- readIORef seeded
                    if firstTime
                        then writeIORef seeded False >> pure (tokenExpiringIn "tok-1" 1000)
                        else throwString "mint boom"
                cfg = defaultRefreshConfig{rcClock = clock, rcMint = mint}
            provider <- refreshingProvider cfg
            -- Expire the token so each call mints synchronously and fails.
            setClock (addUTCTime 2000 t0)
            -- The default threshold is 5: five failures, then the breaker is open
            -- and the sixth call fast-fails with BreakerOpen.
            replicateM_ 5 (currentToken provider `shouldThrow` anyException)
            currentToken provider `shouldThrow` (== BreakerOpen)
            -- The default cooldown is 60s: before it elapses the breaker is still
            -- open; once it does, a half-open probe is admitted again.
            setClock (addUTCTime 30 (addUTCTime 2000 t0))
            currentToken provider `shouldThrow` (== BreakerOpen)

    describe "refreshingProvider (model-based)" $
        it "agrees with a pure cache/clock/breaker model under random operation sequences" $
            hedgehog refreshModelProperty

-- ── model-based state-machine property for refreshingProvider ────────────────

{- | Policy constants the model and the provider-under-test share. Mirrors the
'testConfig' knobs used by the example tests above (refresh at 80% of lifetime,
zero jitter, a 30-second floor, breaker threshold 3, 30-second cooldown) so the
pure model can predict the implementation exactly.
-}
modelRefreshAt :: Double
modelRefreshAt = 0.8

modelRefreshFloor :: NominalDiffTime
modelRefreshFloor = 30

modelBreakerThreshold :: Int
modelBreakerThreshold = 3

modelBreakerCooldown :: NominalDiffTime
modelBreakerCooldown = 30

-- | A token that lives @ttl@ seconds from the given issue instant.
tokenLiving :: Text -> UTCTime -> NominalDiffTime -> AuthToken
tokenLiving s issuedAt ttl =
    AuthToken{authSecret = mkSecret s, authExpiresAt = Just (addUTCTime ttl issuedAt)}

{- | The breaker, modelled exactly as 'Ecluse.Credential.Refresh's private one:
healthy with a consecutive-failure count, open until an instant, or half-open.
-}
data MBreaker
    = MClosed Int
    | MOpen UTCTime
    | MHalfOpen
    deriving stock (Eq, Show)

{- | The pure model of a 'refreshingProvider's cache: a mirror of its private
@CacheState@ plus the injected clock and fail flag the commands drive, and a count
of mints the implementation should have performed so the harness can settle the
background refresh deterministically before each observation.
-}
data RModel (v :: Type -> Type) = RModel
    { rmToken :: AuthToken
    -- ^ The token the cache currently holds (and will serve while valid).
    , rmRefreshDue :: Maybe UTCTime
    -- ^ When a proactive background refresh is due; 'Nothing' never refreshes.
    , rmBreaker :: MBreaker
    -- ^ The circuit-breaker state.
    , rmNow :: UTCTime
    -- ^ The model's clock (advanced by 'AdvanceClock').
    , rmFail :: Bool
    -- ^ Whether the next mint is set to fail (toggled by 'SetFail').
    , rmExpectedMints :: Int
    -- ^ How many mints the implementation should have performed so far.
    , rmNextToken :: Int
    -- ^ Index of the next distinct token the mint will hand out on success.
    }

-- | Whether a token is usable at @now@; a no-expiry token is always valid.
mTokenValid :: UTCTime -> AuthToken -> Bool
mTokenValid now token = case authExpiresAt token of
    Nothing -> True
    Just expiry -> now < expiry

-- | Whether a proactive refresh is due at @now@ (mirrors @refreshNeeded@).
mRefreshNeeded :: UTCTime -> RModel v -> Bool
mRefreshNeeded now m = case rmRefreshDue m of
    Nothing -> False
    Just due -> now >= due

{- | The refresh instant for a freshly minted token, with zero jitter — the exact
arithmetic of 'Ecluse.Credential.Refresh's @refreshDueAt@ for the shared knobs.
-}
mRefreshDueAt :: UTCTime -> AuthToken -> Maybe UTCTime
mRefreshDueAt issuedAt token = case authExpiresAt token of
    Nothing -> Nothing
    Just expiry ->
        let lifetime = realToFrac (diffUTCTime expiry issuedAt) :: Double
            frac = clamp01 modelRefreshAt
            byFraction = addUTCTime (realToFrac (frac * lifetime)) issuedAt
            floorInstant = addUTCTime (negate modelRefreshFloor) expiry
         in Just (max issuedAt (min byFraction floorInstant))
  where
    clamp01 = max 0 . min 1

{- | Whether the breaker admits a mint at @now@, and the breaker state it leaves
behind (mirrors @admitMint@: an elapsed 'MOpen' flips to half-open and admits;
otherwise an open breaker denies; closed/half-open always admit).
-}
mAdmit :: UTCTime -> MBreaker -> (Bool, MBreaker)
mAdmit now = \case
    MOpen until'
        | now < until' -> (False, MOpen until')
        | otherwise -> (True, MHalfOpen)
    other -> (True, other)

-- | Fold a successful mint into the breaker (mirrors @onMintSuccess@): reset it.
mOnSuccess :: MBreaker
mOnSuccess = MClosed 0

{- | Advance the breaker on a failed mint (mirrors @onMintFailure@): count up in
'MClosed' until the threshold trips it open; any other state re-opens.
-}
mOnFailure :: UTCTime -> MBreaker -> MBreaker
mOnFailure now = \case
    MClosed n
        | n + 1 >= modelBreakerThreshold -> tripped
        | otherwise -> MClosed (n + 1)
    _ -> tripped
  where
    tripped = MOpen (addUTCTime modelBreakerCooldown now)

{- | The model's outcome of a 'RequestToken' at the current clock: the token (or
'Nothing' for a thrown error) the caller should observe, and the model after the
call has fully settled (background refresh included). This is the heart of the
oracle — it folds the same decisions 'serve' makes, but purely.
-}
data RequestOutcome
    = ServedToken AuthToken
    | RaisedError
    deriving stock (Eq, Show)

stepRequest :: RModel v -> (RequestOutcome, RModel v)
stepRequest m
    | mTokenValid now (rmToken m) =
        if mRefreshNeeded now m
            then -- Valid but past the threshold: a background refresh fires (if the
            -- breaker admits). The caller still gets the current, valid token.
                (ServedToken (rmToken m), backgroundRefreshed)
            else (ServedToken (rmToken m), m) -- valid, no refresh due: serve cached
    | otherwise = expiredPath -- expired: must mint synchronously
  where
    now = rmNow m

    -- A due background refresh: attempt a mint if the breaker admits, else skip.
    backgroundRefreshed =
        let (admit, br') = mAdmit now (rmBreaker m)
         in if not admit
                then m{rmBreaker = br'}
                else mintInto m{rmBreaker = br'} (rmFail m)

    -- The expired (synchronous) path: breaker may fast-fail without minting.
    expiredPath =
        let (admit, br') = mAdmit now (rmBreaker m)
         in if not admit
                then (RaisedError, m{rmBreaker = br'}) -- BreakerOpen, no mint
                else
                    let m' = mintInto m{rmBreaker = br'} (rmFail m)
                     in if rmFail m
                            then (RaisedError, m') -- expired + failed mint surfaces
                            else (ServedToken (rmToken m'), m')

    -- Apply one mint (success installs a fresh token and resets the breaker and
    -- the refresh schedule; failure keeps the cached token and advances the
    -- breaker). Either way the mint counter advances by one.
    mintInto base failed
        | failed =
            base
                { rmBreaker = mOnFailure now (rmBreaker base)
                , rmExpectedMints = rmExpectedMints base + 1
                }
        | otherwise =
            let fresh = tokenLiving (mintName (rmNextToken base)) now 1000
             in base
                    { rmToken = fresh
                    , rmRefreshDue = mRefreshDueAt now fresh
                    , rmBreaker = mOnSuccess
                    , rmExpectedMints = rmExpectedMints base + 1
                    , rmNextToken = rmNextToken base + 1
                    }

-- | The secret text the @n@-th successful mint hands out (distinct per mint).
mintName :: Int -> Text
mintName n = "tok-" <> show n

-- ── the test harness wiring the model knobs to a real provider ───────────────

{- | The mutable wiring a model run drives: a settable clock, a fail flag, a
running mint count, the index of the next token to hand out, and a live gauge of
mints in flight together with the high-water mark (so a single-flight violation —
two mints overlapping — is caught directly).
-}
data RefreshHarness = RefreshHarness
    { hClock :: IORef UTCTime
    , hFail :: IORef Bool
    , hMintCount :: IORef Int
    , hNextToken :: IORef Int
    , hInFlight :: IORef Int
    , hMaxInFlight :: IORef Int
    }

newHarness :: UTCTime -> IO RefreshHarness
newHarness start =
    RefreshHarness
        <$> newIORef start
        <*> newIORef False
        <*> newIORef 0
        <*> newIORef 0
        <*> newIORef 0
        <*> newIORef 0

{- | Build a provider whose clock and mint are wired to the harness. The mint
records its concurrency (to catch single-flight violations), counts itself, and
either fails (when the fail flag is set) or hands out the next distinct token —
matching the model's 'mintInto' arithmetic.
-}
harnessProvider :: RefreshHarness -> IO CredentialProvider
harnessProvider h =
    refreshingProvider
        defaultRefreshConfig
            { rcClock = readIORef (hClock h)
            , rcJitter = pure 0
            , rcRefreshAt = modelRefreshAt
            , rcRefreshFloor = modelRefreshFloor
            , rcBreakerThreshold = modelBreakerThreshold
            , rcBreakerCooldown = modelBreakerCooldown
            , rcMint = mint
            }
  where
    mint = do
        -- Enter the mint: bump the in-flight gauge and record the high-water mark.
        inFlight <- atomicModifyIORef' (hInFlight h) (\n -> (n + 1, n + 1))
        atomicModifyIORef' (hMaxInFlight h) (\hi -> (max hi inFlight, ()))
        _ <- atomicModifyIORef' (hMintCount h) (\n -> (n + 1, ()))
        now <- readIORef (hClock h)
        bad <- readIORef (hFail h)
        let leave = atomicModifyIORef' (hInFlight h) (\n -> (n - 1, ()))
        if bad
            then leave >> throwString "model mint boom"
            else do
                idx <- atomicModifyIORef' (hNextToken h) (\n -> (n + 1, n))
                leave
                pure (tokenLiving (mintName idx) now 1000)

-- ── command inputs (Hedgehog barbie functors; none carry symbolic variables) ──

data RequestInput (v :: Type -> Type) = RequestInput
    deriving stock (Show)

instance FunctorB RequestInput where
    bmap _ RequestInput = RequestInput

instance TraversableB RequestInput where
    btraverse _ RequestInput = pure RequestInput

newtype AdvanceInput (v :: Type -> Type) = AdvanceInput NominalDiffTime
    deriving stock (Show)

instance FunctorB AdvanceInput where
    bmap _ (AdvanceInput d) = AdvanceInput d

instance TraversableB AdvanceInput where
    btraverse _ (AdvanceInput d) = pure (AdvanceInput d)

newtype SetFailInput (v :: Type -> Type) = SetFailInput Bool
    deriving stock (Show)

instance FunctorB SetFailInput where
    bmap _ (SetFailInput b) = SetFailInput b

instance TraversableB SetFailInput where
    btraverse _ (SetFailInput b) = pure (SetFailInput b)

-- ── commands ─────────────────────────────────────────────────────────────────

{- | 'AdvanceClock dt': move the injected clock forward by @dt@ seconds. The clock
only ever advances (time does not run backwards), so the generated deltas are
non-negative. No mint can be triggered by advancing alone.
-}
advanceCommand :: RefreshHarness -> Command H.Gen (H.PropertyT IO) RModel
advanceCommand h =
    Command
        (const (Just (AdvanceInput . fromInteger <$> Gen.integral (Range.linear 0 600))))
        (\(AdvanceInput d) -> liftIO (atomicModifyIORef' (hClock h) (\now -> (addUTCTime d now, ()))))
        [ Update $ \m (AdvanceInput d) _out -> m{rmNow = addUTCTime d (rmNow m)}
        ]

{- | 'SetFail b': arm or disarm the next mint to fail, modelling a transient token
API outage and its recovery. Touches no token state directly.
-}
setFailCommand :: RefreshHarness -> Command H.Gen (H.PropertyT IO) RModel
setFailCommand h =
    Command
        (const (Just (SetFailInput <$> Gen.bool)))
        (\(SetFailInput b) -> liftIO (writeIORef (hFail h) b))
        [ Update $ \m (SetFailInput b) _out -> m{rmFail = b}
        ]

{- | 'RequestToken': call 'currentToken'. After it returns we settle any
background refresh (waiting for the predicted mint count), so the 'Ensure'
oracle can assert deterministically that the served token — or the thrown error —
exactly matches the model, the served secret is never fabricated, a served token
is always valid at the current clock, and single-flight was never violated.
-}
requestCommand :: RefreshHarness -> CredentialProvider -> Command H.Gen (H.PropertyT IO) RModel
requestCommand h provider =
    Command
        (const (Just (pure RequestInput)))
        execute
        [ Update $ \m RequestInput _out -> snd (stepRequest m)
        , Ensure $ \beforeState _afterState RequestInput (observed, maxInFlight) -> do
            let (expected, _) = stepRequest beforeState
            annotateShow (rmNow beforeState)
            annotateShow expected
            -- The observed outcome (served secret or error) matches the model.
            outcomeMatches expected observed
            -- A served token is always valid at the clock it was served under
            -- (the wrapper never hands back an expired token).
            case observed of
                Right tok -> H.assert (mTokenValid (rmNow beforeState) tok)
                Left _ -> H.success
            -- Single-flight: at no point did two mints overlap (high-water mark of
            -- the in-flight gauge, captured by the harness mint, never exceeds 1).
            H.assert (maxInFlight <= 1)
            -- Non-vacuity: a generated sequence must reach each interesting policy
            -- arm often enough, so the oracle is not silently testing only the
            -- happy path (serve-cached). The percentages are per *step*.
            let tag = coverTag beforeState expected
            H.cover 1 "valid-bg-refresh" (tag == "valid-bg-refresh")
            H.cover 1 "expired-mint-ok" (tag == "expired-mint-ok")
            H.cover 1 "expired-error" (tag == "expired-error")
        ]
  where
    -- Run currentToken, capture either the token or the fact it threw, then let
    -- the background refresh (if the model predicts one) land before returning.
    -- Reports the served outcome together with the single-flight high-water mark,
    -- so the (pure 'Test') 'Ensure' can assert on both without touching 'IO'.
    execute RequestInput = liftIO $ do
        result <- try (currentToken provider)
        -- The conservative settle below waits for quiescence by polling the
        -- in-flight gauge to zero (and the mint count to stop moving), which is
        -- enough to make the asynchronous background refresh deterministic under
        -- the sequential model.
        settleQuiescent h
        maxInFlight <- readIORef (hMaxInFlight h)
        pure (toObserved result, maxInFlight)

    toObserved :: Either SomeException AuthToken -> Either Text AuthToken
    toObserved = first (const "error")

    coverTag :: RModel v -> RequestOutcome -> Text
    coverTag st expected =
        let valid = mTokenValid (rmNow st) (rmToken st)
            refreshDue = mRefreshNeeded (rmNow st) st
         in case (valid, refreshDue, expected) of
                (True, False, _) -> "serve-cached"
                (True, True, _) -> "valid-bg-refresh"
                (False, _, RaisedError) -> "expired-error"
                (False, _, ServedToken _) -> "expired-mint-ok"

    outcomeMatches expected observed = case (expected, observed) of
        (ServedToken tok, Right got) ->
            unSecret (authSecret got) === unSecret (authSecret tok)
        (RaisedError, Left _) -> H.success
        _ -> do
            annotateShow ("outcome mismatch" :: Text, expected, fmap (unSecret . authSecret) observed)
            H.failure

{- | Wait until no mint is in flight and the count has stopped moving, so a
fire-and-forget background refresh has fully landed before the next observation.
A short stability window guards against sampling the gauge in the gap before the
async refresh has even started.
-}
settleQuiescent :: RefreshHarness -> IO ()
settleQuiescent h = void (timeout 2_000_000 (go (0 :: Int) (-1)))
  where
    go stable lastCount = do
        inFlight <- readIORef (hInFlight h)
        count <- readIORef (hMintCount h)
        if inFlight == 0 && count == lastCount
            then
                if stable >= 4
                    then pure ()
                    else threadDelay 500 >> go (stable + 1) count
            else threadDelay 500 >> go 0 count

{- | The property: seed a fresh provider whose first (construction) mint installs
@tok-0@, then drive a random sequence of request / advance-clock / set-fail
operations against it and the pure model, asserting they agree at every step.
-}
refreshModelProperty :: H.PropertyT IO ()
refreshModelProperty = do
    h <- liftIO (newHarness t0)
    provider <- liftIO (harnessProvider h)
    -- Construction performed exactly one mint, installing tok-0 (lives 1000s).
    seedToken <- liftIO (currentToken provider)
    liftIO (settleQuiescent h)
    let initial =
            RModel
                { rmToken = seedToken
                , rmRefreshDue = mRefreshDueAt t0 seedToken
                , rmBreaker = MClosed 0
                , rmNow = t0
                , rmFail = False
                , rmExpectedMints = 1
                , rmNextToken = 1
                }
        commands =
            [ requestCommand h provider
            , advanceCommand h
            , setFailCommand h
            ]
    actions <- H.forAll (Gen.sequential (Range.linear 1 40) initial commands)
    H.executeSequential initial actions
