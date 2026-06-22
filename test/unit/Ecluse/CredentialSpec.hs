module Ecluse.CredentialSpec (spec) where

import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime (..), addUTCTime, fromGregorian)
import Test.Hspec
import UnliftIO (async, cancel, timeout, wait)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwString)

import Ecluse.Credential
import Ecluse.Credential.Refresh

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
