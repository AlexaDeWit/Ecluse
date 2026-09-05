-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TupleSections #-}

module Ecluse.Core.Credential.Refresh.InternalSpec (spec) where

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
import UnliftIO.Exception (throwIO, throwString, try)

import Ecluse.Core.Breaker (Breaker (..), BreakerReporter (..), initialBreaker)
import Ecluse.Core.Credential
import Ecluse.Core.Credential.Refresh
import Ecluse.Core.Credential.Refresh.Internal (
    CacheState (..),
    ServeAction (..),
    decide,
    onMintFailure,
    onMintSuccess,
    refreshDueAt,
    refreshingProviderWith,
    releaseSingleFlight,
 )
import Ecluse.Test.Support (newTestClock)

-- | An arbitrary "epoch" the refresh tests advance their injected clock from.
t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 6 22) 0

-- | Build a token that expires the given number of seconds after 't0'.
tokenExpiringIn :: Text -> NominalDiffTime -> AuthToken
tokenExpiringIn s ttl =
    AuthToken{authSecret = mkSecret s, authExpiresAt = Just (addUTCTime ttl t0)}

{- | A 'RefreshConfig' wired to an injected clock and mint, with zero jitter and a small
breaker, so a test drives the breaker behaviour deterministically.
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

{- | Poll a boolean action until it holds or a generous timeout elapses. It awaits a
background refresh without a fixed, flaky sleep.
-}
waitUntil :: IO Bool -> IO Bool
waitUntil check = fromMaybe False <$> timeout 2_000_000 loop
  where
    loop = do
        ok <- check
        if ok then pure True else threadDelay 1_000 >> loop

-- | Spin until a counter reaches @n@, so a test can wait for a background mint to start.
waitForCount :: IORef Int -> Int -> IO Bool
waitForCount ref n = waitUntil ((>= n) <$> readIORef ref)

{- | A baseline 'CacheState' for the direct helper tests: healthy, not refreshing, holding
a long-lived token. Each test tweaks the one axis it exercises.
-}
aCacheState :: CacheState
aCacheState =
    CacheState
        { csToken = tokenExpiringIn "cached" 1000
        , csRefreshDue = Just (addUTCTime 800 t0)
        , csRefreshing = False
        , csBreaker = initialBreaker
        }

{- | A 'RefreshConfig' carrying only the breaker knobs the pure fold helpers read. Its
effectful leaves stay at the loud 'defaultRefreshConfig' defaults, which these helpers never touch.
-}
breakerCfg :: RefreshConfig
breakerCfg = defaultRefreshConfig{rcBreakerThreshold = 3, rcBreakerCooldown = 30}

{- | A refresh outcome captured by a test 'RefreshReporter': the result and the
remaining-lifetime seconds it carried.
-}
data RefreshEvent = ReportedSuccess (Maybe Int) | ReportedFailure (Maybe Int)
    deriving stock (Eq, Show)

{- | A pair of capturing reporters appending to their own logs, oldest first. A test can then
assert the exact order of breaker transitions and refresh outcomes.
-}
capturingReporters :: IO (IORef [Breaker], IORef [RefreshEvent], BreakerReporter, RefreshReporter)
capturingReporters = do
    breakerLog <- newIORef []
    refreshLog <- newIORef []
    let breakerR = BreakerReporter (\b -> modifyIORef' breakerLog (<> [b]))
        refreshR =
            RefreshReporter
                { onRefreshSucceeded = \mttl -> modifyIORef' refreshLog (<> [ReportedSuccess mttl])
                , onRefreshFailed = \mttl -> modifyIORef' refreshLog (<> [ReportedFailure mttl])
                }
    pure (breakerLog, refreshLog, breakerR, refreshR)

{- | A distinguishable mint failure, so a test can assert the synchronous path rethrows the
mint's own exception rather than one of its own.
-}
data MintBoom = MintBoom
    deriving stock (Eq, Show)

instance Exception MintBoom

{- | A mint that runs the next scripted action on each call, so a test drives a deterministic
sequence of mint outcomes. The eager construction mint consumes the head.
-}
scriptedMint :: IORef [IO AuthToken] -> IO AuthToken
scriptedMint ref = join (atomicModifyIORef' ref next)
  where
    next (a : rest) = (rest, a)
    next [] = ([], throwString "scriptedMint: exhausted")

spec :: Spec
spec = do
    describe "refreshingProvider" $ do
        it "mints once at construction and serves that token while well inside its life" $ do
            (clock, _setClock) <- newTestClock t0
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
            (clock, setClock) <- newTestClock t0
            tokenRef <- newIORef (tokenExpiringIn "tok-1" 1000)
            mintCount <- newIORef (0 :: Int)
            let mint = do
                    _ <- atomicModifyIORef' mintCount (\n -> (n + 1, ()))
                    readIORef tokenRef
            provider <- refreshingProvider (testConfig clock mint)
            readIORef mintCount `shouldReturn` 1
            -- The token the background refresh will pick up next.
            writeIORef tokenRef (tokenExpiringIn "tok-2" 1000)
            -- Cross the 80% threshold (refresh due at 800s, token still valid).
            setClock (addUTCTime 850 t0)
            stale <- currentToken provider
            -- Nothing blocks the caller: it still gets the valid (old) token.
            unSecret (authSecret stale) `shouldBe` "tok-1"
            -- The background refresh eventually swaps in the new token.
            waitUntil ((== "tok-2") . unSecret . authSecret <$> currentToken provider)
                `shouldReturn` True
            -- Exactly one background mint fired on top of the seeding mint.
            readIORef mintCount `shouldReturn` 2

        it "is single-flight: a cohort past the threshold triggers at most one refresh mint" $ do
            (clock, setClock) <- newTestClock t0
            -- The refresh mint blocks on the gate, so it is demonstrably in flight while the
            -- cohort piles in. The seed mint (call #1) does not block.
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
            -- Wait for the (single) refresh mint to start.
            waitForCount mintCount 2 `shouldReturn` True
            -- Give any erroneous extra mints a chance to register, then assert
            -- only one refresh is ever in flight.
            threadDelay 20_000
            readIORef mintCount `shouldReturn` 2
            atomically (putTMVar gate ())
            waitUntil ((== "tok-2") . unSecret . authSecret <$> currentToken provider)
                `shouldReturn` True

        it "releases the single-flight flag when a mint is cancelled mid-flight (no wedge)" $ do
            -- Regression: an async exception (cancellation or timeout) can land between
            -- claiming the single-flight flag and folding the mint result. It must still
            -- release the flag, or every later expired caller wedges on the STM retry.
            (clock, setClock) <- newTestClock t0
            started <- newEmptyTMVarIO
            gate <- newEmptyTMVarIO
            mintCount <- newIORef (0 :: Int)
            let mint = do
                    n <- atomicModifyIORef' mintCount (\c -> (c + 1, c + 1))
                    case n of
                        1 -> pure (tokenExpiringIn "seed" 1000)
                        2 -> do
                            -- In the mint (flag claimed). Block so the test can cancel
                            -- the caller here, mid-flight.
                            atomically (putTMVar started ())
                            (atomically (takeTMVar gate) :: IO ())
                            pure (tokenExpiringIn "unreached" 5000)
                        _ -> pure (tokenExpiringIn "recovered" 5000)
            provider <- refreshingProvider (testConfig clock mint)
            setClock (addUTCTime 2000 t0) -- expired: the next serve mints synchronously
            blocked <- async (currentToken provider)
            atomically (takeTMVar started) -- the caller holds the flag and is in the mint
            cancel blocked -- async-cancel mid-mint: the finally must release the flag
            -- A fresh caller must not wedge: with the flag released it mints (call #3).
            result <- timeout 1_000_000 (currentToken provider)
            (unSecret . authSecret <$> result) `shouldBe` Just "recovered"

        it "releases the single-flight flag when the serving thread is cancelled at the claim handoff (no wedge)" $ do
            -- Regression for the parent-side gap: the serve transaction claims the flag, but
            -- nothing releases it until the mint runner installs its handler. A cancel in that
            -- handoff must still release the flag, or every later expired caller wedges on the STM
            -- retry.
            (clock, setClock) <- newTestClock t0
            reached <- newEmptyTMVarIO
            release <- newEmptyTMVarIO
            armed <- newIORef True -- only the first claim parks, recovery runs free
            mintCount <- newIORef (0 :: Int)
            let mint = do
                    n <- atomicModifyIORef' mintCount (\c -> (c + 1, c + 1))
                    if n == 1
                        then pure (tokenExpiringIn "seed" 1000)
                        else pure (tokenExpiringIn "recovered" 5000)
                -- Runs on the serving thread in the claim -> mint-runner window. Only the first
                -- claim parks here, interruptibly, so the cancel lands inside the window.
                afterClaim = do
                    wasArmed <- atomicModifyIORef' armed (False,)
                    when wasArmed $ do
                        atomically (putTMVar reached ())
                        (atomically (takeTMVar release) :: IO ())
            provider <- refreshingProviderWith afterClaim (testConfig clock mint)
            setClock (addUTCTime 2000 t0) -- expired: the next serve mints synchronously
            blocked <- async (currentToken provider)
            atomically (takeTMVar reached) -- flag claimed, thread parked at the handoff
            cancel blocked -- cancel in the handoff window: the flag must still release
            -- A fresh expired caller must not wedge: with the flag released it mints.
            result <- timeout 1_000_000 (currentToken provider)
            (unSecret . authSecret <$> result) `shouldBe` Just "recovered"

        it "keeps serving the still-valid token when a background mint fails" $ do
            (clock, setClock) <- newTestClock t0
            failRef <- newIORef False
            let mint = do
                    bad <- readIORef failRef
                    if bad then throwString "mint boom" else pure (tokenExpiringIn "tok-1" 1000)
            provider <- refreshingProvider (testConfig clock mint)
            -- From now on every mint fails.
            writeIORef failRef True
            setClock (addUTCTime 850 t0)
            -- Background refresh fires and fails. The caller still gets the valid token.
            replicateM_ 3 (void (currentToken provider))
            _ <- waitUntil (pure True)
            tok <- currentToken provider
            unSecret (authSecret tok) `shouldBe` "tok-1"

        it "surfaces failure to the caller only once the token has expired and mint still fails" $ do
            (clock, setClock) <- newTestClock t0
            failRef <- newIORef False
            let mint = do
                    bad <- readIORef failRef
                    if bad then throwString "mint boom" else pure (tokenExpiringIn "tok-1" 1000)
            provider <- refreshingProvider (testConfig clock mint)
            writeIORef failRef True
            -- Past expiry: no valid token left to serve, and mint fails.
            setClock (addUTCTime 2000 t0)
            currentToken provider `shouldThrow` anyException

        it "rethrows the mint's own exception on the synchronous path" $ do
            (clock, setClock) <- newTestClock t0
            script <- newIORef [pure (tokenExpiringIn "eager" 10), throwIO MintBoom]
            provider <- refreshingProvider (testConfig clock (scriptedMint script))
            setClock (addUTCTime 20 t0)
            currentToken provider `shouldThrow` (== MintBoom)

        it "trips the breaker after repeated failures, then recovers on a half-open probe" $ do
            (clock, setClock) <- newTestClock t0
            failRef <- newIORef False
            mintCount <- newIORef (0 :: Int)
            let mint = do
                    n <- atomicModifyIORef' mintCount (\c -> (c + 1, c + 1))
                    bad <- readIORef failRef
                    if bad
                        then throwString "mint boom"
                        else
                            if n == 1
                                then pure (tokenExpiringIn "tok-2" 1000)
                                else pure (tokenExpiringIn "tok-2" 5000)
            -- Seed succeeds. Failures start afterwards.
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
            -- After the cooldown elapses, the breaker half-opens: it admits one probe
            -- mint, that mint succeeds, and the provider recovers.
            writeIORef failRef False
            setClock (addUTCTime 60 (addUTCTime 2000 t0))
            recovered <- currentToken provider
            unSecret (authSecret recovered) `shouldBe` "tok-2"
            readIORef mintCount `shouldReturn` (afterTrip + 1)

        it "re-opens the breaker when the half-open probe also fails" $ do
            (clock, setClock) <- newTestClock t0
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
            -- Cooldown elapsed: the breaker admits a half-open probe. It fails, so the breaker
            -- re-opens and the next call fast-fails without minting.
            setClock (addUTCTime 60 (addUTCTime 2000 t0))
            currentToken provider `shouldThrow` anyException
            afterProbe <- readIORef mintCount
            currentToken provider `shouldThrow` (== BreakerOpen)
            readIORef mintCount `shouldReturn` afterProbe

        it "waits for an in-flight refresh rather than launching a second mint when expired" $ do
            (clock, setClock) <- newTestClock t0
            gate <- newEmptyTMVarIO
            mintCount <- newIORef (0 :: Int)
            -- The refresh mint (call #2) blocks on the gate and the token expires while it is in
            -- flight. A concurrent caller on the expired path must wait for that mint rather than
            -- start its own.
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
            -- Release the in-flight refresh. The waiter gets its result.
            atomically (putTMVar gate ())
            served <- wait waiter
            unSecret (authSecret served) `shouldBe` "tok-2"
            readIORef mintCount `shouldReturn` 2

        it "stops hammering the mint once repeated background refreshes trip the breaker" $ do
            (clock, setClock) <- newTestClock t0
            -- The token stays valid throughout but sits past its refresh threshold, so every
            -- request wants to refresh. The breaker must cap the failing background mints rather
            -- than retry one per request.
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
            -- Drive a burst of requests. Each tries to refresh in the background.
            replicateM_ 8 (currentToken provider >> threadDelay 5_000)
            -- Let things settle. The breaker caps the failing mints at the seed plus at most the
            -- threshold, rather than one per request.
            threadDelay 30_000
            final <- readIORef mintCount
            final `shouldSatisfy` (<= 4) -- seed + at most threshold (3) failures
            tok <- currentToken provider
            unSecret (authSecret tok) `shouldBe` "tok-1"

        it "drives the default policy knobs (jitter, refresh fraction, breaker) end to end" $ do
            -- defaultRefreshConfig with only the effectful leaves wired: exercises
            -- the shipped defaults rather than test overrides.
            (clock, setClock) <- newTestClock t0
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
            (clock, setClock) <- newTestClock t0
            mintCount <- newIORef (0 :: Int)
            let mint = do
                    _ <- atomicModifyIORef' mintCount (\n -> (n + 1, ()))
                    pure AuthToken{authSecret = mkSecret "forever", authExpiresAt = Nothing}
            provider <- refreshingProvider (testConfig clock mint)
            readIORef mintCount `shouldReturn` 1
            -- No expiry: nothing ever schedules a refresh instant, so advancing the
            -- clock arbitrarily far still serves the original token, never minting.
            setClock (addUTCTime 1_000_000 t0)
            tok <- currentToken provider
            unSecret (authSecret tok) `shouldBe` "forever"
            readIORef mintCount `shouldReturn` 1

        it "fails loudly when built from defaults without wiring the mint and clock" $ do
            -- defaultRefreshConfig leaves rcMint and rcClock unwired, so construction must fail
            -- loudly whichever leaf is the missing one.
            refreshingProvider defaultRefreshConfig `shouldThrow` anyException
            (clock, _setClock) <- newTestClock t0
            refreshingProvider defaultRefreshConfig{rcClock = clock} `shouldThrow` anyException

        it "trips at the default breaker threshold and cooldown" $ do
            (clock, setClock) <- newTestClock t0
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
            -- open. Once it does, the breaker admits a half-open probe again.
            setClock (addUTCTime 30 (addUTCTime 2000 t0))
            currentToken provider `shouldThrow` (== BreakerOpen)

    -- Direct unit tests for the pure state-transition helpers, so a regression localises to
    -- the function rather than only surfacing through 'currentToken'.
    describe "refreshDueAt" $ do
        it "schedules the refresh at the configured fraction of the lifetime" $ do
            -- Default knobs: 0.8 fraction, 30s floor, no jitter. A 1000s token
            -- issued at t0 is due at 800s (well clear of the 30s floor).
            due <- refreshDueAt defaultRefreshConfig t0 (tokenExpiringIn "tok" 1000)
            due `shouldBe` Just (addUTCTime 800 t0)

        it "never schedules later than the floor before expiry" $ do
            -- A short 100s token: 0.8 * 100 = 80s would land 20s before expiry,
            -- inside the 30s floor, so the floor (70s) wins.
            due <- refreshDueAt defaultRefreshConfig t0 (tokenExpiringIn "tok" 100)
            due `shouldBe` Just (addUTCTime 70 t0)

        it "pulls the refresh earlier by the sampled jitter fraction" $ do
            -- Jitter 0.2 pulls the 0.8 fraction down to 0.6 of the lifetime.
            due <- refreshDueAt defaultRefreshConfig{rcJitter = pure 0.2} t0 (tokenExpiringIn "tok" 1000)
            due `shouldBe` Just (addUTCTime 600 t0)

        it "never schedules a refresh before the issue instant" $ do
            -- A fraction of 0 cannot pull the due time before issue.
            due <- refreshDueAt defaultRefreshConfig{rcRefreshAt = 0} t0 (tokenExpiringIn "tok" 1000)
            due `shouldBe` Just t0

        it "never schedules a refresh for a token with no expiry" $ do
            let noExpiryToken = AuthToken{authSecret = mkSecret "forever", authExpiresAt = Nothing}
            refreshDueAt defaultRefreshConfig t0 noExpiryToken `shouldReturn` Nothing

    describe "onMintSuccess" $
        it "installs the new token and due, clears the refreshing flag, and resets the breaker" $ do
            let token' = tokenExpiringIn "fresh" 1000
                due' = Just (addUTCTime 800 t0)
                st0 = aCacheState{csRefreshing = True, csBreaker = Closed 2}
                st1 = onMintSuccess token' due' st0
            csToken st1 `shouldBe` token'
            csRefreshDue st1 `shouldBe` due'
            -- 'releaseSingleFlight' releases the flag, not this fold, so the success
            -- fold leaves it untouched.
            csRefreshing st1 `shouldBe` True
            csBreaker st1 `shouldBe` initialBreaker

    describe "onMintFailure" $ do
        it "advances the breaker's consecutive-failure count below the threshold" $ do
            let st1 = onMintFailure breakerCfg t0 aCacheState{csBreaker = Closed 1}
            csBreaker st1 `shouldBe` Closed 2

        it "trips the breaker open for the cooldown once the threshold is reached" $ do
            -- breakerCfg threshold = 3, cooldown = 30: the third failure trips it.
            let st1 = onMintFailure breakerCfg t0 aCacheState{csBreaker = Closed 2}
            csBreaker st1 `shouldBe` Open (addUTCTime 30 t0)

        it "leaves the cached token and refreshing flag untouched" $ do
            let st0 = aCacheState{csRefreshing = True}
                st1 = onMintFailure breakerCfg t0 st0
            csToken st1 `shouldBe` csToken st0
            csRefreshDue st1 `shouldBe` csRefreshDue st0
            csRefreshing st1 `shouldBe` True

    describe "releaseSingleFlight" $ do
        it "clears the single-flight flag" $ do
            stateVar <- newTVarIO aCacheState{csRefreshing = True}
            releaseSingleFlight stateVar
            csRefreshing <$> readTVarIO stateVar `shouldReturn` False

        it "is idempotent: releasing an already-clear flag is a no-op" $ do
            stateVar <- newTVarIO aCacheState{csRefreshing = False}
            releaseSingleFlight stateVar
            releaseSingleFlight stateVar
            csRefreshing <$> readTVarIO stateVar `shouldReturn` False

    describe "refresh telemetry reporting" $ do
        it "reports each refresh outcome and the breaker trip → probe → reset transitions" $ do
            (clock, setClock) <- newTestClock t0
            (breakerLog, refreshLog, breakerR, refreshR) <- capturingReporters
            script <-
                newIORef
                    [ pure (tokenExpiringIn "eager" 10) -- the eager construction mint
                    , throwString "mint down" -- the next mint fails, tripping the breaker
                    , pure (tokenExpiringIn "recovered" 200) -- the probe mint recovers it
                    ]
            let cfg =
                    (testConfig clock (scriptedMint script))
                        { rcBreakerThreshold = 1
                        , rcReporters = CredentialReporters breakerR refreshR
                        }
            provider <- refreshingProvider cfg
            -- Construction's eager mint records nothing: the reporters fire on refreshes.
            readIORef breakerLog `shouldReturn` []
            readIORef refreshLog `shouldReturn` []
            -- The eager token (expires t0+10) is expired at t0+20, so the serve mints synchronously
            -- and fails. That trips the breaker and reports the failed refresh with the cached
            -- token's (now zero) remaining lifetime.
            setClock (addUTCTime 20 t0)
            currentToken provider `shouldThrow` anyException
            readIORef breakerLog `shouldReturn` [Open (addUTCTime 50 t0)]
            readIORef refreshLog `shouldReturn` [ReportedFailure (Just 0)]
            -- The 30s cooldown elapses by t0+51, so the breaker admits a half-open probe. It
            -- succeeds, resetting the breaker and recording the fresh token's ttl.
            setClock (addUTCTime 51 t0)
            recovered <- currentToken provider
            unSecret (authSecret recovered) `shouldBe` "recovered"
            readIORef breakerLog `shouldReturn` [Open (addUTCTime 50 t0), HalfOpen, Closed 0]
            readIORef refreshLog
                `shouldReturn` [ReportedFailure (Just 0), ReportedSuccess (Just 149)]

        it "is silent and never throws on that account when wired with the default no-op reporters" $ do
            (clock, setClock) <- newTestClock t0
            script <- newIORef [pure (tokenExpiringIn "eager" 10), throwString "mint down"]
            -- 'defaultRefreshConfig' (via 'testConfig') wires 'noCredentialReporters':
            -- a refresh that trips the breaker records nothing.
            provider <- refreshingProvider (testConfig clock (scriptedMint script)){rcBreakerThreshold = 1}
            setClock (addUTCTime 20 t0)
            currentToken provider `shouldThrow` anyException

    describe "decide" $ do
        it "serves the cached token when it is valid and no refresh is due" $ do
            let token' = tokenExpiringIn "valid" 1000
            stateVar <- newTVarIO aCacheState{csToken = token', csRefreshDue = Just (addUTCTime 800 t0)}
            -- Well before the 800s due instant: serve cached, no claim.
            atomically (decide stateVar t0) `shouldReturn` ServeCached token'
            csRefreshing <$> readTVarIO stateVar `shouldReturn` False

        it "claims the flag and routes to a background refresh once one is due" $ do
            let token' = tokenExpiringIn "valid" 1000
            stateVar <- newTVarIO aCacheState{csToken = token', csRefreshDue = Just (addUTCTime 800 t0)}
            -- Past the due instant but still valid: serve it and claim the flag.
            atomically (decide stateVar (addUTCTime 850 t0)) `shouldReturn` ServeAndRefresh token'
            csRefreshing <$> readTVarIO stateVar `shouldReturn` True

        it "does not re-claim a background refresh that is already in flight" $ do
            let token' = tokenExpiringIn "valid" 1000
            stateVar <-
                newTVarIO aCacheState{csToken = token', csRefreshDue = Just (addUTCTime 800 t0), csRefreshing = True}
            -- Past the due instant but a refresh already holds the flag: just serve.
            atomically (decide stateVar (addUTCTime 850 t0)) `shouldReturn` ServeCached token'

        it "claims the flag and mints synchronously when the token has expired" $ do
            let token' = tokenExpiringIn "stale" 1000
            stateVar <- newTVarIO aCacheState{csToken = token'}
            -- Past expiry, nothing in flight: claim the flag and mint now.
            atomically (decide stateVar (addUTCTime 2000 t0)) `shouldReturn` MintNow
            csRefreshing <$> readTVarIO stateVar `shouldReturn` True

        it "blocks (STM retry) on an expired token when a mint is already in flight" $ do
            let token' = tokenExpiringIn "stale" 1000
            stateVar <- newTVarIO aCacheState{csToken = token', csRefreshing = True}
            -- Expired with a mint already holding the flag: 'decide' must block rather than launch
            -- a second mint. The transaction never commits, so the timed wait yields Nothing.
            timeout 50_000 (atomically (decide stateVar (addUTCTime 2000 t0))) `shouldReturn` Nothing

    describe "refreshingProvider (model-based)" $
        it "agrees with a pure cache/clock/breaker model under random operation sequences" $
            hedgehog refreshModelProperty

{- | Policy constants the model and the provider under test share. They mirror the 'testConfig'
knobs, so the pure model predicts the implementation exactly.
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

-- | The breaker, modelled exactly as 'Ecluse.Core.Credential.Refresh's private one.
data MBreaker
    = MClosed Int
    | MOpen UTCTime
    | MHalfOpen
    deriving stock (Eq, Show)

{- | The pure model of a 'refreshingProvider's cache, mirroring its private @CacheState@.
The expected mint count lets the harness settle a background refresh before each observation.
-}
data RModel (v :: Type -> Type) = RModel
    { rmToken :: AuthToken
    -- ^ The token the cache currently holds (and will serve while valid).
    , rmRefreshDue :: Maybe UTCTime
    -- ^ When a proactive background refresh is due. 'Nothing' never refreshes.
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

-- | Whether a token is usable at @now@. A no-expiry token is always valid.
mTokenValid :: UTCTime -> AuthToken -> Bool
mTokenValid now token = case authExpiresAt token of
    Nothing -> True
    Just expiry -> now < expiry

-- | Whether a proactive refresh is due at @now@ (mirrors @refreshNeeded@).
mRefreshNeeded :: UTCTime -> RModel v -> Bool
mRefreshNeeded now m = case rmRefreshDue m of
    Nothing -> False
    Just due -> now >= due

{- | The refresh instant for a freshly minted token, with zero jitter: the exact
arithmetic of 'Ecluse.Core.Credential.Refresh's @refreshDueAt@ for the shared knobs.
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

{- | Whether the breaker admits a mint at @now@, and the breaker state it leaves behind. An
elapsed 'MOpen' flips to half-open and admits.
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
'MClosed' until the threshold trips it open. Any other state re-opens.
-}
mOnFailure :: UTCTime -> MBreaker -> MBreaker
mOnFailure now = \case
    MClosed n
        | n + 1 >= modelBreakerThreshold -> tripped
        | otherwise -> MClosed (n + 1)
    _ -> tripped
  where
    tripped = MOpen (addUTCTime modelBreakerCooldown now)

{- | The outcome the model predicts for a 'RequestToken' at the current clock: the token the
caller observes, or a thrown error.
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

    -- Apply one mint. Success installs a fresh token and resets the breaker and the refresh
    -- schedule. Failure keeps the cached token and advances the breaker.
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

{- | The mutable wiring a model run drives. The in-flight gauge and its high-water mark
catch a single-flight violation, meaning two mints overlapping.
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

{- | Build a provider the harness clock and mint drive. The mint records its concurrency,
so an overlapping pair breaks single-flight, and hands out the next distinct token.
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

{- | 'AdvanceClock dt': move the injected clock forward by @dt@ seconds. The generated
deltas are non-negative, and advancing alone never triggers a mint.
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

{- | 'RequestToken': call 'currentToken', then wait for the predicted mint count so the
oracle can assert the served token or error, token validity, and single-flight exactly.
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
            -- Non-vacuity: a generated sequence must reach each interesting policy arm often enough
            -- that the oracle is not silently testing only serve-cached. Percentages are per step.
            let tag = coverTag beforeState expected
            H.cover 1 "valid-bg-refresh" (tag == "valid-bg-refresh")
            H.cover 1 "expired-mint-ok" (tag == "expired-mint-ok")
            H.cover 1 "expired-error" (tag == "expired-error")
        ]
  where
    -- Reports the served outcome together with the single-flight high-water mark, so the
    -- pure 'Ensure' can assert on both without touching 'IO'.
    execute RequestInput = liftIO $ do
        result <- try (currentToken provider)
        -- The settle polls for quiescence, which makes the asynchronous background refresh
        -- deterministic under the sequential model.
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

{- | Wait until no mint is in flight and the count has stopped moving. The stability window
guards against sampling the gauge before the async refresh has even started.
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

{- | Drive a random sequence of request, advance-clock, and set-fail operations against a
fresh provider and the pure model, asserting they agree at every step.
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
