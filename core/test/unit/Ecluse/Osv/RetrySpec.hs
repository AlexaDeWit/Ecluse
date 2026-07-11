-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Osv.RetrySpec (spec) where

import Control.Retry (
    RetryStatus (rsIterNumber),
    capDelay,
    defaultRetryStatus,
    fullJitterBackoff,
    limitRetries,
    simulatePolicy,
 )
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Client (
    HttpException (..),
    HttpExceptionContent (..),
    Response,
    checkResponse,
    defaultManagerSettings,
    defaultRequest,
    httpLbs,
    newManager,
    parseRequest,
    throwErrorStatusCodes,
 )
import Network.HTTP.Types.Status (Status, status404, status502)
import Test.Hspec
import UnliftIO.Exception (throwIO, try)

import Katip (Environment (..), initLogEnv)
import Katip.Monadic (KatipContextT, runKatipContextT)

import Ecluse.Core.Osv.Retry
import Ecluse.Test.Stub (stubBaseUrl, withStub)

{- | Run a fetch wrapper in @KatipContextT IO@, which already satisfies
@withOsvRetry@'s @MonadMask@ + @KatipContext@ constraints. The log environment
has no scribes, so the retry log lines are discarded and never clutter output.
-}
runTest :: KatipContextT IO a -> IO a
runTest action = do
    le <- initLogEnv "test" (Environment "test")
    runKatipContextT le () mempty action

-- | A transient (retryable) fetch failure: a connection timeout to osv.dev.
transientFailure :: HttpException
transientFailure = HttpExceptionRequest defaultRequest ConnectionTimeout

-- | A permanent (non-retryable) failure: a malformed URL no retry can mend.
permanentFailure :: HttpException
permanentFailure = InvalidUrlException "http://osv.example/npm/all.zip" "unusable"

{- | A stand-in low-level cause wrapped by a 'ConnectionFailure'. The classifier
inspects only the constructor, not the wrapped cause, so a small typed exception
serves (and keeps us clear of a stringly @userError@, per STYLE section 11).
-}
data StubCause = StubCause
    deriving stock (Show)

instance Exception StubCause

{- | Drive a real (in-process) request against a stub answering with the given
status under @throwErrorStatusCodes@, and return the 'HttpException' it throws.
This yields a genuine 'StatusCodeException' without hand-building a 'Response'.
-}
statusException :: Status -> IO HttpException
statusException status =
    withStub status LBS.empty $ \stub -> do
        manager <- newManager defaultManagerSettings
        req0 <- parseRequest (toString (stubBaseUrl stub) <> "/npm/all.zip")
        let req = req0{checkResponse = throwErrorStatusCodes}
        outcome <- try (httpLbs req manager) :: IO (Either HttpException (Response LBS.ByteString))
        case outcome of
            Left err -> pure err
            Right _ -> fail ("stub unexpectedly succeeded for status " <> show status)

spec :: Spec
spec = do
    describe "the retry schedule (Control.Retry policy)" $ do
        it "the shipped default policy stops after five retries" $ do
            -- 'simulatePolicy' walks the policy without sleeping. Past the retry
            -- budget it must yield Nothing, so the loop can never spin forever.
            delays <- map snd <$> simulatePolicy 8 defaultOsvRetryPolicy
            length (filter isJust delays) `shouldBe` 5
            drop 5 delays `shouldSatisfy` all isNothing

        it "is bounded: it stops after the configured number of retries" $ do
            let policy = limitRetries 4 <> capDelay 60_000_000 (fullJitterBackoff 1_000_000)
            delays <- map snd <$> simulatePolicy 8 policy
            length (filter isJust delays) `shouldBe` 4
            drop 4 delays `shouldSatisfy` all isNothing

        it "is truncated: no single backoff exceeds the cap" $ do
            -- A base that doubles past the cap within the budget: every delay must
            -- still be clamped to the cap.
            let cap = 2_000_000
                policy = limitRetries 8 <> capDelay cap (fullJitterBackoff 1_000_000)
            delays <- mapMaybe snd <$> simulatePolicy 8 policy
            delays `shouldSatisfy` all (<= cap)

        it "is jittered: full jitter does not produce a fixed schedule" $ do
            -- Full jitter randomises each wait in [0, capped exponential], so
            -- repeated simulations of the same policy differ. A non-jittered
            -- exponential backoff would be identical every run.
            let policy = limitRetries 6 <> capDelay 60_000_000 (fullJitterBackoff 1_000_000)
            runs <- replicateM 5 (map snd <$> simulatePolicy 6 policy)
            length (ordNub runs) `shouldSatisfy` (> 1)

    describe "isRetryableStatusCode" $
        it "retries 5xx and the throttling codes, but not other 4xx" $ do
            map isRetryableStatusCode [500, 502, 503, 408, 429] `shouldBe` replicate 5 True
            map isRetryableStatusCode [400, 401, 403, 404, 200] `shouldBe` replicate 5 False

    describe "isRetryableHttpException" $ do
        it "retries connection failures and timeouts" $
            map
                isRetryableHttpException
                [ HttpExceptionRequest defaultRequest ConnectionTimeout
                , HttpExceptionRequest defaultRequest ResponseTimeout
                , HttpExceptionRequest defaultRequest (ConnectionFailure (toException StubCause))
                , HttpExceptionRequest defaultRequest NoResponseDataReceived
                , HttpExceptionRequest defaultRequest ConnectionClosed
                ]
                `shouldBe` replicate 5 True

        it "does not retry a malformed URL" $
            isRetryableHttpException permanentFailure `shouldBe` False

        it "treats a request fault outside the transient set as permanent" $
            isRetryableHttpException
                (HttpExceptionRequest defaultRequest (InternalException (toException StubCause)))
                `shouldBe` False

        it "classifies a real 502 as retryable and a real 404 as permanent" $ do
            e502 <- statusException status502
            e404 <- statusException status404
            isRetryableHttpException e502 `shouldBe` True
            isRetryableHttpException e404 `shouldBe` False

    describe "withOsvRetry -- the fetch wrapper" $ do
        it "returns the value on success without retrying" $ do
            attempts <- newIORef (0 :: Int)
            result <- runTest $ withOsvRetry (limitRetries 5) $ do
                modifyIORef' attempts (+ 1)
                pure ("ok" :: Text)
            result `shouldBe` "ok"
            readIORef attempts `shouldReturn` 1

        it "retries a transient failure, then succeeds within the budget" $ do
            attempts <- newIORef (0 :: Int)
            result <- runTest $ withOsvRetry (limitRetries 5) $ do
                n <- atomicModifyIORef' attempts (\k -> (k + 1, k + 1))
                if n < 3 then throwIO transientFailure else pure ("recovered" :: Text)
            result `shouldBe` "recovered"
            readIORef attempts `shouldReturn` 3 -- two transient failures, then success
        it "gives up after the retry budget is spent (no tight loop)" $ do
            attempts <- newIORef (0 :: Int)
            outcome <-
                try (runTest (withOsvRetry (limitRetries 3) (modifyIORef' attempts (+ 1) >> throwIO transientFailure))) ::
                    IO (Either HttpException ())
            case outcome of
                Left err -> isRetryableHttpException err `shouldBe` True
                Right () -> expectationFailure "expected the fetch to give up with an exception"
            readIORef attempts `shouldReturn` 4 -- the initial attempt plus three retries, then it stops
        it "re-throws a permanent failure without retrying" $ do
            attempts <- newIORef (0 :: Int)
            outcome <-
                try (runTest (withOsvRetry (limitRetries 5) (modifyIORef' attempts (+ 1) >> throwIO permanentFailure))) ::
                    IO (Either HttpException ())
            case outcome of
                Left err -> isRetryableHttpException err `shouldBe` False
                Right () -> expectationFailure "expected the permanent failure to propagate"
            readIORef attempts `shouldReturn` 1

    describe "transientMessage -- the retry log line" $ do
        it "counts attempts from one and names the cause" $ do
            let msg = transientMessage defaultRetryStatus{rsIterNumber = 2} transientFailure
            msg `shouldContain` "attempt 3"
            msg `shouldContain` "backing off before the next retry"
            msg `shouldContain` show transientFailure

        it "reports the initial attempt as attempt 1, not 0" $
            transientMessage defaultRetryStatus transientFailure `shouldContain` "attempt 1"
