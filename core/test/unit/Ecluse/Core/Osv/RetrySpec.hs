-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Core.Osv.RetrySpec (spec) where

import Control.Retry (
    RetryStatus (rsIterNumber),
    defaultRetryStatus,
    limitRetries,
    simulatePolicy,
 )
import Data.Text qualified as T
import Network.HTTP.Client (
    HttpException (..),
    HttpExceptionContent (..),
    defaultRequest,
 )
import Network.HTTP.Types.Status (status404, status502, status503)
import Network.TLS qualified as TLS
import Test.Hspec
import UnliftIO.Exception (throwIO, try)

import Ecluse.Core.Fault (TransportCause (TransportTimeout), TransportFault (tfCause), renderTransportCause, transportRetryable)
import Ecluse.Core.Fault.Http (classifyTransport)
import Ecluse.Core.Osv.Retry
import Ecluse.Test.Log (runQuietKatip)
import Ecluse.Test.Stub (closedAuthority, redirectSecrets, redirectedStatusException, requestSecrets, signedRequest)

-- | The configured feed's @host:port@, which every retry line names.
feed :: Text
feed = "osv.example:443"

-- | A transient (retryable) fetch failure: a connection timeout to osv.dev.
transientFailure :: HttpException
transientFailure = HttpExceptionRequest defaultRequest ConnectionTimeout

-- | A permanent (non-retryable) failure: a malformed URL no retry can mend.
permanentFailure :: HttpException
permanentFailure = InvalidUrlException "http://osv.example/npm/all.zip" "unusable"

-- | A stand-in low-level cause wrapped by a 'ConnectionFailure'. The classifier inspects only the constructor.
data StubCause = StubCause
    deriving stock (Show)

instance Exception StubCause

-- | A TLS handshake refusal, in the shape @http-client@ delivers it: wrapped in an 'InternalException'.
tlsRefusal :: SomeException
tlsRefusal = toException (TLS.HandshakeFailed (TLS.Error_Misc "handshake refused"))

spec :: Spec
spec = do
    describe "the retry schedule (Control.Retry policy)" $ do
        it "the shipped default policy stops after five retries" $ do
            -- 'simulatePolicy' walks the policy without sleeping. Past the retry
            -- budget it must yield Nothing, so the loop can never spin forever.
            delays <- map snd <$> simulatePolicy 8 defaultOsvRetryPolicy
            length (filter isJust delays) `shouldBe` 5
            drop 5 delays `shouldSatisfy` all isNothing

        it "the shipped default policy jitters, so two runs of it differ" $ do
            -- Full jitter randomises each wait in [0, the capped exponential], so repeated
            -- simulations of the same policy differ. A fixed exponential backoff would not.
            runs <- replicateM 5 (map snd <$> simulatePolicy 6 defaultOsvRetryPolicy)
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

        it "does not retry a TLS refusal" $
            -- A rejected handshake is a TransportTls fault, and no retry mends it.
            isRetryableHttpException (HttpExceptionRequest defaultRequest (InternalException tlsRefusal))
                `shouldBe` False

        it "reads every fault outside the status arm off the shared transport table" $ do
            -- The retry decision has one home. This example fails the day this caller
            -- re-derives transience from the http-client constructors.
            let faults =
                    [ HttpExceptionRequest defaultRequest ConnectionTimeout
                    , HttpExceptionRequest defaultRequest NoResponseDataReceived
                    , HttpExceptionRequest defaultRequest (InternalException tlsRefusal)
                    , HttpExceptionRequest defaultRequest (InternalException (toException StubCause))
                    , permanentFailure
                    ]
            map isRetryableHttpException faults `shouldBe` [True, True, False, False, False]
            map isRetryableHttpException faults
                `shouldBe` map (transportRetryable . tfCause . classifyTransport) faults

        it "retries a download cut short, which the shared transport table leaves permanent" $ do
            let cut = map (HttpExceptionRequest defaultRequest) [ResponseBodyTooShort 20 5, IncompleteHeaders, InvalidChunkHeaders]
            map isRetryableHttpException cut `shouldBe` [True, True, True]
            map (transportRetryable . tfCause . classifyTransport) cut `shouldBe` [False, False, False]

        it "classifies a real 502 as retryable and a real 404 as permanent" $ do
            (_, e502) <- redirectedStatusException status502
            (_, e404) <- redirectedStatusException status404
            isRetryableHttpException e502 `shouldBe` True
            isRetryableHttpException e404 `shouldBe` False

    describe "withOsvRetry, the fetch wrapper" $ do
        it "returns the value on success without retrying" $ do
            attempts <- newIORef (0 :: Int)
            result <- runQuietKatip $ withOsvRetry feed (limitRetries 5) $ do
                modifyIORef' attempts (+ 1)
                pure ("ok" :: Text)
            result `shouldBe` "ok"
            readIORef attempts `shouldReturn` 1

        it "retries a transient failure, then succeeds within the budget" $ do
            attempts <- newIORef (0 :: Int)
            result <- runQuietKatip $ withOsvRetry feed (limitRetries 5) $ do
                n <- atomicModifyIORef' attempts (\k -> (k + 1, k + 1))
                if n < 3 then throwIO transientFailure else pure ("recovered" :: Text)
            result `shouldBe` "recovered"
            readIORef attempts `shouldReturn` 3 -- two transient failures, then success
        it "gives up after the retry budget is spent (no tight loop)" $ do
            attempts <- newIORef (0 :: Int)
            outcome <-
                try (runQuietKatip (withOsvRetry feed (limitRetries 3) (modifyIORef' attempts (+ 1) >> throwIO transientFailure))) ::
                    IO (Either HttpException ())
            case outcome of
                Left err -> isRetryableHttpException err `shouldBe` True
                Right () -> expectationFailure "expected the fetch to give up with an exception"
            readIORef attempts `shouldReturn` 4 -- the initial attempt plus three retries, then it stops
        it "re-throws a permanent failure without retrying" $ do
            attempts <- newIORef (0 :: Int)
            outcome <-
                try (runQuietKatip (withOsvRetry feed (limitRetries 5) (modifyIORef' attempts (+ 1) >> throwIO permanentFailure))) ::
                    IO (Either HttpException ())
            case outcome of
                Left err -> isRetryableHttpException err `shouldBe` False
                Right () -> expectationFailure "expected the permanent failure to propagate"
            readIORef attempts `shouldReturn` 1

    describe "transientMessage, the retry log line" $ do
        it "counts attempts from one and names a transport fault by the endpoint it dialled" $ do
            let msg = transientMessage feed defaultRetryStatus{rsIterNumber = 2} transientFailure
            msg `shouldSatisfy` T.isInfixOf "attempt 3"
            msg `shouldSatisfy` T.isInfixOf "backing off before the next retry"
            msg `shouldSatisfy` T.isSuffixOf ("localhost:80: ConnectionTimeout (" <> renderTransportCause TransportTimeout <> ")")

        it "reports the initial attempt as attempt 1, not 0" $
            transientMessage feed defaultRetryStatus transientFailure `shouldSatisfy` T.isInfixOf "attempt 1"

        it "renders a redirected status by its code, never the request or the response" $ do
            (_, err) <- redirectedStatusException status503
            let msg = transientMessage feed defaultRetryStatus err
            msg `shouldSatisfy` T.isSuffixOf (feed <> ": HTTP 503")
            for_ redirectSecrets $ \secret -> msg `shouldSatisfy` (not . T.isInfixOf secret)

        it "renders a timeout by its classified cause and the client's reason, never the request" $ do
            timedOut <- (`HttpExceptionRequest` ConnectionTimeout) <$> signedRequest
            let msg = transientMessage feed defaultRetryStatus timedOut
            msg `shouldSatisfy` T.isSuffixOf (closedAuthority <> ": ConnectionTimeout (" <> renderTransportCause TransportTimeout <> ")")
            for_ requestSecrets $ \secret -> msg `shouldSatisfy` (not . T.isInfixOf secret)
