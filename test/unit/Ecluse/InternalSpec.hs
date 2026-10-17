-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | How the typed process perimeter classifies an ending, and the status each one exits with.
module Ecluse.InternalSpec (spec) where

import Amazonka qualified as AWS
import Control.Concurrent qualified as Conc
import Control.Exception (AsyncException (ThreadKilled))
import Data.Text qualified as T
import GHC.Conc (getUncaughtExceptionHandler, setUncaughtExceptionHandler)
import Network.HTTP.Types.Status (status503)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Test.Hspec
import UnliftIO (bracket, throwIO, timeout, try)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Boot (BootAborted (BootAborted))
import Ecluse.Internal (ProcessOutcome (..), exitCodeFor, exitReasonFor, installUncaughtReporter, superviseProcess)
import Ecluse.Runtime.Test.Aws (deliverRefreshFailure, holdsNoPlantedSecret, refreshFailures)
import Ecluse.Runtime.Test.CodeArtifact (refusedMint)
import Ecluse.Test.Log (captureStderr)
import Ecluse.Test.Poll (awaitUntil, threadEnded)
import Ecluse.Test.Stub (closedAuthority, redirectedStatusException, refusedFetch, requestSecrets, signedRequest)

spec :: Spec
spec = do
    describe "superviseProcess (the typed process perimeter)" $ do
        it "classifies a graceful return as ShutdownRequested" $
            superviseProcess (pure ShutdownRequested) `shouldReturn` ShutdownRequested

        it "classifies a boot abort as BootFault carrying the refusal it was raised with" $
            superviseProcess (throwIO (BootAborted "mount npm has no adapter wired in this build"))
                `shouldReturn` BootFault "mount npm has no adapter wired in this build"

        it "classifies a synchronous service escape as ServiceExited with its rendered detail" $ do
            outcome <- superviseProcess (throwIO (ServiceEscape "wiring broke"))
            case outcome of
                ServiceExited detail -> detail `shouldSatisfy` T.isInfixOf "wiring broke"
                other -> expectationFailure ("expected ServiceExited, got " <> show other)

        it "reports a failed mint by its cause at the dialled host:port, never the signed request" $ do
            reason <- exitReasonFor <$> superviseProcess (refusedMint >> pure ShutdownRequested)
            reason `shouldSatisfy` maybe False (T.isPrefixOf ("ecluse: service exited: AWS request failed at " <> closedAuthority <> ": ConnectionFailure"))
            -- The typed text alone, with no exception context after a line break.
            reason `shouldSatisfy` maybe False (not . T.isInfixOf "\n")
            for_ requestSecrets $ \secret -> reason `shouldSatisfy` maybe False (not . T.isInfixOf secret)

        it "reports a credential refresh failure handed to the service's thread by its typed cause, never the request" $ do
            failures <- refreshFailures
            for_ failures $ \failure -> do
                reason <- exitReasonFor <$> superviseProcess (deliverRefreshFailure failure >> pure ShutdownRequested)
                reason `shouldSatisfy` maybe False (T.isPrefixOf "ecluse: service exited: AWS request failed at ")
                reason `shouldSatisfy` maybe False holdsNoPlantedSecret
                reason `shouldSatisfy` maybe False (not . T.isInfixOf "\n")

        it "renders an escaped HTTP fault by its status at the dialled host:port, never the request or the response" $ do
            (authority, err) <- redirectedStatusException status503
            superviseProcess (throwIO err) `shouldReturn` ServiceExited (authority <> ": HTTP 503")

        it "classifies a kill delivery (ThreadKilled) as RunCancelled" $
            superviseProcess (Conc.myThreadId >>= \tid -> Conc.throwTo tid ThreadKilled >> pure ShutdownRequested)
                `shouldReturn` RunCancelled

        it "rethrows a deliberate ExitCode so an intended status is preserved" $ do
            outcome <- try (superviseProcess (throwIO (ExitFailure 130))) :: IO (Either ExitCode ProcessOutcome)
            outcome `shouldBe` Left (ExitFailure 130)

        it "propagates an unrecognised asynchronous exception (not ours to interpret)" $ do
            -- A test's 'timeout' around 'Ecluse.run' must keep its semantics: the private
            -- timeout token passes through rather than reading as a cancellation.
            outcome <- timeout 50000 (superviseProcess (threadDelay 10_000_000 >> pure ShutdownRequested))
            outcome `shouldBe` Nothing

    describe "installUncaughtReporter (the process-wide uncaught-exception handler)" $ do
        it "installs a handler that reports an amazonka error by its typed cause, never the request" $ do
            refused <- refusedFetch =<< signedRequest
            reported <- captureStderr . withUncaughtReporter $ do
                handler <- getUncaughtExceptionHandler
                handler (toException (AWS.TransportError refused))
            reported `shouldSatisfy` T.isPrefixOf ("ecluse: uncaught exception: AWS request failed at " <> closedAuthority)
            reported `shouldSatisfy` holdsNoPlantedSecret
            -- One line, as hPutStrLn ends it: no exception context follows the typed text.
            length (T.lines reported) `shouldBe` 1

        it "reports an amazonka error that ends a forked thread the same way" $ do
            refused <- refusedFetch =<< signedRequest
            reported <- captureStderr . withUncaughtReporter $ do
                thread <- Conc.forkIO (throwIO (AWS.TransportError refused))
                -- The handler runs inside the dying thread, so the thread has reported once it ends.
                void (awaitUntil 1_000_000 1_000 (threadEnded thread))
            reported `shouldSatisfy` T.isPrefixOf ("ecluse: uncaught exception: AWS request failed at " <> closedAuthority)
            reported `shouldSatisfy` holdsNoPlantedSecret
            -- One line, as hPutStrLn ends it: no exception context follows the typed text.
            length (T.lines reported) `shouldBe` 1

    describe "exitCodeFor (the operator-visible exit table)" $
        it "maps each outcome onto its documented status" $ do
            exitCodeFor ShutdownRequested `shouldBe` ExitSuccess
            exitCodeFor (ServiceExited "detail") `shouldBe` ExitFailure 1
            exitCodeFor (BootFault "refusal") `shouldBe` ExitFailure 2
            exitCodeFor RunCancelled `shouldBe` ExitFailure 3

-- Install the reporter for the action, as start-up does, and restore the suite's own handler after.
withUncaughtReporter :: IO () -> IO ()
withUncaughtReporter action =
    bracket getUncaughtExceptionHandler setUncaughtExceptionHandler (const (installUncaughtReporter >> action))

-- | A service fault raised past the perimeter, so a case can watch it classified.
newtype ServiceEscape = ServiceEscape Text
    deriving stock (Eq, Show)

instance Exception ServiceEscape
