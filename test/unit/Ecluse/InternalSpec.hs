-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | How the typed process perimeter classifies an ending, and the status each one exits with.
module Ecluse.InternalSpec (spec) where

import Control.Concurrent qualified as Conc
import Control.Exception (AsyncException (ThreadKilled))
import Data.Text qualified as T
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Test.Hspec
import UnliftIO (throwIO, timeout, try)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Boot (BootAborted (BootAborted))
import Ecluse.Internal (ProcessOutcome (..), exitCodeFor, superviseProcess)

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

    describe "exitCodeFor (the operator-visible exit table)" $
        it "maps each outcome onto its documented status" $ do
            exitCodeFor ShutdownRequested `shouldBe` ExitSuccess
            exitCodeFor (ServiceExited "detail") `shouldBe` ExitFailure 1
            exitCodeFor (BootFault "refusal") `shouldBe` ExitFailure 2
            exitCodeFor RunCancelled `shouldBe` ExitFailure 3

-- | A service fault raised past the perimeter, so a case can watch it classified.
newtype ServiceEscape = ServiceEscape Text
    deriving stock (Eq, Show)

instance Exception ServiceEscape
