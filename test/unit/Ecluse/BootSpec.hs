-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.BootSpec (spec) where

import Prelude hiding (get)

import Control.Concurrent qualified as Conc
import Control.Exception (AsyncException (ThreadKilled))
import Data.Text qualified as T
import System.Environment (setEnv, unsetEnv, withArgs)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Test.Hspec
import UnliftIO (throwIO, timeout, try)
import UnliftIO.Concurrent (threadDelay)

import Ecluse (ProcessOutcome (..), exitCodeFor, run, superviseProcess)
import Ecluse.Boot (BootAborted (..), orExit)

runEnv :: [(String, String)]
runEnv =
    [ ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://private.example.test")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET", "https://mirror.example.test")
    , ("ECLUSE_QUEUE_URL", "https://sqs.example.test/q")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN", "mirror-write-token")
    , ("ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER", "static")
    , ("AWS_ACCESS_KEY_ID", "test")
    , ("AWS_SECRET_ACCESS_KEY", "test")
    , ("ECLUSE_PORT", "0")
    ]

awsRunEnv :: [(String, String)]
awsRunEnv =
    [ ("AWS_REGION", "us-east-1")
    ]
        <> runEnv

spec :: Spec
spec = do
    describe "run" $ do
        it "boots from the environment layer alone (no document) and serves" $ do
            unsetEnv "ECLUSE_COVERAGE_QUIET_PARTIAL"
            traverse_ (uncurry setEnv) awsRunEnv
            outcome <- timeout 100000 (withArgs ["proxy"] run)
            traverse_ (unsetEnv . fst) awsRunEnv
            outcome `shouldBe` Nothing

        it "boots with an inline PROXY_CONFIG document and serves" $ do
            unsetEnv "ECLUSE_COVERAGE_QUIET_PARTIAL"
            traverse_ (uncurry setEnv) awsRunEnv
            outcome <- timeout 100000 (withArgs ["proxy"] run)
            traverse_ (unsetEnv . fst) awsRunEnv
            outcome `shouldBe` Nothing

        it "aborts fast at boot when the mirror-queue backend is not built (pubsub)" $ do
            traverse_ (uncurry setEnv) awsRunEnv
            setEnv "ECLUSE_QUEUE_BACKEND" "pubsub"
            outcome <- try (timeout 100000 (withArgs ["proxy"] run)) :: IO (Either ExitCode (Maybe ()))
            unsetEnv "ECLUSE_QUEUE_BACKEND"
            traverse_ (unsetEnv . fst) awsRunEnv
            -- The typed process supervisor maps the boot abort to exit 2.
            outcome `shouldBe` Left (ExitFailure 2)

        it "boots under the in-memory mirror-queue backend (no AWS settings, no ECLUSE_QUEUE_URL) and serves" $ do
            unsetEnv "ECLUSE_COVERAGE_QUIET_PARTIAL"
            unsetEnv "AWS_REGION"
            unsetEnv "ECLUSE_QUEUE_URL"
            traverse_ (uncurry setEnv) (filter ((/= "ECLUSE_QUEUE_URL") . fst) runEnv)
            setEnv "ECLUSE_QUEUE_BACKEND" "memory"
            outcome <- timeout 100000 (withArgs ["proxy"] run)
            unsetEnv "ECLUSE_QUEUE_BACKEND"
            traverse_ (unsetEnv . fst) runEnv
            outcome `shouldBe` Nothing

        it "aborts fast at boot when the sqs backend has no AWS_REGION" $ do
            unsetEnv "AWS_REGION"
            traverse_ (uncurry setEnv) runEnv
            setEnv "ECLUSE_QUEUE_BACKEND" "sqs"
            outcome <- try (timeout 100000 (withArgs ["proxy"] run)) :: IO (Either ExitCode (Maybe ()))
            unsetEnv "ECLUSE_QUEUE_BACKEND"
            traverse_ (unsetEnv . fst) runEnv
            -- The typed process supervisor maps the boot abort to exit 2.
            outcome `shouldBe` Left (ExitFailure 2)

        it "aborts fast at boot when the sqs backend has no ECLUSE_QUEUE_URL" $ do
            traverse_ (uncurry setEnv) awsRunEnv
            unsetEnv "ECLUSE_QUEUE_URL"
            setEnv "ECLUSE_QUEUE_BACKEND" "sqs"
            outcome <- try (timeout 100000 (withArgs ["proxy"] run)) :: IO (Either ExitCode (Maybe ()))
            unsetEnv "ECLUSE_QUEUE_BACKEND"
            traverse_ (unsetEnv . fst) awsRunEnv
            -- The typed process supervisor maps the boot abort to exit 2.
            outcome `shouldBe` Left (ExitFailure 2)

        it "aborts fast at boot when the gcp-artifact-registry credential provider is selected (not built)" $ do
            traverse_ (uncurry setEnv) awsRunEnv
            setEnv "ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER" "gcp-artifact-registry"
            outcome <- try (timeout 100000 (withArgs ["proxy"] run)) :: IO (Either ExitCode (Maybe ()))
            unsetEnv "ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER"
            traverse_ (unsetEnv . fst) awsRunEnv
            -- The typed process supervisor maps the boot abort to exit 2.
            outcome `shouldBe` Left (ExitFailure 2)

        it "aborts fast at boot when codeartifact is selected but its domain cannot be resolved" $ do
            traverse_ (uncurry setEnv) awsRunEnv
            setEnv "ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER" "codeartifact"
            outcome <- try (timeout 100000 (withArgs ["proxy"] run)) :: IO (Either ExitCode (Maybe ()))
            unsetEnv "ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER"
            traverse_ (unsetEnv . fst) awsRunEnv
            -- The typed process supervisor maps the boot abort to exit 2.
            outcome `shouldBe` Left (ExitFailure 2)

    describe "superviseProcess (the typed process perimeter)" $ do
        it "classifies a graceful return as ShutdownRequested" $
            superviseProcess pass `shouldReturn` ShutdownRequested

        it "classifies a boot abort as BootFault" $
            superviseProcess (throwIO BootAborted) `shouldReturn` BootFault

        it "classifies a synchronous service escape as ServiceExited with its rendered detail" $ do
            outcome <- superviseProcess (throwIO (SimulatedServiceFault "wiring broke"))
            case outcome of
                ServiceExited detail -> detail `shouldSatisfy` T.isInfixOf "wiring broke"
                other -> expectationFailure ("expected ServiceExited, got " <> show other)

        it "classifies a kill delivery (ThreadKilled) as RunCancelled" $
            -- A genuine asynchronous delivery (base 'Conc.throwTo', the exact
            -- channel 'killThread' and the RTS use), so the case pins that the
            -- perimeter observes real kills -- an async-hygienic catch would
            -- rethrow it before the classification could run.
            superviseProcess (Conc.myThreadId >>= \tid -> Conc.throwTo tid ThreadKilled)
                `shouldReturn` RunCancelled

        it "rethrows a deliberate ExitCode so an intended status is preserved" $ do
            outcome <- try (superviseProcess (throwIO (ExitFailure 130))) :: IO (Either ExitCode ProcessOutcome)
            outcome `shouldBe` Left (ExitFailure 130)

        it "propagates an unrecognised asynchronous exception (not ours to interpret)" $ do
            -- A test's 'timeout' around 'run' must keep its semantics: the private
            -- timeout token passes through rather than reading as a cancellation.
            outcome <- timeout 50000 (superviseProcess (threadDelay 10_000_000))
            outcome `shouldBe` Nothing

    describe "exitCodeFor (the operator-visible exit table)" $
        it "maps each outcome onto its documented status" $ do
            exitCodeFor ShutdownRequested `shouldBe` ExitSuccess
            exitCodeFor (ServiceExited "detail") `shouldBe` ExitFailure 1
            exitCodeFor BootFault `shouldBe` ExitFailure 2
            exitCodeFor RunCancelled `shouldBe` ExitFailure 3

    describe "orExit (boot fail-fast)" $ do
        it "yields the value on a Right (a passing boot phase)" $
            orExit (const "unused") (Right 7 :: Either () Int) `shouldReturn` 7

        it "reports the failure and aborts the boot on a Left" $ do
            outcome <- try (orExit (const "boot rejected") (Left ()) :: IO ()) :: IO (Either BootAborted ())
            case outcome of
                Left BootAborted -> pure ()
                Right () -> expectationFailure "expected the boot to abort"

-- | A typed stand-in for a service's synchronous escape.
newtype SimulatedServiceFault = SimulatedServiceFault Text
    deriving stock (Eq, Show)

instance Exception SimulatedServiceFault
