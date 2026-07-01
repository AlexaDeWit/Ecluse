module Ecluse.BootSpec (spec) where

import Prelude hiding (get)

import System.Environment (setEnv, unsetEnv, withArgs)
import Test.Hspec
import UnliftIO (timeout, try)

import Ecluse (run)
import Ecluse.Boot (BootAborted (..), orExit)

runEnv :: [(String, String)]
runEnv =
    [ ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://private.example.test")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET", "https://mirror.example.test")
    , ("ECLUSE_QUEUE_URL", "https://sqs.example.test/q")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN", "mirror-write-token")
    , ("ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER", "static")
    , ("ECLUSE_MOUNTS__PYPI__CREDENTIAL_PROVIDER", "static")
    , ("ECLUSE_MOUNTS__RUBYGEMS__CREDENTIAL_PROVIDER", "static")
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
            outcome <- try (timeout 100000 (withArgs ["proxy"] run)) :: IO (Either BootAborted (Maybe ()))
            unsetEnv "ECLUSE_QUEUE_BACKEND"
            traverse_ (unsetEnv . fst) awsRunEnv
            outcome `shouldBe` Left BootAborted

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
            outcome <- try (timeout 100000 (withArgs ["proxy"] run)) :: IO (Either BootAborted (Maybe ()))
            unsetEnv "ECLUSE_QUEUE_BACKEND"
            traverse_ (unsetEnv . fst) runEnv
            outcome `shouldBe` Left BootAborted

        it "aborts fast at boot when the sqs backend has no ECLUSE_QUEUE_URL" $ do
            traverse_ (uncurry setEnv) awsRunEnv
            unsetEnv "ECLUSE_QUEUE_URL"
            setEnv "ECLUSE_QUEUE_BACKEND" "sqs"
            outcome <- try (timeout 100000 (withArgs ["proxy"] run)) :: IO (Either BootAborted (Maybe ()))
            unsetEnv "ECLUSE_QUEUE_BACKEND"
            traverse_ (unsetEnv . fst) awsRunEnv
            outcome `shouldBe` Left BootAborted

        it "aborts fast at boot when the gcp-artifact-registry credential provider is selected (not built)" $ do
            traverse_ (uncurry setEnv) awsRunEnv
            setEnv "ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER" "gcp-artifact-registry"
            outcome <- try (timeout 100000 (withArgs ["proxy"] run)) :: IO (Either BootAborted (Maybe ()))
            unsetEnv "ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER"
            traverse_ (unsetEnv . fst) awsRunEnv
            outcome `shouldBe` Left BootAborted

        it "aborts fast at boot when codeartifact is selected but its domain cannot be resolved" $ do
            traverse_ (uncurry setEnv) awsRunEnv
            setEnv "ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER" "codeartifact"
            outcome <- try (timeout 100000 (withArgs ["proxy"] run)) :: IO (Either BootAborted (Maybe ()))
            unsetEnv "ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER"
            traverse_ (unsetEnv . fst) awsRunEnv
            outcome `shouldBe` Left BootAborted

    describe "orExit (boot fail-fast)" $ do
        it "yields the value on a Right (a passing boot phase)" $
            orExit (const "unused") (Right 7 :: Either () Int) `shouldReturn` 7

        it "reports the failure and aborts the boot on a Left" $ do
            outcome <- try (orExit (const "boot rejected") (Left ()) :: IO ()) :: IO (Either BootAborted ())
            case outcome of
                Left BootAborted -> pure ()
                Right () -> expectationFailure "expected the boot to abort"
