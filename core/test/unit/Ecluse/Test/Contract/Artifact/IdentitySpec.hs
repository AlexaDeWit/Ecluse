-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Prepared inputs must remain bound to their source, run, plan and image bytes.
module Ecluse.Test.Contract.Artifact.IdentitySpec (spec) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode (ExitFailure))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Ecluse.Test.Contract.Artifact.Identity
import Ecluse.Test.Contract.Manifest
import Ecluse.Test.Contract.Result
import Ecluse.Test.Env (withEnvOverrides)

spec :: Spec
spec = do
    describe "preparation identity" $ do
        it "rejects each identity substitution independently" $ do
            check prepared `shouldBe` []
            forM_ [execution{identitySource = "other"}, execution{identityArchitecture = "amd64"}, execution{identityRunId = "other"}, execution{identityImage = "tag"}, execution{identityPlanDigest = "wrong"}] $ \changed ->
                check prepared{preparedExecution = changed} `shouldSatisfy` (not . null)
            check prepared{preparedSchemaVersion = 2} `shouldSatisfy` (not . null)
            check prepared{preparedArchiveDigest = "wrong"} `shouldSatisfy` (not . null)
        it "rejects a substituted loaded configuration" $
            preparationProblems "source" "arm64" Nothing sha sha (Just ("sha256:" <> T.replicate 64 "b")) prepared
                `shouldBe` ["loaded image configuration mismatch"]
        it "quotes arbitrary source and run identifiers as shell data" $
            identityEnvironment execution{identitySource = "a'$(false)"} `shouldSatisfy` T.isInfixOf "export ECLTEST_SOURCE='a'\"'\"'$(false)'\n"
    describe "immutable preparation" $ do
        it "refuses a second reservation after an interrupted preparation" $
            withSystemTempDirectory "artifact" $ \root -> do
                beginPreparation root
                beginPreparation root `shouldThrow` (== ExitFailure 1)
        it "refuses existing inputs before reserving the root" $
            withSystemTempDirectory "artifact" $ \root -> do
                BS.writeFile (root </> "plan.json") "stale"
                beginPreparation root `shouldThrow` (== ExitFailure 1)
        it "writes a typed identity and rejects changed distributed bytes" $
            withSystemTempDirectory "artifact" $ \root -> withEnvOverrides environment $ do
                beginPreparation root
                BS.writeFile (root </> "plan.json") "plan\n"
                BS.writeFile (root </> "docker-image.tar") "archive\n"
                finishPreparation root ("sha256:" <> sha)
                verified <- verifyPreparation root Nothing
                identityPlanDigest verified `shouldBe` digest "plan\n"
                doesFileExist (root </> "identity.env") `shouldReturn` True
                removeFile (root </> "docker-image.tar")
                verifyPlan root `shouldReturn` verified
                BS.writeFile (root </> "docker-image.tar") "archive\n"
                finishPreparation root ("sha256:" <> sha) `shouldThrow` (== ExitFailure 1)
                BS.writeFile (root </> "plan.json") "changed\n"
                verifyPreparation root Nothing `shouldThrow` (== ExitFailure 1)
                BS.writeFile (root </> "plan.json") "plan\n"
                BS.writeFile (root </> "docker-image.tar") "changed\n"
                verifyPreparation root Nothing `shouldThrow` (== ExitFailure 1)
        it "hashes a streamed file as the exact artifact bytes" $
            withSystemTempDirectory "artifact" $ \root -> do
                let bytes = BS.replicate 1000000 42
                BS.writeFile (root </> "large") bytes
                digestFile (root </> "large") `shouldReturn` digest bytes

check :: PreparedIdentity -> [Text]
check = preparationProblems "source" "arm64" (Just "run") sha sha Nothing

sha :: Text
sha = T.replicate 64 "a"

execution :: ExecutionIdentity
execution = ExecutionIdentity "source" ("sha256:" <> sha) "arm64" sha "run"

prepared :: PreparedIdentity
prepared = PreparedIdentity 1 execution sha

environment :: [(String, String)]
environment = [("ECLTEST_SOURCE", "source"), ("ECLTEST_ARCHITECTURE", "aarch64"), ("ECLTEST_RUN_ID", "run")]
