-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The emulator must enforce conditional writes and retain objects across fresh adapter instances.
module Ecluse.Runtime.Mirror.State.S3.InternalIntegrationSpec (spec) where

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3
import Conduit (runResourceT)
import Test.Hspec
import UnliftIO.Async (concurrently)

import Ecluse.Core.Security (defaultLimits)
import Ecluse.Integration.Ministack (endpointFor, withMinistack)
import Ecluse.Runtime.Aws.S3 (buildS3Env)
import Ecluse.Runtime.Mirror.State.S3.Internal
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Poll (retryingIO)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = aroundAll withMinistack $
    describe "S3 mirror state conditional persistence" $
        it (caseLabel "integration/mirror-state-s3-internal/native-001" "enforces create, stale revision refusal, and read-after-write across fresh clients") $ \container -> do
            let endpoint = Just (endpointFor container)
                bucket = "mirror-state-cas"
                key = "state/v1/project.json"
            env <- buildS3Env endpoint
            retryingIO 21 500_000 (void (runResourceT (AWS.send env (S3.newCreateBucket (S3.BucketName bucket)))))
            (a, b) <-
                concurrently
                    (writeObject (const pass) env defaultLimits bucket key Nothing "first")
                    (writeObject (const pass) env defaultLimits bucket key Nothing "second")
            let successes = [revision | Right (ObjectStored revision) <- [a, b]]
                collisions = [() | Right ObjectConflict <- [a, b]]
            length successes `shouldBe` 1
            length collisions `shouldBe` 1
            firstRead <- readObject (const pass) env defaultLimits bucket key Nothing >>= expectRight
            case firstRead of
                ObjectLoaded revision body -> do
                    body `shouldSatisfy` (`elem` ["first", "second"])
                    replaced <- writeObject (const pass) env defaultLimits bucket key (Just revision) "extended" >>= expectRight
                    case replaced of
                        ObjectStored current -> do
                            writeObject (const pass) env defaultLimits bucket key (Just revision) "stale" `shouldReturn` Right ObjectConflict
                            restarted <- buildS3Env endpoint
                            readObject (const pass) restarted defaultLimits bucket key Nothing `shouldReturn` Right (ObjectLoaded current "extended")
                            readObject (const pass) restarted defaultLimits bucket key (Just current) `shouldReturn` Right (ObjectUnchanged current)
                        ObjectConflict -> expectationFailure "the current revision unexpectedly conflicted"
                other -> expectationFailure ("missing successfully written state: " <> show other)
