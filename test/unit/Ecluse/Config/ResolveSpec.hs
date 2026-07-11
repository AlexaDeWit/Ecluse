-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Config.ResolveSpec (spec) where

import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Ecluse.Config.Resolve (buildEnvAst, deepMerge)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "deepMerge" $ do
        it "merges objects right-biased" $ do
            let left =
                    Object $
                        KeyMap.fromList
                            [ ("a", Number 1)
                            , ("b", Object $ KeyMap.fromList [("x", Number 10)])
                            ]
                right =
                    Object $
                        KeyMap.fromList
                            [ ("b", Object $ KeyMap.fromList [("y", Number 20)])
                            , ("c", Number 3)
                            ]
                expected =
                    Object $
                        KeyMap.fromList
                            [ ("a", Number 1)
                            , ("b", Object $ KeyMap.fromList [("x", Number 10), ("y", Number 20)])
                            , ("c", Number 3)
                            ]
            deepMerge left right `shouldBe` expected

        it "overwrites non-objects completely" $ do
            let left = Object $ KeyMap.fromList [("a", Number 1)]
                right = Array mempty
            deepMerge left right `shouldBe` Array mempty

        it "cascades default <- file <- env correctly" $ do
            let defaultAst = Object $ KeyMap.fromList [("rules", Object $ KeyMap.fromList [("min-age", Object $ KeyMap.fromList [("type", String "AllowIfOlderThan")])])]
                fileAst = Object $ KeyMap.fromList [("rules", Object $ KeyMap.fromList [("min-age", Object $ KeyMap.fromList [("ageSeconds", Number 86400)])])]
                envAst = Object $ KeyMap.fromList [("rules", Object $ KeyMap.fromList [("deny-scripts", Object $ KeyMap.fromList [("type", String "DenyInstallTimeExecution")])])]

            let merged = deepMerge defaultAst (deepMerge fileAst envAst)
            let expected =
                    Object $
                        KeyMap.fromList
                            [
                                ( "rules"
                                , Object $
                                    KeyMap.fromList
                                        [ ("min-age", Object $ KeyMap.fromList [("type", String "AllowIfOlderThan"), ("ageSeconds", Number 86400)])
                                        , ("deny-scripts", Object $ KeyMap.fromList [("type", String "DenyInstallTimeExecution")])
                                        ]
                                )
                            ]
            merged `shouldBe` expected

    describe "buildEnvAst" $ do
        it "filters and nests environment variables" $ do
            let env =
                    [ ("IGNORE_ME", "1")
                    , ("ECLUSE_MOUNTS__NPM__PORT", "8080")
                    , ("ECLUSE_DEBUG", "true")
                    , ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://default.internal")
                    , ("ECLUSE_A__B__C", "hello")
                    ]
            let expected =
                    Object $
                        KeyMap.fromList
                            [
                                ( "mounts"
                                , Object $
                                    KeyMap.fromList
                                        [
                                            ( "npm"
                                            , Object $
                                                KeyMap.fromList
                                                    [ ("port", Number 8080)
                                                    , ("privateUpstream", String "https://default.internal")
                                                    ]
                                            )
                                        ]
                                )
                            , ("debug", Bool True)
                            ,
                                ( "a"
                                , Object $
                                    KeyMap.fromList
                                        [
                                            ( "b"
                                            , Object $
                                                KeyMap.fromList
                                                    [ ("c", String "hello")
                                                    ]
                                            )
                                        ]
                                )
                            ]
            buildEnvAst env `shouldBe` expected

        it "handles empty values and strings correctly" $ do
            let env = [("ECLUSE_EMPTY", "")]
            buildEnvAst env `shouldBe` Object (KeyMap.fromList [("empty", String "")])

        it "provides a comprehensive regression test for all documented environment variables" $ do
            -- This verifies that all top-level Ecluse documented variables correctly map to camelCase keys
            -- expected by `FromJSON EnvConfig` and `FromJSON ConfigDoc`.
            let env =
                    [ ("ECLUSE_PORT", "4873")
                    , ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://private.example.com")
                    , ("ECLUSE_MOUNTS__NPM__PUBLIC_UPSTREAM", "https://public.example.com")
                    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET", "https://mirror.example.com")
                    , ("ECLUSE_QUEUE_BACKEND", "sqs")
                    , ("ECLUSE_QUEUE_URL", "https://sqs.example.com")
                    , ("ECLUSE_QUEUE_MEMORY_MAX_DEPTH", "50000")
                    , ("AWS_REGION", "us-east-1")
                    , ("AWS_ENDPOINT_URL_SQS", "http://localhost:4566")
                    , ("ECLUSE_GOOGLE_PROJECT", "test-project")
                    , ("ECLUSE_AUTH_TOKEN", "secret-token")
                    , ("ECLUSE_MOUNTS__NPM__RESPECT_UPSTREAM_TARBALL_HOST", "true")
                    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN", "mirror-token")
                    , ("ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER", "static")
                    , ("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN", "domain")
                    , ("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_DOMAIN_OWNER", "123456789012")
                    , ("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_REGION", "us-east-1")
                    , ("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_TOKEN_DURATION", "43200")
                    , ("ECLUSE_HELP_MESSAGE", "contact support")
                    , ("ECLUSE_CVE_SYNC_INTERVAL", "3600")
                    , ("ECLUSE_SHUTDOWN_DRAIN_TIMEOUT", "30")
                    , ("ECLUSE_SERVE_MAX_IN_FLIGHT", "128")
                    , ("ECLUSE_PUBLIC_CONNECTIONS_PER_HOST", "10")
                    , ("ECLUSE_CACHE_TTL", "60")
                    , ("ECLUSE_CACHE_MAX_ENTRIES", "1024")
                    , ("ECLUSE_CACHE_MAX_BYTES", "268435456")
                    , ("ECLUSE_MAX_RESPONSE_BYTES", "12582912")
                    , ("ECLUSE_MAX_VERSION_COUNT", "100000")
                    , ("ECLUSE_MAX_NESTING_DEPTH", "64")
                    , ("ECLUSE_LOG_FORMAT", "json")
                    , ("ECLUSE_TELEMETRY", "off")
                    , ("ECLUSE_PUBLIC_URL", "https://registry.example.com")
                    , ("ECLUSE_MIN_PUBLIC_INTEGRITY", "sha256")
                    , ("ECLUSE_MIN_TRUSTED_INTEGRITY", "sha256")
                    , ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET", "https://publish.example.com")
                    , ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN", "publish-token")
                    , ("ECLUSE_MOUNTS__NPM__PUBLISH_SCOPES", "@test")
                    ]

            let expected =
                    Object $
                        KeyMap.fromList
                            [ ("port", Number 4873)
                            ,
                                ( "mounts"
                                , Object $
                                    KeyMap.fromList
                                        [
                                            ( "npm"
                                            , Object $
                                                KeyMap.fromList
                                                    [ ("privateUpstream", String "https://private.example.com")
                                                    , ("publicUpstream", String "https://public.example.com")
                                                    , ("mirrorTarget", String "https://mirror.example.com")
                                                    , ("respectUpstreamTarballHost", Bool True)
                                                    , ("mirrorTargetToken", String "mirror-token")
                                                    , ("credentialProvider", String "static")
                                                    , ("mirrorCodeArtifactDomain", String "domain")
                                                    , ("mirrorCodeArtifactDomainOwner", Number 123456789012)
                                                    , ("mirrorCodeArtifactRegion", String "us-east-1")
                                                    , ("mirrorCodeArtifactTokenDuration", Number 43200)
                                                    , ("publicationTarget", String "https://publish.example.com")
                                                    , ("publicationTargetToken", String "publish-token")
                                                    , ("publishScopes", String "@test")
                                                    ]
                                            )
                                        ]
                                )
                            , ("queueBackend", String "sqs")
                            , ("queueUrl", String "https://sqs.example.com")
                            , ("queueMemoryMaxDepth", Number 50000)
                            , ("awsEndpointUrlSqs", String "http://localhost:4566")
                            , ("awsRegion", String "us-east-1")
                            , ("googleProject", String "test-project")
                            , ("authToken", String "secret-token")
                            , ("helpMessage", String "contact support")
                            , ("cveSyncInterval", Number 3600)
                            , ("shutdownDrainTimeout", Number 30)
                            , ("serveMaxInFlight", Number 128)
                            , ("publicConnectionsPerHost", Number 10)
                            , ("cacheTtl", Number 60)
                            , ("cacheMaxEntries", Number 1024)
                            , ("cacheMaxBytes", Number 268435456)
                            , ("maxResponseBytes", Number 12582912)
                            , ("maxVersionCount", Number 100000)
                            , ("maxNestingDepth", Number 64)
                            , ("logFormat", String "json")
                            , ("telemetry", String "off")
                            , ("publicUrl", String "https://registry.example.com")
                            , ("minPublicIntegrity", String "sha256")
                            , ("minTrustedIntegrity", String "sha256")
                            ]
            buildEnvAst env `shouldBe` expected
