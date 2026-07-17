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

        it "ignores ambient AWS_* variables (SDK environment, never config keys)" $ do
            let env =
                    [ ("AWS_REGION", "us-east-1")
                    , ("AWS_ENDPOINT_URL_SQS", "http://localhost:4566")
                    , ("AWS_ACCESS_KEY_ID", "k")
                    , ("AWS_SECRET_ACCESS_KEY", "s")
                    ]
            buildEnvAst env `shouldBe` Object KeyMap.empty

        it "ignores the reserved process-level ECLUSE_CONFIG path override" $ do
            -- ECLUSE_CONFIG is consumed by Ecluse.Boot before resolution; were it not
            -- reserved, it would transliterate to an unknown "config" document key and
            -- fail every boot that uses the override.
            buildEnvAst [("ECLUSE_CONFIG", "/etc/other/config.yaml")] `shouldBe` Object KeyMap.empty

        it "provides a comprehensive regression test for all documented environment variables" $ do
            -- This verifies that all top-level Ecluse documented variables correctly map to camelCase keys
            -- expected by `FromJSON EnvConfig` and `FromJSON ConfigDoc`.
            let env =
                    [ ("ECLUSE_SERVER__PORT", "4873")
                    , ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://private.example.com")
                    , ("ECLUSE_MOUNTS__NPM__PUBLIC_UPSTREAM", "https://public.example.com")
                    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET", "https://mirror.example.com")
                    , ("ECLUSE_QUEUE__URL", "https://sqs.us-east-1.amazonaws.com/123456789012/mirror")
                    , ("ECLUSE_QUEUE__MEMORY_MAX_DEPTH", "50000")
                    , ("ECLUSE_SERVER__AUTH_TOKEN", "secret-token")
                    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN", "mirror-token")
                    , ("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_TOKEN_DURATION", "43200")
                    , ("ECLUSE_SERVER__HELP_MESSAGE", "contact support")
                    , ("ECLUSE_ADVISORIES__COMPILE_INTERVAL", "3600")
                    , ("ECLUSE_SERVER__SHUTDOWN_DRAIN_TIMEOUT", "30")
                    , ("ECLUSE_RUNTIME__SERVE_MAX_IN_FLIGHT", "128")
                    , ("ECLUSE_RUNTIME__PUBLIC_CONNECTIONS_PER_HOST", "10")
                    , ("ECLUSE_CACHE__TTL", "60")
                    , ("ECLUSE_CACHE__MAX_ENTRIES", "1024")
                    , ("ECLUSE_CACHE__MAX_BYTES", "268435456")
                    , ("ECLUSE_LIMITS__MAX_RESPONSE_BYTES", "12582912")
                    , ("ECLUSE_LIMITS__MAX_VERSION_COUNT", "100000")
                    , ("ECLUSE_LIMITS__MAX_NESTING_DEPTH", "64")
                    , ("ECLUSE_OBSERVABILITY__LOG_FORMAT", "json")
                    , ("ECLUSE_OBSERVABILITY__TELEMETRY", "off")
                    , ("ECLUSE_SERVER__PUBLIC_URL", "https://registry.example.com")
                    , ("ECLUSE_INTEGRITY__MIN_PUBLIC", "sha256")
                    , ("ECLUSE_INTEGRITY__MIN_TRUSTED", "sha256")
                    , ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET", "https://publish.example.com")
                    , ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN", "publish-token")
                    , ("ECLUSE_MOUNTS__NPM__PUBLISH_ALLOW", "@test")
                    ]

            let expected =
                    Object $
                        KeyMap.fromList
                            [
                                ( "server"
                                , Object $
                                    KeyMap.fromList
                                        [ ("port", Number 4873)
                                        , ("publicUrl", String "https://registry.example.com")
                                        , ("authToken", String "secret-token")
                                        , ("helpMessage", String "contact support")
                                        , ("shutdownDrainTimeout", Number 30)
                                        ]
                                )
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
                                                    , ("mirrorTargetToken", String "mirror-token")
                                                    , ("mirrorCodeArtifactTokenDuration", Number 43200)
                                                    , ("publicationTarget", String "https://publish.example.com")
                                                    , ("publicationTargetToken", String "publish-token")
                                                    , ("publishAllow", String "@test")
                                                    ]
                                            )
                                        ]
                                )
                            ,
                                ( "queue"
                                , Object $
                                    KeyMap.fromList
                                        [ ("url", String "https://sqs.us-east-1.amazonaws.com/123456789012/mirror")
                                        , ("memoryMaxDepth", Number 50000)
                                        ]
                                )
                            ,
                                ( "advisories"
                                , Object (KeyMap.fromList [("compileInterval", Number 3600)])
                                )
                            ,
                                ( "runtime"
                                , Object $
                                    KeyMap.fromList
                                        [ ("serveMaxInFlight", Number 128)
                                        , ("publicConnectionsPerHost", Number 10)
                                        ]
                                )
                            ,
                                ( "cache"
                                , Object $
                                    KeyMap.fromList
                                        [ ("ttl", Number 60)
                                        , ("maxEntries", Number 1024)
                                        , ("maxBytes", Number 268435456)
                                        ]
                                )
                            ,
                                ( "limits"
                                , Object $
                                    KeyMap.fromList
                                        [ ("maxResponseBytes", Number 12582912)
                                        , ("maxVersionCount", Number 100000)
                                        , ("maxNestingDepth", Number 64)
                                        ]
                                )
                            ,
                                ( "observability"
                                , Object $
                                    KeyMap.fromList
                                        [ ("logFormat", String "json")
                                        , ("telemetry", String "off")
                                        ]
                                )
                            ,
                                ( "integrity"
                                , Object $
                                    KeyMap.fromList
                                        [ ("minPublic", String "sha256")
                                        , ("minTrusted", String "sha256")
                                        ]
                                )
                            ]
            buildEnvAst env `shouldBe` expected
