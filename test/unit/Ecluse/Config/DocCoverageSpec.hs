-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Config.DocCoverageSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Config (loadConfig)

{- | The golden environment reference: every operator-facing @ECLUSE_*@ spelling,
paired with a value the loader must accept. The two assertions keep it honest in
both directions it can check: each spelling must appear in @USAGE.md@, and each
must actually load. A brand-new config key missed from this list escapes both
(the accepted set is not exported); adding the key here is part of adding it.
-}
documentedEnvVars :: [(String, String)]
documentedEnvVars =
    [ ("ECLUSE_SERVER__PORT", "8081")
    , ("ECLUSE_SERVER__PUBLIC_URL", "https://registry.example.test")
    , ("ECLUSE_SERVER__AUTH_TOKEN", "edge-token")
    , ("ECLUSE_SERVER__HELP_MESSAGE", "ask platform engineering")
    , ("ECLUSE_SERVER__SHUTDOWN_DRAIN_TIMEOUT", "5")
    , ("ECLUSE_QUEUE__URL", "https://sqs.us-east-1.amazonaws.com/123456789012/mirror")
    , ("ECLUSE_QUEUE__MEMORY_MAX_DEPTH", "100")
    , ("ECLUSE_LIMITS__MAX_RESPONSE_BYTES", "1048576")
    , ("ECLUSE_LIMITS__MAX_REQUEST_BYTES", "1048576")
    , ("ECLUSE_LIMITS__MAX_VERSION_COUNT", "100")
    , ("ECLUSE_LIMITS__MAX_NESTING_DEPTH", "16")
    , ("ECLUSE_CACHE__TTL", "30")
    , ("ECLUSE_CACHE__MAX_ENTRIES", "64")
    , ("ECLUSE_CACHE__MAX_BYTES", "1048576")
    , ("ECLUSE_INTEGRITY__MIN_PUBLIC", "sha256")
    , ("ECLUSE_INTEGRITY__MIN_TRUSTED", "sha256")
    , ("ECLUSE_INTEGRITY__DIVERGENCE_POLICY", "warn")
    , ("ECLUSE_EGRESS__ADDITIONAL_BLOCKED_RANGES", "198.18.0.0/15")
    , ("ECLUSE_ADVISORIES__BUCKET", "advisories")
    , ("ECLUSE_ADVISORIES__POLL_INTERVAL", "60")
    , ("ECLUSE_ADVISORIES__COMPILE_INTERVAL", "3600")
    , ("ECLUSE_ADVISORIES__DATA_DIR", "data/osv")
    , ("ECLUSE_ADVISORIES__OSV_EXPORT_BASE_URL", "https://osv.example.test")
    , ("ECLUSE_ADVISORIES__MAX_DATABASE_BYTES", "1048576")
    , ("ECLUSE_RUNTIME__CORES", "2")
    , ("ECLUSE_RUNTIME__MAX_HEAP_BYTES", "268435456")
    , ("ECLUSE_RUNTIME__SERVE_MAX_IN_FLIGHT", "8")
    , ("ECLUSE_RUNTIME__PUBLIC_CONNECTIONS_PER_HOST", "4")
    , ("ECLUSE_RUNTIME__PRIVATE_CONNECTIONS_PER_HOST", "4")
    , ("ECLUSE_OBSERVABILITY__LOG_FORMAT", "json")
    , ("ECLUSE_OBSERVABILITY__TELEMETRY", "off")
    , ("ECLUSE_MOUNTS__NPM__ENABLED", "true")
    , ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://private.example.test")
    , ("ECLUSE_MOUNTS__NPM__PUBLIC_UPSTREAM", "https://registry.npmjs.org")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET", "https://mirror.example.test")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN", "mirror-write-token")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_TOKEN_DURATION", "3600")
    , ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET", "https://publish.example.test")
    , ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN", "publish-token")
    , ("ECLUSE_MOUNTS__NPM__PUBLISH_ALLOW", "@acme")
    , ("ECLUSE_MOUNTS__NPM__MIN_TRUSTED_INTEGRITY", "sha256")
    , ("ECLUSE_MOUNTS__NPM__DIVERGENCE_POLICY", "warn")
    ]

{- | Process-level and indirection spellings: documented, but consumed before (or
beside) config resolution, so only the USAGE assertion applies.
-}
documentedProcessVars :: [String]
documentedProcessVars =
    [ "ECLUSE_CONFIG"
    , "ECLUSE_RULES"
    , "ECLUSE_SERVER__AUTH_TOKEN_FILE"
    , "ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN_FILE"
    , "ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN_FILE"
    ]

spec :: Spec
spec = describe "the environment reference (USAGE.md) covers the accepted variables" $ do
    it "mentions every golden-list spelling" $ do
        usage <- decodeUtf8 <$> readFileBS "USAGE.md"
        let missing =
                [ var
                | var <- map fst documentedEnvVars <> documentedProcessVars
                , not (T.pack var `T.isInfixOf` usage)
                ]
        missing `shouldBe` []

    it "keeps the golden list honest: every listed variable loads together" $
        loadConfig documentedEnvVars Nothing `shouldSatisfy` isRight
