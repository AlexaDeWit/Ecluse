-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared fixtures for the specs that load a real configuration: the minimal valid
environment layers, their targeted mutations, and the expect-helpers that load them.
-}
module Ecluse.Composition.Support (
    fixedNow,
    testLimits,
    fdLimit,
    noCeiling,
    staticEnvVars,
    withoutMirrorTargetUrl,
    withoutQueueUrl,
    overrideEnv,
    expectEnv,
    expectAppConfig,
    expectProviders,
    expectConfig,
) where

import Data.Time (UTCTime (UTCTime), fromGregorian)

import Ecluse.Composition.Credential (CredentialProviders, initCredentialProviders)
import Ecluse.Config (AppConfig, Config (configApp), loadConfig, renderConfigError)
import Ecluse.Core.Security (Limits (..))
import Ecluse.Rts (EffectiveAxis (..), EffectiveRuntimePlan (..), Provenance (FromRts))
import Ecluse.Test.Credential (noCredentialReporters)

-- | A fixed clock for the injected 'pdNow', never advanced (no timing here).
fixedNow :: UTCTime
fixedNow = UTCTime (fromGregorian 2026 6 23) 0

-- | The resolved 'Limits' the composition root would pass in.
testLimits :: Limits
testLimits = Limits{maxBodyBytes = 12582912, maxVersionCount = 100000, maxNestingDepth = 64}

{- | A minimal valid environment. The mirror target is a non-CodeArtifact host with a
static write token, so the mount's mirror credential derives to a static provider.
-}
staticEnvVars :: [(String, String)]
staticEnvVars =
    [ ("ECLUSE_SERVER__PUBLIC_URL", "https://registry.example.test")
    , ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://private.example.test")
    , ("ECLUSE_MOUNTS__NPM__PUBLIC_UPSTREAM", "https://public.example.test")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET", "https://mirror.example.test")
    , ("ECLUSE_QUEUE__URL", "https://sqs.us-east-1.amazonaws.com/123456789012/mirror")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN", "mirror-write-token")
    ]

-- | Drop any ECLUSE_MOUNTS__NPM__MIRROR_TARGET entry, so a test can supply its own.
withoutMirrorTargetUrl :: [(String, String)] -> [(String, String)]
withoutMirrorTargetUrl = filter ((/= "ECLUSE_MOUNTS__NPM__MIRROR_TARGET") . fst)

{- | Drop the ECLUSE_QUEUE__URL entry, so a test can exercise the absent-URL rollover
to the bounded in-memory queue.
-}
withoutQueueUrl :: [(String, String)] -> [(String, String)]
withoutQueueUrl = filter ((/= "ECLUSE_QUEUE__URL") . fst)

-- | Override (or insert) one environment entry.
overrideEnv :: String -> String -> [(String, String)] -> [(String, String)]
overrideEnv k v env = (k, v) : filter ((/= k) . fst) env

-- | Load an environment layer, failing the test on a parse error.
expectEnv :: [(String, String)] -> IO AppConfig
expectEnv env = expectAppConfig env Nothing

-- | 'expectConfig' reduced to the resolved application settings.
expectAppConfig :: [(String, String)] -> Maybe ByteString -> IO AppConfig
expectAppConfig env mDoc = configApp <$> expectConfig env mDoc

-- | Build the credential providers from a resolved 'Config', failing the test on a boot error.
expectProviders :: Config -> IO CredentialProviders
expectProviders config =
    initCredentialProviders noCredentialReporters config >>= either (\errs -> fail ("provider init failed: " <> show errs)) pure

-- | Build a 'Config' from an env and an optional document, failing the test on a policy error.
expectConfig :: [(String, String)] -> Maybe ByteString -> IO Config
expectConfig env mDoc =
    either (\errs -> fail ("config load failed: " <> show (map renderConfigError errs))) pure (loadConfig env mDoc)
