{- | Shared fixtures for the composition-root specs: the minimal valid environment
layers, their targeted mutations, and the expect-helpers that load them. Split out
so each composition module's spec ("Ecluse.Composition.CredentialSpec",
"Ecluse.Composition.MirrorQueueSpec", "Ecluse.Composition.SizingSpec", and the
mount-wiring "Ecluse.CompositionSpec") exercises the same environment vocabulary
without duplicating it.
-}
module Ecluse.Composition.Support (
    fixedNow,
    staticEnvVars,
    noTokenEnvVars,
    withoutMirrorTargetUrl,
    withoutQueueUrl,
    withoutCredentialProvider,
    overrideEnv,
    expectEnv,
    expectProviders,
    expectConfig,
) where

import Data.Time (UTCTime (UTCTime), fromGregorian)

import Ecluse.Composition.Credential (CredentialProviders, initCredentialProviders)
import Ecluse.Config (AppConfig, Config (configApp), loadConfig)
import Ecluse.Core.Credential.Refresh (noCredentialReporters)

-- | A fixed clock for the injected 'pdNow'; never advanced (no timing here).
fixedNow :: UTCTime
fixedNow = UTCTime (fromGregorian 2026 6 23) 0

{- | A minimal valid environment with a static mirror-target token, so the env
single-mount's @static@ credential reference resolves.
-}
staticEnvVars :: [(String, String)]
staticEnvVars =
    [ ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://private.example.test")
    , ("ECLUSE_MOUNTS__NPM__PUBLIC_UPSTREAM", "https://public.example.test")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET", "https://mirror.example.test")
    , ("ECLUSE_QUEUE_URL", "https://sqs.example.test/q")
    , ("ECLUSE_QUEUE_BACKEND", "sqs")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN", "mirror-write-token")
    , ("ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER", "static")
    ]

-- | The same environment without the static write token (no provider initialized).
noTokenEnvVars :: [(String, String)]
noTokenEnvVars = filter ((/= "ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN") . fst) staticEnvVars

{- | Drop any ECLUSE_MOUNTS__NPM__MIRROR_TARGET entry, so a test that supplies its own (a CodeArtifact
endpoint to parse) is not shadowed by the base fixture's value.
-}
withoutMirrorTargetUrl :: [(String, String)] -> [(String, String)]
withoutMirrorTargetUrl = filter ((/= "ECLUSE_MOUNTS__NPM__MIRROR_TARGET") . fst)

{- | Drop the ECLUSE_QUEUE_URL entry, so a test can exercise a backend with no queue URL
set (memory needs none; sqs fails loud without one).
-}
withoutQueueUrl :: [(String, String)] -> [(String, String)]
withoutQueueUrl = filter ((/= "ECLUSE_QUEUE_URL") . fst)

-- | Drop the mount's credential-provider selector.
withoutCredentialProvider :: [(String, String)] -> [(String, String)]
withoutCredentialProvider = filter ((/= "ECLUSE_MOUNTS__NPM__CREDENTIAL_PROVIDER") . fst)

-- | Override (or insert) one environment entry.
overrideEnv :: String -> String -> [(String, String)] -> [(String, String)]
overrideEnv k v env = (k, v) : filter ((/= k) . fst) env

-- | Load an environment layer, failing the test on a parse error.
expectEnv :: [(String, String)] -> IO AppConfig
expectEnv = either (\errs -> fail ("env parse failed: " <> show errs)) (pure . configApp) . (`loadConfig` Nothing)

{- | Build the credential providers, failing the test on a boot error (the static-path
examples expect a clean build).
-}
expectProviders :: AppConfig -> IO CredentialProviders
expectProviders env =
    initCredentialProviders noCredentialReporters env >>= either (\errs -> fail ("provider init failed: " <> show errs)) pure

{- | Build a 'Config' from an env + optional document, failing the test on a policy
error (the composeBindings examples want a successfully-loaded config).
-}
expectConfig :: [(String, String)] -> Maybe ByteString -> IO Config
expectConfig env mDoc =
    either (\errs -> fail ("config load failed: " <> show errs)) pure (loadConfig env mDoc)
