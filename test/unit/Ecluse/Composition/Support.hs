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
    malformedAwsEndpoint,
    withoutMirrorTargetUrl,
    withoutQueueUrl,
    overrideEnv,
    expectEnv,
    expectAppConfig,
    expectProviders,
    expectConfig,
    expectValidated,
) where

import Data.Time (UTCTime (UTCTime), fromGregorian)

import Ecluse.Composition.Credential (CredentialProviders, initCredentialProviders)
import Ecluse.Composition.Types (RegistryRole (MirrorWriter))
import Ecluse.Composition.Validate (ValidatedPlan (vpMounts), VettedMount (vmMount), vetBoot)
import Ecluse.Composition.Vet (runVet)
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

-- | A pinned file-descriptor soft limit, so both connection-pool sizings are deterministic.
fdLimit :: Int
fdLimit = 1024

{- | A posture with no heap-ceiling datapoint, so the memory plan renders its shipped
fallbacks and every number a golden pins is fixed.
-}
noCeiling :: EffectiveRuntimePlan
noCeiling =
    EffectiveRuntimePlan
        { erpCapabilities = EffectiveAxis{axDesired = 2, axObserved = 2, axProvenance = FromRts}
        , erpMaxHeapBytes = EffectiveAxis{axDesired = Nothing, axObserved = Nothing, axProvenance = FromRts}
        , erpAllocAreaBytes = 4 * 1024 * 1024
        , erpNurseryChunkBytes = Nothing
        , erpContainerMemoryBytes = Nothing
        }

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

{- | An ambient @AWS_ENDPOINT_URL@ carrying userinfo, which the egress guard refuses. Both entry
points must report it, and neither may echo the credential it holds.
-}
malformedAwsEndpoint :: String
malformedAwsEndpoint = "http://operator:s3cr3t@localhost:9000"

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

{- | Vet a resolved 'Config' as a writing role would, failing the test on a refusal. It is the
boot's own pure pass, so a spec builds from exactly what the composition root would.
-}
expectValidated :: Config -> IO ValidatedPlan
expectValidated config =
    either (\errs -> fail ("boot vetting refused: " <> show errs)) pure (snd (runVet MirrorWriter (vetBoot config)))

-- | Build the credential providers from a resolved 'Config', failing the test on a boot error.
expectProviders :: Config -> IO CredentialProviders
expectProviders config = do
    plan <- expectValidated config
    initCredentialProviders noCredentialReporters (map vmMount (vpMounts plan))
        >>= either (\errs -> fail ("provider init failed: " <> show errs)) pure

-- | Build a 'Config' from an env and an optional document, failing the test on a policy error.
expectConfig :: [(String, String)] -> Maybe ByteString -> IO Config
expectConfig env mDoc =
    either (\errs -> fail ("config load failed: " <> show (map renderConfigError errs))) pure (loadConfig env mDoc)
