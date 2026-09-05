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
    scopedName,
    codeArtifactMirrorUrl,
    codeArtifactEnvVars,
    noMaintenanceBackend,
    clearedRepository,
    malformedAwsEndpoint,
    withoutMirrorTargetUrl,
    withoutMirrorTargetToken,
    withoutPrivateUpstreamUrl,
    withoutQueueUrl,
    overrideEnv,
    expectEnv,
    expectAppConfig,
    expectProviders,
    expectConfig,
    expectValidated,
    bootInputsFor,
    expectPlan,
    expectPlanFor,
) where

import Data.Time (UTCTime (UTCTime), fromGregorian)

import Ecluse.Composition.BootError (BootError (StoreMaintenanceUnavailable), StoreMaintenanceReason (NoControlPlane))
import Ecluse.Composition.Credential (CredentialProviders, initCredentialProviders)
import Ecluse.Composition.Maintenance (ClearedBackend (ClearedCodeArtifact, ClearedProtocol))
import Ecluse.Composition.Plan (
    BootInputs (BootInputs, biConfig, biDocument, biEnvVars, biFdLimit, biRuntimePlan),
    BootPlan,
    BootReport (brOutcome),
    resolveBootPlan,
 )
import Ecluse.Composition.Types (BootRole (BootWithoutPipeline), RegistryRole (MirrorWriter))
import Ecluse.Composition.Validate (ValidatedPlan (vpMounts), VettedMount (vmMount), vetBoot)
import Ecluse.Composition.Vet (runVet)
import Ecluse.Config (AppConfig, Config (configApp), StoreTag (TagRegistry), loadConfig, renderConfigError)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName, mkScope)
import Ecluse.Core.Security (Limits (..))
import Ecluse.Rts (EffectiveAxis (..), EffectiveRuntimePlan (..), Provenance (FromRts))
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (casRepository)
import Ecluse.Test.Credential (noCredentialReporters)

-- | A fixed clock for the injected 'pdNow', never advanced (no timing here).
fixedNow :: UTCTime
fixedNow = UTCTime (fromGregorian 2026 6 23) 0

-- | The resolved 'Limits' the composition root would pass in.
testLimits :: Limits
testLimits = Limits{maxBodyBytes = 12582912, maxVersionCount = 100000, maxArtifactCount = 100000, maxNestingDepth = 64}

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
    , ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__REGISTRY__URL", "https://private.example.test")
    , ("ECLUSE_MOUNTS__NPM__PUBLIC_UPSTREAM__REGISTRY__URL", "https://public.example.test")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET__REGISTRY__URL", "https://mirror.example.test")
    , ("ECLUSE_QUEUE__URL", "https://sqs.us-east-1.amazonaws.com/123456789012/mirror")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET__REGISTRY__TOKEN", "mirror-write-token")
    ]

{- | 'Ecluse.Test.Package.thingName' under the given scope, for the specs that read a
first-party predicate. The unscoped counterpart is @thingName@ itself.
-}
scopedName :: Text -> PackageName
scopedName scope = mkPackageName Npm (Just (mkScope scope)) "thing"

{- | An ambient @AWS_ENDPOINT_URL@ carrying userinfo, which the egress guard refuses. Both entry
points must report it, and neither may echo the credential it holds.
-}
malformedAwsEndpoint :: String
malformedAwsEndpoint = "http://operator:s3cr3t@localhost:9000"

{- | The CodeArtifact repository endpoint the deleting role's fixtures mirror to: the one host
this build carries a store maintenance backend for.
-}
codeArtifactMirrorUrl :: (IsString s) => s
codeArtifactMirrorUrl = "https://acme-111122223333.d.codeartifact.eu-west-1.amazonaws.com/npm/mirror/"

{- | 'staticEnvVars' mirroring to 'codeArtifactMirrorUrl' under its own tag. That tag mints the
write token, so the static one goes with the registry target it belonged to.
-}
codeArtifactEnvVars :: [(String, String)]
codeArtifactEnvVars =
    overrideEnv "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__CODE_ARTIFACT__URL" codeArtifactMirrorUrl $
        withoutMirrorTargetToken (withoutMirrorTargetUrl staticEnvVars)

-- | The deleting role's refusal of 'staticEnvVars', whose mirror target no backend here sweeps.
noMaintenanceBackend :: BootError
noMaintenanceBackend = StoreMaintenanceUnavailable Npm (NoControlPlane TagRegistry)

-- | The repository a cleared CodeArtifact store addresses, 'Nothing' for any other arm.
clearedRepository :: ClearedBackend -> Maybe Text
clearedRepository = \case
    ClearedCodeArtifact store _ -> Just (casRepository store)
    ClearedProtocol{} -> Nothing

-- | Drop the registry mirror-target URL, so a test can declare its own target under any tag.
withoutMirrorTargetUrl :: [(String, String)] -> [(String, String)]
withoutMirrorTargetUrl = filter ((/= "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__REGISTRY__URL") . fst)

-- | Drop the registry mirror-target token, for a tag that mints its own credential.
withoutMirrorTargetToken :: [(String, String)] -> [(String, String)]
withoutMirrorTargetToken = filter ((/= "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__REGISTRY__TOKEN") . fst)

-- | Drop the registry private upstream, so a test can declare its own under any tag.
withoutPrivateUpstreamUrl :: [(String, String)] -> [(String, String)]
withoutPrivateUpstreamUrl = filter ((/= "ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__REGISTRY__URL") . fst)

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
    initCredentialProviders (const noCredentialReporters) (map vmMount (vpMounts plan))
        >>= either (\errs -> fail ("provider init failed: " <> show errs)) pure

-- | Build a 'Config' from an env and an optional document, failing the test on a policy error.
expectConfig :: [(String, String)] -> Maybe ByteString -> IO Config
expectConfig env mDoc =
    either (\errs -> fail ("config load failed: " <> show (map renderConfigError errs))) pure (loadConfig env mDoc)

{- | The pure tier's inputs for a fixture: the layers a configuration loaded from, and the process
facts a boot would have measured.
-}
bootInputsFor :: [(String, String)] -> Maybe ByteString -> Config -> EffectiveRuntimePlan -> BootInputs
bootInputsFor envVars docBlob config effective =
    BootInputs
        { biEnvVars = envVars
        , biDocument = docBlob
        , biConfig = config
        , biRuntimePlan = effective
        , biFdLimit = fdLimit
        }

-- | Resolve the boot plan a checker's own role settles for a fixture, failing on a refusal.
expectPlan :: [(String, String)] -> Maybe ByteString -> Config -> EffectiveRuntimePlan -> IO BootPlan
expectPlan = expectPlanFor BootWithoutPipeline

-- | 'expectPlan' for a named role, for a spec that drives a role's own arm of a later phase.
expectPlanFor :: BootRole -> [(String, String)] -> Maybe ByteString -> Config -> EffectiveRuntimePlan -> IO BootPlan
expectPlanFor role envVars docBlob config effective =
    either
        (\errs -> fail ("boot plan refused: " <> show errs))
        pure
        (brOutcome (resolveBootPlan role (bootInputsFor envVars docBlob config effective)))
