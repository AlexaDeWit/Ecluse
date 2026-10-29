-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Configuration fixtures, targeted overrides and boot-refusal capture for composition tests.
module Ecluse.Composition.Support (
    NoCredentials (NoCredentials),
    fixedNow,
    testLimits,
    fdLimit,
    mib,
    gib,
    noCeiling,
    staticEnvVars,
    pubUrlEnv,
    pypiMirrorEnv,
    publicGateEnv,
    cargoGateEnv,
    cargoMirrorEnv,
    cargoCodeArtifactEnv,
    cargoScopeRefusal,
    withRubyInstallBlock,
    rubyInstallRefusal,
    rubyScopeRefusal,
    rubyDeprecationRefusal,
    cargoDeprecationRefusal,
    privateUpstreamUrl,
    collapsingMirrorTarget,
    collapsedMirrorRefusal,
    scopedName,
    mountDocFor,
    npmMountDoc,
    completeMountDoc,
    codeArtifactDomain,
    codeArtifactMirrorUrl,
    codeArtifactEnvVars,
    codeArtifactPypiMirrorUrl,
    codeArtifactPypiCacheUrl,
    twoStoreEnv,
    withObservablePrivate,
    withDredgeablePrivate,
    noMaintenanceBackend,
    privateInventoryRefusal,
    clearedUrl,
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
    captureBoot,
    reportLines,
) where

import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)
import System.Exit (ExitCode)
import UnliftIO (timeout, try)

import Ecluse.Composition.BootError (BootError (MirrorTargetOnMountEndpoint, RuleBlockedOnMount, StoreMaintenanceUnavailable), StoreMaintenanceReason (NoControlPlane, PrivateCacheUnavailable))
import Ecluse.Composition.Credential (CredentialProviders, initCredentialProviders, initTargetCredentialProviders)
import Ecluse.Composition.Maintenance.Plan (ClearedBackend (cbUrl))
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
import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, RubyGems))
import Ecluse.Core.Package (PackageName, mkPackageName, mkScope)
import Ecluse.Core.Rules.Types (RuleType (AllowScopeType, DenyIfDeprecatedType, DenyInstallTimeExecutionType))
import Ecluse.Core.Security (Limits, defaultLimits)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Rts (EffectiveAxis (..), EffectiveRuntimePlan (..), Provenance (FromRts))
import Ecluse.Test.Credential (noCredentialReporters)
import Ecluse.Test.Log (captureStderrWith)

-- | Stands in for amazonka's credential-discovery failure: a boot folds it into a refusal naming this constructor.
data NoCredentials = NoCredentials
    deriving stock (Show)

instance Exception NoCredentials

-- | A fixed clock for the injected 'pdNow', never advanced (no timing here).
fixedNow :: UTCTime
fixedNow = UTCTime (fromGregorian 2026 6 23) 0

-- | The resolved 'Limits' the composition root would pass in.
testLimits :: Limits
testLimits = defaultLimits

-- | A pinned file-descriptor soft limit, so both connection-pool sizings are deterministic.
fdLimit :: Int
fdLimit = 1024

mib :: Int
mib = 1024 * 1024

gib :: Int
gib = 1024 * mib

-- | A posture with no heap ceiling, so the memory plan renders its shipped fallbacks and every golden number is fixed.
noCeiling :: EffectiveRuntimePlan
noCeiling =
    EffectiveRuntimePlan
        { erpCapabilities = EffectiveAxis{axDesired = 2, axObserved = 2, axProvenance = FromRts}
        , erpMaxHeapBytes = EffectiveAxis{axDesired = Nothing, axObserved = Nothing, axProvenance = FromRts}
        , erpAllocAreaBytes = 4 * 1024 * 1024
        , erpAllocAreaProvenance = FromRts
        , erpNurseryChunkBytes = Nothing
        , erpContainerMemoryBytes = Nothing
        }

-- | A minimal valid environment whose registry mirror target takes a static write token, so its credential is a static provider.
staticEnvVars :: [(String, String)]
staticEnvVars =
    [ ("ECLUSE_SERVER__PUBLIC_URL", "https://registry.example.test")
    , ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__REGISTRY__URL", privateUpstreamUrl)
    , ("ECLUSE_MOUNTS__NPM__PUBLIC_UPSTREAM__REGISTRY__URL", "https://public.example.test")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET__REGISTRY__URL", "https://mirror.example.test")
    , ("ECLUSE_QUEUE__URL", "https://sqs.us-east-1.amazonaws.com/123456789012/mirror")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET__REGISTRY__TOKEN", "mirror-write-token")
    ]

-- | The one environment entry every document fixture needs: the public URL a mount derives from.
pubUrlEnv :: [(String, String)]
pubUrlEnv = [("ECLUSE_SERVER__PUBLIC_URL", "https://registry.example.test")]

-- | The npm public gate alone: the smallest environment that boots and serves.
publicGateEnv :: [(String, String)]
publicGateEnv = ("ECLUSE_MOUNTS__NPM__ENABLED", "true") : pubUrlEnv <> [("ECLUSE_SERVER__PORT", "0")]

-- | The native public gate alone, with npm explicitly disabled.
cargoGateEnv :: [(String, String)]
cargoGateEnv = overrideEnv "ECLUSE_MOUNTS__NPM__ENABLED" "false" (("ECLUSE_MOUNTS__CARGO__ENABLED", "true") : publicGateEnv)

-- | The native scope refusal shared by boot and pure validation observations.
cargoScopeRefusal :: BootError
cargoScopeRefusal = RuleBlockedOnMount Cargo AllowScopeType "Cargo package names have no scope."

-- | The native deprecation refusal shared by boot and pure validation observations.
cargoDeprecationRefusal :: BootError
cargoDeprecationRefusal = RuleBlockedOnMount Cargo DenyIfDeprecatedType "Cargo's sparse index supplies no deprecation flag."

-- | Activate a Ruby mount whose explicit installation rule the adapter cannot support.
withRubyInstallBlock :: [(String, String)] -> [(String, String)]
withRubyInstallBlock =
    overrideEnv "ECLUSE_MOUNTS__RUBYGEMS__RULES" "{\"native-builds\":{\"type\":\"DenyInstallTimeExecution\"}}"
        . overrideEnv "ECLUSE_MOUNTS__RUBYGEMS__ENABLED" "true"

-- | The independent native installation refusal used by boot and validation tests.
rubyInstallRefusal :: BootError
rubyInstallRefusal = RuleBlockedOnMount RubyGems DenyInstallTimeExecutionType "RubyGems native builds are normal installation behaviour. This rule is unsupported for the ecosystem."

-- | A shared scope rule also refuses the unscoped Ruby mount.
rubyScopeRefusal :: BootError
rubyScopeRefusal = RuleBlockedOnMount RubyGems AllowScopeType "RubyGems package names have no scope."

-- | A deprecation deny refuses the Ruby mount, whose metadata carries no deprecation flag.
rubyDeprecationRefusal :: BootError
rubyDeprecationRefusal = RuleBlockedOnMount RubyGems DenyIfDeprecatedType "RubyGems metadata supplies no supported deprecation flag."

-- | The private upstream the composition fixtures declare.
privateUpstreamUrl :: (IsString s) => s
privateUpstreamUrl = "https://private.example.test"

-- | Point the npm mirror target at its own private upstream: a writing role advises on the collapse, and the deleting role refuses it.
collapsingMirrorTarget :: [(String, String)] -> [(String, String)]
collapsingMirrorTarget = overrideEnv "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__REGISTRY__URL" privateUpstreamUrl

-- | The deleting role's refusal of 'collapsingMirrorTarget'.
collapsedMirrorRefusal :: BootError
collapsedMirrorRefusal = MirrorTargetOnMountEndpoint Npm Npm "privateUpstream" privateUpstreamUrl

-- | A one-mount configuration document for the given ecosystem, carrying exactly these keys.
mountDocFor :: Text -> [Text] -> ByteString
mountDocFor eco keys = encodeUtf8 ("{\"mounts\":{\"" <> eco <> "\":{" <> T.intercalate "," keys <> "}}}")

-- | 'mountDocFor' on npm, the ecosystem most document fixtures declare.
npmMountDoc :: [Text] -> ByteString
npmMountDoc = mountDocFor "npm"

-- | A complete mount (both upstreams, and a registry mirror target with its token), so a case refuses only on the fact it varies.
completeMountDoc :: Text -> ByteString
completeMountDoc eco =
    mountDocFor
        eco
        [ "\"privateUpstream\":{\"registry\":{\"url\":\"https://private.example.test\"}}"
        , "\"publicUpstream\":{\"registry\":{\"url\":\"https://public.example.test\"}}"
        , "\"mirrorTarget\":{\"registry\":{\"url\":\"https://mirror.example.test\",\"token\":\"t\"}}"
        ]

-- | 'Ecluse.Test.Package.thingName' under the given scope, for the specs that read a first-party predicate.
scopedName :: Text -> PackageName
scopedName scope = mkPackageName Npm (Just (mkScope scope)) "thing"

-- | An ambient @AWS_ENDPOINT_URL@ with userinfo, which the egress guard refuses: both entry points report it, and neither may echo its credential.
malformedAwsEndpoint :: String
malformedAwsEndpoint = "http://operator:s3cr3t@localhost:9000"

-- | The CodeArtifact domain endpoint the fixtures address, in the account and region shapes the host parser accepts.
codeArtifactDomain :: (IsString s) => s
codeArtifactDomain = "https://acme-111122223333.d.codeartifact.eu-west-1.amazonaws.com"

-- | The repository under 'codeArtifactDomain' the deleting role's fixtures mirror to.
codeArtifactMirrorUrl :: (IsString s, Semigroup s) => s
codeArtifactMirrorUrl = codeArtifactDomain <> "/npm/mirror/"

-- | 'staticEnvVars' mirroring to 'codeArtifactMirrorUrl', which mints its own write token, beside a dredgeable Verdaccio cache.
codeArtifactEnvVars :: [(String, String)]
codeArtifactEnvVars =
    withDredgeablePrivate $
        overrideEnv "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__CODE_ARTIFACT__URL" codeArtifactMirrorUrl $
            withoutMirrorTargetToken (withoutMirrorTargetUrl staticEnvVars)

-- | 'codeArtifactEnvVars' plus a PyPI mount that mirrors to and caches from CodeArtifact, so a plan has two mounts' stores.
twoStoreEnv :: [(String, String)]
twoStoreEnv =
    overrideEnv "ECLUSE_MOUNTS__PYPI__ENABLED" "true" $
        overrideEnv "ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM__CODE_ARTIFACT__URL" codeArtifactPypiCacheUrl $
            overrideEnv "ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__CODE_ARTIFACT__URL" codeArtifactPypiMirrorUrl codeArtifactEnvVars

-- | The repository 'twoStoreEnv' mirrors PyPI to.
codeArtifactPypiMirrorUrl :: (IsString s, Semigroup s) => s
codeArtifactPypiMirrorUrl = codeArtifactDomain <> "/pypi/mirror/"

-- | The repository 'twoStoreEnv' caches PyPI in.
codeArtifactPypiCacheUrl :: (IsString s, Semigroup s) => s
codeArtifactPypiCacheUrl = codeArtifactDomain <> "/pypi/internal/"

-- | Declare the existing anonymous private fixture under its observable protocol backend.
withObservablePrivate :: [(String, String)] -> [(String, String)]
withObservablePrivate =
    map
        ( \(key, value) ->
            (if key == "ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__REGISTRY__URL" then "ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__VERDACCIO__URL" else key, value)
        )

-- | The protocol cache has its own maintenance token and explicit deletion consent.
withDredgeablePrivate :: [(String, String)] -> [(String, String)]
withDredgeablePrivate env =
    [ ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__VERDACCIO__TOKEN", "private-maintenance-token")
    , ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__VERDACCIO__PERMIT_DELETION", "true")
    ]
        <> withObservablePrivate env

-- | The deleting role's refusal of 'staticEnvVars', whose mirror target no backend here sweeps.
noMaintenanceBackend :: BootError
noMaintenanceBackend = StoreMaintenanceUnavailable Npm (NoControlPlane TagRegistry)

-- | A generic private registry has no backend that can enumerate its stored versions.
privateInventoryRefusal :: BootError
privateInventoryRefusal = StoreMaintenanceUnavailable Npm (PrivateCacheUnavailable "registry has no inventory control plane")

-- | The endpoint a cleared store reads and deletes over, which its declaration named.
clearedUrl :: ClearedBackend -> Text
clearedUrl = registryUrlText . cbUrl

-- | Drop the registry mirror-target URL, so a test can declare its own target under any tag.
withoutMirrorTargetUrl :: [(String, String)] -> [(String, String)]
withoutMirrorTargetUrl = filter ((/= "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__REGISTRY__URL") . fst)

-- | Drop the registry mirror-target token, for a tag that mints its own credential.
withoutMirrorTargetToken :: [(String, String)] -> [(String, String)]
withoutMirrorTargetToken = filter ((/= "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__REGISTRY__TOKEN") . fst)

-- | Drop the registry private upstream, so a test can declare its own under any tag.
withoutPrivateUpstreamUrl :: [(String, String)] -> [(String, String)]
withoutPrivateUpstreamUrl = filter (not . ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__" `isPrefixOf`) . fst)

-- | Drop @ECLUSE_QUEUE__URL@, so a test reaches the bounded in-memory queue.
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

-- | Vet a 'Config' through the boot's own pass as a writing role would, failing the test on a refusal.
expectValidated :: Config -> IO ValidatedPlan
expectValidated config =
    either (\errs -> fail ("boot vetting refused: " <> show errs)) pure (snd (runVet MirrorWriter (vetBoot config)))

-- | Build the credential providers from a resolved 'Config', failing the test on a boot error.
expectProviders :: Config -> IO CredentialProviders
expectProviders config = do
    plan <- expectValidated config
    initCredentialProviders initTargetCredentialProviders (const (const noCredentialReporters)) (map vmMount (vpMounts plan))
        >>= either (\errs -> fail ("provider init failed: " <> show errs)) pure

-- | Build a 'Config' from an env and an optional document, failing the test on a policy error.
expectConfig :: [(String, String)] -> Maybe ByteString -> IO Config
expectConfig env mDoc =
    either (\errs -> fail ("config load failed: " <> show (map renderConfigError errs))) pure (loadConfig env mDoc)

-- | The pure tier's inputs for a fixture: its configuration layers, and the process facts a boot would measure.
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

-- | A whole boot's status and its standard-error lines. The timeout catches a hung boot, not a slow refusal.
captureBoot :: IO () -> IO (Either ExitCode (Maybe ()), [Text])
captureBoot boot = do
    (outcome, report) <- captureStderrWith (try (timeout 5_000_000 boot))
    pure (outcome, reportLines report)

-- | The non-blank lines of a report.
reportLines :: Text -> [Text]
reportLines = filter (not . T.null) . lines

-- | A generic PyPI mirror with separate native upload and durable metadata declarations.
pypiMirrorEnv :: [(String, String)]
pypiMirrorEnv =
    pubUrlEnv
        <> [ ("ECLUSE_MIRROR_STATE__URL", "s3://mirror-state/records")
           , ("ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM__REGISTRY__URL", "https://private.example.test/")
           , ("ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__REGISTRY__URL", "https://target.example.test/")
           , ("ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__REGISTRY__UPLOAD_URL", "https://target.example.test/legacy/")
           , ("ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__REGISTRY__USERNAME", "publisher")
           , ("ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__REGISTRY__TOKEN", "fixture-token")
           ]

-- | A native mirror with durable source state and token-only upload credentials.
cargoMirrorEnv :: [(String, String)]
cargoMirrorEnv =
    cargoGateEnv
        <> [ ("ECLUSE_MIRROR_STATE__URL", "s3://mirror-state/records")
           , ("ECLUSE_MOUNTS__CARGO__PRIVATE_UPSTREAM__REGISTRY__URL", "https://private.example.test/")
           , ("ECLUSE_MOUNTS__CARGO__MIRROR_TARGET__REGISTRY__URL", "https://target.example.test/")
           , ("ECLUSE_MOUNTS__CARGO__MIRROR_TARGET__REGISTRY__UPLOAD_URL", "https://target.example.test/api/")
           , ("ECLUSE_MOUNTS__CARGO__MIRROR_TARGET__REGISTRY__TOKEN", "fixture-token")
           ]

-- | Separate native CodeArtifact stores with inventory and physical deletion capabilities.
cargoCodeArtifactEnv :: [(String, String)]
cargoCodeArtifactEnv =
    cargoGateEnv
        <> [ ("ECLUSE_MOUNTS__CARGO__PRIVATE_UPSTREAM__CODE_ARTIFACT__URL", codeArtifactDomain <> "/cargo/internal/")
           , ("ECLUSE_MOUNTS__CARGO__MIRROR_TARGET__CODE_ARTIFACT__URL", codeArtifactDomain <> "/cargo/mirror/")
           ]
