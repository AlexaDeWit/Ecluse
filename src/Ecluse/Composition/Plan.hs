-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The boot's config-decidable tier: one pure pass from the loaded configuration to every
decision a role's boot settles without touching its environment again, plus the ordered lines
that report them.

'resolveBootPlan' is the whole of that tier. The boot ('Ecluse.Boot.withBootEnv') logs its lines
and hands the plan to the role, and the dry-run checker ('Ecluse.CheckConfig.runCheckConfig')
prints the same lines and applies nothing. Every refusal here is one the checker reaches, as its
own verdict or, for a role it does not vet under, as a 'roleRefusalWarnings' line.
-}
module Ecluse.Composition.Plan (
    -- * The config-decidable tier
    BootInputs (..),
    BootReport (brProvenance, brAdvisories, brOutcome),
    BootPlan (
        bpRole,
        bpValidated,
        bpMirrorRuntime,
        bpMemoryPlan,
        bpLimits,
        bpCacheConfig,
        bpS3Endpoint,
        bpPrivateConnections,
        bpPublicConnections,
        bpLines,
        bpWarnings
    ),
    resolveBootPlan,
    roleRefusalWarnings,

    -- * Locating the configuration document
    defaultConfigPath,
    explicitConfigPath,
    configDocumentPath,
) where

import Data.List (lookup)
import Data.Map.Strict qualified as Map

import Ecluse.Composition.BootError (
    BootError (AwsEndpointMalformed, MemoryPlanOverrideUnsafe),
    renderBootError,
 )
import Ecluse.Composition.MemoryPlan (
    MemoryPlan (mpDegradations, mpMaxResponseBytes, mpOverrideViolations, mpQueueMemoryMaxDepth),
    planCacheConfig,
    queueTenantDemand,
    resolveMemoryPlan,
 )
import Ecluse.Composition.MirrorQueue (
    MirrorQueuePlan (MemoryBackend, SqsBackend),
    MirrorRuntimePlan (MirrorWith, NoMirroring),
    mirrorQueuePlanWarning,
    planMirrorRuntime,
 )
import Ecluse.Composition.MirrorRole (mirrorRoleRefusal)
import Ecluse.Composition.Sizing (resolvePrivateConnections, resolvePublicConnections)
import Ecluse.Composition.Types (BootRole, bootInvocation, everyBootRole, pipelineRoleOf, registryRoleOf)
import Ecluse.Composition.Validate (ValidatedPlan, vetBoot)
import Ecluse.Composition.Vet (decided, runVet)
import Ecluse.Config (
    AppConfig (cfgCache, cfgLimits, cfgMounts, cfgQueue, cfgRuntime),
    Config (configApp),
    LimitsSettings (limMaxNestingDepth, limMaxVersionCount),
    MountConfig (mntPublicationTarget),
    RuntimeSettings (rtPrivateConnectionsPerHost, rtPublicConnectionsPerHost, rtServeMaxInFlight),
    mountPostureLines,
    resolvedKeyProvenance,
 )
import Ecluse.Config.Ambient (AmbientAws, ambientAwsFromEnv, ambientS3Endpoint)
import Ecluse.Core.Security (Limits (Limits, maxBodyBytes, maxNestingDepth, maxVersionCount))
import Ecluse.Core.Server.Cache (CacheConfig)
import Ecluse.Core.Text (nonBlank)
import Ecluse.Rts (EffectiveRuntimePlan)
import Ecluse.Runtime.Aws.Env (AwsEndpoint)
import Ecluse.Runtime.Queue.Sqs (SqsConfig (sqsQueueUrl, sqsRegion))

{- | What the pure tier reads: the loaded configuration, the layers it came from, and the process
facts a boot measured before any decision. Nothing past this is read from the environment again.
-}
data BootInputs = BootInputs
    { biEnvVars :: [(String, String)]
    -- ^ The process environment, for the provenance lines and the ambient AWS override.
    , biDocument :: Maybe ByteString
    -- ^ The config document's bytes, absent where none exists at the resolved path.
    , biConfig :: Config
    -- ^ The loaded configuration every rule vets.
    , biRuntimePlan :: EffectiveRuntimePlan
    -- ^ The runtime posture every byte-valued bound is sized against.
    , biFdLimit :: Int
    -- ^ The open-file soft limit both connection-pool sizes are computed from.
    }

{- | What one role's pass reported. Every field comes back on the refusing path too, so a refusal
naming a config key stays traceable and an advisory beside it is not lost with the plan.
-}
data BootReport = BootReport
    { brProvenance :: [Text]
    -- ^ The config-document line and the per-key provenance lines, reported ahead of any refusal.
    , brAdvisories :: [Text]
    -- ^ The advisories the vetting pass logged, whatever it decided.
    , brOutcome :: Either [BootError] BootPlan
    -- ^ Every refusal the role earned, or the decisions it cleared.
    }

{- | What a boot resolves: the role it starts, the decisions that role applies, and the lines
both entry points report in the order they hold here.
-}
data BootPlan = BootPlan
    { bpRole :: BootRole
    {- ^ The role the pass vetted under. A boot selects the behaviour it starts from this, so
    the severities it cleared and the behaviour it runs name one role.
    -}
    , bpValidated :: ValidatedPlan
    -- ^ What the pure pass cleared: the mounts, the endpoints, and the settings no rule vets.
    , bpMirrorRuntime :: MirrorRuntimePlan
    -- ^ Whether a mirror runtime runs at all, and which queue backend carries it.
    , bpMemoryPlan :: MemoryPlan
    -- ^ The heap partition every byte-valued bound comes from.
    , bpLimits :: Limits
    -- ^ The request-shape bounds every mount serves under.
    , bpCacheConfig :: CacheConfig
    -- ^ The metadata cache's budgets, split out of the memory plan's cache aggregate.
    , bpS3Endpoint :: Maybe AwsEndpoint
    {- ^ The @AWS_ENDPOINT_URL@ override the S3 advisory client dials. 'Nothing' is no override,
    since a malformed one refused the boot.
    -}
    , bpPrivateConnections :: Int
    -- ^ The private-upstream connection-pool size.
    , bpPublicConnections :: Int
    -- ^ The public-upstream connection-pool size.
    , bpLines :: [Text]
    -- ^ The information lines after the provenance block, in emission order.
    , bpWarnings :: [Text]
    -- ^ The warning lines, in emission order.
    }

{- | Decide everything one role's boot settles from the configuration alone. @ecluse check-config@
runs this whole tier, and a boot adds no pure refusal of its own past it.
-}
resolveBootPlan :: BootRole -> BootInputs -> BootReport
resolveBootPlan role inputs =
    BootReport
        { brProvenance = configDocumentLine envVars document : resolvedKeyProvenance envVars document
        , brAdvisories = advisories
        , brOutcome = outcome
        }
  where
    envVars = biEnvVars inputs
    document = biDocument inputs
    (advisories, outcome) = planDecisions role inputs

{- | Every refusal a role other than @own@ earns from this configuration, each named by the command
that earns it. A checker picks no subcommand, so it reports these beside its own verdict.
-}
roleRefusalWarnings :: BootRole -> BootInputs -> [Text]
roleRefusalWarnings own inputs =
    [ bootInvocation role <> " would refuse to boot: " <> renderBootError err
    | role <- everyBootRole
    , role /= own
    , err <- fromLeft [] (brOutcome (resolveBootPlan role inputs))
    ]

-- Every decision past the provenance block, joined by '<*>' so every group reports.
planDecisions :: BootRole -> BootInputs -> ([Text], Either [BootError] BootPlan)
planDecisions role inputs =
    second (fmap (bootPlanFrom role inputs)) . runVet (registryRoleOf role) $
        (,,)
            <$> vetBoot config
            <*> decided (mirrorDecision role ambient (biRuntimePlan inputs) config)
            <*> decided (ambientEndpoint ambient)
  where
    config = biConfig inputs
    ambient = ambientAwsFromEnv (biEnvVars inputs)

{- The ambient @AWS_ENDPOINT_URL@ override the S3 advisory client dials, settled over the same
environment the rest of the pass reads, so it accumulates with the rest rather than after it. -}
ambientEndpoint :: AmbientAws -> Either [BootError] (Maybe AwsEndpoint)
ambientEndpoint = first (pure . AwsEndpointMalformed) . ambientS3Endpoint

{- What the mirror pipeline resolved to: the queue backend, the memory plan sized against it, and
that plan's own lines. The memory plan reads the selected backend, so the two resolve together. -}
data MirrorDecision = MirrorDecision
    { mdRuntime :: MirrorRuntimePlan
    , mdMemoryPlan :: MemoryPlan
    , mdMemoryLines :: [Text]
    }

{- The backend selection precedes the rest, because the queue tenant exists only when that
selection picks the in-memory backend, and the role's fitness is judged against it. -}
mirrorDecision :: BootRole -> AmbientAws -> EffectiveRuntimePlan -> Config -> Either [BootError] MirrorDecision
mirrorDecision role ambient effective config = do
    runtime <- planMirrorRuntime ambient config
    let (memoryPlan, memoryLines) = memoryPlanFor (configApp config) effective runtime
        violations = mpOverrideViolations memoryPlan
    -- A degraded-but-coherent plan boots, because shrinking is what makes it safe. Only
    -- an explicit override the shed ladder cannot work around refuses.
    case roleRefusals runtime <> [MemoryPlanOverrideUnsafe violations | not (null violations)] of
        [] -> Right MirrorDecision{mdRuntime = runtime, mdMemoryPlan = memoryPlan, mdMemoryLines = memoryLines}
        errs -> Left errs
  where
    roleRefusals runtime =
        fromLeft [] (maybe (Right ()) (`mirrorRoleRefusal` runtime) (pipelineRoleOf role))

bootPlanFrom :: BootRole -> BootInputs -> (ValidatedPlan, MirrorDecision, Maybe AwsEndpoint) -> BootPlan
bootPlanFrom role inputs (validated, mirror, s3Endpoint) =
    BootPlan
        { bpRole = role
        , bpValidated = validated
        , bpMirrorRuntime = mdRuntime mirror
        , bpMemoryPlan = memoryPlan
        , bpLimits =
            Limits
                { maxBodyBytes = mpMaxResponseBytes memoryPlan
                , maxVersionCount = limMaxVersionCount (cfgLimits app)
                , maxNestingDepth = limMaxNestingDepth (cfgLimits app)
                }
        , bpCacheConfig = planCacheConfig (cfgCache app) memoryPlan
        , bpS3Endpoint = s3Endpoint
        , bpPrivateConnections = privateConnections
        , bpPublicConnections = publicConnections
        , bpLines =
            concat
                [ [privateLine, publicLine]
                , mdMemoryLines mirror
                , mirrorRuntimeLines (mpQueueMemoryMaxDepth memoryPlan) (mdRuntime mirror)
                , mountPostureLines config
                ]
        , bpWarnings = mpDegradations memoryPlan <> mirrorRuntimeWarnings (mdRuntime mirror)
        }
  where
    config = biConfig inputs
    app = configApp config
    memoryPlan = mdMemoryPlan mirror
    runtimeSettings = cfgRuntime app
    (privateConnections, privateLine) = resolvePrivateConnections (rtPrivateConnectionsPerHost runtimeSettings) (biFdLimit inputs)
    (publicConnections, publicLine) = resolvePublicConnections (rtPublicConnectionsPerHost runtimeSettings) (biFdLimit inputs)

{- The memory plan and its lines. The 'publishConfigured' predicate and this settings
projection live here, so the two entry points cannot disagree on either. -}
memoryPlanFor :: AppConfig -> EffectiveRuntimePlan -> MirrorRuntimePlan -> (MemoryPlan, [Text])
memoryPlanFor appConfig effective mirrorRuntime =
    resolveMemoryPlan
        (cfgCache appConfig)
        (cfgLimits appConfig)
        (cfgQueue appConfig)
        (rtServeMaxInFlight (cfgRuntime appConfig))
        effective
        (queueTenantDemand mirrorRuntime)
        publishConfigured
  where
    publishConfigured = any (isJust . mntPublicationTarget) (Map.elems (cfgMounts appConfig))

{- The document the boot read, or the path it found none at. An absent document is
normal: the environment layer alone is a complete configuration. -}
configDocumentLine :: [(String, String)] -> Maybe ByteString -> Text
configDocumentLine envVars = \case
    Just _ -> "Config document: " <> path
    Nothing -> "Config document: none at " <> path <> " (defaults and environment only)"
  where
    path = toText (configDocumentPath envVars)

{- What the mirror runtime resolved to: the queue an operator gets, or the fact that no
mount mirrors, so the boot builds nothing. -}
mirrorRuntimeLines :: Int -> MirrorRuntimePlan -> [Text]
mirrorRuntimeLines memoryDepth = \case
    NoMirroring -> ["mirror runtime disabled: no mount mirrors, so no queue is built and no worker starts"]
    MirrorWith (SqsBackend sqs) ->
        ["mirror queue: sqs, " <> sqsQueueUrl sqs <> " (region " <> sqsRegion sqs <> ")"]
    MirrorWith MemoryBackend ->
        ["mirror queue: in-memory (depth " <> show memoryDepth <> ")"]

-- The warning the selected backend warrants. A durable backend warrants none.
mirrorRuntimeWarnings :: MirrorRuntimePlan -> [Text]
mirrorRuntimeWarnings = \case
    NoMirroring -> []
    MirrorWith queuePlan -> maybeToList (mirrorQueuePlanWarning queuePlan)

-- | The shipped config-document path. A non-blank @ECLUSE_CONFIG@ relocates it.
defaultConfigPath :: FilePath
defaultConfigPath = "/etc/ecluse/config.yaml"

{- | The path a non-blank @ECLUSE_CONFIG@ names, trimmed of surrounding whitespace. 'Nothing'
leaves 'defaultConfigPath' standing, where an absent document is not a failure.
-}
explicitConfigPath :: [(String, String)] -> Maybe FilePath
explicitConfigPath envVars = do
    raw <- lookup "ECLUSE_CONFIG" envVars
    toString <$> nonBlank (toText raw)

-- | The config-document path this environment resolves to.
configDocumentPath :: [(String, String)] -> FilePath
configDocumentPath = fromMaybe defaultConfigPath . explicitConfigPath
