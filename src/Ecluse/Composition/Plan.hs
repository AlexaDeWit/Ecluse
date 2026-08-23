-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The composition root's boot plan: every decision a boot resolves once the
configuration has loaded, plus the ordered lines that report them. The boot
('Ecluse.Boot.withBootEnv') logs the lines and hands the decisions to the role. The
dry-run checker ('Ecluse.CheckConfig.runCheckConfig') prints the same lines and applies
nothing, so the two transcripts carry the same text in the same order.
-}
module Ecluse.Composition.Plan (
    BootPlan (..),
    resolveBootPlan,

    -- * Locating the configuration document
    defaultConfigPath,
    explicitConfigPath,
    configDocumentPath,
) where

import Data.List (lookup)
import Data.Map.Strict qualified as Map

import Ecluse.Composition (validateComposition)
import Ecluse.Composition.BootError (BootError (MemoryPlanOverrideUnsafe))
import Ecluse.Composition.MemoryPlan (
    MemoryPlan (mpDegradations, mpOverrideViolations, mpQueueMemoryMaxDepth),
    queueTenantDemand,
    resolveMemoryPlan,
 )
import Ecluse.Composition.MirrorQueue (
    MirrorQueuePlan (MemoryBackend, SqsBackend),
    MirrorRuntimePlan (MirrorWith, NoMirroring),
    mirrorQueuePlanWarning,
    planMirrorRuntime,
 )
import Ecluse.Composition.Sizing (resolvePrivateConnections, resolvePublicConnections)
import Ecluse.Config (
    AppConfig (cfgCache, cfgLimits, cfgMounts, cfgQueue, cfgRuntime),
    Config (configApp),
    MountConfig (mntPublicationTarget),
    RuntimeSettings (rtPrivateConnectionsPerHost, rtPublicConnectionsPerHost, rtServeMaxInFlight),
    mountCollisionWarnings,
    mountPostureLines,
    resolvedKeyProvenance,
 )
import Ecluse.Config.Ambient (AmbientAws, ambientAwsFromEnv)
import Ecluse.Core.Text (nonBlank)
import Ecluse.Rts (EffectiveRuntimePlan)
import Ecluse.Runtime.Queue.Sqs (SqsConfig (sqsQueueUrl, sqsRegion))

{- | What a boot resolves: the decisions the role applies, and the lines both entry
points report in the order they hold here.
-}
data BootPlan = BootPlan
    { bpMirrorRuntime :: MirrorRuntimePlan
    -- ^ Whether a mirror runtime runs at all, and which queue backend carries it.
    , bpMemoryPlan :: MemoryPlan
    -- ^ The heap partition every byte-valued bound comes from.
    , bpPrivateConnections :: Int
    -- ^ The private-upstream connection-pool size.
    , bpPublicConnections :: Int
    -- ^ The public-upstream connection-pool size.
    , bpLines :: [Text]
    -- ^ The information lines after the preamble, in emission order.
    , bpWarnings :: [Text]
    -- ^ The warning lines, in emission order.
    }
    deriving stock (Eq, Show)

{- | Resolve every boot decision. The preamble, the config document line and the per-key
provenance lines, comes back even on a refusal, so a refusal naming a key stays traceable.
-}
resolveBootPlan ::
    [(String, String)] ->
    Maybe ByteString ->
    Config ->
    EffectiveRuntimePlan ->
    Int ->
    ([Text], Either [BootError] BootPlan)
resolveBootPlan envVars docBlob config effective fdLimit =
    ( configDocumentLine envVars docBlob : resolvedKeyProvenance envVars docBlob
    , planDecisions (ambientAwsFromEnv envVars) config effective fdLimit
    )

{- Every decision past the preamble, with the lines reporting them. A composition,
mirror-runtime, or override fault refuses. -}
planDecisions :: AmbientAws -> Config -> EffectiveRuntimePlan -> Int -> Either [BootError] BootPlan
planDecisions ambient config effective fdLimit = do
    refuseOn (validateComposition config)
    -- The backend selection precedes the memory plan, because the queue tenant exists
    -- only when that selection picks the in-memory backend.
    mirrorRuntime <- planMirrorRuntime ambient config
    let (memoryPlan, memoryPlanLines) = memoryPlanFor appConfig effective mirrorRuntime
        violations = mpOverrideViolations memoryPlan
    -- A degraded-but-coherent plan boots, because shrinking is what makes it safe. Only
    -- an explicit override the shed ladder cannot work around refuses.
    refuseOn [MemoryPlanOverrideUnsafe violations | not (null violations)]
    let (privateConnections, privateLine) = resolvePrivateConnections (rtPrivateConnectionsPerHost runtimeSettings) fdLimit
        (publicConnections, publicLine) = resolvePublicConnections (rtPublicConnectionsPerHost runtimeSettings) fdLimit
    pure
        BootPlan
            { bpMirrorRuntime = mirrorRuntime
            , bpMemoryPlan = memoryPlan
            , bpPrivateConnections = privateConnections
            , bpPublicConnections = publicConnections
            , bpLines =
                concat
                    [ [privateLine, publicLine]
                    , memoryPlanLines
                    , mirrorRuntimeLines (mpQueueMemoryMaxDepth memoryPlan) mirrorRuntime
                    , mountPostureLines config
                    ]
            , bpWarnings =
                concat
                    [ mpDegradations memoryPlan
                    , mirrorRuntimeWarnings mirrorRuntime
                    , mountCollisionWarnings config
                    ]
            }
  where
    appConfig = configApp config
    runtimeSettings = cfgRuntime appConfig

-- Aggregate a phase's errors into one refusal, or carry on with an error-free phase.
refuseOn :: [BootError] -> Either [BootError] ()
refuseOn = \case
    [] -> Right ()
    errs -> Left errs

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

{- | The path a non-blank @ECLUSE_CONFIG@ names. 'Nothing' leaves 'defaultConfigPath'
standing, where an absent document is not a failure.
-}
explicitConfigPath :: [(String, String)] -> Maybe FilePath
explicitConfigPath envVars = do
    path <- lookup "ECLUSE_CONFIG" envVars
    path <$ nonBlank (toText path)

-- | The config-document path this environment resolves to.
configDocumentPath :: [(String, String)] -> FilePath
configDocumentPath = fromMaybe defaultConfigPath . explicitConfigPath
