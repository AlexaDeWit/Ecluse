-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | @ecluse check-config@: validate the configuration exactly as a boot would and
print the whole resolved posture, without starting anything.

The subcommand runs the same resolution chain the proxy boots through --
'Ecluse.Config.loadConfig', the runtime plan, the admission and pool sizings, the
memory plan, and the mirror-queue selection -- but applies none of it: no socket
opens, no capability count changes, no re-exec, no cloud call. Failures print the
same aggregated reports a boot would log and exit @2@; a valid configuration
prints per-key provenance and every resolver's decision lines and exits @0@, so an
operator (or a CI step) reads exactly what a boot would do before running one.
-}
module Ecluse.CheckConfig (runCheckConfig) where

import Data.Text.IO qualified as TIO
import System.Environment (getEnvironment)
import System.Exit (ExitCode (ExitFailure))

import Ecluse.Boot (applySecretFileIndirection, readConfigDocument)
import Ecluse.Composition (validateComposition)
import Ecluse.Composition.BootError (BootError (MemoryPlanOverrideUnsafe), renderBootError)
import Ecluse.Composition.MemoryPlan (
    MemoryPlan (mpDegradations, mpOverrideViolations, mpQueueMemoryMaxDepth),
 )
import Ecluse.Composition.MirrorQueue (
    MirrorQueuePlan (MemoryBackend, SqsBackend),
    MirrorRuntimePlan (MirrorWith, NoMirroring),
    memoryQueueBootWarning,
    planMirrorRuntime,
 )
import Ecluse.Composition.Plan (resolveMemoryPlanFor)
import Ecluse.Composition.Sizing (
    openFileSoftLimit,
    resolvePrivateConnections,
    resolvePublicConnections,
 )
import Ecluse.Config (
    AppConfig (cfgRuntime),
    Config (configApp),
    RuntimeSettings (rtCores, rtMaxHeapBytes, rtPrivateConnectionsPerHost, rtPublicConnectionsPerHost),
    loadConfig,
    mountCollisionWarnings,
    mountPostureLines,
    renderConfigError,
    resolvedKeyProvenance,
 )
import Ecluse.Config.Ambient (ambientAwsFromEnv)
import Ecluse.Rts (
    appliedRuntimePlan,
    currentRtsPosture,
    readCgroupLimits,
    renderEffectivePosture,
    resolveRuntimePlan,
 )
import Ecluse.Runtime.Queue.Sqs (SqsConfig (sqsQueueUrl, sqsRegion))

-- | Validate, print the resolved posture, and exit: @0@ valid, @2@ refused.
runCheckConfig :: IO ()
runCheckConfig = do
    rawEnvVars <- getEnvironment
    envVars <- applySecretFileIndirection rawEnvVars >>= either refuseWith pure
    docE <- readConfigDocument envVars
    (docBlob, docPath) <- either refuseWith pure docE
    TIO.putStrLn $ case docBlob of
        Just _ -> "config document: " <> toText docPath
        Nothing -> "config document: none at " <> toText docPath <> " (defaults and environment only)"
    config <- either (refuseWith . renderErrs renderConfigError) pure (loadConfig envVars docBlob)
    -- The pure structural half of the composition (missing adapters, publish
    -- policy): the same validateComposition the boot's composeBindings runs, so a
    -- configuration the proxy would refuse can never validate here.
    case validateComposition config of
        [] -> pass
        errs -> refuseWith (renderErrs renderBootError errs)
    let env = configApp config
        runtimeSettings = cfgRuntime env
    -- The pure half of the posture chain: resolved exactly as a boot would, never
    -- applied (no capability change, no re-exec). The sizings compute from the
    -- plan a successful application would produce ('appliedRuntimePlan'): the
    -- checker's own process posture says nothing about the boot it is checking.
    rts <- currentRtsPosture
    cgroup <- readCgroupLimits
    fdLimit <- openFileSoftLimit
    -- Backend selection precedes the plan, exactly as a boot orders it: the
    -- queue tenant exists only when the memory backend was selected.
    runtimePlan <-
        either
            (refuseWith . renderErrs renderBootError)
            pure
            (planMirrorRuntime (ambientAwsFromEnv envVars) config)
    let plan = resolveRuntimePlan (rtCores runtimeSettings) (rtMaxHeapBytes runtimeSettings) cgroup rts
        effective = appliedRuntimePlan cgroup plan rts
        (_, privateLine) = resolvePrivateConnections (rtPrivateConnectionsPerHost runtimeSettings) fdLimit
        (_, publicLine) = resolvePublicConnections (rtPublicConnectionsPerHost runtimeSettings) fdLimit
        (memoryPlan, memoryPlanLines) = resolveMemoryPlanFor env effective runtimePlan
    traverse_ TIO.putStrLn $
        concat
            [ resolvedKeyProvenance envVars docBlob
            , renderEffectivePosture effective
            , [privateLine, publicLine]
            , memoryPlanLines
            , -- The shed ladder, printed exactly as the boot would warn it: a
              -- degraded-but-coherent plan validates (the plan is made safe by
              -- shrinking); only an explicit override refuses below.
              map ("warning: " <>) (mpDegradations memoryPlan)
            , mirrorRuntimeLines (mpQueueMemoryMaxDepth memoryPlan) runtimePlan
            , mountPostureLines config
            , map ("warning: " <>) (mountCollisionWarnings config)
            ]
    unless (null (mpOverrideViolations memoryPlan)) $
        refuseWith (renderErrs renderBootError [MemoryPlanOverrideUnsafe (mpOverrideViolations memoryPlan)])
    TIO.putStrLn "configuration: valid"
    exitSuccess
  where
    renderErrs :: (e -> Text) -> [e] -> Text
    renderErrs render = unlines . map render

    refuseWith :: Text -> IO a
    refuseWith message = do
        TIO.hPutStrLn stderr message
        TIO.hPutStrLn stderr "configuration: refused"
        exitWith (ExitFailure 2)

    mirrorRuntimeLines :: Int -> MirrorRuntimePlan -> [Text]
    mirrorRuntimeLines memoryDepth = \case
        NoMirroring -> ["mirror runtime: disabled (no mount mirrors; no queue is built and no worker starts)"]
        MirrorWith (SqsBackend sqs) ->
            ["mirror queue: sqs, " <> sqsQueueUrl sqs <> " (region " <> sqsRegion sqs <> ")"]
        MirrorWith MemoryBackend ->
            [ "mirror queue: in-memory (depth " <> show memoryDepth <> ")"
            , "warning: " <> memoryQueueBootWarning
            ]
