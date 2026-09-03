-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | @ecluse check-config@: run the boot's config-decidable tier and print the resolved posture,
without starting anything. It runs 'Ecluse.Composition.Plan.resolveBootPlan' once for its own pass
and once per other boot role, and prints the plan's lines, applying none of it: no socket opens, no
capability count changes, no re-exec, no cloud call. It predicts the posture from
'appliedRuntimePlan', because the checker's own process posture is not the boot's. It exits @0@ on
a valid configuration and @2@ where its own pass refuses, and a refusal only some roles earn prints
as a warning naming the command that earns it.
-}
module Ecluse.CheckConfig (runCheckConfig) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (getEnvironment)

import Ecluse.Boot (applySecretFileIndirection, orExit, readConfigDocument, refuseBoot)
import Ecluse.Composition.BootError (renderBootErrors)
import Ecluse.Composition.Plan (
    BootInputs (BootInputs, biConfig, biDocument, biEnvVars, biFdLimit, biRuntimePlan),
    BootPlan (bpLines, bpWarnings),
    BootReport (brAdvisories, brOutcome, brProvenance),
    resolveBootPlan,
    roleRefusalWarnings,
 )
import Ecluse.Composition.Sizing (openFileSoftLimit)
import Ecluse.Composition.Types (BootRole (BootWithoutPipeline))
import Ecluse.Config (
    AppConfig (cfgRuntime),
    Config (configApp),
    RuntimeSettings (rtCores, rtCoresCeiling, rtMaxHeapBytes),
    loadConfig,
    renderConfigError,
 )
import Ecluse.Rts (
    RuntimeOverrides (RuntimeOverrides, roCores, roCoresCeiling, roMaxHeapBytes),
    appliedRuntimePlan,
    currentRtsPosture,
    readCgroupLimits,
    renderEffectivePosture,
    renderPostureWarnings,
    resolveRuntimePlan,
 )

{- | Validate the configuration and print the resolved posture. A valid configuration
returns (exit @0@) and a refused one aborts (exit @2@).
-}
runCheckConfig :: IO ()
runCheckConfig = do
    rawEnvVars <- getEnvironment
    envVars <- applySecretFileIndirection rawEnvVars >>= orRefuse id
    docBlob <- readConfigDocument envVars >>= orRefuse id
    config <- orRefuse (T.unlines . map renderConfigError) (loadConfig envVars docBlob)
    rts <- currentRtsPosture
    cgroup <- readCgroupLimits
    fdLimit <- openFileSoftLimit
    let runtimeSettings = cfgRuntime (configApp config)
        overrides =
            RuntimeOverrides
                { roCores = rtCores runtimeSettings
                , roCoresCeiling = rtCoresCeiling runtimeSettings
                , roMaxHeapBytes = rtMaxHeapBytes runtimeSettings
                }
        runtimePlan = resolveRuntimePlan overrides cgroup rts
        effective = appliedRuntimePlan cgroup runtimePlan rts
    -- The checker runs no mirror pipeline and prunes no store, so its own pass vets under the
    -- writing roles' severities. Every other role's verdict follows below.
    let inputs =
            BootInputs
                { biEnvVars = envVars
                , biDocument = docBlob
                , biConfig = config
                , biRuntimePlan = effective
                , biFdLimit = fdLimit
                }
        report = resolveBootPlan BootWithoutPipeline inputs
    -- The boot logs these posture lines and warnings from 'Ecluse.Rts.applyRuntimePosture',
    -- which the checker never runs. They stand in that position here.
    traverse_ TIO.putStrLn (renderEffectivePosture effective)
    traverse_ warn (renderPostureWarnings effective)
    -- Printed ahead of every refusable phase, exactly where the boot logs it.
    traverse_ TIO.putStrLn (brProvenance report)
    bootPlan <- case brOutcome report of
        Left errs -> do
            traverse_ warn (brAdvisories report)
            refuseBoot (renderBootErrors errs <> "\nconfiguration: refused")
        Right plan -> pure plan
    traverse_ TIO.putStrLn (bpLines bootPlan)
    traverse_ warn (bpWarnings bootPlan)
    traverse_ warn (brAdvisories report)
    -- A configuration one role refuses and another boots is a normal deployment, so the other
    -- roles' refusals report as warnings rather than deciding the exit status.
    traverse_ warn (roleRefusalWarnings BootWithoutPipeline inputs)
    TIO.putStrLn "configuration: valid"
  where
    {- Carry the aggregated report and the verdict into the boot's own typed abort, which
    'Ecluse.superviseProcess' maps to exit 2 and 'Ecluse.run' reports. -}
    orRefuse :: (e -> Text) -> Either e a -> IO a
    orRefuse render = orExit (\err -> render err <> "\nconfiguration: refused")

    -- Standard output carries no severity field, so the prefix stands in for the boot's
    -- katip WarningS.
    warn :: Text -> IO ()
    warn = TIO.putStrLn . ("warning: " <>)
