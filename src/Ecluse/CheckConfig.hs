-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | @ecluse check-config@: validate the configuration without a role and print the resolved
posture, without starting anything. It hands the loaded config to
'Ecluse.Composition.Plan.resolveBootPlan', takes its verdict through the boot's own
'bootRefusals', and prints the plan's lines, applying none of it: no socket opens, no
capability count changes, no re-exec, no cloud call. It predicts the posture from
'appliedRuntimePlan', because the checker's own process posture is not the boot's. It exits
@0@ on a valid configuration and @2@ on a refused one, where a collapse that only
@ecluse dredger@ refuses is a warning rather than a refusal.
-}
module Ecluse.CheckConfig (runCheckConfig) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (getEnvironment)

import Ecluse.Boot (applySecretFileIndirection, bootRefusals, orExit, readConfigDocument)
import Ecluse.Composition.BootError (renderBootError)
import Ecluse.Composition.Plan (BootPlan (bpLines, bpWarnings), resolveBootPlan)
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
    -- The checker runs no mirror pipeline and prunes no store, so it vets under the writing
    -- roles' severities and earns no pipeline refusal of its own.
    let (preamble, planE) = resolveBootPlan BootWithoutPipeline envVars docBlob config effective fdLimit
    -- The boot logs these posture lines and warnings from 'Ecluse.Rts.applyRuntimePosture',
    -- which the checker never runs. They stand in that position here.
    traverse_ TIO.putStrLn (renderEffectivePosture effective)
    traverse_ (TIO.putStrLn . ("warning: " <>)) (renderPostureWarnings effective)
    -- Printed ahead of every refusable phase, exactly where the boot logs it.
    traverse_ TIO.putStrLn preamble
    -- The boot resolves the ambient AWS_ENDPOINT_URL beside the plan, so the pre-flight
    -- verdict must cover it too. The endpoint is a boot resource this tool never dials.
    (bootPlan, _endpoint) <- orRefuse (T.unlines . map renderBootError) (bootRefusals envVars planE)
    traverse_ TIO.putStrLn (bpLines bootPlan)
    -- Standard output carries no severity field, so the prefix stands in for the boot's
    -- katip WarningS.
    traverse_ (TIO.putStrLn . ("warning: " <>)) (bpWarnings bootPlan)
    TIO.putStrLn "configuration: valid"
  where
    {- Print the aggregated report and the verdict, then abort through the boot's own
    typed path, which 'Ecluse.superviseProcess' maps to exit 2. -}
    orRefuse :: (e -> Text) -> Either e a -> IO a
    orRefuse render = orExit (\err -> render err <> "\nconfiguration: refused")
