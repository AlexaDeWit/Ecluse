-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Check configuration without starting services or making cloud calls.
Print predicted runtime posture and role-specific refusals. Exit @2@ when the checker's pass refuses.
-}
module Ecluse.CheckConfig (runCheckConfig) where

import Data.Text.IO qualified as TIO

import Ecluse.Boot (loadBootConfig, refuseBoot, runtimeOverridesOf)
import Ecluse.Composition.BootError (renderAdvisory, renderBootErrors)
import Ecluse.Composition.Plan (
    BootInputs (BootInputs, biConfig, biDocument, biEnvVars, biFdLimit, biRuntimePlan),
    BootPlan (bpLines, bpWarnings),
    BootReport (brAdvisories, brOutcome, brProvenance),
    resolveBootPlan,
    roleRefusalWarnings,
 )
import Ecluse.Composition.Sizing (openFileSoftLimit)
import Ecluse.Composition.Types (BootRole (BootWithoutPipeline))
import Ecluse.Config (AppConfig (cfgRuntime), Config (configApp))
import Ecluse.Rts (appliedRuntimePlan, currentRtsPosture, renderEffectivePosture, renderPostureWarnings, resolveRuntimePlan)
import Ecluse.Rts.Cgroup (readCgroupLimits)

{- | Validate the configuration and print the resolved posture. A valid configuration
returns (exit @0@) and a refused one aborts (exit @2@).
-}
runCheckConfig :: IO ()
runCheckConfig = do
    {- The refusal suffix carries the verdict into the boot's own typed abort, which
    'Ecluse.Internal.superviseProcess' maps to exit 2 and 'Ecluse.Startup.runWith' reports. -}
    (envVars, docBlob, config) <- loadBootConfig (<> "\nconfiguration: refused")
    rts <- currentRtsPosture
    cgroup <- readCgroupLimits
    fdLimit <- openFileSoftLimit
    let overrides = runtimeOverridesOf (cfgRuntime (configApp config))
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
            traverse_ (warn . renderAdvisory) (brAdvisories report)
            refuseBoot (renderBootErrors errs <> "\nconfiguration: refused")
        Right plan -> pure plan
    traverse_ TIO.putStrLn (bpLines bootPlan)
    traverse_ warn (bpWarnings bootPlan)
    traverse_ (warn . renderAdvisory) (brAdvisories report)
    -- A configuration one role refuses and another boots is a normal deployment, so the other
    -- roles' refusals report as warnings rather than deciding the exit status.
    traverse_ warn (roleRefusalWarnings BootWithoutPipeline inputs)
    TIO.putStrLn "configuration: valid"
  where
    -- Standard output carries no severity field, so the prefix stands in for the boot's
    -- katip WarningS.
    warn :: Text -> IO ()
    warn = TIO.putStrLn . ("warning: " <>)
