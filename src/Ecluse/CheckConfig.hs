-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | @ecluse check-config@: validate the configuration exactly as a boot would and
print the whole resolved posture, without starting anything.

The subcommand loads the configuration through 'Ecluse.Config.loadConfig' and hands it
to 'Ecluse.Composition.Plan.resolveBootPlan', the one function a boot takes its
decisions from. It prints that plan's lines and applies none of it: no socket opens, no
capability count changes, no re-exec, no cloud call. It predicts the runtime posture
from 'appliedRuntimePlan', because the checker's own process posture says nothing about
the boot it is checking.

A failure prints the same aggregated report a boot would log and exits @2@ through the
shared 'Ecluse.Boot.BootAborted' path. A valid configuration prints the boot's lines
and exits @0@. An operator or a CI step reads exactly what a boot would do before
running one.
-}
module Ecluse.CheckConfig (runCheckConfig) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (getEnvironment)

import Ecluse.Boot (applySecretFileIndirection, orExit, readConfigDocument)
import Ecluse.Composition.BootError (renderBootError)
import Ecluse.Composition.Plan (BootPlan (bpLines, bpWarnings), resolveBootPlan)
import Ecluse.Composition.Sizing (openFileSoftLimit)
import Ecluse.Config (
    AppConfig (cfgRuntime),
    Config (configApp),
    RuntimeSettings (rtCores, rtMaxHeapBytes),
    loadConfig,
    renderConfigError,
 )
import Ecluse.Rts (
    appliedRuntimePlan,
    currentRtsPosture,
    readCgroupLimits,
    renderEffectivePosture,
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
        runtimePlan = resolveRuntimePlan (rtCores runtimeSettings) (rtMaxHeapBytes runtimeSettings) cgroup rts
        effective = appliedRuntimePlan cgroup runtimePlan rts
    bootPlan <- orRefuse (T.unlines . map renderBootError) (resolveBootPlan envVars docBlob config effective fdLimit)
    -- The boot logs these posture lines from 'Ecluse.Rts.applyRuntimePosture', which the
    -- checker never runs. They stand in that position here.
    traverse_ TIO.putStrLn (renderEffectivePosture effective)
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
