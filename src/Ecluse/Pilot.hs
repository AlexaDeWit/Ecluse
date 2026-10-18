-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The Pilot role's front door. Every decision it acts on is planned before it starts, in
"Ecluse.Pilot.Plan" and the boot arm that spends the plan's refusal, so what is left here is
the effect each plan names.
-}
module Ecluse.Pilot (
    runPilot,
    runExportLoop,
    superviseExportCycles,

    -- * One-shot compilation
    PilotCompileOptions (..),
    UploadRequest (..),
    runPilotCompile,
    PilotUploadUnconfigured (..),
) where

import Conduit (MonadResource, runResourceT)
import Control.Monad.Catch (MonadMask)
import Katip (KatipContext, LogEnv, Severity (InfoS, WarningS), logFM, ls)
import Network.HTTP.Client (Manager)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (mapConcurrently_)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (throwIO)

import Ecluse.Boot (BootEnv (..), probeServerConfig)
import Ecluse.Composition.Plan (BootPlan (bpS3Endpoint))
import Ecluse.Config (
    AdvisoriesSettings (advDataDir, advUrl),
    AdvisoryStoreUrl,
    AppConfig (cfgAdvisories, cfgEgress),
    Config (configApp, configMounts),
    EgressSettings (egrOperatorContact),
    advisoryStoreUrlText,
 )
import Ecluse.Config.AdvisoryStore (advisoryStoreCapability)
import Ecluse.Core.BuildIdentity (renderUserAgent)
import Ecluse.Core.Ecosystem (ecosystemName, parseEcosystem)
import Ecluse.Core.Osv.Compile (OsvCandidate, candidateDestination, newFeedManagerWithIdentity, promoteCandidate, withOsvCandidate)
import Ecluse.Core.Osv.Ecosystem (osvEcosystemFor, osvEcosystemNamed)
import Ecluse.Core.Osv.Publication (Rebaseline)
import Ecluse.Core.Supervision (
    BackoffSchedule (BackoffSchedule, bsBaseMicros, bsCapMicros),
    superviseLoop,
    transientPolicy,
 )
import Ecluse.Pilot.Plan (
    ExportLoopPlan (ExportIdle, ExportTo),
    ExportTarget (etEcosystem, etEpss),
    PilotCompileOptions (..),
    PilotUploadUnconfigured (..),
    UploadPlan (UploadSkipped, UploadTo),
    UploadRequest (..),
    compileEpssRequirement,
    configuredSources,
    exportCadenceMicros,
    idleCadenceMicros,
    quietTimeFor,
    shrinkPolicyOf,
    unmountedCompileWarning,
    uploadPlan,
    uploadTarget,
 )
import Ecluse.Runtime.Aws.Env (AwsEndpoint)
import Ecluse.Runtime.Aws.Fault (retypeAmazonkaEscapes)
import Ecluse.Runtime.Aws.S3 (buildS3Env)
import Ecluse.Runtime.Log (moduleContext)
import Ecluse.Runtime.Pilot.Export (publishCandidate)
import Ecluse.Runtime.Server (probeOnlyApplication, raceServerAgainstLoop, runWarp)
import Ecluse.Runtime.Telemetry (Telemetry, telemetryTracerProvider)
import Ecluse.Runtime.Telemetry.Instruments (Metrics, advisoryCompileMetricsPortOf, newMetrics)

-- | The Pilot worker mode. The export loop never returns, so the server's return on shutdown cancels it.
runPilot :: BootEnv -> ExportLoopPlan -> IO ()
runPilot bootEnv exportPlan = do
    let cfg = probeServerConfig (configApp (beConfig bootEnv))
    moduleContext (beLogEnv bootEnv) "Ecluse.Pilot" $ do
        logFM InfoS "Pilot mode starting up"
        raceServerAgainstLoop
            (liftIO $ runWarp (beLogEnv bootEnv) "Pilot health probes" cfg probeOnlyApplication)
            (runExportLoop (beTelemetry bootEnv) (bpS3Endpoint (beBootPlan bootEnv)) (beConfig bootEnv) exportPlan)

-- | Run the loop the boot planned, never returning. Every fault inside a cycle is transient: a cycle has no wiring to fail on.
runExportLoop :: (MonadMask m, MonadUnliftIO m, KatipContext m) => Telemetry -> Maybe AwsEndpoint -> Config -> ExportLoopPlan -> m ()
runExportLoop telemetry s3Endpoint config = \case
    ExportIdle -> do
        logFM InfoS "No advisory store configured for OSV database export; export loop disabled."
        forever (threadDelay idleCadenceMicros)
    ExportTo store targets -> do
        logFM InfoS (ls ("Export loop starting up. Target store: " <> advisoryStoreUrlText store))
        -- One instrument set for the whole loop. Rebuilding it per cycle would register
        -- the catalogue again and split each signal across two streams.
        metrics <- liftIO (newMetrics telemetry)
        feeds <- liftIO (newFeedManagerWithIdentity (renderUserAgent (egrOperatorContact (cfgEgress (configApp config)))))
        superviseExportCycles schedule targets $ \target -> do
            runResourceT (exportEcosystem metrics feeds target telemetry s3Endpoint advisories store)
            threadDelay cadence
  where
    advisories = cfgAdvisories (configApp config)
    cadence = exportCadenceMicros advisories
    schedule = BackoffSchedule{bsBaseMicros = cadence, bsCapMicros = cadence}

-- | One supervised cycle loop per ecosystem, side by side, each with its own backoff, so one failing feed holds back no other.
superviseExportCycles :: (MonadUnliftIO m, KatipContext m) => BackoffSchedule -> NonEmpty ExportTarget -> (ExportTarget -> m ()) -> m ()
superviseExportCycles schedule targets runCycle =
    -- amazonka hands a failed credential refresh to the cycle thread that discovered the credentials.
    mapConcurrently_ (\target -> superviseLoop (policyFor target) (retypeAmazonkaEscapes (runCycle target))) targets
  where
    policyFor target = transientPolicy ("pilot-export-" <> ecosystemName (etEcosystem target)) schedule

-- One full cycle for one ecosystem: compile its OSV artifact, publish it, then keep it locally.
exportEcosystem :: (MonadResource m, MonadMask m, MonadUnliftIO m, KatipContext m) => Metrics -> Manager -> ExportTarget -> Telemetry -> Maybe AwsEndpoint -> AdvisoriesSettings -> AdvisoryStoreUrl -> m ()
exportEcosystem metrics feeds target telemetry s3Endpoint advisories store = do
    logFM InfoS (ls ("Starting " <> ecosystemName eco <> " OSV database compilation"))
    withOsvCandidate
        (advisoryCompileMetricsPortOf metrics (Just eco))
        (telemetryTracerProvider telemetry)
        feeds
        (advDataDir advisories)
        osvEco
        (etEpss target)
        (configuredSources advisories osvEco)
        (quietTimeFor advisories (Just eco))
        (\candidate -> publishToStore telemetry s3Endpoint advisories store Nothing candidate >> void (promoteCandidate candidate))
  where
    eco = etEcosystem target
    osvEco = osvEcosystemFor eco

-- Publish to the key 'uploadTarget' derives, where the sync reads. A stop throws, so the loop's
-- supervision retries it on the cadence and a one-shot run exits non-zero.
publishToStore :: (MonadUnliftIO m, KatipContext m) => Telemetry -> Maybe AwsEndpoint -> AdvisoriesSettings -> AdvisoryStoreUrl -> Maybe Rebaseline -> OsvCandidate -> m ()
publishToStore telemetry s3Endpoint advisories store rebaseline candidate = do
    env <- liftIO (buildS3Env s3Endpoint)
    let (bucket, key) = uploadTarget store (candidateDestination candidate)
    publishCandidate (telemetryTracerProvider telemetry) env (shrinkPolicyOf advisories) (advisoryStoreCapability store) rebaseline bucket key candidate
        >>= either throwIO (const pass)

-- Act on a planned upload. A skipped one is the caller's success path, not an error.
runUploadPlan :: (MonadUnliftIO m, KatipContext m) => Telemetry -> Maybe AwsEndpoint -> AdvisoriesSettings -> UploadPlan -> OsvCandidate -> m ()
runUploadPlan telemetry s3Endpoint advisories plan candidate = case plan of
    UploadSkipped -> pass
    UploadTo store rebaseline -> publishToStore telemetry s3Endpoint advisories store rebaseline candidate

-- | Compile once, optionally publish, and return the path. A compile failure or a publication refusal throws, so the command exits non-zero.
runPilotCompile :: LogEnv -> Telemetry -> Maybe AwsEndpoint -> Config -> PilotCompileOptions -> IO FilePath
runPilotCompile logEnv telemetry s3Endpoint config opts = do
    metrics <- newMetrics telemetry
    feeds <- newFeedManagerWithIdentity (renderUserAgent (egrOperatorContact (cfgEgress (configApp config))))
    -- The metric label domain is the closed 'Ecosystem' enum. A one-shot compile of a name
    -- outside it still writes its artifact, and records no series.
    let compileMetrics = advisoryCompileMetricsPortOf metrics (parseEcosystem (pcoEcosystem opts))
    moduleContext logEnv "Ecluse.Pilot" $
        runResourceT $ do
            -- Raised before the compile, so an upload the wiring cannot satisfy fails
            -- without first compiling the artifact it could never publish.
            plan <- either throwIO pure planned
            traverse_ (logFM WarningS . ls) (unmountedCompileWarning (configMounts config) opts)
            withOsvCandidate
                compileMetrics
                (telemetryTracerProvider telemetry)
                feeds
                (pcoOutDir opts)
                osvEco
                (compileEpssRequirement advisories (configMounts config) opts)
                (configuredSources advisories osvEco)
                (quietTimeFor advisories (parseEcosystem (pcoEcosystem opts)))
                (\candidate -> runUploadPlan telemetry s3Endpoint advisories plan candidate >> promoteCandidate candidate)
  where
    advisories = cfgAdvisories (configApp config)
    osvEco = osvEcosystemNamed (pcoEcosystem opts)
    planned = uploadPlan opts (advUrl advisories)
