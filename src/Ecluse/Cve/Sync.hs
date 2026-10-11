-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The advisory-sync plan: one 'CveSyncHandle' per mount ecosystem ('planCveSync'), the
projections the composition root reads off it, and one supervised sync task per handle.
-}
module Ecluse.Cve.Sync (
    CveSyncHandle (..),
    AdvisoryNeed (..),
    planCveSync,
    sweepStaleTemps,
    sweepStep,
    cveRuleDepsFor,
    advisoryFreshnessFor,
    reportPushAge,
    katipOutageReporter,
    outageReportPeriod,
    cveSyncReadiness,
    cveSyncScheduleFor,
    cveSyncTasks,
    registerAdvisoryAges,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, getCurrentTime)
import Katip (LogEnv, Severity (ErrorS, InfoS, WarningS), SimpleLogPayload, runKatipContextT, sl)
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
import System.FilePath (isExtensionOf, (</>))
import System.IO.Error (IOError, catchIOError)

import Ecluse.Config (
    AdvisoriesSettings (advDataDir, advPollInterval, advUrl),
    AdvisoryStoreUrl,
    AppConfig (cfgAdvisories, cfgLimits),
    LimitsSettings (limMaxAdvisoryDatabaseBytes),
    advisoryObjectKey,
    advisoryStoreBucket,
    advisoryStoreUrlText,
 )
import Ecluse.Core.Breaker (BreakerReporter)
import Ecluse.Core.Clock (secondsToMicros)
import Ecluse.Core.Cve.Slot (AdvisorySource (asPushedAt), CveSlot, currentAdvisoryEtag, currentAdvisorySource, generationInstalledAt, newCveSlot, withSlotGeneration)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Osv.Schema (EpssRequirement, osvDbFileName)
import Ecluse.Core.Rules (AdvisoryDatabase (AdvisoryDatabase, NoAdvisoryDatabase), AdvisoryGeneration (..), RuleDeps (..), SourceReporter, noSourceReporter)
import Ecluse.Core.Rules.Freshness (
    AdvisoryFreshness (AdvisoryFresh),
    AdvisoryPublication (NoGeneration, PublishedAt, UndatedGeneration),
    MaxAdvisoryAge,
    ageAlarmStep,
    assessAdvisoryAge,
 )
import Ecluse.Core.Rules.Outage (OutageReport (..), OutageState (Healthy), sourceReporter, tvarOutageStore)
import Ecluse.Core.Rules.Types (AdvisoryAge (advisoryAge, advisoryMaxAge, advisoryPushedAt))
import Ecluse.Core.Server.Readiness (
    DatabaseRequirement,
    MountReadiness,
    Readiness,
    mountReadiness,
    mountStateFor,
 )
import Ecluse.Core.Supervision (
    backgroundLoopBackoff,
    superviseLoop,
    transientPolicy,
 )
import Ecluse.Core.Text.Iso8601 (renderIso8601Utc)
import Ecluse.Runtime.Aws.Env (AwsEndpoint)
import Ecluse.Runtime.Cve.Sync (S3CveSource, SyncEnv (..), SyncHooks (SyncHooks, hookFirstSync, hookPushAge), SyncSchedule (SyncSchedule, schedAbsentReport, schedBootBackoff, schedPollDelay), absentReportInterval, bootBackoffDelays, newS3CveSource, runCveSync, s3CveFetchFor)
import Ecluse.Runtime.Log (logLine, moduleField)
import Ecluse.Runtime.Telemetry (Telemetry)
import Ecluse.Runtime.Telemetry.Instruments (Metrics, advisorySyncMetricsPortOf, registerAdvisoryDatabaseAge, registerAdvisorySourceAge)
import Ecluse.Runtime.Telemetry.Tracing (advisorySyncTracingPortOf)

-- | Bind each ecosystem's rules to its own advisory slot and observers. Missing handles configure no database.
cveRuleDepsFor :: Map.Map Ecosystem CveSyncHandle -> BreakerReporter -> (Ecosystem -> OutageReport -> IO ()) -> Ecosystem -> RuleDeps
cveRuleDepsFor plan reporter reportOutage eco =
    RuleDeps
        { rdAdvisoryDatabase = maybe NoAdvisoryDatabase advisoryDatabaseOf handle
        , rdCurrentAdvisoryEtag = maybe (pure Nothing) (currentAdvisoryEtag . syncSlot . csEnv) handle
        , rdBreakerReporter = reporter
        , rdSourceReporter = maybe noSourceReporter (sourceReporterOf (reportOutage eco)) handle
        , rdAdvisoryFreshness = advisoryFreshnessFor plan eco
        }
  where
    handle = Map.lookup eco plan

advisoryDatabaseOf :: CveSyncHandle -> AdvisoryDatabase
advisoryDatabaseOf handle = AdvisoryDatabase $ \use ->
    withSlotGeneration (syncSlot (csEnv handle)) $ \generation ->
        use
            AdvisoryGeneration
                { agFreshness = assessPublicationOf handle ((\(_, _, source) -> source) <$> generation)
                , agLookup = \readLookup -> readLookup ((\(etag, lookup, _) -> (etag, lookup)) <$> generation)
                }

sourceReporterOf :: (OutageReport -> IO ()) -> CveSyncHandle -> SourceReporter
sourceReporterOf emit handle = sourceReporter outageReportPeriod (csClock handle) (tvarOutageStore (csOutage handle)) emit

-- | The reminder interval for continuing outages, shared with unloaded-database reports.
outageReportPeriod :: NominalDiffTime
outageReportPeriod = fromIntegral absentReportInterval / 1_000_000

-- | Assess the live slot's publication age. Ecosystems without an advisory stack have nothing to age.
advisoryFreshnessFor :: Map.Map Ecosystem CveSyncHandle -> Ecosystem -> IO AdvisoryFreshness
advisoryFreshnessFor plan eco = maybe (pure AdvisoryFresh) advisoryFreshnessOf (Map.lookup eco plan)

-- | Assess the live slot's store timestamp against this handle's clock and maximum age.
advisoryFreshnessOf :: CveSyncHandle -> IO AdvisoryFreshness
advisoryFreshnessOf handle = currentAdvisorySource (syncSlot (csEnv handle)) >>= assessPublicationOf handle

assessPublicationOf :: CveSyncHandle -> Maybe AdvisorySource -> IO AdvisoryFreshness
assessPublicationOf handle source = do
    now <- csClock handle
    pure (assessAdvisoryAge (csMaxAge handle) now (publicationOf source))

-- Nothing serving, a dated push, or a generation the store gave no publication time for. The
-- third is unverified evidence rather than an absent database, so it is kept distinct here.
publicationOf :: Maybe AdvisorySource -> AdvisoryPublication
publicationOf = maybe NoGeneration (maybe UndatedGeneration PublishedAt . asPushedAt)

-- | Report crossing half the maximum age once. A fresh push re-arms the report.
reportPushAge :: LogEnv -> Ecosystem -> CveSyncHandle -> IO ()
reportPushAge logEnv eco handle = do
    freshness <- advisoryFreshnessOf handle
    crossing <- atomically $ do
        latched <- readTVar (csAgeAlarmed handle)
        let (latched', crossed) = ageAlarmStep latched freshness
        writeTVar (csAgeAlarmed handle) latched'
        pure crossed
    whenJust crossing (logPushAge logEnv eco)

-- What the crossing line carries: enough to tell an update outage from a maximum set too short.
logPushAge :: LogEnv -> Ecosystem -> AdvisoryAge -> IO ()
logPushAge logEnv eco observed =
    logLine
        logEnv
        ( moduleField "Ecluse.Cve.Sync"
            <> sl "ecosystem" (ecosystemName eco)
            <> sl "pushed_at" (renderIso8601Utc (advisoryPushedAt observed))
            <> sl "age_seconds" (round (advisoryAge observed) :: Integer)
            <> sl "max_age_seconds" (round (advisoryMaxAge observed) :: Integer)
        )
        ErrorS
        "the advisory push age has passed half its maximum; past the maximum, CVE-based denial refuses"

advisoryPushTime :: CveSyncHandle -> IO (Maybe UTCTime)
advisoryPushTime handle = (asPushedAt =<<) <$> currentAdvisorySource (syncSlot (csEnv handle))

-- | Log outage starts and reminders at ERROR, and recovery at INFO. Fault details reach no client.
katipOutageReporter :: LogEnv -> Ecosystem -> OutageReport -> IO ()
katipOutageReporter logEnv eco = \case
    OutageBegan rule cause ->
        logLine logEnv (payload <> sl "rule" rule <> sl "cause" cause) ErrorS "advisory source outage began: a rule cannot consult it"
    OutageContinues since rules ->
        logLine logEnv (payload <> sl "since" (renderIso8601Utc since) <> sl "rules" (renderCauses rules)) ErrorS "advisory source outage continues"
    OutageRecovered since ->
        logLine logEnv (payload <> sl "since" (renderIso8601Utc since)) InfoS "advisory source outage recovered: every rule consults it again"
  where
    payload = moduleField "Ecluse.Core.Rules" <> sl "ecosystem" (ecosystemName eco)
    renderCauses = T.intercalate "; " . map (\(rule, cause) -> rule <> ": " <> cause) . Map.toList

-- | Wait for the first sync only on mounts with advisory denies. Other ecosystems remain routable.
cveSyncReadiness :: Map.Map Ecosystem CveSyncHandle -> IO Readiness
cveSyncReadiness plan = mountReadiness <$> traverse mountStateOf plan

-- One mount's advisory state, from what its rules need and the one-way flag its sync task flips.
mountStateOf :: CveSyncHandle -> IO MountReadiness
mountStateOf handle = mountStateFor (csDatabase handle) <$> readTVarIO (csReady handle)

-- | Use the boot retry burst and configured poll interval. Configuration bounds the microsecond conversion.
cveSyncScheduleFor :: AppConfig -> SyncSchedule
cveSyncScheduleFor env =
    SyncSchedule
        { schedBootBackoff = bootBackoffDelays
        , schedPollDelay = secondsToMicros (advPollInterval (cfgAdvisories env))
        , schedAbsentReport = absentReportInterval
        }

-- | Run one supervised sync task per ecosystem, marking first-sync readiness once for every evaluating role.
cveSyncTasks :: LogEnv -> Metrics -> Telemetry -> SyncSchedule -> Map.Map Ecosystem CveSyncHandle -> [IO ()]
cveSyncTasks logEnv metrics telemetry schedule plan =
    [ void . runKatipContextT logEnv (mempty :: SimpleLogPayload) "cve-sync" $
        superviseLoop
            (transientPolicy ("cve-sync[" <> show (syncEcosystem (csEnv handle)) <> "]") backgroundLoopBackoff)
            (runCveSync syncMetrics syncTracing (csEnv handle) schedule (hooksFor eco handle))
    | (eco, handle) <- Map.toList plan
    ]
  where
    syncMetrics = advisorySyncMetricsPortOf metrics
    syncTracing = advisorySyncTracingPortOf telemetry
    hooksFor eco handle =
        SyncHooks
            { hookFirstSync = atomically (writeTVar (csReady handle) True)
            , hookPushAge = reportPushAge logEnv eco handle
            }

-- | Register once per role. Callbacks read the slots, so observations survive sync-task restarts.
registerAdvisoryAges :: Metrics -> Map.Map Ecosystem CveSyncHandle -> IO ()
registerAdvisoryAges metrics plan =
    for_ (Map.toList plan) $ \(eco, handle) -> do
        registerAdvisoryDatabaseAge metrics eco (generationInstalledAt (syncSlot (csEnv handle)))
        registerAdvisorySourceAge metrics eco (advisoryPushTime handle)

-- | One configured ecosystem's advisory-sync wiring.
data CveSyncHandle = CveSyncHandle
    { csReady :: TVar Bool
    -- ^ The one-way first-sync readiness flag.
    , csEnv :: SyncEnv
    -- ^ The sync task's environment. Its 'syncSlot' is the slot this ecosystem's rules borrow through.
    , csMaxAge :: MaxAdvisoryAge
    -- ^ This mount's effective maximum push age, derived once at boot from its own rules.
    , csClock :: IO UTCTime
    -- ^ The wall clock the push age is read on, injected so a suite can fix it.
    , csAgeAlarmed :: TVar Bool
    -- ^ Whether the half-maximum crossing has already been reported for the current push.
    , csOutage :: TVar OutageState
    -- ^ The rules-side outage state every mount of this ecosystem reports through.
    , csDatabase :: DatabaseRequirement
    -- ^ Whether this mount's own rules deny on the database, which is what its readiness turns on.
    }

-- | What one vetted mount's rules ask of the advisory stack, read off its own policy at boot.
data AdvisoryNeed = AdvisoryNeed
    { anEcosystem :: Ecosystem
    , anMaxAge :: MaxAdvisoryAge
    , anEpss :: EpssRequirement
    , anDatabase :: DatabaseRequirement
    }

-- | Build one sync handle per vetted ecosystem. No store yields no handles, and unsupported mounts stay unready.
planCveSync :: LogEnv -> Maybe AwsEndpoint -> AppConfig -> [AdvisoryNeed] -> IO (Map.Map Ecosystem CveSyncHandle)
planCveSync logEnv s3Endpoint appCfg needs = case advUrl (cfgAdvisories appCfg) of
    Nothing -> pure Map.empty
    Just store -> do
        let dataDir = advDataDir (cfgAdvisories appCfg)
        createDirectoryIfMissing True dataDir
        sweepStaleTemps logEnv dataDir
        cveSource <- newS3CveSource s3Endpoint
        Map.fromList <$> traverse (cveSyncHandleFor appCfg cveSource store) needs

cveSyncHandleFor :: AppConfig -> S3CveSource -> AdvisoryStoreUrl -> AdvisoryNeed -> IO (Ecosystem, CveSyncHandle)
cveSyncHandleFor appCfg cveSource store need = do
    slot <- newCveSlot
    ready <- newTVarIO False
    alarmed <- newTVarIO False
    outage <- newTVarIO Healthy
    pure
        ( anEcosystem need
        , CveSyncHandle
            { csReady = ready
            , csEnv = syncEnvFor appCfg cveSource store need slot
            , csMaxAge = anMaxAge need
            , csClock = getCurrentTime
            , csAgeAlarmed = alarmed
            , csOutage = outage
            , csDatabase = anDatabase need
            }
        )

-- 'cveSource' captures the S3 environment once, so every ecosystem's transport shares one
-- credential discovery. The store addresses the remote object, the local copy its bare file name.
syncEnvFor :: AppConfig -> S3CveSource -> AdvisoryStoreUrl -> AdvisoryNeed -> CveSlot -> SyncEnv
syncEnvFor appCfg cveSource store need slot =
    SyncEnv
        { syncFetch =
            s3CveFetchFor
                cveSource
                (advisoryStoreBucket store)
                (advisoryObjectKey store fileName)
                (limMaxAdvisoryDatabaseBytes (cfgLimits appCfg))
        , syncEcosystem = eco
        , syncEpssRequirement = anEpss need
        , syncDbPath = advDataDir (cfgAdvisories appCfg) </> fileName
        , syncSlot = slot
        , syncStoreRef = advisoryStoreUrlText store
        }
  where
    eco = anEcosystem need
    fileName = osvDbFileName (ecosystemName eco)

-- | Remove stale downloads retained across container restarts, with best-effort errors handled by 'sweepStep'.
sweepStaleTemps :: LogEnv -> FilePath -> IO ()
sweepStaleTemps logEnv dataDir =
    sweepStep logEnv dataDir $ do
        entries <- listDirectory dataDir
        traverse_ (removeStaleTemp logEnv dataDir) (filter (isExtensionOf "tmp") entries)

-- Remove one stray @.tmp@ entry, tolerating a per-entry filesystem fault so a single
-- unremovable file does not abort the rest of the sweep.
removeStaleTemp :: LogEnv -> FilePath -> FilePath -> IO ()
removeStaleTemp logEnv dataDir entry =
    let path = dataDir </> entry in sweepStep logEnv path (removeFile path)

-- | Log and swallow an 'IOError' from one sweep step. Other exceptions propagate.
sweepStep :: LogEnv -> FilePath -> IO () -> IO ()
sweepStep logEnv path step = step `catchIOError` logSweepFailure logEnv path

-- The logged OS error detail is the operator's own filesystem, not untrusted input.
logSweepFailure :: LogEnv -> FilePath -> IOError -> IO ()
logSweepFailure logEnv path err =
    logLine logEnv payload WarningS ("could not sweep stale advisory temp files: " <> show err)
  where
    payload = moduleField "Ecluse.Cve.Sync" <> sl "path" (toText path)
