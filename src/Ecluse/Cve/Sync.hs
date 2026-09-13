-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The advisory-sync plan: one ecosystem's sync wiring ('CveSyncHandle') and the
config-driven plan that builds it ('planCveSync'). It also holds the projections the
composition root reads off that plan: the per-ecosystem rule capabilities, the
per-mount readiness verdict, the sync schedule, and database-age observations. Every consuming
role registers the observations once and runs one supervised sync task per handle.
-}
module Ecluse.Cve.Sync (
    CveSyncHandle (..),
    planCveSync,
    sweepStaleTemps,
    sweepStep,
    cveRuleDepsFor,
    advisoryFreshnessFor,
    reportPushAge,
    katipFaultReporter,
    cveSyncReadiness,
    cveSyncScheduleFor,
    cveSyncTasks,
    registerAdvisoryAges,
    backgroundLoopBackoff,
) where

import Data.Map.Strict qualified as Map
import Data.Time (UTCTime, getCurrentTime)
import Katip (LogEnv, Severity (ErrorS, WarningS), SimpleLogPayload, runKatipContextT, sl)
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
 )
import Ecluse.Core.Breaker (BreakerReporter)
import Ecluse.Core.Cve.Slot (AdvisorySource (asPushedAt), currentAdvisoryEtag, currentAdvisorySource, generationInstalledAt, newCveSlot, withSlotGeneration)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Osv.Schema (EpssRequirement, osvDbFileName)
import Ecluse.Core.Rules (FaultReporter (..), RuleDeps (..))
import Ecluse.Core.Rules.Freshness (
    AdvisoryAge (advisoryAge, advisoryMaxAge, advisoryPushedAt),
    AdvisoryFreshness (AdvisoryFresh),
    AdvisoryPublication (NoGeneration, PublishedAt, UndatedGeneration),
    MaxAdvisoryAge,
    ageAlarmStep,
    assessAdvisoryAge,
 )
import Ecluse.Core.Server.Readiness (
    MountReadiness (MountAwaitingFirstSync, MountReady),
    Readiness,
    mountReadiness,
 )
import Ecluse.Core.Supervision (
    BackoffSchedule (BackoffSchedule, bsBaseMicros, bsCapMicros),
    secondsToMicros,
    superviseLoop,
    transientPolicy,
 )
import Ecluse.Core.Text (renderIso8601Utc)
import Ecluse.Runtime.Aws.Env (AwsEndpoint)
import Ecluse.Runtime.Cve.Sync (S3CveSource, SyncEnv (..), SyncHooks (SyncHooks, hookFirstSync, hookPushAge), SyncSchedule (SyncSchedule, schedBootBackoff, schedPollDelay), bootBackoffDelays, newS3CveSource, runCveSync, s3CveFetchFor)
import Ecluse.Runtime.Log (logLine, moduleField)
import Ecluse.Runtime.Telemetry (Telemetry)
import Ecluse.Runtime.Telemetry.Instruments (Metrics, advisorySyncMetricsPortOf, registerAdvisoryDatabaseAge, registerAdvisorySourceAge)
import Ecluse.Runtime.Telemetry.Tracing (advisorySyncTracingPortOf)

{- | The rules' boot-bound capabilities for one mount ecosystem. A mount's rules read only their own
ecosystem's advisory database, and abstain when the sync plan carries no slot for it.
-}
cveRuleDepsFor :: Map.Map Ecosystem CveSyncHandle -> BreakerReporter -> FaultReporter -> Ecosystem -> RuleDeps
cveRuleDepsFor plan reporter faultReporter eco =
    RuleDeps
        { rdWithCveLookup = maybe (\use -> use Nothing) (withSlotGeneration . syncSlot . csEnv) (Map.lookup eco plan)
        , rdCurrentAdvisoryEtag = maybe (pure Nothing) (currentAdvisoryEtag . syncSlot . csEnv) (Map.lookup eco plan)
        , rdBreakerReporter = reporter
        , rdFaultReporter = faultReporter
        , rdAdvisoryFreshness = advisoryFreshnessFor plan eco
        }

{- | How old one mount's serving artifact's push is. An ecosystem the plan carries no handle for
has no advisory stack at all, so nothing ages and the absent-database path decides instead.
-}
advisoryFreshnessFor :: Map.Map Ecosystem CveSyncHandle -> Ecosystem -> IO AdvisoryFreshness
advisoryFreshnessFor plan eco = maybe (pure AdvisoryFresh) advisoryFreshnessOf (Map.lookup eco plan)

{- | One handle's reading: the slot's publication time against this mount's maximum, on the
handle's own clock. A failed poll never swaps, so a warm process keeps the last time it read.
-}
advisoryFreshnessOf :: CveSyncHandle -> IO AdvisoryFreshness
advisoryFreshnessOf handle = do
    now <- csClock handle
    assessAdvisoryAge (csMaxAge handle) now . publicationOf <$> currentAdvisorySource (syncSlot (csEnv handle))

{- Nothing serving, a dated push, or a generation the store gave no publication time for. The
third is unverified evidence rather than an absent database, so it is kept distinct here. -}
publicationOf :: Maybe AdvisorySource -> AdvisoryPublication
publicationOf = maybe NoGeneration (maybe UndatedGeneration PublishedAt . asPushedAt)

{- | Report one ecosystem's push age when it passes half its maximum, once per crossing. The latch
re-arms when a fresh push brings the age back under, so a long outage does not repeat every poll.
-}
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

-- The publication time the serving artifact carries, or nothing before the first sync.
advisoryPushTime :: CveSyncHandle -> IO (Maybe UTCTime)
advisoryPushTime handle = (asPushedAt =<<) <$> currentAdvisorySource (syncSlot (csEnv handle))

{- | A 'FaultReporter' logging an exhausted rule's fault detail, so a fault stays diagnosable
rather than a bare @Unavailable@. The detail is bounded, carries no secret, and reaches no client.
-}
katipFaultReporter :: LogEnv -> FaultReporter
katipFaultReporter logEnv =
    FaultReporter $ \ruleName detail ->
        logLine
            logEnv
            (moduleField "Ecluse.Core.Rules" <> sl "rule" ruleName <> sl "fault" detail)
            WarningS
            "effectful rule evaluation faulted"

{- | The readiness verdict over the sync plan: routable once at least one configured ecosystem
completes its first sync, so one ecosystem's missing artifact leaves the others routable.
-}
cveSyncReadiness :: Map.Map Ecosystem CveSyncHandle -> IO Readiness
cveSyncReadiness plan = mountReadiness <$> traverse mountStateOf plan

-- One mount's advisory state, read off the one-way flag its own sync task flips.
mountStateOf :: CveSyncHandle -> IO MountReadiness
mountStateOf = fmap (bool MountAwaitingFirstSync MountReady) . readTVarIO . csReady

{- | The sync tasks' timing: the shipped boot burst over the configured poll interval. The microsecond
conversion cannot wrap: the config decoder bounds the interval to @[1, maxBound div 1_000_000]@ seconds.
-}
cveSyncScheduleFor :: AppConfig -> SyncSchedule
cveSyncScheduleFor env =
    SyncSchedule
        { schedBootBackoff = bootBackoffDelays
        , schedPollDelay = secondsToMicros (advPollInterval (cfgAdvisories env))
        }

{- | One supervised sync task per configured ecosystem, each flipping its own one-way readiness
flag once its first sync lands. Every role that evaluates rules runs these.
-}
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

{- | The pace every shell background loop retries a transient fault at: one second after the
first failure, doubling to a thirty-second ceiling.
-}
backgroundLoopBackoff :: BackoffSchedule
backgroundLoopBackoff = BackoffSchedule{bsBaseMicros = 1_000_000, bsCapMicros = 30_000_000}

-- | One configured ecosystem's advisory-sync wiring.
data CveSyncHandle = CveSyncHandle
    { csReady :: TVar Bool
    -- ^ The one-way first-sync readiness flag.
    , csEnv :: SyncEnv
    {- ^ The sync task's environment. Its 'syncSlot' is the slot this ecosystem's mount
    rules borrow through.
    -}
    , csMaxAge :: MaxAdvisoryAge
    -- ^ This mount's effective maximum push age, derived once at boot from its own rules.
    , csClock :: IO UTCTime
    -- ^ The wall clock the push age is read on, injected so a suite can fix it.
    , csAgeAlarmed :: TVar Bool
    -- ^ Whether the half-maximum crossing has already been reported for the current push.
    }

{- | Build the advisory-sync plan, one 'CveSyncHandle' per vetted mount ecosystem, or nothing with
no store. A mount the build does not ship awaits an artifact that never comes, so it stays unready.
-}
planCveSync :: LogEnv -> Maybe AwsEndpoint -> AppConfig -> [(Ecosystem, MaxAdvisoryAge, EpssRequirement)] -> IO (Map.Map Ecosystem CveSyncHandle)
planCveSync logEnv s3Endpoint appCfg limits = case advUrl (cfgAdvisories appCfg) of
    Nothing -> pure Map.empty
    Just store -> do
        let dataDir = advDataDir (cfgAdvisories appCfg)
        createDirectoryIfMissing True dataDir
        sweepStaleTemps logEnv dataDir
        cveSource <- newS3CveSource s3Endpoint
        Map.fromList <$> traverse (cveSyncHandleFor appCfg cveSource store) limits

-- 'cveSource' captures the S3 environment once, so every ecosystem's transport shares one
-- credential discovery. The store addresses the remote object, the local copy its bare file name.
cveSyncHandleFor :: AppConfig -> S3CveSource -> AdvisoryStoreUrl -> (Ecosystem, MaxAdvisoryAge, EpssRequirement) -> IO (Ecosystem, CveSyncHandle)
cveSyncHandleFor appCfg cveSource store (eco, maxAge, epssRequirement) = do
    slot <- newCveSlot
    ready <- newTVarIO False
    alarmed <- newTVarIO False
    let fileName = osvDbFileName (ecosystemName eco)
        maxBytes = limMaxAdvisoryDatabaseBytes (cfgLimits appCfg)
        syncEnv =
            SyncEnv
                { syncFetch =
                    s3CveFetchFor
                        cveSource
                        (advisoryStoreBucket store)
                        (advisoryObjectKey store fileName)
                        maxBytes
                , syncEcosystem = eco
                , syncEpssRequirement = epssRequirement
                , syncDbPath = advDataDir (cfgAdvisories appCfg) </> fileName
                , syncSlot = slot
                }
    pure
        ( eco
        , CveSyncHandle
            { csReady = ready
            , csEnv = syncEnv
            , csMaxAge = maxAge
            , csClock = getCurrentTime
            , csAgeAlarmed = alarmed
            }
        )

{- | Sweep the in-progress downloads an interrupted run left behind, which an @emptyDir@ keeps
across a container restart. The sweep is best effort, per 'sweepStep'.
-}
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

{- | Run one best-effort step of the stale-temp sweep. It logs and swallows an 'IOError', so a
read-only or mispermissioned data dir does not stop the boot, and any other exception propagates.
-}
sweepStep :: LogEnv -> FilePath -> IO () -> IO ()
sweepStep logEnv path step = step `catchIOError` logSweepFailure logEnv path

-- The logged OS error detail is the operator's own filesystem, not untrusted input.
logSweepFailure :: LogEnv -> FilePath -> IOError -> IO ()
logSweepFailure logEnv path err =
    logLine logEnv payload WarningS ("could not sweep stale advisory temp files: " <> show err)
  where
    payload = moduleField "Ecluse.Cve.Sync" <> sl "path" (toText path)
