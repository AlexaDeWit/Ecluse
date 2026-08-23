-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The advisory-sync plan: one ecosystem's sync wiring ('CveSyncHandle') and the
config-driven plan that builds it ('planCveSync'). It also holds the projections the
composition root reads off that plan: the per-ecosystem rule capabilities, the
first-sync readiness gate, and the sync schedule. "Ecluse.Proxy"'s @runProxy@ builds
the plan at boot and runs one supervised sync task per handle.
-}
module Ecluse.Proxy.CveSync (
    CveSyncHandle (..),
    planCveSync,
    sweepStaleTemps,
    sweepStep,
    cveRuleDepsFor,
    katipFaultReporter,
    cveSyncReady,
    cveSyncScheduleFor,
) where

import Data.Map.Strict qualified as Map
import Katip (LogEnv, Severity (WarningS), sl)
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
import System.FilePath (isExtensionOf, (</>))
import System.IO.Error (IOError, catchIOError)

import Ecluse.Config (
    AdvisoriesSettings (advBucket, advDataDir, advMaxDatabaseBytes, advPollInterval),
    AppConfig (cfgAdvisories, cfgMounts),
 )
import Ecluse.Config.Ambient (AmbientAws (ambientAwsEndpointUrl), parseEndpointUrl)
import Ecluse.Core.Breaker (BreakerReporter)
import Ecluse.Core.Cve.Slot (CveSlot, currentAdvisoryEtag, newCveSlot, withSlotLookup)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Osv.Schema (osvDbFileName)
import Ecluse.Core.Rules (FaultReporter (..), RuleDeps (..))
import Ecluse.Runtime.Cve.Sync (S3CveSource, SyncEnv (..), SyncSchedule (SyncSchedule, schedBootBackoff, schedPollDelay), bootBackoffDelays, newS3CveSource, s3CveFetchFor)
import Ecluse.Runtime.Log (logLine, moduleField)

{- | The rules' boot-bound capabilities for one mount ecosystem. A mount's rules read only their own
ecosystem's advisory database, and abstain when the sync plan carries no slot for it.
-}
cveRuleDepsFor :: Map.Map Ecosystem CveSyncHandle -> BreakerReporter -> FaultReporter -> Ecosystem -> RuleDeps
cveRuleDepsFor plan reporter faultReporter eco =
    RuleDeps
        { rdWithCveLookup = maybe (\use -> use Nothing) (withSlotLookup . csSlot) (Map.lookup eco plan)
        , rdCurrentAdvisoryEtag = maybe (pure Nothing) (currentAdvisoryEtag . csSlot) (Map.lookup eco plan)
        , rdBreakerReporter = reporter
        , rdFaultReporter = faultReporter
        }

{- | A 'FaultReporter' that logs an exhausted rule's fault detail, so a query fault stays
diagnosable rather than collapsing to a bare @Unavailable@. The detail never reaches a client.
-}
katipFaultReporter :: LogEnv -> FaultReporter
katipFaultReporter logEnv =
    FaultReporter $ \ruleName detail ->
        logLine
            logEnv
            (moduleField "Ecluse.Core.Rules" <> sl "rule" ruleName <> sl "fault" detail)
            WarningS
            "effectful rule evaluation faulted"

{- | The readiness gate over the sync plan: ready once every ecosystem completes its first sync.
Each flag flips one way, so readiness never flaps. An empty plan is vacuously ready.
-}
cveSyncReady :: Map.Map Ecosystem CveSyncHandle -> IO Bool
cveSyncReady plan = allM (readTVarIO . csReady) (Map.elems plan)

{- | The sync tasks' timing: the shipped boot burst over the configured poll
interval. The microsecond conversion cannot wrap: the config decoder bounds
the interval to @[1, maxBound div 1_000_000]@ seconds.
-}
cveSyncScheduleFor :: AppConfig -> SyncSchedule
cveSyncScheduleFor env =
    SyncSchedule
        { schedBootBackoff = bootBackoffDelays
        , schedPollDelay = round (advPollInterval (cfgAdvisories env)) * 1_000_000
        }

-- | One configured ecosystem's advisory-sync wiring.
data CveSyncHandle = CveSyncHandle
    { csSlot :: CveSlot
    -- ^ The slot this ecosystem's mount rules borrow through.
    , csReady :: TVar Bool
    -- ^ The one-way first-sync readiness flag.
    , csEnv :: SyncEnv
    -- ^ The sync task's environment.
    }

{- | Build the advisory-sync plan from config, one 'CveSyncHandle' per mount ecosystem, or nothing
when no vulnerability-database bucket is configured. An operator who mounts an ecosystem the build
does not ship declares an artifact that never arrives, so the pod never reports ready.
-}
planCveSync :: LogEnv -> AmbientAws -> AppConfig -> IO (Map.Map Ecosystem CveSyncHandle)
planCveSync logEnv ambient appCfg = case advBucket (cfgAdvisories appCfg) of
    Nothing -> pure Map.empty
    Just bucket -> do
        let dataDir = advDataDir (cfgAdvisories appCfg)
        createDirectoryIfMissing True dataDir
        sweepStaleTemps logEnv dataDir
        cveSource <- newS3CveSource (ambientAwsEndpointUrl ambient >>= parseEndpointUrl)
        Map.fromList <$> traverse (cveSyncHandleFor appCfg cveSource bucket) (Map.keys (cfgMounts appCfg))

-- 'cveSource' captures the S3 environment once, so every ecosystem's transport shares one
-- credential discovery.
cveSyncHandleFor :: AppConfig -> S3CveSource -> Text -> Ecosystem -> IO (Ecosystem, CveSyncHandle)
cveSyncHandleFor appCfg cveSource bucket eco = do
    slot <- newCveSlot
    ready <- newTVarIO False
    let key = osvDbFileName (ecosystemName eco)
        syncEnv =
            SyncEnv
                { syncFetch = s3CveFetchFor cveSource bucket (toText key) (advMaxDatabaseBytes (cfgAdvisories appCfg))
                , syncEcosystem = eco
                , syncDbPath = advDataDir (cfgAdvisories appCfg) </> key
                , syncSlot = slot
                }
    pure (eco, CveSyncHandle{csSlot = slot, csReady = ready, csEnv = syncEnv})

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
    payload = moduleField "Ecluse.Proxy.CveSync" <> sl "path" (toText path)
