-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The advisory-sync plan: one ecosystem's sync wiring ('CveSyncHandle'), the
config-driven plan that builds it ('planCveSync'), and the projections the
composition root reads off the plan (the per-ecosystem rule capabilities, the
first-sync readiness gate, and the sync schedule). "Ecluse.Proxy"'s @runProxy@
builds the plan at boot and runs one supervised sync task per handle.
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

import Amazonka qualified as AWS
import Data.Map.Strict qualified as Map
import Katip (LogEnv, Severity (WarningS), logFM, ls, sl)
import Katip.Monadic (runKatipContextT)
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
import System.FilePath (isExtensionOf, (</>))
import System.IO.Error (IOError, catchIOError)

import Ecluse.Composition.MirrorQueue (parseEndpointUrl)
import Ecluse.Config (
    AdvisoriesSettings (advBucket, advDataDir, advMaxDatabaseBytes, advPollInterval),
    AppConfig (cfgAdvisories, cfgMounts),
 )
import Ecluse.Config.Ambient (AmbientAws (ambientAwsEndpointUrl))
import Ecluse.Core.Breaker (BreakerReporter)
import Ecluse.Core.Cve.Slot (CveSlot, currentAdvisoryEtag, newCveSlot, withSlotLookup)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Osv.Schema (osvDbFileName)
import Ecluse.Core.Rules (FaultReporter (..), RuleDeps (..))
import Ecluse.Runtime.Cve.Sync (SyncEnv (..), SyncSchedule (SyncSchedule, schedBootBackoff, schedPollDelay), bootBackoffDelays, s3CveFetch)
import Ecluse.Runtime.Log (moduleField)
import Ecluse.Runtime.Pilot.Export (buildS3Env)

{- | The rules' boot-bound capabilities for one mount ecosystem: the CVE
lookup borrows through that ecosystem's own slot when the sync plan carries
one, and abstains otherwise, so a mount's rules can never read a neighbouring
ecosystem's advisory database.
-}
cveRuleDepsFor :: Map.Map Ecosystem CveSyncHandle -> BreakerReporter -> FaultReporter -> Ecosystem -> RuleDeps
cveRuleDepsFor plan reporter faultReporter eco =
    RuleDeps
        { rdWithCveLookup = maybe (\use -> use Nothing) (withSlotLookup . csSlot) (Map.lookup eco plan)
        , rdCurrentAdvisoryEtag = maybe (pure Nothing) (currentAdvisoryEtag . csSlot) (Map.lookup eco plan)
        , rdBreakerReporter = reporter
        , rdFaultReporter = faultReporter
        }

{- | A 'FaultReporter' that logs an exhausted effectful-rule evaluation's fault detail
to a katip @WarningS@ line (the @rule@ and @fault@ fields), so a live advisory-database
query fault is diagnosable rather than collapsing to a bare @Unavailable@. The rendered
detail is bounded (the driver's @SQLError@ text or a timeout); it carries no secret (a
'Ecluse.Core.Credential.Secret' redacts under @show@) and never reaches the client
response.
-}
katipFaultReporter :: LogEnv -> FaultReporter
katipFaultReporter logEnv =
    FaultReporter $ \ruleName detail ->
        runKatipContextT logEnv (moduleField "Ecluse.Core.Rules" <> sl "rule" ruleName <> sl "fault" detail) mempty $
            logFM WarningS (ls ("effectful rule evaluation faulted" :: Text))

{- | The readiness gate over the sync plan: ready once every configured
ecosystem's advisory database has first-synced. The flags flip one way, so
readiness never flaps on this; an empty plan (no bucket) is vacuously ready.
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

{- | Build the advisory-sync plan from config: nothing without a configured
vulnerability-database bucket; otherwise one 'CveSyncHandle' per configured
mount ecosystem, each against its own stable per-ecosystem object key and
canonical on-disk path under the OSV data dir. Prepares the data dir (created
if missing; stray @.tmp@ downloads from an interrupted run swept) so the sync
tasks start clean. Note the readiness consequence: an operator who mounts an
ecosystem Pilot does not compile has declared an artifact that never arrives,
and the pod honestly never reports ready.
-}
planCveSync :: LogEnv -> AmbientAws -> AppConfig -> IO (Map.Map Ecosystem CveSyncHandle)
planCveSync logEnv ambient appCfg = case advBucket (cfgAdvisories appCfg) of
    Nothing -> pure Map.empty
    Just bucket -> do
        let dataDir = advDataDir (cfgAdvisories appCfg)
        createDirectoryIfMissing True dataDir
        sweepStaleTemps logEnv dataDir
        awsEnv <- buildS3Env (ambientAwsEndpointUrl ambient >>= parseEndpointUrl)
        Map.fromList <$> traverse (cveSyncHandleFor appCfg awsEnv bucket) (Map.keys (cfgMounts appCfg))

-- One ecosystem's sync wiring: a fresh slot and readiness flag, and the sync
-- environment against the ecosystem's stable object key and canonical on-disk
-- path under the OSV data dir.
cveSyncHandleFor :: AppConfig -> AWS.Env -> Text -> Ecosystem -> IO (Ecosystem, CveSyncHandle)
cveSyncHandleFor appCfg awsEnv bucket eco = do
    slot <- newCveSlot
    ready <- newTVarIO False
    let key = osvDbFileName (ecosystemName eco)
        syncEnv =
            SyncEnv
                { syncFetch = s3CveFetch awsEnv bucket (toText key) (advMaxDatabaseBytes (cfgAdvisories appCfg))
                , syncEcosystem = eco
                , syncDbPath = advDataDir (cfgAdvisories appCfg) </> key
                , syncSlot = slot
                }
    pure (eco, CveSyncHandle{csSlot = slot, csReady = ready, csEnv = syncEnv})

{- | Sweep stray in-progress downloads an interrupted run left beside the canonical
artifacts (relevant to in-pod container restarts, where an @emptyDir@ survives).
Best-effort: a filesystem fault (a read-only or mispermissioned data dir) is logged
at 'WarningS' against the affected path and the boot proceeds on a fresh-start
assumption, since a truly unusable dir surfaces again when the sync task downloads.
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

{- | Run one best-effort step of the stale-temp sweep: an 'IOError' (a read-only or
mispermissioned data dir) is logged at 'WarningS' against the affected path and
swallowed so the boot proceeds, while any non-'IO' exception propagates rather than
being hidden.
-}
sweepStep :: LogEnv -> FilePath -> IO () -> IO ()
sweepStep logEnv path step = step `catchIOError` logSweepFailure logEnv path

-- Warn that a stale-temp sweep step could not touch a path, so a read-only or
-- mispermissioned OSV data dir is visible at boot. The path rides a structured field;
-- the OS error detail is the operator's own filesystem, not untrusted input.
logSweepFailure :: LogEnv -> FilePath -> IOError -> IO ()
logSweepFailure logEnv path err =
    runKatipContextT logEnv payload mempty (logFM WarningS (ls message))
  where
    payload = moduleField "Ecluse.Proxy.CveSync" <> sl "path" (toText path)
    message = "could not sweep stale advisory temp files: " <> show err :: Text
