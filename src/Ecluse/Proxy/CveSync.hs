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
    cveRuleDepsFor,
    cveSyncReady,
    cveSyncScheduleFor,
) where

import Amazonka qualified as AWS
import Data.Map.Strict qualified as Map
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
import System.FilePath (isExtensionOf, (</>))
import UnliftIO.Exception (catchAny)

import Ecluse.Composition.MirrorQueue (parseEndpointUrl)
import Ecluse.Config (
    AppConfig (cfgAwsEndpointUrl, cfgCveDbPollInterval, cfgMaxOsvDbBytes, cfgMounts, cfgOsvDataDir, cfgVulnerabilityDatabaseBucket),
 )
import Ecluse.Core.Breaker (BreakerReporter)
import Ecluse.Core.Cve.Slot (CveSlot, currentAdvisoryEtag, newCveSlot, withSlotLookup)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Osv.Schema (osvDbFileName)
import Ecluse.Core.Rules (RuleDeps (..))
import Ecluse.Runtime.Cve.Sync (SyncEnv (..), SyncSchedule (SyncSchedule, schedBootBackoff, schedPollDelay), bootBackoffDelays, s3CveFetch)
import Ecluse.Runtime.Pilot.Export (buildS3Env)

{- | The rules' boot-bound capabilities for one mount ecosystem: the CVE
lookup borrows through that ecosystem's own slot when the sync plan carries
one, and abstains otherwise, so a mount's rules can never read a neighbouring
ecosystem's advisory database.
-}
cveRuleDepsFor :: Map.Map Ecosystem CveSyncHandle -> BreakerReporter -> Ecosystem -> RuleDeps
cveRuleDepsFor plan reporter eco =
    RuleDeps
        { rdWithCveLookup = maybe (\use -> use Nothing) (withSlotLookup . csSlot) (Map.lookup eco plan)
        , rdCurrentAdvisoryEtag = maybe (pure Nothing) (currentAdvisoryEtag . csSlot) (Map.lookup eco plan)
        , rdBreakerReporter = reporter
        }

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
        , schedPollDelay = round (cfgCveDbPollInterval env) * 1_000_000
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
planCveSync :: AppConfig -> IO (Map.Map Ecosystem CveSyncHandle)
planCveSync appCfg = case cfgVulnerabilityDatabaseBucket appCfg of
    Nothing -> pure Map.empty
    Just bucket -> do
        let dataDir = cfgOsvDataDir appCfg
        createDirectoryIfMissing True dataDir
        sweepStaleTemps dataDir
        awsEnv <- buildS3Env (cfgAwsEndpointUrl appCfg >>= parseEndpointUrl)
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
                { syncFetch = s3CveFetch awsEnv bucket (toText key) (cfgMaxOsvDbBytes appCfg)
                , syncEcosystem = eco
                , syncDbPath = cfgOsvDataDir appCfg </> key
                , syncSlot = slot
                }
    pure (eco, CveSyncHandle{csSlot = slot, csReady = ready, csEnv = syncEnv})

-- Sweep stray in-progress downloads an interrupted run left beside the
-- canonical artifacts (relevant to in-pod container restarts, where an
-- emptyDir survives). Best-effort: an unreadable dir is a fresh start.
sweepStaleTemps :: FilePath -> IO ()
sweepStaleTemps dataDir =
    ( do
        entries <- listDirectory dataDir
        forM_ [e | e <- entries, "tmp" `isExtensionOf` e] (\e -> removeFile (dataDir </> e) `catchAny` const pass)
    )
        `catchAny` const pass
