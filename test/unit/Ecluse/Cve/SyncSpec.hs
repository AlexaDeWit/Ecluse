-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Advisory sync planning and lifecycle regressions.
Artifact paths follow the shared schema epoch.
-}
module Ecluse.Cve.SyncSpec (spec) where

import Control.Retry (simulatePolicy)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Katip (closeScribes)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.Environment (setEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import UnliftIO.Exception (throwIO)

import Ecluse.Composition.Support (expectAppConfig)
import Ecluse.Core.Breaker (noBreakerReporter)
import Ecluse.Core.Cve (DbEtag (..))
import Ecluse.Core.Cve.Slot (newCveSlot, swapIn, withSlotLookup)
import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Rules (RuleDeps (rdWithCveLookup))
import Ecluse.Core.Server.Readiness (
    MountReadiness (MountAwaitingFirstSync, MountReady),
    Readiness (AwaitingMounts, Routable),
 )
import Ecluse.Core.Supervision (delayListPolicy)
import Ecluse.Cve.Sync (CveSyncHandle (..), cveRuleDepsFor, cveSyncReadiness, cveSyncScheduleFor, planCveSync, sweepStaleTemps, sweepStep)
import Ecluse.Runtime.Cve.Sync (SyncEnv (..), SyncSchedule (..), bootBackoffDelays)
import Ecluse.Runtime.Test.Cve (refusingFetch)
import Ecluse.Test.Cve (fakeCveDb)
import Ecluse.Test.Log (captureStdout, jsonLogEnv, newTestLogEnv)
import Ecluse.Test.Rules (noFaultReporter)

spec :: Spec
spec = do
    describe "planCveSync -- the per-ecosystem advisory-sync plan" $ do
        it "plans nothing without a configured advisory store" $ do
            cfg <- expectAppConfig [] Nothing
            logEnv <- newTestLogEnv
            plan <- planCveSync logEnv Nothing cfg []
            Map.keys plan `shouldBe` []

        it "plans one handle per configured mount ecosystem and prepares the data dir" $
            withSystemTempDirectory "ecluse-cve-sync-plan" $ \dir -> do
                setDummyAwsCredentials
                let dataDir = dir </> "osv"
                -- A stale in-progress download and a canonical artifact from a
                -- previous run: the sweep removes the former and keeps the latter.
                createDirectoryIfMissing True dataDir
                writeFileBS (dataDir </> "npm-osv-schema4.db.tmp") "stale partial download"
                writeFileBS (dataDir </> "npm-osv-schema4.db") "prior artifact"
                cfg <-
                    expectAppConfig
                        [ ("ECLUSE_ADVISORIES__URL", "s3://advisories")
                        , ("ECLUSE_ADVISORIES__DATA_DIR", dataDir)
                        ]
                        (Just mountedNpmDoc)
                logEnv <- newTestLogEnv
                plan <- planCveSync logEnv Nothing cfg [Npm]
                Map.keys plan `shouldBe` [Npm]
                for_ (Map.lookup Npm plan) $ \handle -> do
                    syncEcosystem (csEnv handle) `shouldBe` Npm
                    syncDbPath (csEnv handle) `shouldBe` dataDir </> "npm-osv-schema4.db"
                    -- Not ready and serving nothing until the first sync.
                    readTVarIO (csReady handle) `shouldReturn` False
                    withSlotLookup (syncSlot (csEnv handle)) (pure . isJust) `shouldReturn` False
                doesFileExist (dataDir </> "npm-osv-schema4.db.tmp") `shouldReturn` False
                doesFileExist (dataDir </> "npm-osv-schema4.db") `shouldReturn` True

    describe "sweepStep -- the sweep's best-effort filesystem boundary" $ do
        it "propagates a non-IO exception rather than swallowing it" $ do
            logEnv <- newTestLogEnv
            sweepStep logEnv "/srv/osv" (throwIO SweepBoom) `shouldThrow` (\SweepBoom -> True)

        it "swallows an IOError, logs it at Warning against the path, and returns so boot proceeds" $
            withSystemTempDirectory "ecluse-sweep-io" $ \dir -> do
                logEnv <- jsonLogEnv
                let missing = dir </> "npm-osv-schema4.db.tmp"
                logged <- captureStdout $ do
                    -- Removing a file that is not there raises an 'IOError': the step must
                    -- log it and return, not propagate it.
                    sweepStep logEnv missing (removeFile missing)
                    void (closeScribes logEnv)
                logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Warning\""
                logged `shouldSatisfy` T.isInfixOf "\"module\":\"Ecluse.Cve.Sync\""
                logged `shouldSatisfy` T.isInfixOf "npm-osv-schema4.db.tmp"
                logged `shouldSatisfy` T.isInfixOf "could not sweep"

    describe "sweepStaleTemps -- the whole-directory sweep" $
        it "swallows a listing fault on a missing dir and returns, logging it at Warning" $
            withSystemTempDirectory "ecluse-sweep-missing" $ \dir -> do
                logEnv <- jsonLogEnv
                logged <- captureStdout $ do
                    sweepStaleTemps logEnv (dir </> "missing")
                    void (closeScribes logEnv)
                logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Warning\""
                logged `shouldSatisfy` T.isInfixOf "\"module\":\"Ecluse.Cve.Sync\""

    describe "cveRuleDepsFor -- per-ecosystem capability dispatch" $ do
        it "borrows through the mount ecosystem's own slot" $ do
            handle <- stubSyncHandle
            swapIn (syncSlot (csEnv handle)) (DbEtag "e1") (fakeCveDb [])
            let deps = cveRuleDepsFor (Map.singleton Npm handle) noBreakerReporter noFaultReporter
            rdWithCveLookup (deps Npm) (pure . isJust) `shouldReturn` True

        it "abstains for an ecosystem the plan does not carry" $ do
            handle <- stubSyncHandle
            swapIn (syncSlot (csEnv handle)) (DbEtag "e1") (fakeCveDb [])
            let deps = cveRuleDepsFor (Map.singleton Npm handle) noBreakerReporter noFaultReporter
            rdWithCveLookup (deps PyPI) (pure . isJust) `shouldReturn` False

    describe "cveSyncReadiness -- the per-mount first-sync verdict" $ do
        it "is routable with no advisory store (an empty plan)" $
            cveSyncReadiness Map.empty `shouldReturn` Routable Map.empty

        it "awaits the mounts while neither artifact exists" $ do
            (plan, _) <- twoMountPlan
            cveSyncReadiness plan `shouldReturn` AwaitingMounts (bothAt MountAwaitingFirstSync)

        it "keeps npm routable while the PyPI artifact is missing, then reports the recovery" $ do
            (plan, (npmHandle, pypiHandle)) <- twoMountPlan
            landed npmHandle
            -- The isolation the owner ruled on: npm stays routable and PyPI is named as awaiting.
            cveSyncReadiness plan `shouldReturn` Routable (Map.fromList [(Npm, MountReady), (PyPI, MountAwaitingFirstSync)])
            landed pypiHandle
            cveSyncReadiness plan `shouldReturn` Routable (bothAt MountReady)

        it "keeps PyPI routable while the npm artifact is missing" $ do
            (plan, (_, pypiHandle)) <- twoMountPlan
            landed pypiHandle
            cveSyncReadiness plan `shouldReturn` Routable (Map.fromList [(Npm, MountAwaitingFirstSync), (PyPI, MountReady)])

    describe "cveSyncScheduleFor" $
        it "converts the configured poll interval to microseconds over the shipped burst" $ do
            cfg <- expectAppConfig [("ECLUSE_ADVISORIES__POLL_INTERVAL", "90")] Nothing
            let schedule = cveSyncScheduleFor cfg
            schedPollDelay schedule `shouldBe` 90_000_000
            schedBootBackoff schedule `shouldBe` bootBackoffDelays

    describe "the boot burst compiled to a retry policy" $
        it "attempts immediately, backs off over the shipped schedule, then concedes" $ do
            -- 'simulatePolicy' walks the policy without sleeping, so this case pins the burst
            -- schedule directly. The length of 'bootBackoffDelays' is the retry budget.
            delays <- simulatePolicy (length bootBackoffDelays) (delayListPolicy bootBackoffDelays)
            map snd delays `shouldBe` map Just bootBackoffDelays <> [Nothing]

-- An npm and a PyPI mount, neither having synced yet, with their handles for flipping.
twoMountPlan :: IO (Map.Map Ecosystem CveSyncHandle, (CveSyncHandle, CveSyncHandle))
twoMountPlan = do
    npmHandle <- stubSyncHandle
    pypiHandle <- stubSyncHandle
    pure (Map.fromList [(Npm, npmHandle), (PyPI, pypiHandle)], (npmHandle, pypiHandle))

-- Both mounts in the same state, the expectation either artifact's absence is read against.
bothAt :: MountReadiness -> Map.Map Ecosystem MountReadiness
bothAt readiness = Map.fromList [(Npm, readiness), (PyPI, readiness)]

-- One mount's first sync landing, which is the only way its flag flips.
landed :: CveSyncHandle -> IO ()
landed handle = atomically (writeTVar (csReady handle) True)

-- A handle as 'planCveSync' would build it, minus the transport (the tests
-- above never fetch): a fresh empty slot and a readiness flag at False.
stubSyncHandle :: IO CveSyncHandle
stubSyncHandle = do
    slot <- newCveSlot
    ready <- newTVarIO False
    pure
        CveSyncHandle
            { csReady = ready
            , csEnv =
                SyncEnv
                    { syncFetch = refusingFetch
                    , syncEcosystem = Npm
                    , syncDbPath = "unused.db"
                    , syncSlot = slot
                    }
            }

-- The S3 env discovers credentials from the process environment. The plan only wires
-- the transport and makes no request, so dummies satisfy it.
setDummyAwsCredentials :: IO ()
setDummyAwsCredentials = do
    setEnv "AWS_ACCESS_KEY_ID" "test"
    setEnv "AWS_SECRET_ACCESS_KEY" "test"
    setEnv "AWS_REGION" "us-east-1"

mountedNpmDoc :: ByteString
mountedNpmDoc =
    "{\"server\":{\"publicUrl\":\"https://registry.example.test\"},\
    \\"mounts\":{\"npm\":{\
    \\"privateUpstream\":{\"registry\":{\"url\":\"https://private.example.test\"}},\
    \\"publicUpstream\":{\"registry\":{\"url\":\"https://registry.npmjs.org\"}},\
    \\"mirrorTarget\":{\"registry\":{\"url\":\"https://mirror.example.test\",\"token\":\"token\"}}}}}"

-- | A non-'IO' exception, to prove the sweep does not swallow every fault.
data SweepBoom = SweepBoom
    deriving stock (Show)

instance Exception SweepBoom
