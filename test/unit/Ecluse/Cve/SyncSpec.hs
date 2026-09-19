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
import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian, getCurrentTime, nominalDay)
import Database.SQLite.Simple (close, execute_, open)
import Katip (closeScribes)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import UnliftIO.Async (withAsync)
import UnliftIO.Exception (bracket, throwIO)
import UnliftIO.STM (checkSTM)
import UnliftIO.Timeout (timeout)

import Ecluse.Composition.Support (expectAppConfig)
import Ecluse.Core.Breaker (noBreakerReporter)
import Ecluse.Core.Cve (CveDbRejected (CveDbEpssNotEstablished))
import Ecluse.Core.Cve.Slot (newCveSlot, swapIn, withSlotGeneration)
import Ecluse.Core.Cve.Types (DbEtag (..))
import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Osv.Schema (EpssRequirement (..))
import Ecluse.Core.Package (PackageDetails (pkgPublishedAt), mkPackageName)
import Ecluse.Core.Rules (RuleDeps (rdAdvisoryFreshness, rdWithCveLookup), evalRules, prepare)
import Ecluse.Core.Rules.Freshness (
    AdvisoryAge (AdvisoryAge),
    AdvisoryFreshness (AdvisoryAging, AdvisoryFresh, AdvisoryStale, AdvisoryUndated),
    MaxAdvisoryAge,
    maxAdvisoryAgeFor,
 )
import Ecluse.Core.Rules.Outage (OutageReport (..), OutageState (Healthy))
import Ecluse.Core.Rules.Types (Decision (Admitted), DenyIfCveParams (DenyIfCveParams), EvalContext (EvalContext), FailureAlignment (FailNoDecision), PrecededRule (PrecededRule), Rule (AllowIfOlderThan, DenyIfCve), RuleEvidence, SkippedCheck (SkippedUnavailable), completeEvidence, defaultPrecedence)
import Ecluse.Core.Server.Readiness (
    DatabaseRequirement (DatabaseOptional, DatabaseRequired),
    MountReadiness (MountAwaitingFirstSync, MountReady),
    Readiness (AwaitingMounts, Routable),
 )
import Ecluse.Core.Supervision (delayListPolicy)
import Ecluse.Cve.Sync (AdvisoryNeed (..), CveSyncHandle (..), advisoryFreshnessFor, cveRuleDepsFor, cveSyncReadiness, cveSyncScheduleFor, katipOutageReporter, outageReportPeriod, planCveSync, reportPushAge, sweepStaleTemps, sweepStep)
import Ecluse.Runtime.Cve.Sync.Internal (FetchedObject (..), SyncEnv (..), SyncHooks (..), SyncOutcome (..), SyncSchedule (..), absentReportInterval, bootBackoffDelays, runCveSync, syncStep)
import Ecluse.Runtime.Test.Cve (fetchServingAt, headOnlyFetch, refusingFetch)
import Ecluse.Test.Cve (fakeCveDb)
import Ecluse.Test.Env (withAmbientAws)
import Ecluse.Test.Log (captureStdout, jsonLogEnv, newTestLogEnv, runQuietKatip)
import Ecluse.Test.Osv (mkMinimalValidDbWithMeta)
import Ecluse.Test.Package (sampleDetails, v1_0_0)
import Ecluse.Test.Port (noopAdvisorySyncMetricsPort, recordingAdvisorySyncTracingPort)

spec :: Spec
spec = do
    describe "planCveSync -- the per-ecosystem advisory-sync plan" $ do
        it "plans nothing without a configured advisory store" $ do
            cfg <- expectAppConfig [] Nothing
            logEnv <- newTestLogEnv
            plan <- planCveSync logEnv Nothing cfg []
            Map.keys plan `shouldBe` []

        it "plans one handle per configured mount ecosystem and prepares the data dir" $
            withSystemTempDirectory "ecluse-cve-sync-plan" $ \dir -> withAmbientAws $ do
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
                plan <- planCveSync logEnv Nothing cfg [needFor Npm EpssRequired DatabaseRequired, needFor PyPI EpssOptional DatabaseOptional]
                Map.keys plan `shouldBe` [Npm, PyPI]
                Map.map (syncEpssRequirement . csEnv) plan `shouldBe` Map.fromList [(Npm, EpssRequired), (PyPI, EpssOptional)]
                for_ (Map.lookup Npm plan) $ \handle -> do
                    syncEcosystem (csEnv handle) `shouldBe` Npm
                    syncDbPath (csEnv handle) `shouldBe` dataDir </> "npm-osv-schema4.db"
                    syncStoreRef (csEnv handle) `shouldBe` "s3://advisories"
                    -- Not ready and serving nothing until the first sync.
                    readTVarIO (csReady handle) `shouldReturn` False
                    withSlotGeneration (syncSlot (csEnv handle)) (pure . isJust) `shouldReturn` False
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
            swapIn (syncSlot (csEnv handle)) (DbEtag "e1") Nothing (fakeCveDb [])
            let deps = cveRuleDepsFor (Map.singleton Npm handle) noBreakerReporter noOutageReport
            rdWithCveLookup (deps Npm) (pure . isJust) `shouldReturn` True

        it "abstains for an ecosystem the plan does not carry" $ do
            handle <- stubSyncHandle
            swapIn (syncSlot (csEnv handle)) (DbEtag "e1") Nothing (fakeCveDb [])
            let deps = cveRuleDepsFor (Map.singleton Npm handle) noBreakerReporter noOutageReport
            rdWithCveLookup (deps PyPI) (pure . isJust) `shouldReturn` False

    describe "cveRuleDepsFor -- bounded outage reporting for the rules" $ do
        it "reports an absent database once as an outage, not once per evaluation, and its recovery" $ do
            clock <- newIORef alarmNow
            handle <- stubHandleAt sixDayLimit (readIORef clock)
            captured <- newIORef []
            let deps = cveRuleDepsFor (Map.singleton Npm handle) noBreakerReporter (\eco r -> modifyIORef' captured ((eco, r) :))
            rules <- prepare (deps Npm) skipPolicy
            decisions <- replicateM 20 (evalRules evalCtx rules oldVersion)
            -- Every admission carries the evidence, and the outage reports once.
            forM_ decisions $ \case
                Admitted "AllowIfOlderThan" _ skipped -> skipped `shouldBe` [SkippedUnavailable "DenyIfCve" "no advisory database loaded"]
                other -> expectationFailure ("expected the quarantine allow, got " <> show other)
            reverse <$> readIORef captured `shouldReturn` [(Npm, OutageBegan "DenyIfCve" "no advisory database loaded")]
            -- Past the reminder gap the outage reports again, still once.
            writeIORef clock (addUTCTime outageReportPeriod alarmNow)
            replicateM_ 5 (evalRules evalCtx rules oldVersion)
            reverse <$> readIORef captured
                `shouldReturn` [ (Npm, OutageBegan "DenyIfCve" "no advisory database loaded")
                               , (Npm, OutageContinues alarmNow (Map.singleton "DenyIfCve" "no advisory database loaded"))
                               ]
            -- A synced database ends it: one recovery, and clean admissions after.
            install handle (agoDays 1)
            evalRules evalCtx rules oldVersion >>= \case
                Admitted _ _ skipped -> skipped `shouldBe` []
                other -> expectationFailure ("expected an admission, got " <> show other)
            replicateM_ 5 (evalRules evalCtx rules oldVersion)
            reverse <$> readIORef captured
                `shouldReturn` [ (Npm, OutageBegan "DenyIfCve" "no advisory database loaded")
                               , (Npm, OutageContinues alarmNow (Map.singleton "DenyIfCve" "no advisory database loaded"))
                               , (Npm, OutageRecovered alarmNow)
                               ]

        it "paces the reminder on the unloaded-database report's own gap" $
            outageReportPeriod `shouldBe` fromIntegral absentReportInterval / 1_000_000

        it "reports nowhere for an ecosystem the plan does not carry" $ do
            handle <- stubSyncHandle
            captured <- newIORef []
            let deps = cveRuleDepsFor (Map.singleton Npm handle) noBreakerReporter (\eco r -> modifyIORef' captured ((eco, r) :))
            rules <- prepare (deps PyPI) skipPolicy
            void (evalRules evalCtx rules oldVersion)
            readIORef captured `shouldReturn` []

    describe "katipOutageReporter -- the outage lines" $ do
        it "logs the start and the reminder at Error, naming the ecosystem, the rule, and the cause" $ do
            logEnv <- jsonLogEnv
            logged <- captureStdout $ do
                katipOutageReporter logEnv Npm (OutageBegan "DenyIfCve" "no advisory database loaded")
                katipOutageReporter logEnv Npm (OutageContinues alarmNow (Map.fromList [("DenyIfCve", "the rule source circuit breaker is open"), ("DenyIfEpss", "the rule threw: boom")]))
                void (closeScribes logEnv)
            length (filter (T.isInfixOf "\"sev\":\"Error\"") (lines logged)) `shouldBe` 2
            logged `shouldSatisfy` T.isInfixOf "\"module\":\"Ecluse.Core.Rules\""
            logged `shouldSatisfy` T.isInfixOf "\"ecosystem\":\"npm\""
            logged `shouldSatisfy` T.isInfixOf "\"rule\":\"DenyIfCve\""
            logged `shouldSatisfy` T.isInfixOf "\"cause\":\"no advisory database loaded\""
            logged `shouldSatisfy` T.isInfixOf "outage began"
            logged `shouldSatisfy` T.isInfixOf "outage continues"
            logged `shouldSatisfy` T.isInfixOf "DenyIfCve: the rule source circuit breaker is open; DenyIfEpss: the rule threw: boom"

        it "logs the recovery at Info, so the paging level carries only the outage" $ do
            logEnv <- jsonLogEnv
            logged <- captureStdout $ do
                katipOutageReporter logEnv Npm (OutageRecovered alarmNow)
                void (closeScribes logEnv)
            logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Info\""
            logged `shouldSatisfy` (not . T.isInfixOf "\"sev\":\"Error\"")
            logged `shouldSatisfy` T.isInfixOf "outage recovered"

    describe "advisoryFreshnessFor -- the push-age reading the rules gate on" $ do
        it "is fresh before the first sync, leaving the absent-database path to decide" $ do
            handle <- stubHandleAt sixDayLimit (pure alarmNow)
            advisoryFreshnessFor (Map.singleton Npm handle) Npm `shouldReturn` AdvisoryFresh

        it "is fresh for an ecosystem the plan does not carry" $ do
            handle <- stubHandleAt sixDayLimit (pure alarmNow)
            advisoryFreshnessFor (Map.singleton Npm handle) PyPI `shouldReturn` AdvisoryFresh

        it "expires a push past the maximum and keeps it across a poll that swaps nothing" $ do
            handle <- stubHandleAt sixDayLimit (pure alarmNow)
            install handle (agoDays 9)
            reading <- advisoryFreshnessFor (Map.singleton Npm handle) Npm
            reading `shouldSatisfy` isStale
            advisoryFreshnessFor (Map.singleton Npm handle) Npm `shouldReturn` reading

        it "refuses on a serving generation the store gave no publication time for" $ do
            handle <- stubHandleAt sixDayLimit (pure alarmNow)
            swapIn (syncSlot (csEnv handle)) (DbEtag "e1") Nothing (fakeCveDb [])
            let fetch = headOnlyFetch (Right (Just (FetchedObject (DbEtag "e1") Nothing)))
            void (syncStep (csEnv handle){syncFetch = fetch} (Just (DbEtag "e1")))
            advisoryFreshnessFor (Map.singleton Npm handle) Npm `shouldReturn` AdvisoryUndated

        it "resets on a fresh push of the same artifact" $ do
            handle <- stubHandleAt sixDayLimit (pure alarmNow)
            install handle (agoDays 9)
            let fetch = headOnlyFetch (Right (Just (FetchedObject (DbEtag "e1") (Just (agoDays 1)))))
            syncStep (csEnv handle){syncFetch = fetch} (Just (DbEtag "e1")) >>= \case
                SyncUnchanged -> pass
                other -> expectationFailure ("expected publication observation, got " <> show other)
            advisoryFreshnessFor (Map.singleton Npm handle) Npm `shouldReturn` AdvisoryFresh

        it "expires retained qualified evidence after a rejected replacement and republication" $
            withSystemTempDirectory "epss-retained-age" $ \dir -> do
                clock <- newIORef alarmNow
                handle <- stubHandleAt sixDayLimit (readIORef clock)
                let env = (csEnv handle){syncDbPath = dir </> "npm.db", syncEpssRequirement = EpssRequired}
                    plan = Map.singleton Npm handle
                    good = fetchServingAt (Just (agoDays 5)) (Just "good") (\dest -> mkMinimalValidDbWithMeta dest "pkg" [("epss_status", "available")])
                    bad = fetchServingAt (Just alarmNow) (Just "bad") (\dest -> mkMinimalValidDbWithMeta dest "pkg" [])
                void (syncStep env{syncFetch = good} Nothing)
                advisoryFreshnessFor plan Npm `shouldReturn` AdvisoryAging (AdvisoryAge (agoDays 5) (5 * nominalDay) (6 * nominalDay))
                syncStep env{syncFetch = bad} (Just (DbEtag "good")) >>= \case
                    SyncRejected _ rejection -> rejection `shouldBe` CveDbEpssNotEstablished
                    other -> expectationFailure ("expected qualification rejection, got " <> show other)
                writeIORef clock (addUTCTime (2 * nominalDay) alarmNow)
                let repeated = headOnlyFetch (Right (Just (FetchedObject (DbEtag "bad") (Just (addUTCTime (2 * nominalDay) alarmNow)))))
                void (syncStep env{syncFetch = repeated} (Just (DbEtag "bad")))
                advisoryFreshnessFor plan Npm >>= (`shouldSatisfy` isStale)

        it "carries that reading onto the mount's rule capabilities" $ do
            handle <- stubHandleAt sixDayLimit (pure alarmNow)
            install handle (agoDays 9)
            let deps = cveRuleDepsFor (Map.singleton Npm handle) noBreakerReporter noOutageReport
            rdAdvisoryFreshness (deps Npm) >>= (`shouldSatisfy` isStale)

    describe "reportPushAge -- the consumer's early warning" $
        it "logs at Error once per crossing, naming the push, the age, and the limit" $ do
            logEnv <- jsonLogEnv
            handle <- stubHandleAt sixDayLimit (pure alarmNow)
            logged <- captureStdout $ do
                install handle (agoDays 4)
                reportPushAge logEnv Npm handle
                -- Latched: a second poll over the same push stays silent.
                reportPushAge logEnv Npm handle
                -- A fresh push re-arms the alarm, and the next crossing reports again.
                install handle (agoDays 1)
                reportPushAge logEnv Npm handle
                install handle (agoDays 5)
                reportPushAge logEnv Npm handle
                void (closeScribes logEnv)
            T.count "\"sev\":\"Error\"" logged `shouldBe` 2
            logged `shouldSatisfy` T.isInfixOf "\"ecosystem\":\"npm\""
            logged `shouldSatisfy` T.isInfixOf "\"age_seconds\":345600"
            logged `shouldSatisfy` T.isInfixOf "\"max_age_seconds\":518400"
            logged `shouldSatisfy` T.isInfixOf "\"pushed_at\":\"2026-09-08T00:00:00Z\""

    describe "cveSyncReadiness -- the per-mount first-sync verdict" $ do
        it "is routable with no advisory store (an empty plan)" $
            cveSyncReadiness Map.empty `shouldReturn` Routable Map.empty

        it "awaits the mounts while neither artifact exists" $ do
            (plan, _) <- twoMountPlan
            cveSyncReadiness plan `shouldReturn` AwaitingMounts (bothAt MountAwaitingFirstSync)

        it "reports a mount whose rules never deny on the database ready before any sync" $ do
            npmHandle <- stubSyncHandle
            pypiHandle <- stubSyncHandle
            let plan = Map.fromList [(Npm, npmHandle), (PyPI, pypiHandle{csDatabase = DatabaseOptional})]
            cveSyncReadiness plan
                `shouldReturn` Routable (Map.fromList [(Npm, MountAwaitingFirstSync), (PyPI, MountReady)])

        it "keeps npm routable while the PyPI artifact is missing, then reports the recovery" $ do
            (plan, (npmHandle, pypiHandle)) <- twoMountPlan
            landed npmHandle
            -- The isolation the owner ruled on: npm stays routable and PyPI is named as awaiting.
            cveSyncReadiness plan `shouldReturn` Routable (Map.fromList [(Npm, MountReady), (PyPI, MountAwaitingFirstSync)])
            landed pypiHandle
            cveSyncReadiness plan `shouldReturn` Routable (bothAt MountReady)

        it "keeps an optional ecosystem ready when the required ecosystem rejects its artifact" $
            withSystemTempDirectory "epss-readiness" $ \dir -> do
                (plan, (npmHandle, pypiHandle)) <- twoMountPlan
                let npmEnv = (csEnv npmHandle){syncDbPath = dir </> "npm.db", syncEpssRequirement = EpssRequired, syncFetch = markerFree Npm}
                    pypiEnv = (csEnv pypiHandle){syncDbPath = dir </> "pypi.db", syncEcosystem = PyPI, syncFetch = markerFree PyPI}
                    markerFree eco = fetchServingAt (Just alarmNow) (Just "legacy") $ \dest -> do
                        mkMinimalValidDbWithMeta dest "pkg" []
                        when (eco == PyPI) $ bracket (open dest) close $ \conn -> execute_ conn "UPDATE meta SET value = 'pypi' WHERE key = 'ecosystem'"
                    schedule = SyncSchedule [] 600_000_000 absentReportInterval
                done <- newTVarIO (0 :: Int)
                (tracing, _) <- recordingAdvisorySyncTracingPort
                let run handle env = runQuietKatip $ runCveSync noopAdvisorySyncMetricsPort tracing env schedule (SyncHooks (landed handle) (atomically (modifyTVar' done (+ 1))))
                withAsync (run npmHandle npmEnv) $ \_ ->
                    withAsync (run pypiHandle pypiEnv) $ \_ -> do
                        timeout 5_000_000 (atomically (readTVar done >>= checkSTM . (>= 2))) `shouldReturn` Just ()
                        cveSyncReadiness plan `shouldReturn` Routable (Map.fromList [(Npm, MountAwaitingFirstSync), (PyPI, MountReady)])
                        withSlotGeneration (syncSlot npmEnv) (pure . isJust) `shouldReturn` False
                        withSlotGeneration (syncSlot pypiEnv) (pure . isJust) `shouldReturn` True

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
            schedAbsentReport schedule `shouldBe` absentReportInterval

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

-- One mount's ask of the advisory stack, for the planning cases.
needFor :: Ecosystem -> EpssRequirement -> DatabaseRequirement -> AdvisoryNeed
needFor eco epss database =
    AdvisoryNeed{anEcosystem = eco, anMaxAge = sixDayLimit, anEpss = epss, anDatabase = database}

-- A fresh plan handle whose transport refuses unless the test replaces it.
stubSyncHandle :: IO CveSyncHandle
stubSyncHandle = stubHandleAt sixDayLimit getCurrentTime

-- | As 'stubSyncHandle', under a chosen maximum and clock, for the push-age cases.
stubHandleAt :: MaxAdvisoryAge -> IO UTCTime -> IO CveSyncHandle
stubHandleAt maxAge clock = do
    slot <- newCveSlot
    ready <- newTVarIO False
    alarmed <- newTVarIO False
    outage <- newTVarIO Healthy
    pure
        CveSyncHandle
            { csReady = ready
            , csEnv =
                SyncEnv
                    { syncFetch = refusingFetch
                    , syncEcosystem = Npm
                    , syncEpssRequirement = EpssOptional
                    , syncDbPath = "unused.db"
                    , syncSlot = slot
                    , syncStoreRef = "s3://advisories"
                    }
            , csMaxAge = maxAge
            , csClock = clock
            , csAgeAlarmed = alarmed
            , csOutage = outage
            , csDatabase = DatabaseRequired
            }

-- | Discard outage reports, for a case about the capabilities rather than the outage line.
noOutageReport :: Ecosystem -> OutageReport -> IO ()
noOutageReport _ _ = pass

-- | The issue's reproduction: the shipped quarantine beside an opt-in advisory deny set to skip.
skipPolicy :: [PrecededRule]
skipPolicy = [PrecededRule (defaultPrecedence r) r | r <- [AllowIfOlderThan (7 * nominalDay), DenyIfCve (DenyIfCveParams 8.0 FailNoDecision)]]

-- | An old public version the quarantine admits, evaluated at 'alarmNow'.
oldVersion :: RuleEvidence
oldVersion = completeEvidence (sampleDetails (mkPackageName Npm Nothing "acme") v1_0_0){pkgPublishedAt = Just (agoDays 30)}

evalCtx :: EvalContext
evalCtx = EvalContext alarmNow Nothing

-- | The maximum a mount deriving from the shipped seven-day quarantine gets: six days.
sixDayLimit :: MaxAdvisoryAge
sixDayLimit = maxAdvisoryAgeFor Nothing [AllowIfOlderThan (7 * nominalDay)]

-- | A fixed "now" so the push-age cases read the same age on every run.
alarmNow :: UTCTime
alarmNow = UTCTime (fromGregorian 2026 9 12) 0

agoDays :: Integer -> UTCTime
agoDays days = addUTCTime (negate (fromInteger days * nominalDay)) alarmNow

-- One generation landing with the given publication time, as a successful sync would install it.
install :: CveSyncHandle -> UTCTime -> IO ()
install handle pushedAt = swapIn (syncSlot (csEnv handle)) (DbEtag "e1") (Just pushedAt) (fakeCveDb [])

isStale :: AdvisoryFreshness -> Bool
isStale = \case
    AdvisoryStale{} -> True
    _ -> False

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
