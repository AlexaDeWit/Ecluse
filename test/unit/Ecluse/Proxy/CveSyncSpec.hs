module Ecluse.Proxy.CveSyncSpec (spec) where

import Data.Map.Strict qualified as Map
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (setEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import UnliftIO.Exception (throwString)

import Ecluse.Config (AppConfig, Config (configApp), loadConfig)
import Ecluse.Core.Breaker (noBreakerReporter)
import Ecluse.Core.Cve (CveDb (..), DbEtag (..))
import Ecluse.Core.Cve.Slot (newCveSlot, swapIn, withSlotLookup)
import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Rules (RuleDeps (rdWithCveLookup))
import Ecluse.Proxy.CveSync (CveSyncHandle (..), cveRuleDepsFor, cveSyncReady, cveSyncScheduleFor, planCveSync)
import Ecluse.Runtime.Cve.Sync (CveFetch (..), SyncEnv (..), SyncSchedule (..), bootBackoffDelays)
import Ecluse.Test.Cve (fakeCveLookup)

spec :: Spec
spec = do
    describe "planCveSync -- the per-ecosystem advisory-sync plan" $ do
        it "plans nothing without a configured advisory bucket" $ do
            cfg <- appConfigFrom [] Nothing
            plan <- planCveSync cfg
            Map.keys plan `shouldBe` []

        it "plans one handle per configured mount ecosystem and prepares the data dir" $
            withSystemTempDirectory "ecluse-proxy-plan" $ \dir -> do
                setDummyAwsCredentials
                let dataDir = dir </> "osv"
                -- A stale in-progress download and a canonical artifact from a
                -- previous run: the sweep takes the former, keeps the latter.
                createDirectoryIfMissing True dataDir
                writeFileBS (dataDir </> "npm-osv-schema3.db.tmp") "stale partial download"
                writeFileBS (dataDir </> "npm-osv-schema3.db") "prior artifact"
                cfg <-
                    appConfigFrom
                        [ ("ECLUSE_VULNERABILITY_DATABASE_BUCKET", "advisories")
                        , ("ECLUSE_OSV_DATA_DIR", dataDir)
                        ]
                        (Just mountedNpmDoc)
                plan <- planCveSync cfg
                Map.keys plan `shouldBe` [Npm]
                for_ (Map.lookup Npm plan) $ \handle -> do
                    syncEcosystem (csEnv handle) `shouldBe` Npm
                    syncDbPath (csEnv handle) `shouldBe` dataDir </> "npm-osv-schema3.db"
                    -- Not ready and serving nothing until the first sync.
                    readTVarIO (csReady handle) `shouldReturn` False
                    withSlotLookup (csSlot handle) (pure . isJust) `shouldReturn` False
                doesFileExist (dataDir </> "npm-osv-schema3.db.tmp") `shouldReturn` False
                doesFileExist (dataDir </> "npm-osv-schema3.db") `shouldReturn` True

    describe "cveRuleDepsFor -- per-ecosystem capability dispatch" $ do
        it "borrows through the mount ecosystem's own slot" $ do
            handle <- stubSyncHandle
            swapIn (csSlot handle) (DbEtag "e1") fakeDb
            let deps = cveRuleDepsFor (Map.singleton Npm handle) noBreakerReporter
            rdWithCveLookup (deps Npm) (pure . isJust) `shouldReturn` True

        it "abstains for an ecosystem the plan does not carry" $ do
            handle <- stubSyncHandle
            swapIn (csSlot handle) (DbEtag "e1") fakeDb
            let deps = cveRuleDepsFor (Map.singleton Npm handle) noBreakerReporter
            rdWithCveLookup (deps PyPI) (pure . isJust) `shouldReturn` False

    describe "cveSyncReady -- the first-sync readiness gate" $ do
        it "is vacuously ready with no advisory bucket (an empty plan)" $
            cveSyncReady Map.empty `shouldReturn` True

        it "waits for every configured ecosystem, then reports ready" $ do
            npmHandle <- stubSyncHandle
            pypiHandle <- stubSyncHandle
            let plan = Map.fromList [(Npm, npmHandle), (PyPI, pypiHandle)]
            cveSyncReady plan `shouldReturn` False
            atomically (writeTVar (csReady npmHandle) True)
            cveSyncReady plan `shouldReturn` False
            atomically (writeTVar (csReady pypiHandle) True)
            cveSyncReady plan `shouldReturn` True

    describe "cveSyncScheduleFor" $
        it "converts the configured poll interval to microseconds over the shipped burst" $ do
            cfg <- appConfigFrom [("ECLUSE_CVE_DB_POLL_INTERVAL", "90")] Nothing
            let schedule = cveSyncScheduleFor cfg
            schedPollDelay schedule `shouldBe` 90_000_000
            schedBootBackoff schedule `shouldBe` bootBackoffDelays

-- A handle as 'planCveSync' would build it, minus the transport (the tests
-- above never fetch): a fresh empty slot and a readiness flag at False.
stubSyncHandle :: IO CveSyncHandle
stubSyncHandle = do
    slot <- newCveSlot
    ready <- newTVarIO False
    pure
        CveSyncHandle
            { csSlot = slot
            , csReady = ready
            , csEnv =
                SyncEnv
                    { syncFetch =
                        CveFetch
                            { fetchHeadEtag = throwString "stubSyncHandle: no fetch in these tests"
                            , fetchDownload = \_ -> throwString "stubSyncHandle: no fetch in these tests"
                            }
                    , syncEcosystem = Npm
                    , syncDbPath = "unused.db"
                    , syncSlot = slot
                    }
            }

-- An owning handle over an in-memory lookup; closing is a no-op.
fakeDb :: CveDb
fakeDb = CveDb{cveDbLookup = fakeCveLookup [], cveDbClose = pass, cveDbMeta = []}

appConfigFrom :: [(String, String)] -> Maybe ByteString -> IO AppConfig
appConfigFrom envVars doc = case loadConfig envVars doc of
    Right c -> pure (configApp c)
    Left e -> fail ("Config error: " <> show e)

-- 'Ecluse.Runtime.Pilot.Export.buildS3Env' discovers credentials from the process
-- environment; the plan only wires the transport (no request is made), so
-- dummies satisfy it.
setDummyAwsCredentials :: IO ()
setDummyAwsCredentials = do
    setEnv "AWS_ACCESS_KEY_ID" "test"
    setEnv "AWS_SECRET_ACCESS_KEY" "test"
    setEnv "AWS_REGION" "us-east-1"

mountedNpmDoc :: ByteString
mountedNpmDoc =
    "{\"queueBackend\":\"sqs\",\"mounts\":{\"npm\":{\
    \\"privateUpstream\":\"https://private.example.test\",\
    \\"publicUpstream\":\"https://registry.npmjs.org\",\
    \\"respectUpstreamTarballHost\":false,\
    \\"mirrorTarget\":\"https://mirror.example.test\",\"credentialProvider\":\"codeartifact\"}}}"
