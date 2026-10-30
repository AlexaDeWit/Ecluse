-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | State allocation follows vetted capabilities and binds reads to the configured mirror target.
module Ecluse.Composition.MirrorState.InternalSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec
import UnliftIO.Exception (throwIO)

import Ecluse.Composition.MirrorState (MirrorStateBinding (msbStore, msbTarget, msbView))
import Ecluse.Composition.MirrorState.Internal (newMirrorStatesWith)
import Ecluse.Composition.Support (expectConfig, expectValidated, pypiMirrorEnv, staticEnvVars, testLimits)
import Ecluse.Composition.Validate (ValidatedPlan (vpMounts, vpSettings), VettedMount (vmAdapter, vmConfig))
import Ecluse.Config (AppConfig (cfgMirrorState), MountConfig (mntMirrorTarget))
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Mirror.State (
    MirrorState (..),
    MirrorStateFault,
    StateRead (StateLoaded),
    StateTarget,
    extendProjectRecord,
    stateTarget,
 )
import Ecluse.Core.Mirror.State.Internal (StateRevision (StateRevision))
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Adapter (RegistryAdapter (adapterPublish))
import Ecluse.Core.Registry.Adapter.Capability (AdapterPublish (publishMirrorRequirements), MirrorRequirements (mirrorStateRequirement), MirrorStateRequirement (DurableVersionState))
import Ecluse.Core.Registry.PyPI.Mirror (pypiMirrorDocuments)
import Ecluse.Core.Server.Admission.Meter (withMemoryEntry)
import Ecluse.Core.Server.Cache (CacheConfig (cacheMaxBytes, cacheMaxEntries))
import Ecluse.Core.Server.Cache.Backend.Local (newLocalPool)
import Ecluse.Core.Server.MirrorView (validateMirrorProject)
import Ecluse.Test.Mirror.State (mirrorPackage, mirrorSample)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Server.Cache (defaultCacheConfig)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape), expectRight, testMemoryMeter)

spec :: Spec
spec = describe "durable mirror state allocation" $ do
    it "does not construct storage when state is unconfigured" $ do
        (app, mounts) <- settings pypiMirrorEnv
        bindings <- allocateWith forbiddenAllocator app{cfgMirrorState = Nothing} mounts
        Map.keys bindings `shouldBe` []

    it "does not construct storage for npm's native completion" $ do
        (app, mounts) <- settings (("ECLUSE_MIRROR_STATE__URL", "s3://mirror-state/records") : staticEnvVars)
        bindings <- allocateWith forbiddenAllocator app mounts
        Map.keys bindings `shouldBe` []

    it "does not construct storage for a mount without a publication capability" $ do
        (app, mounts) <- settings pypiMirrorEnv
        let withoutPublish mount = mount{vmAdapter = (vmAdapter mount){adapterPublish = Nothing}}
        bindings <- allocateWith forbiddenAllocator app (map withoutPublish mounts)
        Map.keys bindings `shouldBe` []

    it "does not construct storage for a durable capability without a mirror target" $ do
        (app, mounts) <- settings pypiMirrorEnv
        let withoutTarget mount = mount{vmConfig = (vmConfig mount){mntMirrorTarget = Nothing}}
        bindings <- allocateWith forbiddenAllocator app (map withoutTarget mounts)
        Map.keys bindings `shouldBe` []

    it "binds the allocated store and managed view to PyPI's exact read target" $ do
        (app, mounts) <- settings pypiMirrorEnv
        inventory <- mirrorSample [1] False "https://public.test/" "original source"
        let expectedTarget = stateTarget "https://target.example.test/"
            revision = StateRevision expectedTarget mirrorPackage "revision"
        record <- expectRight (extendProjectRecord testLimits expectedTarget inventory Nothing)
        observedReads <- newIORef []
        let store = readOnlyStore $ \name previous -> do
                modifyIORef' observedReads (<> [(name, previous)])
                pure (Right (StateLoaded revision record))
        calls <- newIORef []
        bindings <- allocateWith (recordingAllocator calls store) app mounts
        Map.keys bindings `shouldBe` [PyPI]
        readIORef calls `shouldReturn` [("mirror-state", "records", expectedTarget)]
        binding <- maybe (fail "missing durable PyPI binding") pure (Map.lookup PyPI bindings)
        msbTarget binding `shouldBe` expectedTarget
        msReadProject (msbStore binding) (const pass) mirrorPackage Nothing `shouldReturn` Right (StateLoaded revision record)
        meter <- testMemoryMeter
        admitted <- withMemoryEntry noopMetricsPort meter $ \ticket -> validateMirrorProject (msbView binding) ticket mirrorPackage
        result <- expectRight admitted
        result `shouldBe` Right (Just (revision, record))
        readIORef observedReads `shouldReturn` [(mirrorPackage, Nothing), (mirrorPackage, Nothing)]

    it "allocates from the vetted npm capability and uses the root storage prefix when absent" $ do
        (app, mounts) <- settings (("ECLUSE_MIRROR_STATE__URL", "s3://root-state") : staticEnvVars)
        let requireDurable mount = mount{vmAdapter = adapter{adapterPublish = fmap durable (adapterPublish adapter)}}
              where
                adapter = vmAdapter mount
            durable publish = publish{publishMirrorRequirements = (publishMirrorRequirements publish){mirrorStateRequirement = DurableVersionState pypiMirrorDocuments}}
        calls <- newIORef []
        let store = readOnlyStore (\_ _ -> unexpected)
            expectedTarget = stateTarget "https://mirror.example.test"
        bindings <- allocateWith (recordingAllocator calls store) app (map requireDurable mounts)
        Map.keys bindings `shouldBe` [Npm]
        readIORef calls `shouldReturn` [("root-state", "", expectedTarget)]
        fmap msbTarget (Map.lookup Npm bindings) `shouldBe` Just expectedTarget

settings :: [(String, String)] -> IO (AppConfig, [VettedMount])
settings env = do
    config <- expectConfig env Nothing
    plan <- expectValidated config
    pure (vpSettings plan, vpMounts plan)

allocateWith :: (Text -> Text -> StateTarget -> IO MirrorState) -> AppConfig -> [VettedMount] -> IO (Map Ecosystem MirrorStateBinding)
allocateWith allocate app mounts = do
    pool <- newLocalPool (cacheMaxEntries defaultCacheConfig) (cacheMaxBytes defaultCacheConfig)
    newMirrorStatesWith allocate app mounts testLimits defaultCacheConfig pool

recordingAllocator :: IORef [(Text, Text, StateTarget)] -> MirrorState -> Text -> Text -> StateTarget -> IO MirrorState
recordingAllocator calls store bucket prefix target = do
    modifyIORef' calls (<> [(bucket, prefix, target)])
    pure store

forbiddenAllocator :: Text -> Text -> StateTarget -> IO MirrorState
forbiddenAllocator _ _ _ = unexpected

readOnlyStore :: (PackageName -> Maybe StateRevision -> IO (Either MirrorStateFault StateRead)) -> MirrorState
readOnlyStore readProject = MirrorState (const readProject) (\_ _ _ _ -> unexpected) (\_ _ -> unexpected) (\_ _ -> unexpected)

unexpected :: IO a
unexpected = throwIO (TestContractEscape "unexpected durable state operation")
