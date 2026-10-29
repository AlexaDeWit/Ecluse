-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Fresh native clients test retained original source facts and failure of incomplete mirror proofs.
module Ecluse.E2E.Cargo.MirrorProofE2ESpec (cargoMirrorProofContract) where

import Data.Aeson (Value (Array, Bool, Object, String), encode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.FilePath ((</>))
import Test.Hspec (around, expectationFailure, it, shouldBe, shouldReturn, shouldSatisfy)

import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Mirror.State (stateTarget)
import Ecluse.Core.Mirror.State.Internal (projectKey)
import Ecluse.Core.Package (mkPackageName)
import Ecluse.E2E.Cargo.InstallE2ESpec (cargoConsumer)
import Ecluse.E2E.Cargo.MirrorInstallE2ESpec
import Ecluse.E2E.Fixtures.Cargo
import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (..))
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.Cargo
import Ecluse.E2E.Harness.CargoRegistry
import Ecluse.E2E.Harness.MirrorState (mirrorRetryAttempts, mutateMirrorStateObject)
import Ecluse.E2E.Harness.NativeEdge
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Phase (timedFixture)
import Ecluse.Test.Contract.Runner (DeclaredGroup (..))
import Ecluse.Test.Contract.Serving (ServingResponse (..))
import Ecluse.Test.Contract.Types
import Ecluse.Test.Contract.Types qualified as Contract
import Ecluse.Test.Registry.Cargo.Fixtures

-- | Each fault owns fresh native stores, queue and durable state through the shared fixture lifecycle.
cargoMirrorProofContract :: DeclaredGroup
cargoMirrorProofContract = DeclaredGroup metadata $ \recorder ->
    around (timedFixture recorder (withFixture fixture)) $ do
        it (label "source-roundtrip" "restores every source row and exact archive after process restart and public outage") sourceRoundtrip
        it (label "pinned-yank" "retains a pinned admitted source yank despite target reset and rejects fresh unpinned resolution") (yankCase True)
        it (label "yank-negative-control" "resolves the same fresh release when its original source does not yank it") (yankCase False)
        it (label "lost-reply" "recovers a physically committed native upload after its response is lost") lostReply
        it (label "missing-archive" "fails a fresh locked native build when the target loses a completed archive") (faultCase MissingArchive)
        it (label "checksum-mutation" "refuses a target checksum which differs from retained source proof") (faultCase ChangedChecksum)
        it (label "missing-state" "excludes unmanaged target rows when retained source state is absent") (faultCase MissingState)
        it (label "corrupt-state" "refuses malformed retained source state") (faultCase CorruptState)
        it (label "private-auth" "requires current target authorisation before exposing retained source metadata") (faultCase PrivateAuthorisation)
  where
    fixture = cargoWriteFixture "mirror-proof" ["read", "mirror", "client-install"]
    metadata =
        Group
            "cargo/mirror-proof"
            "cargo/mirror-proof"
            "cargo/write"
            Contract.E2E
            "cargo"
            [Case ("cargo/mirror-proof/" <> suffix) ["cargo/mirror-proof"] | suffix <- ["source-roundtrip", "pinned-yank", "yank-negative-control", "lost-reply", "missing-archive", "checksum-mutation", "missing-state", "corrupt-state", "private-auth"]]
            ["read", "mirror", "client-install"]
            ["docker", "image", "openssl", "cargo", "cargo-1.74", "rustc"]
            ["proxy", "nginx", "cargo-registry", "ministack"]
            ["docker-daemon-exclusive", "group-mutable-state", "serial-cases"]
    label name = caseLabel ("cargo/mirror-proof/" <> name)

complete :: GlobalDataPlane -> CargoRegistry -> [CargoRelease] -> E2EConfig -> IO ()
complete = completeWithin 120

completeWithin :: Int -> GlobalDataPlane -> CargoRegistry -> [CargoRelease] -> E2EConfig -> IO ()
completeWithin attempts plane registry releases config = withObservedNativeProxy config plane $ \observed -> do
    cargoConsumer (observedProxy observed) currentCargo $ \project -> do
        void (runCargo project ["build"] >>= shouldSucceed)
        void (cargoFetch project >>= shouldSucceed)
    awaitCargoRegistry registry releases `shouldReturn` True
    for_ releases $ \release -> awaitProxyLog (observedProxy observed) (cargoMirrorComplete (crateName (releaseCrate release)) (crateVersion (releaseCrate release))) attempts `shouldReturn` True

sourceRoundtrip :: GlobalDataPlane -> IO ()
sourceRoundtrip plane = withCargoMirrorFixture plane $ \registry releases -> do
    queue <- sharedMirrorQueue plane
    let config = cargoMirrorConfig{ecQueueUrl = Just queue}
        sourceRow release = case cargoIndexRecord release of
            Object fields -> Object (KeyMap.delete "links" fields)
            other -> other
    for_ (ordNub (map (crateName . releaseCrate) releases)) $ \name ->
        writeFileLBS (gdpWorkDir plane </> "cargo" </> toString (cargoShard name)) (foldMap (\release -> encode (sourceRow release) <> "\n") [release | release <- releases, crateName (releaseCrate release) == name])
    complete plane registry releases config
    sourceRoot <- cargoRelease releases cargoRoot cargoVersion
    let sourceTargets = case cargoIndexRecord sourceRoot of
            Object fields -> case KeyMap.lookup "deps" fields of
                Just (Array dependencies) -> [KeyMap.lookup "target" dependency | Object dependency <- toList dependencies]
                _ -> []
            _ -> []
    sourceTargets `shouldSatisfy` elem (Just (String "cfg(all(unix, any(unix, windows)))"))
    files <- cargoRegistryFiles registry
    root <- maybe (fail "native mirror omitted root archive") pure (Map.lookup ("cargo-mirror", cargoRoot, cargoVersion) files)
    case storedRow root of
        Object row -> do
            KeyMap.lookup "pubtime" row `shouldBe` Just (String "2099-01-01T00:00:00Z")
            KeyMap.lookup "ecluse-fixture" row `shouldBe` Nothing
        _ -> expectationFailure "native target row is not an object"
    withStubFault plane (CargoBackend (registryPort registry) True) $
        withObservedNativeProxy config{ecArgs = ["proxy", "--no-worker"]} plane $ \observed -> do
            for_ (ordNub (map (crateName . releaseCrate) releases)) $ \name -> do
                actual <- cargoReadIndex (observedProxy observed) "/cargo/" name
                map Object actual `shouldBe` [sourceRow release | release <- releases, crateName (releaseCrate release) == name]
            for_ releases $ \release -> do
                (status, bytes) <- proxyGet (observedProxy observed) (cargoArchiveRoute (crateName (releaseCrate release)) (crateVersion (releaseCrate release)))
                status `shouldBe` 200
                LBS.toStrict bytes `shouldBe` releaseArchive release
            for_ [currentCargo, floorCargo] $ \toolchain -> cargoConsumer (observedProxy observed) toolchain $ \project -> do
                writeFileText (cargoDirectory project </> "Cargo.lock") (cargoFixtureLock "registry+https://github.com/rust-lang/crates.io-index" releases)
                void (runCargo project ["build", "--locked"] >>= shouldSucceed)

yankCase :: Bool -> GlobalDataPlane -> IO ()
yankCase yanked plane = withCargoMirrorFixture plane $ \registry releases -> do
    queue <- sharedMirrorQueue plane
    root <- cargoRelease releases cargoRoot cargoVersion
    let sourceRow = case cargoIndexRecord root of Object row -> Object (KeyMap.insert "yanked" (Bool yanked) row); other -> other
        config = cargoMirrorConfig{ecQueueUrl = Just queue}
    writeFileLBS (gdpWorkDir plane </> "cargo" </> toString (cargoShard cargoRoot)) (encode sourceRow <> "\n")
    withObservedNativeProxy config plane $ \observed -> do
        cargoConsumer (observedProxy observed) currentCargo $ \project -> do
            writeFileText (cargoDirectory project </> "Cargo.lock") (cargoFixtureLock "registry+https://github.com/rust-lang/crates.io-index" releases)
            void (runCargo project ["build", "--locked"] >>= shouldSucceed)
            void (cargoFetch project >>= shouldSucceed)
        awaitCargoRegistry registry releases `shouldReturn` True
        for_ releases $ \release -> awaitProxyLog (observedProxy observed) (cargoMirrorComplete (crateName (releaseCrate release)) (crateVersion (releaseCrate release))) 120 `shouldReturn` True
    withStubFault plane (CargoBackend (registryPort registry) True) $
        withObservedNativeProxy config{ecArgs = ["proxy", "--no-worker"]} plane $ \observed -> do
            rows <- cargoReadIndex (observedProxy observed) "/cargo/" cargoRoot
            map Object rows `shouldBe` [sourceRow]
            cargoConsumer (observedProxy observed) floorCargo $ \project -> do
                writeFileText (cargoDirectory project </> "Cargo.lock") (cargoFixtureLock "registry+https://github.com/rust-lang/crates.io-index" releases)
                void (runCargo project ["build", "--locked"] >>= shouldSucceed)
            cargoConsumer (observedProxy observed) currentCargo $ \project -> do
                result <- runCargo project ["build"]
                void (if yanked then shouldFail result else shouldSucceed result)

lostReply :: GlobalDataPlane -> IO ()
lostReply plane = withCargoMirrorFixture plane $ \registry releases -> do
    queue <- sharedMirrorQueue plane
    let config = cargoMirrorConfig{ecQueueUrl = Just queue, ecExtraEnv = ecExtraEnv cargoMirrorConfig <> [("ECLUSE_OBSERVABILITY__LOG_LEVEL", "debug")]}
        retryAttempts = mirrorRetryAttempts queue
    setCargoRegistryFault registry CargoRegistryLoseReply
    completeWithin retryAttempts plane registry releases config
    withStubFault plane (CargoBackend (registryPort registry) True) $
        withObservedNativeProxy config{ecArgs = ["proxy", "--no-worker"]} plane $ \observed ->
            cargoConsumer (observedProxy observed) currentCargo $ \project -> void (runCargo project ["build"] >>= shouldSucceed)

data MirrorFault = MissingArchive | ChangedChecksum | MissingState | CorruptState | PrivateAuthorisation

faultCase :: MirrorFault -> GlobalDataPlane -> IO ()
faultCase fault plane = withCargoMirrorFixture plane $ \registry releases -> do
    queue <- sharedMirrorQueue plane
    let config = cargoMirrorConfig{ecQueueUrl = Just queue}
    complete plane registry releases config
    case fault of
        MissingArchive -> setCargoRegistryFault registry (CargoRegistryMissingArchive cargoRoot cargoVersion)
        ChangedChecksum -> atomically (modifyTVar' (registryFiles registry) (Map.adjust changed ("cargo-mirror", cargoRoot, cargoVersion)))
        MissingState -> mutateState plane "DELETE" ""
        CorruptState -> mutateState plane "PUT" "{invalid retained Cargo state"
        PrivateAuthorisation -> setCargoRegistryFault registry (CargoRegistryRequireToken True)
    withStubFault plane (CargoBackend (registryPort registry) True) $
        withObservedNativeProxy config{ecArgs = ["proxy", "--no-worker"]} plane $ \observed -> do
            response <- proxyRequest "GET" [] "" (observedProxy observed) ("/cargo/" <> cargoShard cargoRoot)
            case fault of
                MissingArchive -> proxyStatus (observedProxy observed) (cargoArchiveRoute cargoRoot cargoVersion) `shouldReturn` 503
                PrivateAuthorisation -> do
                    responseStatus response `shouldBe` 401
                    LBS.toStrict (responseBody response) `shouldSatisfy` (not . ("ecluse-fixture" `T.isInfixOf`) . decodeUtf8)
                _ -> responseStatus response `shouldBe` 503
            cargoConsumer (observedProxy observed) currentCargo $ \project -> do
                writeFileText (cargoDirectory project </> "Cargo.lock") (cargoFixtureLock "registry+https://github.com/rust-lang/crates.io-index" releases)
                void (runCargo project ["build", "--locked"] >>= shouldFail)
  where
    changed stored = stored{storedRow = case storedRow stored of Object row -> Object (KeyMap.insert "cksum" (String (T.replicate 64 "0")) row); other -> other}

mutateState :: GlobalDataPlane -> ByteString -> ByteString -> IO ()
mutateState plane = mutateMirrorStateObject plane key
  where
    key = "cargo-mirror/" <> projectKey (stateTarget (stubUrl CargoMirror)) (mkPackageName Cargo Nothing cargoRoot) <> "/project.json"
