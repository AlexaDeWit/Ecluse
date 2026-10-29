-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Configured Cargo execution policy reaches native clients, worker readmission and store revocation.
module Ecluse.E2E.Cargo.ExecutionE2ESpec (cargoExecutionContract) where

import Data.Aeson (object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import Test.Hspec (around, it, shouldBe, shouldReturn, shouldSatisfy)

import Ecluse.E2E.Cargo.InstallE2ESpec (assertCargoAssets, assertCargoRouting, cargoConsumer)
import Ecluse.E2E.Fixtures.Cargo
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.Cargo
import Ecluse.E2E.Harness.CargoDredger qualified as Native
import Ecluse.E2E.Harness.CargoDredger.Client (cargoSweepClientOps)
import Ecluse.E2E.Harness.NativeEdge
import Ecluse.E2E.Harness.CargoRegistry
import Ecluse.E2E.Harness.CodeArtifact (codeArtifactDeleteRequests)
import Ecluse.E2E.Harness.Sweep (minAgeRule, permissiveRules, renderRules)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Phase (timedFixture)
import Ecluse.Test.Contract.Runner (DeclaredGroup (..))
import Ecluse.Test.Contract.Serving (ServingResponse (..))
import Ecluse.Test.Contract.Sweep.Types (ClientOps (clientPublish))
import Ecluse.Test.Contract.Types
import Ecluse.Test.Contract.Types qualified as Contract
import Ecluse.Test.Registry.Cargo.Fixtures

-- | Fixed cases require the image and both native clients in the normal Cargo subject.
cargoExecutionContract :: DeclaredGroup
cargoExecutionContract = DeclaredGroup executionGroup $ \recorder ->
    around (timedFixture recorder (withDataPlane cargoTopology{topologyAws = True})) $ do
        it (label "build-script" "denies an owned root build script before native clients download its archive") (deniedNative cargoRoot)
        it (label "proc-macro" "checks the procedural macro through its own named source and denies native clients") (deniedNative macroName)
        it (label "plain-source" "admits ordinary Rust source and builds it with both native clients") plainNative
        it (label "exact-override" "admits a higher-ranked exact override without reading execution evidence at metadata admission") exactOverride
        it (label "unavailable-evidence" "fails required execution policy when the admitted archive is unavailable") unavailableEvidence
        it (label "corrupt-evidence" "fails required execution policy when archive bytes disagree with the admitted checksum") corruptEvidence
        it (label "unsupported-evidence" "fails required execution policy for a checksum-correct unsupported manifest declaration") unsupportedEvidence
        it (label "default-metadata" "keeps default metadata archive-free while both clients can build the owned execution graph") defaultMetadata
        it (label "worker-readmission" "rechecks queued archives under the configured worker policy before publication") workerReadmission
        it (label "dredger-revocation" "reads authenticated stored archives and removes execution hooks under configured revocation policy") dredgerRevocation
  where
    label name = caseLabel ("cargo/execution/" <> name)

executionGroup :: Group
executionGroup =
    Group
        { groupId = "cargo/execution"
        , groupContractId = "cargo/execution"
        , groupProfileId = "cargo/execution"
        , groupTier = Contract.E2E
        , groupPartition = "cargo"
        , groupCases = [Case ("cargo/execution/" <> name) ["cargo/execution"] | name <- ["build-script", "proc-macro", "plain-source", "exact-override", "unavailable-evidence", "corrupt-evidence", "unsupported-evidence", "default-metadata", "worker-readmission", "dredger-revocation"]]
        , groupCapabilities = ["read", "mirror", "maintenance", "client-install"]
        , groupPrerequisites = ["docker", "image", "openssl", "cargo", "cargo-1.74", "rustc"]
        , groupResources = ["proxy", "nginx", "cargo-registry", "codeartifact-fixture", "ministack"]
        , groupIsolation = ["docker-daemon-exclusive", "group-mutable-state", "serial-cases"]
        }

macroName :: Text
macroName = "ecluse-e2e-macro"

plainName :: Text
plainName = "ecluse-execution-plain"

executionRules :: Bool -> Text
executionRules revoke =
    renderRules
        [ minAgeRule
        , "execution" .= object ["type" .= ("DenyInstallTimeExecution" :: Text), "appliesTo" .= phases]
        ]
  where
    phases = if revoke then ["admission", "revocation"] else ["admission"] :: [Text]

executionConfig :: E2EConfig
executionConfig = defaultE2EConfig{ecExtraEnv = [("ECLUSE_RULES", executionRules False)]}

withPublicRegistry :: GlobalDataPlane -> [CargoRelease] -> (CargoRegistry -> IO a) -> IO a
withPublicRegistry plane releases action = withCargoRegistry $ \registry -> do
    seedCargoRegistry registry "cargo-upstream" releases
    withStubFault plane (CargoPublicBackend (registryPort registry)) (action registry)

deniedNative :: Text -> GlobalDataPlane -> IO ()
deniedNative name plane = do
    releases <- cargoFixtureReleases
    withPublicRegistry plane releases $ \registry ->
        withObservedNativeProxy executionConfig plane $ \observed -> do
            before <- nativeTraffic observed
            response <- proxyRequest "GET" [] "" (observedProxy observed) ("/cargo/" <> cargoShard name)
            responseStatus response `shouldBe` 403
            decodeUtf8 (LBS.toStrict (responseBody response)) `shouldSatisfy` T.isInfixOf "runs code on install"
            evidence <- nativeTraffic observed >>= nativeTrafficSince before
            publicArchives evidence `shouldBe` [(archivePath name, 200)]
            archiveRequests (nativeClientRequests evidence) `shouldBe` []
            for_ [currentCargo, floorCargo] $ \toolchain ->
                selectedConsumer observed toolchain name $ \project -> do
                    invocation <- observeCargo (nativeTraffic observed) project ["build"]
                    void (shouldFail (cargoInvocationClient invocation))
                    archiveRequests (nativeClientRequests (cargoInvocationTraffic invocation)) `shouldBe` []
            proxyStatus (observedProxy observed) (cargoArchiveRoute name cargoVersion) `shouldReturn` 403
            assertPublicAnonymous registry

plainNative :: GlobalDataPlane -> IO ()
plainNative plane = do
    plain <- cargoLeafRelease plainName cargoVersion
    withPublicRegistry plane [plain] $ \registry ->
        withObservedNativeProxy executionConfig plane $ \observed -> do
            before <- nativeTraffic observed
            cargoReadVersions (observedProxy observed) "/cargo/" plainName `shouldReturn` [cargoVersion]
            evidence <- nativeTraffic observed >>= nativeTrafficSince before
            publicArchives evidence `shouldBe` [(archivePath plainName, 200)]
            archiveRequests (nativeClientRequests evidence) `shouldBe` []
            for_ [currentCargo, floorCargo] $ \toolchain ->
                selectedConsumer observed toolchain plainName $ \project -> do
                    invocation <- observeCargo (nativeTraffic observed) project ["build"]
                    void (shouldSucceed (cargoInvocationClient invocation))
                    assertCargoAssets project (cargoInvocationTraffic invocation)
            assertPublicAnonymous registry

exactOverride :: GlobalDataPlane -> IO ()
exactOverride plane = do
    releases <- cargoFixtureReleases
    let rules = renderRules [minAgeRule, "execution" .= object ["type" .= ("DenyInstallTimeExecution" :: Text)], "override" .= object ["type" .= ("AllowByIdentity" :: Text), "identity" .= (macroName <> "@" <> cargoVersion), "precedence" .= (301 :: Int)]]
        config = defaultE2EConfig{ecExtraEnv = [("ECLUSE_RULES", rules)]}
    withPublicRegistry plane releases $ \registry ->
        withObservedNativeProxy config plane $ \observed -> do
            before <- nativeTraffic observed
            cargoReadVersions (observedProxy observed) "/cargo/" macroName `shouldReturn` [cargoVersion]
            metadata <- nativeTraffic observed >>= nativeTrafficSince before
            publicArchives metadata `shouldBe` []
            archiveRequests (nativeClientRequests metadata) `shouldBe` []
            for_ [currentCargo, floorCargo] $ \toolchain ->
                selectedConsumer observed toolchain macroName $ \project -> do
                    invocation <- observeCargo (nativeTraffic observed) project ["build"]
                    void (shouldSucceed (cargoInvocationClient invocation))
                    assertCargoAssets project (cargoInvocationTraffic invocation)
                    assertCargoRouting (cargoInvocationTraffic invocation)
            assertPublicAnonymous registry

unavailableEvidence :: GlobalDataPlane -> IO ()
unavailableEvidence plane = do
    plain <- cargoLeafRelease plainName cargoVersion
    withPublicRegistry plane [plain] $ \registry -> do
        setCargoRegistryFault registry (CargoRegistryMissingArchive plainName cargoVersion)
        failedEvidence plane registry plainName "archive source answered HTTP 404" 404

corruptEvidence :: GlobalDataPlane -> IO ()
corruptEvidence plane = do
    plain <- cargoLeafRelease plainName cargoVersion
    withPublicRegistry plane [plain] $ \registry -> do
        atomically $ modifyTVar' (registryFiles registry) (Map.adjust (\stored -> stored{storedArchive = "corrupt owned archive bytes"}) ("cargo-upstream", plainName, cargoVersion))
        failedEvidence plane registry plainName "Cargo compressed archive disagrees with its admitted SHA-256" 200

unsupportedEvidence :: GlobalDataPlane -> IO ()
unsupportedEvidence plane = do
    plain <- cargoLeafReleaseWithManifest plainName cargoVersion (T.replace "[package]" "[package]\nbuild = []")
    withPublicRegistry plane [plain] $ \registry ->
        failedEvidence plane registry plainName "unsupported Cargo build declaration" 200

failedEvidence :: GlobalDataPlane -> CargoRegistry -> Text -> Text -> Int -> IO ()
failedEvidence plane registry name diagnostic sourceStatus =
    withObservedNativeProxy executionConfig plane $ \observed -> do
        before <- nativeTraffic observed
        response <- proxyRequest "GET" [] "" (observedProxy observed) ("/cargo/" <> cargoShard name)
        responseStatus response `shouldBe` 500
        decodeUtf8 (LBS.toStrict (responseBody response)) `shouldSatisfy` T.isInfixOf "could not be evaluated"
        awaitProxyLog (observedProxy observed) (T.isInfixOf diagnostic) 40 `shouldReturn` True
        evidence <- nativeTraffic observed >>= nativeTrafficSince before
        publicArchives evidence `shouldBe` [(archivePath name, sourceStatus)]
        for_ [currentCargo, floorCargo] $ \toolchain ->
            selectedConsumer observed toolchain name $ \project -> do
                invocation <- observeCargo (nativeTraffic observed) project ["build"]
                void (shouldFail (cargoInvocationClient invocation))
                archiveRequests (nativeClientRequests (cargoInvocationTraffic invocation)) `shouldBe` []
        assertPublicAnonymous registry

defaultMetadata :: GlobalDataPlane -> IO ()
defaultMetadata plane = withObservedNativeProxy defaultE2EConfig plane $ \observed -> do
    before <- nativeTraffic observed
    for_ [cargoRoot, macroName] $ \name -> cargoReadVersions (observedProxy observed) "/cargo/" name `shouldReturn` [cargoVersion]
    metadata <- nativeTraffic observed >>= nativeTrafficSince before
    publicArchives metadata `shouldBe` []
    for_ [currentCargo, floorCargo] $ \toolchain ->
        cargoConsumer (observedProxy observed) toolchain $ \project -> do
            invocation <- observeCargo (nativeTraffic observed) project ["build"]
            void (shouldSucceed (cargoInvocationClient invocation))
            assertCargoAssets project (cargoInvocationTraffic invocation)
            assertCargoRouting (cargoInvocationTraffic invocation)

selectedConsumer :: ObservedNativeProxy -> CargoToolchain -> Text -> (CargoProject -> IO a) -> IO a
selectedConsumer observed toolchain name action =
    withCargoProject toolchain CargoReplacement (observedProxy observed) ("sparse+" <> e2eBaseUrl (observedProxy observed) <> "/cargo/") Nothing $ \project -> do
        cargoSelectDependency CargoReplacement name cargoVersion project
        action project

archiveRequests :: [NativeRequest] -> [NativeRequest]
archiveRequests = filter (T.isSuffixOf ".crate" . nativeRequestPath)

archivePath :: Text -> Text
archivePath name = "/" <> cargoArchivePathFor name cargoVersion

publicArchives :: NativeTraffic -> [(Text, Int)]
publicArchives traffic = [(nativeRequestPath request, nativeRequestStatus request) | request <- archiveRequests (nativeUpstreamRequests traffic), nativeRequestHost request == "cargo-upstream"]

assertPublicAnonymous :: CargoRegistry -> IO ()
assertPublicAnonymous registry = do
    tokens <- readTVarIO (registryTokens registry)
    tokens `shouldSatisfy` (not . null)
    map snd tokens `shouldSatisfy` all isNothing

withExecutionStores :: GlobalDataPlane -> (Native.CargoSweepPlane -> [CargoRelease] -> IO a) -> IO a
withExecutionStores global action = Native.withCargoSweepPlane global $ \plane -> do
    graph <- cargoFixtureReleases
    plain <- cargoLeafRelease plainName cargoVersion
    root <- cargoRelease graph cargoRoot cargoVersion
    macro <- cargoRelease graph macroName cargoVersion
    let releases = [root, macro, plain]
        sources = Native.cspReleases plane <> graph <> [plain]
    writeCargoRegistry (gdpWorkDir global </> "cargo") (stubUrl CargoPublic) Nothing False sources
    action plane{Native.cspReleases = sources} releases

workerReadmission :: GlobalDataPlane -> IO ()
workerReadmission global = withExecutionStores global $ \plane _ -> do
    queue <- sharedMirrorQueue global
    Native.withSweepProxy plane [("ECLUSE_RULES", permissiveRules)] (Just queue) ["proxy", "--no-worker"] $ \store ->
        for_ [cargoRoot, macroName, plainName] $ \name ->
            proxyRequest "GET" [("Authorization", encodeUtf8 cargoRegistryToken)] "" (observedProxy (Native.cssObserved store)) (cargoArchiveRoute name cargoVersion) >>= (`shouldBe` 200) . responseStatus
    cargoRegistryFiles (Native.cspRegistry plane) `shouldReturn` Map.empty
    before <- stubReadsNow global
    let environment = Native.proxyRoleEnv <> [("ECLUSE_RULES", executionRules False), ("ECLUSE_SERVER__PUBLIC_URL", "http://127.0.0.1:4873"), ("ECLUSE_QUEUE__URL", queue), ("AWS_ENDPOINT_URL_SQS", "http://ministack:4566")]
    withMirrorRole global environment $ \worker -> do
        for_ [cargoRoot, macroName] $ \name ->
            awaitContainerLog worker (T.isInfixOf ("current policy denies " <> name <> "@" <> cargoVersion <> ": blocked by DenyInstallTimeExecution")) 120 `shouldReturn` True
        awaitContainerLog worker (T.isInfixOf ("mirror version complete: " <> plainName <> "@" <> cargoVersion)) 120 `shouldReturn` True
        logs <- containerLogs worker
        logs `shouldSatisfy` T.isInfixOf "runs code on install"
        for_ [cargoRoot, macroName] $ \name -> do
            cargoRegistryVersions (Native.cspRegistry plane) "cargo-mirror" name `shouldReturn` []
            logs `shouldSatisfy` not . T.isInfixOf ("mirrored artifact published: " <> name <> "@" <> cargoVersion)
        cargoRegistryVersions (Native.cspRegistry plane) "cargo-mirror" plainName `shouldReturn` [cargoVersion]
        after <- stubReadsNow global
        let workerRequests = drop (length before) after
        for_ [cargoRoot, macroName, plainName] $ \name ->
            workerRequests `shouldSatisfy` any (\request -> srRoute request == CargoPublic && srMethod request == "GET" && srPath request == archivePath name && srStatus request == 200)

dredgerRevocation :: GlobalDataPlane -> IO ()
dredgerRevocation global = withExecutionStores global $ \plane releases -> do
    queue <- sharedMirrorQueue global
    Native.withSweepProxy plane [("ECLUSE_RULES", permissiveRules)] (Just queue) ["proxy", "--no-worker"] $ \store -> do
        for_ [Native.directStore store, Native.cacheStore store] $ \target ->
            for_ releases $ \release ->
                void (clientPublish cargoSweepClientOps target (crateName (releaseCrate release)) (crateVersion (releaseCrate release)) >>= shouldSucceed)
        for_ [store, Native.cacheStore store] $ \target ->
            for_ [cargoRoot, macroName, plainName] $ \name -> Native.storeVersions target name `shouldReturn` [cargoVersion]
        let registry = Native.cspRegistry plane
        atomically (writeTVar (registryTokens registry) [])
        atomically (writeTVar (registryRequests registry) [])
        before <- stubReadsNow global
        deletes <- codeArtifactDeleteRequests (Native.cspControl plane)
        run <- runRoleOnce global (Native.sweepRoleEnv <> [("ECLUSE_RULES", executionRules True), ("ECLUSE_DREDGER__FULL_WALK", "true")]) ["dredger", "--once"]
        (roleExit run, roleOutput run) `shouldSatisfy` ((== ExitSuccess) . fst)
        roleOutput run `shouldSatisfy` T.isInfixOf "runs code on install"
        for_ [store, Native.cacheStore store] $ \target -> do
            for_ [cargoRoot, macroName] $ \name -> do
                Native.storeVersions target name `shouldReturn` []
                (fst <$> Native.storeArtifactBytes target name cargoVersion) `shouldReturn` 404
            Native.storeVersions target plainName `shouldReturn` [cargoVersion]
        made <- drop (length deletes) <$> codeArtifactDeleteRequests (Native.cspControl plane)
        made `shouldBe` replicate 4 ("POST", "v1/package/versions/delete")
        requests <- readTVarIO (registryRequests registry)
        for_ ["cargo-mirror", "cargo-private"] $ \host ->
            for_ [cargoRoot, macroName, plainName] $ \name ->
                requests `shouldSatisfy` any (\request -> nativeRequestHost request == host && nativeRequestMethod request == "GET" && nativeRequestPath request == archivePath name && nativeRequestStatus request == 200)
        tokens <- readTVarIO (registryTokens registry)
        map snd tokens `shouldSatisfy` all (== Just (encodeUtf8 cargoRegistryToken))
        after <- stubReadsNow global
        [request | request <- drop (length before) after, srRoute request == CargoPublic] `shouldBe` []
