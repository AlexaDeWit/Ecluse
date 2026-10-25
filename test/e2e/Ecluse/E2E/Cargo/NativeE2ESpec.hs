-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native client compatibility, source identities, authentication and download routing remain observable.
module Ecluse.E2E.Cargo.NativeE2ESpec (cargoNativeContract) where

import Data.Aeson (Value (Array, Object, String), encode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import Test.Hspec

import Ecluse.E2E.Cargo.InstallE2ESpec
import Ecluse.E2E.Cargo.PublishE2ESpec
import Ecluse.E2E.Fixtures.Cargo
import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (..))
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.Cargo
import Ecluse.E2E.Harness.CargoGateway
import Ecluse.E2E.Harness.CargoRegistry
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Phase (timedFixture)
import Ecluse.Test.Contract.Runner (DeclaredGroup (..))
import Ecluse.Test.Contract.Serving (ServingResponse (..))
import Ecluse.Test.Contract.Types (Group)
import Ecluse.Test.Registry.Cargo.Fixtures
import Ecluse.Test.Registry.Cargo.Reference

-- | Each native state owns one existing fixture lifecycle and keeps every named control executable.
cargoNativeContract :: ConcreteFixture Group -> DeclaredGroup
cargoNativeContract fixture = DeclaredGroup (fixtureMetadata fixture) $ \recorder ->
    around (timedFixture recorder (withFixture fixture)) $ do
        it (label "current-client" "builds and installs the owned native graph with the current Cargo client") (clientCase currentCargo False)
        it (label "floor-client" "builds and installs the owned native graph with Cargo 1.74") (clientCase floorCargo True)
        it (label "asset-routing" "rewrites discovery and observes every native archive at the product and public upstream") $ \plane ->
            withObservedCargoProxy defaultE2EConfig plane $ \observed -> do
                discovery <- proxyRequest "GET" [] "" (observedProxy observed) "/cargo/config.json"
                assertDiscovery (observedProxy observed) "/cargo" Nothing discovery
                cargoConsumer (observedProxy observed) currentCargo $ \project -> do
                    invocation <- observeCargo (cargoTraffic observed) project ["build"]
                    void (shouldSucceed (cargoInvocationClient invocation))
                    assertCargoRouting (cargoInvocationTraffic invocation)
        it (label "metadata-no-archives" "serves default policy metadata without requesting archive evidence") $ \plane ->
            withObservedCargoProxy defaultE2EConfig plane $ \observed -> do
                proxyStatus (observedProxy observed) ("/cargo/" <> cargoShard cargoRoot) `shouldReturn` 200
                traffic <- cargoTraffic observed
                filter (T.isSuffixOf ".crate" . cargoRequestPath) (cargoUpstreamRequests traffic) `shouldBe` []
        it (label "denial-control" "denies both native installation and a direct archive request without fetching archive bytes") $ \plane ->
            withObservedCargoProxy cargoDeniedConfig plane $ \observed -> do
                before <- cargoTraffic observed
                cargoConsumer (observedProxy observed) currentCargo $ \project -> void (runCargo project ["build"] >>= shouldFail)
                proxyStatus (observedProxy observed) (cargoArchiveRoute cargoRoot cargoVersion) `shouldReturn` 403
                after <- cargoTraffic observed
                filter (T.isSuffixOf ".crate" . cargoRequestPath) (drop (length (cargoUpstreamRequests before)) (cargoUpstreamRequests after)) `shouldBe` []
        it (label "features" "retains renamed dependencies, default features and forwarded namespaced features") $ \plane ->
            withObservedCargoProxy defaultE2EConfig plane $ \observed -> cargoConsumer (observedProxy observed) currentCargo $ \project -> do
                void (runCargo project ["build"] >>= shouldSucceed)
                ordinary <- cargoRun project [] >>= shouldSucceed
                T.strip (crStdout ordinary) `shouldBe` cargoExpectedOutput False False
                forwarded <- cargoRun project ["--features", "ecluse-e2e-root/forwarded"] >>= shouldSucceed
                T.strip (crStdout forwarded) `shouldBe` cargoExpectedOutput False True
                graph <- readGraph project []
                root <- nativePackage cargoRoot graph
                cargoPackageDependencies root `shouldSatisfy` any (\dependency -> cargoDependencyName dependency == "ecluse-e2e-leaf" && cargoDependencyRename dependency == Just "renamed_leaf" && not (cargoDependencyDefaults dependency) && cargoDependencyFeatures dependency == ["base"])
                cargoPackageDependencies root `shouldSatisfy` any (\dependency -> cargoDependencyName dependency == "ecluse-e2e-extra" && cargoDependencyOptional dependency && not (cargoDependencyDefaults dependency))
        it (label "targets" "retains target conditions and selects native graph edges for each declared platform") $ \plane ->
            withObservedCargoProxy defaultE2EConfig plane $ \observed -> cargoConsumer (observedProxy observed) currentCargo $ \project -> do
                void (cargoResolve project [] >>= shouldSucceed)
                unix <- readGraph project ["--filter-platform", "x86_64-unknown-linux-gnu"]
                windows <- readGraph project ["--filter-platform", "x86_64-pc-windows-gnu"]
                root <- nativePackage cargoRoot unix
                cargoPackageDependencies root `shouldSatisfy` any (\dependency -> cargoDependencyName dependency == "ecluse-e2e-unix" && cargoDependencyTarget dependency == Just "cfg(unix)")
                cargoPackageDependencies root `shouldSatisfy` any (\dependency -> cargoDependencyName dependency == "ecluse-e2e-windows" && cargoDependencyTarget dependency == Just "cfg(windows)")
                activeDependencies cargoRoot unix `shouldSatisfy` elem "ecluse_e2e_unix"
                activeDependencies cargoRoot unix `shouldSatisfy` notElem "ecluse_e2e_windows"
                activeDependencies cargoRoot windows `shouldSatisfy` elem "ecluse_e2e_windows"
                activeDependencies cargoRoot windows `shouldSatisfy` notElem "ecluse_e2e_unix"
        it (label "lock-checksums" "retains native source identities and every exact archive checksum in Cargo.lock") $ \plane ->
            withObservedCargoProxy defaultE2EConfig plane $ \observed -> cargoConsumer (observedProxy observed) floorCargo $ \project -> do
                void (runCargo project ["build"] >>= shouldSucceed)
                releases <- cargoFixtureReleases
                locked <- cargoLock project >>= either (fail . toString) pure
                let packages = [(cargoLockedName entry, cargoLockedVersion entry, cargoLockedChecksum entry, cargoLockedSource entry) | entry <- locked, isJust (cargoLockedSource entry)]
                    expected = [(crateName (releaseCrate release), crateVersion (releaseCrate release), Just (releaseChecksum release), Just "registry+https://github.com/rust-lang/crates.io-index") | release <- releases]
                packages `shouldMatchList` expected
        it (label "msrv" "preserves native Rust-version metadata and rejects an owned crate requiring a newer compiler") $ \plane -> do
            releases <- msrvReleases plane
            root <- cargoRelease releases cargoRoot cargoVersion
            let row = case cargoIndexRecord root of Object fields -> Object (KeyMap.insert "rust_version" (String "99.0") fields); other -> other
            writeCargoRegistry (gdpWorkDir plane </> "cargo") (stubUrl CargoPublic) Nothing False releases
            writeFileLBS (gdpWorkDir plane </> "cargo" </> toString (cargoShard cargoRoot)) (encode row <> "\n")
            for_ [currentCargo, floorCargo] $ \toolchain -> withObservedCargoProxy defaultE2EConfig plane $ \observed -> cargoConsumer (observedProxy observed) toolchain $ \project -> do
                rows <- cargoReadIndex (observedProxy observed) "/cargo/" cargoRoot
                map (KeyMap.lookup "rust_version") rows `shouldBe` [Just (String "99.0")]
                result <- runCargo project ["build"] >>= shouldFail
                (crStdout result <> crStderr result) `shouldSatisfy` T.isInfixOf "99.0"
        it (label "discovery-views" "keeps local discovery, served validators and private cache identity distinct") discoveryViews
        it (label "private-no-public" "reads arbitrary private names and refuses public fallback and first-party public reads") privateNoPublic
        it (label "raw-auth" "retries a native 401 with the raw Cargo credential provider token") (authenticationCase (CargoRegistryRequireToken True))
        it (label "private-auth-required" "uses raw credentials after an auth-required discovery response") (authenticationCase (CargoRegistryRequireToken False))
        it (label "public-token-control" "requires the native edge token while keeping every public exchange anonymous") publicTokenControl
        it (label "private-anonymous" "preserves anonymous native reads when trusted discovery allows them") anonymousPrivate
        it (label "publication-framing" "refuses malformed frames and identity conflicts before any native destination write") publicationFraming
        it (label "foreign-registry" "publishes and builds a private crate with an explicit foreign registry dependency") foreignRegistry
  where
    label name = caseLabel ("cargo/native/" <> name)

clientCase :: CargoToolchain -> Bool -> GlobalDataPlane -> IO ()
clientCase toolchain floorExpected plane = withObservedCargoProxy defaultE2EConfig plane $ \observed ->
    cargoConsumer (observedProxy observed) toolchain $ \project -> do
        (client, compiler) <- cargoVersions project
        void (shouldSucceed client)
        void (shouldSucceed compiler)
        when floorExpected (crStdout client `shouldSatisfy` T.isPrefixOf "cargo 1.74.")
        invocation <- observeCargo (cargoTraffic observed) project ["build"]
        void (shouldSucceed (cargoInvocationClient invocation))
        assertCargoRouting (cargoInvocationTraffic invocation)
        void (cargoInstall project cargoRoot cargoVersion [] >>= shouldSucceed)
        executable <- cargoInstalledExecutable project cargoExecutable >>= shouldSucceed
        T.strip (crStdout executable) `shouldBe` cargoExpectedOutput False False
        graph <- readGraph project []
        root <- nativePackage cargoRoot graph
        cargoPackageRustVersion root `shouldBe` Just "1.74"

readGraph :: CargoProject -> [String] -> IO CargoGraph
readGraph project arguments = cargoMetadata project arguments >>= shouldSucceed >>= either (fail . toString) pure . cargoGraph

nativePackage :: Text -> CargoGraph -> IO CargoPackage
nativePackage name graph = case [package | package <- Map.elems (cargoGraphPackages graph), cargoPackageName package == name] of
    [package] -> pure package
    _ -> fail ("Cargo metadata lacks one package " <> toString name)

activeDependencies :: Text -> CargoGraph -> [Text]
activeDependencies name graph = [alias | (identifier, package) <- Map.toList (cargoGraphPackages graph), cargoPackageName package == name, Just node <- [Map.lookup identifier (cargoGraphNodes graph)], (alias, _, _) <- cargoNodeDependencies node]

assertDiscovery :: E2E -> Text -> Maybe Text -> ServingResponse -> IO ()
assertDiscovery proxy view expectedApi response = do
    responseStatus response `shouldBe` 200
    (download, api, _, _) <- either (fail . toString) pure (referenceDiscovery (LBS.toStrict (responseBody response)))
    download `shouldBe` e2eBaseUrl proxy <> view <> "/crates/{crate}/{version}/{crate}-{version}.crate"
    api `shouldBe` expectedApi
    responseBody response `shouldSatisfy` (not . BS.isInfixOf "cargo-upstream" . LBS.toStrict)

privateConfig :: E2EConfig
privateConfig = defaultE2EConfig{ecExtraEnv = [("ECLUSE_MOUNTS__CARGO__PRIVATE_UPSTREAM__REGISTRY__URL", stubUrl CargoPrivate)]}

withPrivateRegistry :: GlobalDataPlane -> CargoRegistryFault -> (CargoRegistry -> ObservedCargoProxy -> IO a) -> IO a
withPrivateRegistry plane fault action = withCargoRegistry $ \registry -> do
    leaf <- cargoLeafRelease "ecluse-e2e-private" cargoVersion
    seedCargoRegistry registry "cargo-private" [leaf]
    setCargoRegistryFault registry fault
    withStubFault plane (CargoBackend (registryPort registry) False) $
        withObservedCargoProxy privateConfig plane (action registry)

authenticationCase :: CargoRegistryFault -> GlobalDataPlane -> IO ()
authenticationCase fault plane = withPrivateRegistry plane fault $ \registry observed -> do
    cargoPrivateConsumer (observedProxy observed) currentCargo "ecluse-e2e-private" cargoVersion $ \project -> do
        invocation <- observeCargo (cargoTraffic observed) project ["build"]
        void (shouldSucceed (cargoInvocationClient invocation))
        assertCargoViewRouting "/cargo/private" "cargo-private" (cargoInvocationTraffic invocation)
    tokens <- readTVarIO (registryTokens registry)
    tokens `shouldSatisfy` any ((== Just (encodeUtf8 cargoRegistryToken)) . snd)
    when (fault == CargoRegistryRequireToken True) (tokens `shouldSatisfy` any (isNothing . snd))
    logs <- proxyContainerLogs (observedProxy observed)
    logs `shouldSatisfy` not . T.isInfixOf cargoRegistryToken
    public <- filter ((== CargoPublic) . srRoute) <$> stubReadsNow plane
    public `shouldBe` []

anonymousPrivate :: GlobalDataPlane -> IO ()
anonymousPrivate plane = withPrivateRegistry plane CargoRegistryHealthy $ \registry observed ->
    withCargoProject currentCargo CargoAlternate ("sparse+" <> e2eBaseUrl (observedProxy observed) <> "/cargo/private/") Nothing $ \project -> do
        cargoSelectDependency CargoAlternate "ecluse-e2e-private" cargoVersion project
        void (runCargo project ["build"] >>= shouldSucceed)
        tokens <- readTVarIO (registryTokens registry)
        tokens `shouldSatisfy` (not . null)
        map snd tokens `shouldSatisfy` all isNothing

privateNoPublic :: GlobalDataPlane -> IO ()
privateNoPublic plane = withPrivateRegistry plane CargoRegistryHealthy $ \_ observed -> do
    cargoPrivateConsumer (observedProxy observed) currentCargo "ecluse-e2e-private" cargoVersion $ \project -> void (runCargo project ["build"] >>= shouldSucceed)
    cargoPrivateConsumer (observedProxy observed) currentCargo cargoRoot cargoVersion $ \project -> void (runCargo project ["build"] >>= shouldFail)
    proxyStatus (observedProxy observed) ("/cargo/private/" <> cargoShard cargoRoot) `shouldReturn` 404
    filter ((== CargoPublic) . srRoute) <$> stubReadsNow plane >>= (`shouldBe` [])
    let config = privateConfig{ecExtraEnv = ecExtraEnv privateConfig <> [("ECLUSE_MOUNTS__CARGO__FIRST_PARTY", "ecluse-e2e-build")]}
    withObservedCargoProxy config plane $ \guarded -> do
        before <- stubReadsNow plane
        proxyStatus (observedProxy guarded) ("/cargo/" <> cargoShard "ecluse-e2e-build") `shouldReturn` 403
        stubReadsNow plane `shouldReturn` before

discoveryViews :: GlobalDataPlane -> IO ()
discoveryViews plane = withPrivateRegistry plane CargoRegistryHealthy $ \registry observed -> do
    let proxy = observedProxy observed
    releases <- cargoFixtureReleases
    root <- cargoRelease releases cargoRoot cargoVersion
    seedCargoRegistry registry "cargo-private" [root]
    public <- proxyRequest "GET" [] "" proxy "/cargo/config.json"
    private <- proxyRequest "GET" [] "" proxy "/cargo/private/config.json"
    assertDiscovery proxy "/cargo" Nothing public
    assertDiscovery proxy "/cargo/private" Nothing private
    publicIndex <- proxyRequest "GET" [] "" proxy ("/cargo/" <> cargoShard cargoRoot)
    privateIndex <- proxyRequest "GET" [] "" proxy ("/cargo/private/" <> cargoShard cargoRoot)
    responseStatus publicIndex `shouldBe` 200
    responseStatus privateIndex `shouldBe` 200
    publicTag <- maybe (fail "public Cargo view emitted no ETag") pure (lookup "ETag" (responseHeaders publicIndex))
    privateTag <- maybe (fail "private Cargo view emitted no ETag") pure (lookup "ETag" (responseHeaders privateIndex))
    publicTag `shouldNotBe` privateTag
    own <- proxyRequest "GET" [("If-None-Match", publicTag)] "" proxy ("/cargo/" <> cargoShard cargoRoot)
    cross <- proxyRequest "GET" [("If-None-Match", publicTag)] "" proxy ("/cargo/private/" <> cargoShard cargoRoot)
    responseStatus own `shouldBe` 304
    responseStatus cross `shouldBe` 200

publicationFraming :: GlobalDataPlane -> IO ()
publicationFraming plane = withCargoRegistry $ \registry -> withStubFault plane (CargoBackend (registryPort registry) False) $
    withObservedCargoProxy cargoPublicationConfig plane $ \observed -> do
        let proxy = observedProxy observed
        discovery <- proxyRequest "GET" [("Authorization", encodeUtf8 cargoRegistryToken)] "" proxy "/cargo/private/config.json"
        assertDiscovery proxy "/cargo/private" (Just (e2eBaseUrl proxy <> "/cargo/private")) discovery
        before <- readTVarIO (registryRequests registry)
        releases <- cargoFixtureReleases
        release <- cargoRelease releases cargoPublishName cargoVersion
        valid <- either (fail . toString) pure (cargoUpload release)
        conflict <- either (fail . toString) pure (cargoUpload release{releaseCrate = (releaseCrate release){crateName = cargoRetainedName}})
        for_ [conflict, BS.take 3 valid, BS.take (BS.length valid - 1) valid, valid <> "trailing", BS.replicate 4 255 <> BS.drop 4 valid] $ \body -> do
            response <- proxyRequest "PUT" [("Authorization", encodeUtf8 cargoRegistryToken), ("Content-Type", "application/octet-stream")] body (observedProxy observed) "/cargo/private/api/v1/crates/new"
            responseStatus response `shouldBe` 400
        cargoRegistryFiles registry `shouldReturn` Map.empty
        readTVarIO (registryRequests registry) `shouldReturn` before

foreignRegistry :: GlobalDataPlane -> IO ()
foreignRegistry plane = withCargoRegistry $ \registry -> withStubFault plane (CargoBackend (registryPort registry) False) $
    withObservedCargoProxy config plane $ \observed -> do
        let proxy = observedProxy observed
            source = "sparse+" <> e2eBaseUrl proxy <> "/cargo/"
            spec = CargoCrate name cargoVersion "../foreign" [] (Object mempty) (Object mempty) ["Cargo.toml", "README.md", "src/lib.rs"]
        withCargoProject currentCargo CargoAlternate ("sparse+" <> e2eBaseUrl proxy <> "/cargo/private/") (Just cargoRegistryToken) $ \project -> do
            cargoUseFixture project "test/fixtures/cargo/graph" spec
            addForeign project source
            void (cargoPublish project >>= shouldSucceed)
        rows <- cargoReadIndex proxy "/cargo/private/" name
        let registries = [KeyMap.lookup "registry" dependency | row <- rows, Just (Array dependencies) <- [KeyMap.lookup "deps" row], Object dependency <- toList dependencies]
        registries `shouldBe` [Just (String source)]
        cargoPrivateConsumer proxy currentCargo name cargoVersion $ \project -> do
            addForeign project source
            invocation <- observeCargo (cargoTraffic observed) project ["build"]
            void (shouldSucceed (cargoInvocationClient invocation))
            assertCargoViewRouting "/cargo/private" "cargo-private" (cargoInvocationTraffic invocation)
            assertCargoViewRouting "/cargo" "cargo-upstream" (cargoInvocationTraffic invocation)
            graph <- readGraph project []
            package <- nativePackage name graph
            map cargoDependencyRegistry (cargoPackageDependencies package) `shouldBe` [Just source]
        files <- cargoRegistryFiles registry
        Map.member ("cargo-private", name, cargoVersion) files `shouldBe` True
  where
    name = "ecluse-e2e-private-foreign"
    config = cargoPublicationConfig{ecExtraEnv = [(key, if key == "ECLUSE_MOUNTS__CARGO__FIRST_PARTY" then name else value) | (key, value) <- ecExtraEnv cargoPublicationConfig, key /= "ECLUSE_SERVER__AUTH_TOKEN"]}
    addForeign project source = do
        let path = cargoDirectory project </> "home/config.toml"
        existing <- readFileText path
        writeFileText path (existing <> "\n[registries.foreign]\nindex = " <> quoted source <> "\n")
    quoted = decodeUtf8 . LBS.toStrict . encode

msrvReleases :: GlobalDataPlane -> IO [CargoRelease]
msrvReleases plane = do
    let destination = gdpWorkDir plane </> "msrv-source"
    for_ cargoCrates $ \spec -> for_ (crateFiles spec) $ \path -> do
        bytes <- readFileBS ("test/fixtures/cargo/graph" </> crateDirectory spec </> path)
        let target = destination </> crateDirectory spec </> path
            retained = if crateName spec == cargoRoot && path == "Cargo.toml" then encodeUtf8 (T.replace "rust-version = \"1.74\"" "rust-version = \"99.0\"" (decodeUtf8 bytes)) else bytes
        createDirectoryIfMissing True (takeDirectory target)
        writeFileBS target retained
    loadCargoGraph destination

publicTokenControl :: GlobalDataPlane -> IO ()
publicTokenControl plane = withCargoRegistry $ \registry -> do
    releases <- cargoFixtureReleases
    seedCargoRegistry registry "cargo-upstream" releases
    withStubFault plane (CargoPublicBackend (registryPort registry)) $
        withObservedCargoProxy config plane $ \observed -> do
            let index = "sparse+" <> e2eBaseUrl (observedProxy observed) <> "/cargo/"
            withCargoProject currentCargo CargoReplacement index (Just cargoRegistryToken) $ \project -> do
                invocation <- observeCargo (cargoTraffic observed) project ["build"]
                void (shouldSucceed (cargoInvocationClient invocation))
                assertCargoRouting (cargoInvocationTraffic invocation)
            tokens <- readTVarIO (registryTokens registry)
            tokens `shouldSatisfy` (not . null)
            map fst tokens `shouldSatisfy` all (== "cargo-upstream")
            map snd tokens `shouldSatisfy` all isNothing
            before <- readTVarIO (registryRequests registry)
            withCargoProject currentCargo CargoReplacement index (Just "wrong-native-edge-token") $ \project -> void (runCargo project ["build"] >>= shouldFail)
            readTVarIO (registryRequests registry) `shouldReturn` before
  where
    config = defaultE2EConfig{ecExtraEnv = [("ECLUSE_SERVER__AUTH_TOKEN", cargoRegistryToken)]}
