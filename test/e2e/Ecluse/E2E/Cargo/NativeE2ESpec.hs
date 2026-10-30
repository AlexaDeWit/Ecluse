-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native client compatibility, the merged registry, authentication and download routing remain observable.
module Ecluse.E2E.Cargo.NativeE2ESpec (cargoNativeContract) where

import Data.Aeson (Value (Array, Null, Number, Object, String), encode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import Test.Hspec (around, it, shouldBe, shouldMatchList, shouldReturn, shouldSatisfy)

import Ecluse.E2E.Cargo.InstallE2ESpec
import Ecluse.E2E.Cargo.PublishE2ESpec
import Ecluse.E2E.Fixtures.Cargo
import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (..))
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.Cargo
import Ecluse.E2E.Harness.CargoRegistry
import Ecluse.E2E.Harness.NativeEdge
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
        it (label "current-client" "builds, installs and publishes with the current Cargo client and requires trusted TLS") (clientCase currentCargo False)
        it (label "floor-client" "builds, installs and publishes with Cargo 1.74 and requires trusted TLS") (clientCase floorCargo True)
        it (label "asset-routing" "rewrites discovery and observes every native archive at the product and public upstream") $ \plane ->
            withObservedNativeProxy defaultE2EConfig plane $ \observed -> do
                discovery <- proxyRequest "GET" [] "" (observedProxy observed) "/cargo/config.json"
                assertDiscovery (observedProxy observed) Nothing False discovery
                cargoConsumer (observedProxy observed) currentCargo $ \project -> do
                    invocation <- observeCargo (nativeTraffic observed) project ["build"]
                    void (shouldSucceed (cargoInvocationClient invocation))
                    assertCargoRouting (cargoInvocationTraffic invocation)
                    assertCargoAssets project (cargoInvocationTraffic invocation)
        it (label "metadata-no-archives" "serves default policy metadata without requesting archive evidence") $ \plane ->
            withObservedNativeProxy defaultE2EConfig plane $ \observed -> do
                proxyStatus (observedProxy observed) ("/cargo/" <> cargoShard cargoRoot) `shouldReturn` 200
                traffic <- nativeTraffic observed
                filter (T.isSuffixOf ".crate" . nativeRequestPath) (nativeUpstreamRequests traffic) `shouldBe` []
        it (label "denial-control" "denies both native installation and a direct archive request without fetching archive bytes") $ \plane ->
            withObservedNativeProxy cargoDeniedConfig plane $ \observed -> do
                before <- nativeTraffic observed
                cargoConsumer (observedProxy observed) currentCargo $ \project -> void (runCargo project ["build"] >>= shouldFail)
                proxyStatus (observedProxy observed) (cargoArchiveRoute cargoRoot cargoVersion) `shouldReturn` 403
                after <- nativeTraffic observed
                filter (T.isSuffixOf ".crate" . nativeRequestPath) (drop (length (nativeUpstreamRequests before)) (nativeUpstreamRequests after)) `shouldBe` []
        it (label "features" "retains renamed dependencies, default features and forwarded namespaced features") $ \plane ->
            withObservedNativeProxy defaultE2EConfig plane $ \observed -> cargoConsumer (observedProxy observed) currentCargo $ \project -> do
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
            withObservedNativeProxy defaultE2EConfig plane $ \observed -> cargoConsumer (observedProxy observed) currentCargo $ \project -> do
                void (cargoResolve project [] >>= shouldSucceed)
                unix <- readGraph project ["--filter-platform", "x86_64-unknown-linux-gnu"]
                windows <- readGraph project ["--filter-platform", "x86_64-pc-windows-gnu"]
                root <- nativePackage cargoRoot unix
                cargoPackageDependencies root `shouldSatisfy` any (\dependency -> cargoDependencyName dependency == "ecluse-e2e-unix" && cargoDependencyTarget dependency == Just "cfg(all(unix, any(unix, windows)))")
                cargoPackageDependencies root `shouldSatisfy` any (\dependency -> cargoDependencyName dependency == "ecluse-e2e-windows" && cargoDependencyTarget dependency == Just "cfg(windows)")
                activeDependencies cargoRoot unix `shouldSatisfy` elem "ecluse_e2e_unix"
                activeDependencies cargoRoot unix `shouldSatisfy` notElem "ecluse_e2e_windows"
                activeDependencies cargoRoot windows `shouldSatisfy` elem "ecluse_e2e_windows"
                activeDependencies cargoRoot windows `shouldSatisfy` notElem "ecluse_e2e_unix"
        it (label "lock-checksums" "retains native source identities and every exact archive checksum in Cargo.lock") $ \plane ->
            withObservedNativeProxy defaultE2EConfig plane $ \observed -> cargoConsumer (observedProxy observed) floorCargo $ \project -> do
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
            for_ [currentCargo, floorCargo] $ \toolchain -> withObservedNativeProxy defaultE2EConfig plane $ \observed -> cargoConsumer (observedProxy observed) toolchain $ \project -> do
                rows <- cargoReadIndex (observedProxy observed) "/cargo/" cargoRoot
                map (KeyMap.lookup "rust_version") rows `shouldBe` [Just (String "99.0")]
                result <- runCargo project ["build"] >>= shouldFail
                (crStdout result <> crStderr result) `shouldSatisfy` T.isInfixOf "99.0"
        it (label "local-discovery" "answers discovery locally, requires authentication with a private upstream and serves one merged index") localDiscovery
        it (label "merged-private" "resolves private crates as plain dependencies beside admitted crates.io crates and records them under the crates.io source") mergedPrivate
        it (label "raw-auth" "retries a native 401 with the raw Cargo credential provider token") rawAuthentication
        it (label "private-auth-required" "uses raw credentials after an auth-required discovery response") privateAuthenticationRequired
        it (label "public-token-control" "requires the native edge token while keeping every public exchange anonymous") publicTokenControl
        it (label "placeholder-token" "requires a credential provider token with a private upstream and forwards a placeholder token to an anonymous registry") placeholderToken
        it (label "publication-framing" "refuses malformed frames and identity conflicts before any native destination write") publicationFraming
        it (label "dependency-sources" "publishes and builds plain and registry-named Écluse dependencies with current and minimum native clients") dependencySources
        it (label "publisher-configuration" "refuses an unnamed publish registry and a crates.io publish outside the package publish list") publisherConfiguration
  where
    label name = caseLabel ("cargo/native/" <> name)

clientCase :: CargoToolchain -> Bool -> GlobalDataPlane -> IO ()
clientCase toolchain floorExpected plane = do
    withObservedNativeProxy defaultE2EConfig plane $ \observed -> do
        cargoConsumer (observedProxy observed) toolchain $ \project -> do
            (client, compiler) <- cargoVersions project
            void (shouldSucceed client)
            void (shouldSucceed compiler)
            when floorExpected (crStdout client `shouldSatisfy` T.isPrefixOf "cargo 1.74.")
            invocation <- observeCargo (nativeTraffic observed) project ["build"]
            void (shouldSucceed (cargoInvocationClient invocation))
            assertCargoRouting (cargoInvocationTraffic invocation)
            assertCargoAssets project (cargoInvocationTraffic invocation)
            void (cargoInstall project cargoRoot cargoVersion [] >>= shouldSucceed)
            executable <- cargoInstalledExecutable project cargoExecutable >>= shouldSucceed
            T.strip (crStdout executable) `shouldBe` cargoExpectedOutput False False
            graph <- readGraph project []
            root <- nativePackage cargoRoot graph
            cargoPackageRustVersion root `shouldBe` Just "1.74"
        untrustedCargo toolchain observed
    nativePublication toolchain plane

untrustedCargo :: CargoToolchain -> ObservedNativeProxy -> IO ()
untrustedCargo toolchain observed = cargoConsumer (observedProxy observed) toolchain $ \project -> do
    systemCa <- maybe (fail "the controlled Cargo environment lacks the system CA bundle") pure (lookup "NIX_SSL_CERT_FILE" (cargoEnvironment project))
    let untrusted = project{cargoEnvironment = ("CARGO_HTTP_CAINFO", systemCa) : filter ((/= "CARGO_HTTP_CAINFO") . fst) (cargoEnvironment project)}
    before <- nativeTraffic observed
    result <- runCargo untrusted ["build"] >>= shouldFail
    (crStdout result <> crStderr result) `shouldSatisfy` T.isInfixOf "certificate"
    nativeTraffic observed `shouldReturn` before

nativePublication :: CargoToolchain -> GlobalDataPlane -> IO ()
nativePublication toolchain plane = withCargoRegistry $ \registry ->
    withStubFault plane (CargoBackend (registryPort registry) False) $
        withObservedNativeProxy cargoPublicationConfig plane $ \observed -> do
            void (cargoPublishFixture (observedProxy observed) toolchain cargoPublishName >>= shouldSucceed)
            received <- readTVarIO (observedUploads observed)
            relayed <- readTVarIO (registryUploads registry)
            length received `shouldBe` 1
            relayed `shouldBe` received
            bytes <- maybe (fail "the native client produced no upload frame") pure (listToMaybe received)
            frame <- either (fail . toString) pure (referenceUpload bytes)
            KeyMap.lookup "name" (uploadMetadata frame) `shouldBe` Just (String cargoPublishName)
            KeyMap.lookup "vers" (uploadMetadata frame) `shouldBe` Just (String cargoVersion)
            KeyMap.lookup "deps" (uploadMetadata frame) `shouldBe` Just (Array mempty)
            KeyMap.lookup "authors" (uploadMetadata frame) `shouldBe` Just (Array (fromList [String "Ecluse contributors"]))
            files <- cargoRegistryFiles registry
            stored <- maybe (fail "the destination omitted the native publication") pure (Map.lookup ("cargo-private", cargoPublishName, cargoVersion) files)
            storedArchive stored `shouldBe` uploadArchive frame
            traffic <- nativeTraffic observed
            let uploads = filter ((== "PUT") . nativeRequestMethod) (nativeClientRequests traffic)
            map nativeRequestPath uploads `shouldBe` ["/cargo/api/v1/crates/new"]
            map nativeRequestScheme uploads `shouldBe` ["https"]

readGraph :: CargoProject -> [String] -> IO CargoGraph
readGraph project arguments = cargoMetadata project arguments >>= shouldSucceed >>= either (fail . toString) pure . cargoGraph

nativePackage :: Text -> CargoGraph -> IO CargoPackage
nativePackage name graph = case [package | package <- Map.elems (cargoGraphPackages graph), cargoPackageName package == name] of
    [package] -> pure package
    _ -> fail ("Cargo metadata lacks one package " <> toString name)

activeDependencies :: Text -> CargoGraph -> [Text]
activeDependencies name graph = [alias | (identifier, package) <- Map.toList (cargoGraphPackages graph), cargoPackageName package == name, Just node <- [Map.lookup identifier (cargoGraphNodes graph)], (alias, _, _) <- cargoNodeDependencies node]

assertDiscovery :: E2E -> Maybe Text -> Bool -> ServingResponse -> IO ()
assertDiscovery proxy expectedApi expectedAuth response = do
    responseStatus response `shouldBe` 200
    (download, api, required, _) <- either (fail . toString) pure (referenceDiscovery (LBS.toStrict (responseBody response)))
    download `shouldSatisfy` T.isPrefixOf "https://"
    for_ api (\location -> location `shouldSatisfy` T.isPrefixOf "https://")
    download `shouldBe` e2eBaseUrl proxy <> "/cargo/crates/{crate}/{version}/{crate}-{version}.crate"
    api `shouldBe` expectedApi
    required `shouldBe` expectedAuth
    responseBody response `shouldSatisfy` (not . BS.isInfixOf "cargo-upstream" . LBS.toStrict)

privateConfig :: E2EConfig
privateConfig = defaultE2EConfig{ecExtraEnv = [("ECLUSE_MOUNTS__CARGO__PRIVATE_UPSTREAM__REGISTRY__URL", stubUrl CargoPrivate)]}

-- The private crate is first-party, so its reads never reach the public fixture.
firstPartyPrivate :: E2EConfig -> E2EConfig
firstPartyPrivate config = config{ecExtraEnv = ecExtraEnv config <> [("ECLUSE_MOUNTS__CARGO__FIRST_PARTY", "ecluse-e2e-private")]}

withPrivateRegistry :: GlobalDataPlane -> E2EConfig -> CargoRegistryFault -> (CargoRegistry -> ObservedNativeProxy -> IO a) -> IO a
withPrivateRegistry plane config fault action = withCargoRegistry $ \registry -> do
    leaf <- cargoLeafRelease "ecluse-e2e-private" cargoVersion
    seedCargoRegistry registry "cargo-private" [leaf]
    setCargoRegistryFault registry fault
    withStubFault plane (CargoBackend (registryPort registry) False) $
        withObservedNativeProxy config plane (action registry)

-- Build the private crate and return the client's discovery statuses.
buildPrivate :: ObservedNativeProxy -> IO [Int]
buildPrivate observed =
    cargoPrivateConsumer (observedProxy observed) currentCargo "ecluse-e2e-private" cargoVersion $ \project -> do
        invocation <- observeCargo (nativeTraffic observed) project ["build"]
        void (shouldSucceed (cargoInvocationClient invocation))
        assertCargoOriginRouting "cargo-private" (cargoInvocationTraffic invocation)
        assertCargoAssets project (cargoInvocationTraffic invocation)
        pure [nativeRequestStatus request | request <- nativeClientRequests (cargoInvocationTraffic invocation), nativeRequestPath request == "/cargo/config.json"]

rawAuthentication :: GlobalDataPlane -> IO ()
rawAuthentication plane =
    withPrivateRegistry plane config (CargoRegistryRequireToken True) $ \registry observed -> do
        statuses <- buildPrivate observed
        take 1 statuses `shouldBe` [401]
        statuses `shouldSatisfy` elem 200
        tokens <- readTVarIO (registryTokens registry)
        tokens `shouldSatisfy` (not . null)
        map snd tokens `shouldSatisfy` all (== Just (encodeUtf8 cargoRegistryToken))
        logs <- proxyContainerLogs (observedProxy observed)
        logs `shouldSatisfy` not . T.isInfixOf cargoRegistryToken
        public <- filter ((== CargoPublic) . srRoute) <$> stubReadsNow plane
        public `shouldBe` []
  where
    config = firstPartyPrivate privateConfig{ecExtraEnv = ecExtraEnv privateConfig <> [("ECLUSE_SERVER__AUTH_TOKEN", cargoRegistryToken)]}

privateAuthenticationRequired :: GlobalDataPlane -> IO ()
privateAuthenticationRequired plane =
    withPrivateRegistry plane (firstPartyPrivate privateConfig) (CargoRegistryRequireToken False) $ \registry observed -> do
        statuses <- buildPrivate observed
        statuses `shouldSatisfy` (not . null)
        statuses `shouldSatisfy` all (== 200)
        tokens <- readTVarIO (registryTokens registry)
        map snd tokens `shouldSatisfy` elem (Just (encodeUtf8 cargoRegistryToken))
        public <- filter ((== CargoPublic) . srRoute) <$> stubReadsNow plane
        public `shouldBe` []

placeholderToken :: GlobalDataPlane -> IO ()
placeholderToken plane = withPrivateRegistry plane privateConfig CargoRegistryHealthy $ \registry observed -> do
    let proxy = observedProxy observed
        index = "sparse+" <> e2eBaseUrl proxy <> "/cargo/"
    withCargoProject currentCargo CargoConsumer proxy index Nothing $ \project -> do
        cargoSelectDependency "ecluse-e2e-private" cargoVersion project
        result <- runCargo project ["build"] >>= shouldFail
        (crStdout result <> crStderr result) `shouldSatisfy` T.isInfixOf "no token found"
    readTVarIO (registryRequests registry) `shouldReturn` []
    withCargoProject currentCargo CargoConsumer proxy index (Just "placeholder") $ \project -> do
        cargoSelectDependency "ecluse-e2e-private" cargoVersion project
        void (runCargo project ["build"] >>= shouldSucceed)
    tokens <- readTVarIO (registryTokens registry)
    tokens `shouldSatisfy` (not . null)
    map snd tokens `shouldSatisfy` all (== Just "placeholder")

mergedPrivate :: GlobalDataPlane -> IO ()
mergedPrivate plane = withPrivateRegistry plane privateConfig CargoRegistryHealthy $ \_ observed -> do
    leaf <- cargoLeafRelease "ecluse-e2e-private" cargoVersion
    cargoPrivateConsumer (observedProxy observed) currentCargo "ecluse-e2e-private" cargoVersion $ \project -> do
        invocation <- observeCargo (nativeTraffic observed) project ["build"]
        void (shouldSucceed (cargoInvocationClient invocation))
        assertCargoOriginRouting "cargo-private" (cargoInvocationTraffic invocation)
        locked <- cargoLock project >>= either (fail . toString) pure
        [(cargoLockedSource entry, cargoLockedChecksum entry) | entry <- locked, cargoLockedName entry == "ecluse-e2e-private"]
            `shouldBe` [(Just "registry+https://github.com/rust-lang/crates.io-index", Just (releaseChecksum leaf))]
    cargoConsumer (observedProxy observed) currentCargo $ \project -> do
        invocation <- observeCargo (nativeTraffic observed) project ["build"]
        void (shouldSucceed (cargoInvocationClient invocation))
        assertCargoRouting (cargoInvocationTraffic invocation)
    let config = privateConfig{ecExtraEnv = ecExtraEnv privateConfig <> [("ECLUSE_MOUNTS__CARGO__FIRST_PARTY", "ecluse-e2e-build")]}
    withObservedNativeProxy config plane $ \firstPartyProxy -> do
        before <- stubReadsNow plane
        proxyStatus (observedProxy firstPartyProxy) ("/cargo/" <> cargoShard "ecluse-e2e-build") `shouldReturn` 404
        stubReadsNow plane `shouldReturn` before

localDiscovery :: GlobalDataPlane -> IO ()
localDiscovery plane = withPrivateRegistry plane privateConfig CargoRegistryHealthy $ \registry observed -> do
    let proxy = observedProxy observed
    releases <- cargoFixtureReleases
    root <- cargoRelease releases cargoRoot cargoVersion
    seedCargoRegistry registry "cargo-private" [root]
    before <- readTVarIO (registryRequests registry)
    discovery <- proxyRequest "GET" [] "" proxy "/cargo/config.json"
    assertDiscovery proxy Nothing True discovery
    readTVarIO (registryRequests registry) `shouldReturn` before
    removed <- proxyRequest "GET" [] "" proxy "/cargo/private/config.json"
    responseStatus removed `shouldBe` 404
    index <- proxyRequest "GET" [] "" proxy ("/cargo/" <> cargoShard cargoRoot)
    responseStatus index `shouldBe` 200
    tag <- maybe (fail "the merged Cargo index emitted no ETag") pure (lookup "ETag" (responseHeaders index))
    matched <- proxyRequest "GET" [("If-None-Match", tag)] "" proxy ("/cargo/" <> cargoShard cargoRoot)
    responseStatus matched `shouldBe` 304

publicationFraming :: GlobalDataPlane -> IO ()
publicationFraming plane = withCargoRegistry $ \registry -> withStubFault plane (CargoBackend (registryPort registry) False) $
    withObservedNativeProxy cargoPublicationConfig plane $ \observed -> do
        let proxy = observedProxy observed
        discovery <- proxyRequest "GET" [("Authorization", encodeUtf8 cargoRegistryToken)] "" proxy "/cargo/config.json"
        assertDiscovery proxy (Just (e2eBaseUrl proxy <> "/cargo")) True discovery
        before <- readTVarIO (registryRequests registry)
        releases <- cargoFixtureReleases
        release <- cargoRelease releases cargoPublishName cargoVersion
        valid <- either (fail . toString) pure (cargoUpload release)
        conflict <- either (fail . toString) pure (cargoUpload release{releaseCrate = (releaseCrate release){crateName = cargoRetainedName}})
        metadata <- case cargoUploadMetadata release of
            Object fields -> pure fields
            _ -> fail "the upload fixture metadata is not an object"
        malformed <- traverse (\fields -> either (fail . toString) pure (cargoUploadWithMetadata (Object fields) (releaseArchive release))) [KeyMap.insert "deps" (Array (fromList [Null])) metadata, KeyMap.insert "authors" (Array (fromList [Number 42])) metadata]
        for_ ([conflict, BS.take 3 valid, BS.take (BS.length valid - 1) valid, valid <> "trailing", BS.replicate 4 255 <> BS.drop 4 valid] <> malformed) $ \body -> do
            response <- proxyRequest "PUT" [("Authorization", encodeUtf8 cargoRegistryToken), ("Content-Type", "application/octet-stream")] body (observedProxy observed) "/cargo/api/v1/crates/new"
            responseStatus response `shouldBe` 400
        cargoRegistryFiles registry `shouldReturn` Map.empty
        readTVarIO (registryUploads registry) `shouldReturn` []
        readTVarIO (registryRequests registry) `shouldReturn` before

dependencySources :: GlobalDataPlane -> IO ()
dependencySources plane = for_ [currentCargo, floorCargo] $ \toolchain ->
    for_ [False, True] $ \named -> withCargoRegistry $ \registry -> do
        leaf <- cargoLeafRelease "ecluse-e2e-leaf" cargoVersion
        seedCargoRegistry registry "cargo-private" [leaf]
        withStubFault plane (CargoBackend (registryPort registry) False) $
            withObservedNativeProxy config plane $ \observed -> do
                let proxy = observedProxy observed
                    spec = CargoCrate name cargoVersion "../foreign" [] (Object mempty) (Object mempty) ["Cargo.toml", "README.md", "src/lib.rs", "build.rs"]
                    -- A plain declaration uploads the crates.io index, and one naming Écluse uploads none.
                    expectedRegistry = if named then Nothing else Just (String "https://github.com/rust-lang/crates.io-index")
                withCargoProject toolchain CargoPublisher proxy ("sparse+" <> e2eBaseUrl proxy <> "/cargo/") (Just cargoRegistryToken) $ \project -> do
                    cargoUseFixture project "test/fixtures/cargo/graph" spec
                    let path = cargoDirectory project </> "Cargo.toml"
                    manifest <- decodeUtf8 <$> readFileBS path
                    writeFileText path (T.replace ", registry = \"foreign\"" (if named then ", registry = \"ecluse-test\"" else "") manifest)
                    cargoRestrictPublication project
                    void (cargoPublish project >>= shouldSucceed)
                received <- readTVarIO (observedUploads observed)
                relayed <- readTVarIO (registryUploads registry)
                length received `shouldBe` 1
                relayed `shouldBe` received
                bytes <- maybe (fail "the native client produced no dependency upload") pure (listToMaybe received)
                frame <- either (fail . toString) pure (referenceUpload bytes)
                let uploaded field = case KeyMap.lookup "deps" (uploadMetadata frame) of
                        Just (Array dependencies) -> [KeyMap.lookup field dependency | Object dependency <- toList dependencies]
                        _ -> []
                uploaded "registry" `shouldBe` [expectedRegistry]
                uploaded "explicit_name_in_toml" `shouldBe` [Just (String "renamed_leaf")]
                uploaded "target" `shouldBe` [Just (String "cfg(any(unix, windows))")]
                KeyMap.lookup "links" (uploadMetadata frame) `shouldBe` Just (String "native_foreign")
                index <- proxyRequest "GET" [("Authorization", encodeUtf8 cargoRegistryToken), ("Accept-Encoding", "identity")] "" proxy ("/cargo/" <> cargoShard name)
                responseStatus index `shouldBe` 200
                rows <- traverse (either (fail . toString) pure . indexValue) (referenceIndex (LBS.toStrict (responseBody index)))
                [KeyMap.lookup "registry" dependency | row <- rows, Just (Array dependencies) <- [KeyMap.lookup "deps" row], Object dependency <- toList dependencies] `shouldBe` [expectedRegistry]
                cargoPrivateConsumer proxy toolchain name cargoVersion $ \project -> do
                    invocation <- observeCargo (nativeTraffic observed) project ["build"]
                    void (shouldSucceed (cargoInvocationClient invocation))
                    assertCargoOriginRouting "cargo-private" (cargoInvocationTraffic invocation)
                    assertCargoAssets project (cargoInvocationTraffic invocation)
                    graph <- readGraph project []
                    package <- nativePackage name graph
                    map cargoDependencyRegistry (cargoPackageDependencies package) `shouldBe` [Nothing]
                    map cargoDependencyRename (cargoPackageDependencies package) `shouldBe` [Just "renamed_leaf"]
                    void (runCargo project ["build", "--features", toString (name <> "/forwarded")] >>= shouldSucceed)
                files <- cargoRegistryFiles registry
                stored <- maybe (fail "the target omitted the native dependency publication") pure (Map.lookup ("cargo-private", name, cargoVersion) files)
                storedArchive stored `shouldBe` uploadArchive frame
  where
    name = "ecluse-e2e-private-foreign"
    config = cargoPublicationConfig{ecExtraEnv = [(key, if key == "ECLUSE_MOUNTS__CARGO__FIRST_PARTY" then name else value) | (key, value) <- ecExtraEnv cargoPublicationConfig]}

publisherConfiguration :: GlobalDataPlane -> IO ()
publisherConfiguration plane = for_ [currentCargo, floorCargo] $ \toolchain -> withCargoRegistry $ \registry ->
    withStubFault plane (CargoBackend (registryPort registry) False) $
        withObservedNativeProxy cargoPublicationConfig plane $ \observed -> do
            let proxy = observedProxy observed
                publish project options = runCargo project (["publish", "--manifest-path", cargoDirectory project </> "Cargo.toml", "--allow-dirty"] <> options)
            spec <- maybe (fail "owned Cargo publication fixture is absent") pure (find (\crate -> crateName crate == cargoPublishName && crateVersion crate == cargoVersion) cargoCrates)
            withCargoProject toolchain CargoConsumer proxy ("sparse+" <> e2eBaseUrl proxy <> "/cargo/") (Just cargoRegistryToken) $ \project -> do
                cargoUseFixture project "test/fixtures/cargo/graph" spec
                unnamed <- publish project [] >>= shouldFail
                (crStdout unnamed <> crStderr unnamed) `shouldSatisfy` T.isInfixOf "is replaced with remote registry ecluse-test"
                cargoRestrictPublication project
                leaked <- publish project ["--registry", "crates-io"] >>= shouldFail
                (crStdout leaked <> crStderr leaked) `shouldSatisfy` T.isInfixOf "is not listed in the `package.publish` value"
            readTVarIO (observedUploads observed) `shouldReturn` []
            readTVarIO (registryUploads registry) `shouldReturn` []
            traffic <- nativeTraffic observed
            filter ((== "PUT") . nativeRequestMethod) (nativeClientRequests traffic) `shouldBe` []

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
        withObservedNativeProxy config plane $ \observed -> do
            let index = "sparse+" <> e2eBaseUrl (observedProxy observed) <> "/cargo/"
            withCargoProject currentCargo CargoConsumer (observedProxy observed) index (Just cargoRegistryToken) $ \project -> do
                invocation <- observeCargo (nativeTraffic observed) project ["build"]
                void (shouldSucceed (cargoInvocationClient invocation))
                assertCargoRouting (cargoInvocationTraffic invocation)
                assertCargoAssets project (cargoInvocationTraffic invocation)
            tokens <- readTVarIO (registryTokens registry)
            tokens `shouldSatisfy` (not . null)
            map fst tokens `shouldSatisfy` all (== "cargo-upstream")
            map snd tokens `shouldSatisfy` all isNothing
            before <- readTVarIO (registryRequests registry)
            withCargoProject currentCargo CargoConsumer (observedProxy observed) index (Just "wrong-native-edge-token") $ \project -> void (runCargo project ["build"] >>= shouldFail)
            readTVarIO (registryRequests registry) `shouldReturn` before
  where
    config = defaultE2EConfig{ecExtraEnv = [("ECLUSE_SERVER__AUTH_TOKEN", cargoRegistryToken)]}
