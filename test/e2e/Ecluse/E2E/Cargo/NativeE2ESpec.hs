-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native client compatibility, source identities, authentication and download routing remain observable.
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
import Test.Hspec (around, it, shouldBe, shouldMatchList, shouldNotBe, shouldReturn, shouldSatisfy)

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
                assertDiscovery (observedProxy observed) "/cargo" Nothing discovery
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
        it (label "discovery-views" "keeps local discovery, served validators and private cache identity distinct") discoveryViews
        it (label "private-no-public" "reads arbitrary private names and refuses public fallback and first-party public reads") privateNoPublic
        it (label "raw-auth" "retries a native 401 with the raw Cargo credential provider token") (authenticationCase (CargoRegistryRequireToken True))
        it (label "private-auth-required" "uses raw credentials after an auth-required discovery response") (authenticationCase (CargoRegistryRequireToken False))
        it (label "public-token-control" "requires the native edge token while keeping every public exchange anonymous") publicTokenControl
        it (label "private-anonymous" "preserves anonymous native reads when trusted discovery allows them") anonymousPrivate
        it (label "publication-framing" "refuses malformed frames and identity conflicts before any native destination write") publicationFraming
        it (label "foreign-registry" "publishes and builds current and foreign registry dependencies with current and minimum native clients") foreignRegistry
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
            map nativeRequestPath uploads `shouldBe` ["/cargo/private/api/v1/crates/new"]
            map nativeRequestScheme uploads `shouldBe` ["https"]

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
    download `shouldSatisfy` T.isPrefixOf "https://"
    for_ api (\location -> location `shouldSatisfy` T.isPrefixOf "https://")
    download `shouldBe` e2eBaseUrl proxy <> view <> "/crates/{crate}/{version}/{crate}-{version}.crate"
    api `shouldBe` expectedApi
    responseBody response `shouldSatisfy` (not . BS.isInfixOf "cargo-upstream" . LBS.toStrict)

privateConfig :: E2EConfig
privateConfig = defaultE2EConfig{ecExtraEnv = [("ECLUSE_MOUNTS__CARGO__PRIVATE_UPSTREAM__REGISTRY__URL", stubUrl CargoPrivate)]}

withPrivateRegistry :: GlobalDataPlane -> CargoRegistryFault -> (CargoRegistry -> ObservedNativeProxy -> IO a) -> IO a
withPrivateRegistry plane fault action = withCargoRegistry $ \registry -> do
    leaf <- cargoLeafRelease "ecluse-e2e-private" cargoVersion
    seedCargoRegistry registry "cargo-private" [leaf]
    setCargoRegistryFault registry fault
    withStubFault plane (CargoBackend (registryPort registry) False) $
        withObservedNativeProxy privateConfig plane (action registry)

authenticationCase :: CargoRegistryFault -> GlobalDataPlane -> IO ()
authenticationCase fault plane = withPrivateRegistry plane fault $ \registry observed -> do
    cargoPrivateConsumer (observedProxy observed) currentCargo "ecluse-e2e-private" cargoVersion $ \project -> do
        invocation <- observeCargo (nativeTraffic observed) project ["build"]
        void (shouldSucceed (cargoInvocationClient invocation))
        assertCargoViewRouting "/cargo/private" "cargo-private" (cargoInvocationTraffic invocation)
        assertCargoAssets project (cargoInvocationTraffic invocation)
    tokens <- readTVarIO (registryTokens registry)
    tokens `shouldSatisfy` any ((== Just (encodeUtf8 cargoRegistryToken)) . snd)
    when (fault == CargoRegistryRequireToken True) (tokens `shouldSatisfy` any (isNothing . snd))
    logs <- proxyContainerLogs (observedProxy observed)
    logs `shouldSatisfy` not . T.isInfixOf cargoRegistryToken
    public <- filter ((== CargoPublic) . srRoute) <$> stubReadsNow plane
    public `shouldBe` []

anonymousPrivate :: GlobalDataPlane -> IO ()
anonymousPrivate plane = withPrivateRegistry plane CargoRegistryHealthy $ \registry observed ->
    withCargoProject currentCargo CargoAlternate (observedProxy observed) ("sparse+" <> e2eBaseUrl (observedProxy observed) <> "/cargo/private/") Nothing $ \project -> do
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
    (filter ((== CargoPublic) . srRoute) <$> stubReadsNow plane) `shouldReturn` []
    let config = privateConfig{ecExtraEnv = ecExtraEnv privateConfig <> [("ECLUSE_MOUNTS__CARGO__FIRST_PARTY", "ecluse-e2e-build")]}
    withObservedNativeProxy config plane $ \firstPartyProxy -> do
        before <- stubReadsNow plane
        proxyStatus (observedProxy firstPartyProxy) ("/cargo/" <> cargoShard "ecluse-e2e-build") `shouldReturn` 404
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
    withObservedNativeProxy cargoPublicationConfig plane $ \observed -> do
        let proxy = observedProxy observed
        discovery <- proxyRequest "GET" [("Authorization", encodeUtf8 cargoRegistryToken)] "" proxy "/cargo/private/config.json"
        assertDiscovery proxy "/cargo/private" (Just (e2eBaseUrl proxy <> "/cargo/private")) discovery
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
            response <- proxyRequest "PUT" [("Authorization", encodeUtf8 cargoRegistryToken), ("Content-Type", "application/octet-stream")] body (observedProxy observed) "/cargo/private/api/v1/crates/new"
            responseStatus response `shouldBe` 400
        cargoRegistryFiles registry `shouldReturn` Map.empty
        readTVarIO (registryUploads registry) `shouldReturn` []
        readTVarIO (registryRequests registry) `shouldReturn` before

foreignRegistry :: GlobalDataPlane -> IO ()
foreignRegistry plane = for_ [currentCargo, floorCargo] $ \toolchain ->
    for_ [False, True] $ \sameRegistry -> withCargoRegistry $ \registry -> do
        when sameRegistry $ do
            leaf <- cargoLeafRelease "ecluse-e2e-leaf" cargoVersion
            seedCargoRegistry registry "cargo-private" [leaf]
        withStubFault plane (CargoBackend (registryPort registry) False) $
            withObservedNativeProxy config plane $ \observed -> do
                let proxy = observedProxy observed
                    source = "sparse+" <> e2eBaseUrl proxy <> (if sameRegistry then "/cargo/private/" else "/cargo/")
                    spec = CargoCrate name cargoVersion "../foreign" [] (Object mempty) (Object mempty) ["Cargo.toml", "README.md", "src/lib.rs", "build.rs"]
                withCargoProject toolchain CargoAlternate proxy ("sparse+" <> e2eBaseUrl proxy <> "/cargo/private/") (Just cargoRegistryToken) $ \project -> do
                    cargoUseFixture project "test/fixtures/cargo/graph" spec
                    if sameRegistry
                        then do
                            let path = cargoDirectory project </> "Cargo.toml"
                            manifest <- decodeUtf8 <$> readFileBS path
                            writeFileText path (T.replace "registry = \"foreign\"" "registry = \"ecluse-test\"" manifest)
                        else addForeign project source
                    void (cargoPublish project >>= shouldSucceed)
                received <- readTVarIO (observedUploads observed)
                relayed <- readTVarIO (registryUploads registry)
                length received `shouldBe` 1
                relayed `shouldBe` received
                bytes <- maybe (fail "the native client produced no dependency upload") pure (listToMaybe received)
                frame <- either (fail . toString) pure (referenceUpload bytes)
                let uploadRegistries = case KeyMap.lookup "deps" (uploadMetadata frame) of
                        Just (Array dependencies) -> [KeyMap.lookup "registry" dependency | Object dependency <- toList dependencies]
                        _ -> []
                uploadRegistries `shouldBe` [if sameRegistry then Nothing else Just (String source)]
                let aliases = case KeyMap.lookup "deps" (uploadMetadata frame) of
                        Just (Array dependencies) -> [KeyMap.lookup "explicit_name_in_toml" dependency | Object dependency <- toList dependencies]
                        _ -> []
                aliases `shouldBe` [Just (String "renamed_leaf")]
                KeyMap.lookup "links" (uploadMetadata frame) `shouldBe` Just (String "native_foreign")
                let targets = case KeyMap.lookup "deps" (uploadMetadata frame) of
                        Just (Array dependencies) -> [KeyMap.lookup "target" dependency | Object dependency <- toList dependencies]
                        _ -> []
                targets `shouldBe` [Just (String "cfg(any(unix, windows))")]
                index <- proxyRequest "GET" [("Authorization", encodeUtf8 cargoRegistryToken), ("Accept-Encoding", "identity")] "" proxy ("/cargo/private/" <> cargoShard name)
                responseStatus index `shouldBe` 200
                rows <- traverse (either (fail . toString) pure . indexValue) (referenceIndex (LBS.toStrict (responseBody index)))
                let registries = [KeyMap.lookup "registry" dependency | row <- rows, Just (Array dependencies) <- [KeyMap.lookup "deps" row], Object dependency <- toList dependencies]
                registries `shouldBe` [if sameRegistry then Nothing else Just (String source)]
                cargoPrivateConsumer proxy toolchain name cargoVersion $ \project -> do
                    unless sameRegistry (addForeign project source)
                    invocation <- observeCargo (nativeTraffic observed) project ["build"]
                    void (shouldSucceed (cargoInvocationClient invocation))
                    assertCargoViewRouting "/cargo/private" "cargo-private" (cargoInvocationTraffic invocation)
                    unless sameRegistry (assertCargoViewRouting "/cargo" "cargo-upstream" (cargoInvocationTraffic invocation))
                    assertCargoAssets project (cargoInvocationTraffic invocation)
                    graph <- readGraph project []
                    package <- nativePackage name graph
                    map cargoDependencyRegistry (cargoPackageDependencies package) `shouldBe` [Just source]
                    map cargoDependencyRename (cargoPackageDependencies package) `shouldBe` [Just "renamed_leaf"]
                    void (runCargo project ["build", "--features", toString (name <> "/forwarded")] >>= shouldSucceed)
                files <- cargoRegistryFiles registry
                stored <- maybe (fail "the target omitted the native dependency publication") pure (Map.lookup ("cargo-private", name, cargoVersion) files)
                storedArchive stored `shouldBe` uploadArchive frame
  where
    name = "ecluse-e2e-private-foreign"
    config = cargoPublicationConfig{ecExtraEnv = [(key, if key == "ECLUSE_MOUNTS__CARGO__FIRST_PARTY" then name else value) | (key, value) <- ecExtraEnv cargoPublicationConfig]}
    addForeign project source = do
        let path = cargoDirectory project </> "home/config.toml"
        existing <- decodeUtf8 <$> readFileBS path
        writeFileText path (existing <> "\n[registries.foreign]\nindex = " <> quoted source <> "\n")
        let credentials = cargoDirectory project </> "home/credentials.toml"
        existingCredentials <- decodeUtf8 <$> readFileBS credentials
        writeFileText credentials (existingCredentials <> "\n[registries.foreign]\ntoken = " <> quoted cargoRegistryToken <> "\n")
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
        withObservedNativeProxy config plane $ \observed -> do
            let index = "sparse+" <> e2eBaseUrl (observedProxy observed) <> "/cargo/"
            withCargoProject currentCargo CargoReplacement (observedProxy observed) index (Just cargoRegistryToken) $ \project -> do
                invocation <- observeCargo (nativeTraffic observed) project ["build"]
                void (shouldSucceed (cargoInvocationClient invocation))
                assertCargoRouting (cargoInvocationTraffic invocation)
                assertCargoAssets project (cargoInvocationTraffic invocation)
            tokens <- readTVarIO (registryTokens registry)
            tokens `shouldSatisfy` (not . null)
            map fst tokens `shouldSatisfy` all (== "cargo-upstream")
            map snd tokens `shouldSatisfy` all isNothing
            before <- readTVarIO (registryRequests registry)
            withCargoProject currentCargo CargoReplacement (observedProxy observed) index (Just "wrong-native-edge-token") $ \project -> void (runCargo project ["build"] >>= shouldFail)
            readTVarIO (registryRequests registry) `shouldReturn` before
  where
    config = defaultE2EConfig{ecExtraEnv = [("ECLUSE_SERVER__AUTH_TOKEN", cargoRegistryToken)]}
