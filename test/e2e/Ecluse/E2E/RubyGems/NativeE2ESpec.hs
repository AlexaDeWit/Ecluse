-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Supported Bundler clients expose real resolution, installation, update and refusal observations.
module Ecluse.E2E.RubyGems.NativeE2ESpec (rubyNativeContract, assertRubyPlatformInstall) where

import Data.Aeson (withObject, (.:))
import Data.Aeson.Types (parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import Test.Hspec (around, it, shouldBe, shouldReturn, shouldSatisfy)

import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (..))
import Ecluse.E2E.Fixtures.RubyGems
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.RubyGems
import Ecluse.E2E.Harness.NativeEdge
import Ecluse.E2E.Harness.RubyGemsRegistry
import Ecluse.E2E.RubyGems.InstallE2ESpec
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Phase (timedFixture)
import Ecluse.Test.Contract.Runner (DeclaredGroup (..))
import Ecluse.Test.Contract.Serving (ServingResponse (..))
import Ecluse.Test.Registry.RubyGems.Fixtures
import Ecluse.Test.Registry.RubyGems.Reference (RubyInfoRow (..), referenceInfo, runRubyReference)

-- | Native client observations include isolated installs and retained-cache revalidation.
rubyNativeContract :: DeclaredGroup
rubyNativeContract = DeclaredGroup (fixtureMetadata rubyNativeFixture) $ \recorder ->
    around (timedFixture recorder (withFixture rubyNativeFixture)) $ do
        it (label "minimum-client" "locks, installs, freezes and updates with Bundler 2.6.9") (clientCase minimumBundler)
        it (label "current-client" "locks, installs, freezes and updates with Bundler 4.0.23") (clientCase currentBundler)
        it (label "compact-required" "refuses checksum-free client fallback while named metadata and archives remain available") $ \plane ->
            for_ [minimumBundler, currentBundler] $ \client ->
                for_ [400, 404, 503] $ \status -> compactRequiredCase client False status plane
        it (label "info-required" "refuses checksum-free fallback after an info failure while archives remain available") $ \plane ->
            for_ [minimumBundler, currentBundler] $ \client ->
                for_ [400, 401, 503] $ \status -> compactRequiredCase client True status plane
        it (label "minimum-cache" "revalidates private visibility and checksums with retained Bundler 2.6.9 caches") (retainedCacheCase minimumBundler)
        it (label "current-cache" "revalidates private visibility and checksums with retained Bundler 4.0.23 caches") (retainedCacheCase currentBundler)
        it (label "compact-replacement" "returns complete personalised discovery and info bodies for Range requests") $ \plane ->
            withObservedNativeProxy defaultE2EConfig plane $ \observed -> do
                let proxy = observedProxy observed
                for_ ["/rubygems/versions", "/rubygems/info/ecluse-fixture-root"] $ \path -> do
                    ordinary <- proxyRequest "GET" [] "" proxy path
                    responseStatus ordinary `shouldBe` 200
                    replacement <- proxyRequest "GET" [("Range", "bytes=7-"), ("If-Range", "\"old-client-view\"")] "" proxy path
                    responseStatus replacement `shouldBe` 200
                    responseBody replacement `shouldBe` responseBody ordinary
                (_, directory) <- proxyGet proxy "/rubygems/versions"
                let rows = filter (T.isPrefixOf "ecluse-fixture-root ") (T.lines (decodeUtf8 (LBS.toStrict directory)))
                rows `shouldSatisfy` (not . null)
                rows `shouldSatisfy` all (T.isSuffixOf " ")
                (_, info) <- proxyGet proxy "/rubygems/info/ecluse-fixture-root"
                native <- either (fail . toString) pure (referenceInfo (LBS.toStrict info))
                native `shouldSatisfy` (not . null)
                releases <- filter ((== rubyRoot) . releaseName) <$> loadRubyReleases
                for_ native $ \row -> do
                    release <- rubyRelease releases rubyRoot (rowVersion row) (rowPlatform row)
                    rowFields row `shouldSatisfy` elem ("checksum", releaseChecksum release)
        it (label "platforms" "retains the Ruby, Java, Linux musl and Darwin variants in a native lockfile") $ \plane ->
            withObservedNativeProxy defaultE2EConfig plane $ \observed -> rubyConsumer (observedProxy observed) currentBundler $ \project -> do
                releases <- filter ((== "ecluse-fixture-platform") . releaseName) <$> loadRubyReleases
                rubyDependency project "ecluse-fixture-platform" "= 1.0.0"
                (status, body) <- proxyGet (observedProxy observed) "/rubygems/info/ecluse-fixture-platform"
                status `shouldBe` 200
                nativeRows <- either (fail . toString) pure (referenceInfo (LBS.toStrict body))
                sort [(rowVersion row, rowPlatform row) | row <- nativeRows] `shouldBe` sort [(releaseVersion release, releasePlatform release) | release <- releases]
                assertRubyPlatformInstall observed releases project
        it (label "checksum-failure" "refuses a wrong lockfile digest against current native metadata") $ \plane ->
            withObservedNativeProxy defaultE2EConfig plane $ \observed -> do
                releases <- loadRubyReleases
                release <- rubyRelease releases rubyRoot rubyVersion "ruby"
                lock <- rubyConsumer (observedProxy observed) currentBundler $ \control -> do
                    void (rubyLock control >>= shouldSucceed)
                    void (rubyInstall control False >>= shouldSucceed)
                    rubyLockfile control >>= maybe (fail "checksum control did not create a lockfile") pure
                lock `shouldSatisfy` T.isInfixOf (releaseChecksum release)
                rubyConsumer (observedProxy observed) currentBundler $ \project -> do
                    writeFileText (rubyDirectory project </> "Gemfile.lock") (T.replace (releaseChecksum release) (T.replicate 64 "f") lock)
                    before <- nativeTraffic observed
                    result <- rubyInstall project True >>= shouldFail
                    T.toLower (crStdout result <> crStderr result) `shouldSatisfy` T.isInfixOf "checksum"
                    after <- nativeTraffic observed
                    interval <- nativeTrafficSince before after
                    nativeClientRequests interval `shouldSatisfy` any (\request -> nativeRequestPath request == "/rubygems/info/ecluse-fixture-root" && nativeRequestStatus request == 200)
                    tree <- rubyInstalledTree project
                    Map.keys tree `shouldSatisfy` (not . any (T.isSuffixOf "ecluse_fixture_root.rb" . toText))
        it (label "prerelease" "installs an explicitly requested native prerelease") $ \plane ->
            withObservedNativeProxy defaultE2EConfig plane $ \observed -> rubyConsumer (observedProxy observed) currentBundler $ \project -> do
                rubyDependency project rubyLeaf "= 2.1.0.pre.1"
                void (rubyInstall project False >>= shouldSucceed)
                result <- rubyEvaluate project "require 'ecluse_fixture_leaf'; puts EcluseFixtureLeaf::VALUE" >>= shouldSucceed
                T.strip (crStdout result) `shouldBe` "ecluse-fixture-leaf-2.1.0.pre.1-ruby"
        it (label "ruby-requirement" "refuses a gem requiring Ruby 99 while an ordinary sibling installs") $ \plane ->
            withObservedNativeProxy defaultE2EConfig plane $ \observed -> do
                rubyConsumer (observedProxy observed) currentBundler $ \control -> void (rubyInstall control False >>= shouldSucceed)
                rubyConsumer (observedProxy observed) currentBundler $ \project -> do
                    rubyDependency project "ecluse-fixture-future" "= 1.0.0"
                    result <- rubyInstall project False >>= shouldFail
                    (crStdout result <> crStderr result) `shouldSatisfy` T.isInfixOf "99.0"
                    tree <- rubyInstalledTree project
                    tree `shouldBe` mempty
        it (label "direct-denial" "refuses pinned and direct archives without a public artifact fetch") $ \plane -> for_ [minimumBundler, currentBundler] $ \client -> do
            releases <- loadRubyReleases
            release <- rubyRelease releases rubyRoot rubyVersion "ruby"
            (source, lock, cache) <- withObservedNativeProxy defaultE2EConfig plane $ \control -> do
                proxyStatus (observedProxy control) (rubyArchiveRoute release) `shouldReturn` 200
                rubyConsumer (observedProxy control) client $ \project -> do
                    void (rubyLock project >>= shouldSucceed)
                    resolved <- rubyLockfile project >>= maybe (fail "admitted control did not create a lockfile") pure
                    resolved `shouldSatisfy` T.isInfixOf "CHECKSUMS"
                    void (rubyInstall project False >>= shouldSucceed)
                    admitted <- rubyLockfile project >>= maybe (fail "admitted installation did not retain its lockfile") pure
                    void (rubyInstall project True >>= shouldSucceed)
                    rubyLockfile project `shouldReturn` Just admitted
                    retained <- snapshotTree (rubyDirectory project) ["home/.bundle/cache/compact_index"]
                    let cache = [(path, bytes) | (path, TreeFile _ bytes) <- Map.toList retained]
                    cache `shouldSatisfy` (not . null)
                    pure (rubySource project, admitted, cache)
            withObservedNativeProxy rubyDeniedConfig plane $ \observed -> do
                withRubyProject client (observedProxy observed) source Nothing $ \project -> do
                    for_ cache $ \(path, bytes) -> do
                        let destination = rubyDirectory project </> path
                        createDirectoryIfMissing True (takeDirectory destination)
                        BS.writeFile destination bytes
                    rubyInstalledTree project `shouldReturn` mempty
                    writeFileText (rubyDirectory project </> "Gemfile.lock") lock
                    void (runBundler project ["config", "set", "--local", toString ("mirror." <> source), toString (e2eBaseUrl (observedProxy observed) <> "/rubygems/")] >>= shouldSucceed)
                    before <- nativeTraffic observed
                    void (rubyInstall project True >>= shouldFail)
                    rubyLockfile project `shouldReturn` Just lock
                    tree <- rubyInstalledTree project
                    Map.keys tree `shouldSatisfy` (not . any (T.isSuffixOf ".rb" . toText))
                    Map.keys tree `shouldSatisfy` (not . any (T.isSuffixOf ".gem" . toText))
                    after <- nativeTraffic observed
                    interval <- nativeTrafficSince before after
                    nativeClientRequests interval `shouldSatisfy` any (\request -> "/rubygems/info/" `T.isPrefixOf` nativeRequestPath request && nativeRequestStatus request == 403)
                    filter (T.isSuffixOf ".gem" . nativeRequestPath) (nativeClientRequests interval) `shouldBe` []
                    map nativeRequestPath (nativeUpstreamRequests interval) `shouldSatisfy` all (\path -> path == "/versions" || T.isPrefixOf "/info/" path)
                    filter (T.isSuffixOf ".gem" . nativeRequestPath) (nativeUpstreamRequests interval) `shouldBe` []
                before <- nativeTraffic observed
                proxyStatus (observedProxy observed) (rubyArchiveRoute release) `shouldReturn` 403
                proxyGet (observedProxy observed) "/rubygems/specs.4.8.gz" `shouldReturn` (404, "")
                after <- nativeTraffic observed
                interval <- nativeTrafficSince before after
                map nativeRequestPath (nativeUpstreamRequests interval) `shouldSatisfy` all (\path -> path == "/versions" || T.isPrefixOf "/info/" path)
        it (label "native-extension" "builds and loads an admitted native extension under the supported default policy") $ \plane ->
            withObservedNativeProxy defaultE2EConfig plane $ \observed -> rubyConsumer (observedProxy observed) currentBundler $ \project -> do
                rubyDependency project "ecluse-fixture-native" "= 1.0.0"
                before <- nativeTraffic observed
                void (rubyInstall project False >>= shouldSucceed)
                tree <- rubyInstalledTree project
                Map.keys tree `shouldSatisfy` any (T.isSuffixOf "gem.build_complete" . toText)
                Map.keys tree `shouldSatisfy` any (T.isSuffixOf "ecluse_fixture_native.so" . toText)
                loaded <- rubyEvaluate project "spec = Gem.loaded_specs.fetch('ecluse-fixture-native'); require File.join(spec.extension_dir, 'ecluse_fixture_native.' + RbConfig::CONFIG.fetch('DLEXT')); require 'ecluse_fixture_native'; puts EcluseFixtureNative::VALUE" >>= shouldSucceed
                T.strip (crStdout loaded) `shouldBe` "ecluse-fixture-native-1.0.0-ruby"
                after <- nativeTraffic observed
                interval <- nativeTrafficSince before after
                assertRubyRouting interval
                assertRubyAssets project interval
        it (label "metadata-no-archives" "reads only named metadata without policy-only archive or compact-spec requests") $ \plane ->
            withObservedNativeProxy defaultE2EConfig plane $ \observed -> do
                before <- nativeTraffic observed
                for_ [rubyRoot, rubyLeaf, "ecluse-fixture-native"] $ \name ->
                    proxyStatus (observedProxy observed) ("/rubygems/info/" <> name) `shouldReturn` 200
                after <- nativeTraffic observed
                interval <- nativeTrafficSince before after
                let requests = nativeUpstreamRequests interval
                requests `shouldSatisfy` (not . null)
                map nativeRequestPath requests `shouldSatisfy` all (\path -> path == "/versions" || T.isPrefixOf "/info/" path)
        it (label "private-authority" "keeps authorised private names available while refusing anonymous and wrong callers") $ \plane ->
            withRubyRegistry $ \registry -> do
                releases <- loadRubyReleases
                selected <- traverse (\name -> rubyRelease releases name rubyVersion "ruby") [rubyRoot, rubyLeaf]
                seedRubyRegistry registry "rubygems-private" selected
                setRubyRegistryFault registry "rubygems-private" RubyRegistryRequireRead
                let config = rubyDeniedConfig{ecExtraEnv = ecExtraEnv rubyDeniedConfig <> [("ECLUSE_MOUNTS__RUBYGEMS__PRIVATE_UPSTREAM__REGISTRY__URL", stubUrl RubyGemsPrivate)]}
                withStubFault plane (RubyGemsBackend (rubyRegistryPort registry) False) $
                    withObservedNativeProxy config plane $ \observed -> do
                        let proxy = observedProxy observed
                        withRubyProject currentBundler proxy (e2eBaseUrl proxy <> "/rubygems/") (Just "ruby:read") $ \project -> do
                            void (rubyInstall project False >>= shouldSucceed)
                            assertRubyContents project "ecluse-fixture-leaf-1.0.0-ruby|root-1.0.0"
                        before <- nativeTraffic observed
                        forM_ [Nothing, Just "ruby:wrong"] $ \credentials ->
                            withRubyProject currentBundler proxy (e2eBaseUrl proxy <> "/rubygems/") credentials $ \project -> do
                                void (rubyInstall project False >>= shouldFail)
                                rubyInstalledTree project `shouldReturn` mempty
                        after <- nativeTraffic observed
                        interval <- nativeTrafficSince before after
                        filter (T.isSuffixOf ".gem" . nativeRequestPath) (nativeUpstreamRequests interval) `shouldBe` []
                        observedCredentials <- readTVarIO (rubyRegistryCredentials registry)
                        let public = [credential | (host, _, credential) <- observedCredentials, host == "rubygems-upstream"]
                            private = [credential | (host, _, credential) <- observedCredentials, host == "rubygems-private"]
                        public `shouldSatisfy` (not . null)
                        public `shouldSatisfy` all isNothing
                        private `shouldSatisfy` elem (Just "Basic cnVieTpyZWFk")
        it (label "native-surface" "serves native info and refuses retired or unsupported index routes without source reads") $ \plane ->
            withObservedNativeProxy defaultE2EConfig plane $ \observed -> do
                let proxy = observedProxy observed
                proxyStatus proxy "/rubygems/info/ecluse-fixture-root" `shouldReturn` 200
                before <- nativeTraffic observed
                for_ ["/rubygems/api/v1/dependencies", "/rubygems/api/v1/dependencies?gems=ecluse-fixture-root", "/rubygems/specs.4.8.gz", "/rubygems/info/"] $ \path ->
                    proxyStatus proxy path `shouldReturn` 404
                after <- nativeTraffic observed
                interval <- nativeTrafficSince before after
                nativeUpstreamRequests interval `shouldBe` []
  where
    label name = caseLabel ("rubygems/native/" <> name)

-- | Bundler selects every foreign archive, while only the compatible host fixture executes.
assertRubyPlatformInstall :: ObservedNativeProxy -> [RubyRelease] -> RubyProject -> IO ()
assertRubyPlatformInstall observed releases project = do
    sort (map releasePlatform releases) `shouldBe` ["arm64-darwin", "java", "ruby", "x86_64-linux-musl"]
    before <- nativeTraffic observed
    rubyLockfile project `shouldReturn` Nothing
    void (rubyInstall project False >>= shouldSucceed)
    -- Bundler 4.0.23 materialises checksums before applying platforms in the same command.
    void (runBundler project ["lock", "--add-platform", "ruby", "java", "x86_64-linux-musl", "arm64-darwin"] >>= shouldSucceed)
    void (rubyLock project >>= shouldSucceed)
    lock <- rubyLockfile project >>= maybe (fail "platform resolution did not create a lockfile") pure
    for_ releases $ \release -> lock `shouldSatisfy` T.isInfixOf ("    " <> lockCoordinate release <> "\n")
    void (runBundler project ["cache", "--all-platforms", "--no-install"] >>= shouldSucceed)
    for_ releases $ \release -> do
        bytes <- BS.readFile (rubyDirectory project </> "vendor/cache" </> toString (releaseFilename release))
        bytes `shouldBe` releaseArchive release
        native <- runRubyReference "archive" bytes
        coordinate <- either fail pure (parseEither (withObject "native archive" (\row -> (,,,) <$> row .: "name" <*> row .: "version" <*> row .: "platform" <*> row .: "sha256")) native)
        coordinate `shouldBe` (releaseName release, releaseVersion release, releasePlatform release, releaseChecksum release)
    void (rubyInstall project False >>= shouldSucceed)
    installedLock <- rubyLockfile project >>= maybe (fail "platform installation did not retain its lockfile") pure
    for_ releases $ \release -> installedLock `shouldSatisfy` T.isInfixOf ("  " <> lockCoordinate release <> " sha256=" <> releaseChecksum release <> "\n")
    void (rubyInstall project True >>= shouldSucceed)
    rubyLockfile project `shouldReturn` Just installedLock
    installed <- rubyEvaluate project "require 'ecluse_fixture_platform'; puts EcluseFixturePlatform::VALUE" >>= shouldSucceed
    T.strip (crStdout installed) `shouldBe` "ecluse-fixture-platform-1.0.0-ruby"
    after <- nativeTraffic observed
    interval <- nativeTrafficSince before after
    let requests = nativeClientRequests interval
        paths = ["/rubygems/versions", "/rubygems/info/ecluse-fixture-platform"] <> concatMap (\release -> [rubyArchiveRoute release, "/rubygems/" <> toText (releaseCompactPath release)]) releases
        archives = [T.takeWhileEnd (/= '/') (nativeRequestPath request) | request <- requests, ".gem" `T.isSuffixOf` nativeRequestPath request, nativeRequestStatus request == 200]
    map nativeRequestPath requests `shouldSatisfy` all (`elem` paths)
    for_ releases $ \release -> archives `shouldSatisfy` elem (releaseFilename release)
  where
    lockCoordinate release = releaseName release <> " (" <> releaseVersion release <> (if releasePlatform release == "ruby" then "" else "-" <> releasePlatform release) <> ")"

clientCase :: BundlerClient -> GlobalDataPlane -> IO ()
clientCase client plane = withObservedNativeProxy defaultE2EConfig plane $ \observed -> rubyConsumer (observedProxy observed) client $ \project -> do
    before <- nativeTraffic observed
    rubyLockfile project `shouldReturn` Nothing
    void (rubyInstall project False >>= shouldSucceed)
    void (rubyLock project >>= shouldSucceed)
    lock <- rubyLockfile project >>= maybe (fail "Bundler installation did not retain its lockfile") pure
    releases <- loadRubyReleases
    forM_ [rubyRoot, rubyLeaf] $ \name -> do
        release <- rubyRelease releases name rubyVersion "ruby"
        lock `shouldSatisfy` T.isInfixOf (releaseChecksum release)
    assertRubyContents project "ecluse-fixture-leaf-1.0.0-ruby|root-1.0.0"
    void (rubyInstall project True >>= shouldSucceed)
    rubyLockfile project `shouldReturn` Just lock
    rubyDependency project rubyRoot "~> 2.0"
    void (rubyUpdate project >>= shouldSucceed)
    assertRubyContents project "ecluse-fixture-leaf-2.0.0-ruby|root-2.0.0"
    after <- nativeTraffic observed
    traffic <- nativeTrafficSince before after
    assertRubyRouting traffic
    assertRubyAssets project traffic
    nativeClientRequests traffic `shouldSatisfy` any (\request -> nativeRequestPath request == "/rubygems/versions" && nativeRequestStatus request == 200)
    nativeClientRequests traffic `shouldSatisfy` (not . any (T.isPrefixOf "/rubygems/api/v1/dependencies" . nativeRequestPath))
    let requestedInfo = filter (T.isPrefixOf "/info/") (map nativeRequestPath (nativeUpstreamRequests traffic))
    requestedInfo `shouldSatisfy` all (`elem` ["/info/ecluse-fixture-root", "/info/ecluse-fixture-leaf"])

retainedCacheCase :: BundlerClient -> GlobalDataPlane -> IO ()
retainedCacheCase client plane = withRubyRegistry $ \registry -> do
    releases <- loadRubyReleases
    selected <- traverse (\name -> rubyRelease releases name rubyVersion "ruby") [rubyRoot, rubyLeaf]
    root <- rubyRelease releases rubyRoot rubyVersion "ruby"
    let privateRoot = root{releaseChecksum = T.replicate 64 "a", releaseInfoRow = T.replace (releaseChecksum root) (T.replicate 64 "a") (releaseInfoRow root)}
        config = defaultE2EConfig{ecArgs = ["proxy", "--no-worker"], ecExtraEnv = [("ECLUSE_MOUNTS__RUBYGEMS__PRIVATE_UPSTREAM__REGISTRY__URL", stubUrl RubyGemsPrivate), ("ECLUSE_SERVER__METADATA_GZIP", "true")]}
    seedRubyRegistry registry "rubygems-private" selected
    withStubFault plane (RubyGemsBackend (rubyRegistryPort registry) False) $
        withObservedNativeProxy config plane $ \observed -> do
            let proxy = observedProxy observed
            withRubyProject client proxy (e2eBaseUrl proxy <> "/rubygems/") Nothing $ \project -> do
                void (rubyInstall project False >>= shouldSucceed)
                void (rubyLock project >>= shouldSucceed)
                seedRubyRegistry registry "rubygems-private" [privateRoot]
                before <- nativeTraffic observed
                void (rubyUpdate project >>= shouldFail)
                after <- nativeTraffic observed
                interval <- nativeTrafficSince before after
                nativeClientRequests interval `shouldSatisfy` any (\request -> nativeRequestPath request == "/rubygems/info/ecluse-fixture-root" && nativeRequestStatus request == 200)
                (_, info) <- proxyGet proxy "/rubygems/info/ecluse-fixture-root"
                rows <- either (fail . toString) pure (referenceInfo (LBS.toStrict info))
                rows `shouldSatisfy` any (elem ("checksum", releaseChecksum privateRoot) . rowFields)
                seedRubyRegistry registry "rubygems-private" selected
                setRubyRegistryFault registry "rubygems-private" RubyRegistryRequireRead
                void (rubyUpdate project >>= shouldFail)
                setRubyRegistryFault registry "rubygems-private" RubyRegistryHealthy
                void (rubyUpdate project >>= shouldSucceed)

compactRequiredCase :: BundlerClient -> Bool -> Int -> GlobalDataPlane -> IO ()
compactRequiredCase client failInfo status plane = withRubyRegistry $ \registry -> do
    releases <- loadRubyReleases
    release <- rubyRelease releases rubyRoot rubyVersion "ruby"
    setRubyRegistryFault registry "rubygems-upstream" (if failInfo then RubyRegistryInfoStatus rubyLeaf status else RubyRegistryDirectoryStatus status)
    withStubFault plane (RubyGemsBackend (rubyRegistryPort registry) False) $
        withObservedNativeProxy defaultE2EConfig{ecArgs = ["proxy", "--no-worker"]} plane $ \observed -> do
            let proxy = observedProxy observed
                named = "/rubygems/info/ecluse-fixture-root"
            proxyStatus proxy named `shouldReturn` 200
            unless failInfo (proxyStatus proxy "/rubygems/info/ecluse-fixture-leaf" `shouldReturn` 200)
            proxyGet proxy (rubyArchiveRoute release) `shouldReturn` (200, LBS.fromStrict (releaseArchive release))
            rubyConsumer proxy client $ \project -> do
                rubyLockfile project `shouldReturn` Nothing
                before <- nativeTraffic observed
                void (rubyInstall project False >>= shouldFail)
                rubyInstalledTree project `shouldReturn` mempty
                after <- nativeTraffic observed
                traffic <- nativeTrafficSince before after
                let clients = nativeClientRequests traffic
                if failInfo
                    then do
                        clients `shouldSatisfy` any (\request -> nativeRequestPath request == "/rubygems/versions" && nativeRequestStatus request == 200)
                        clients `shouldSatisfy` any (\request -> nativeRequestPath request == "/rubygems/info/ecluse-fixture-leaf" && nativeRequestStatus request >= 400)
                        sources <- readTVarIO (rubyRegistryRequests registry)
                        let failed = filter (\request -> nativeRequestHost request == "rubygems-upstream" && nativeRequestPath request == "/info/ecluse-fixture-leaf") sources
                        failed `shouldSatisfy` (not . null)
                        map nativeRequestStatus failed `shouldBe` replicate (length failed) status
                    else clients `shouldSatisfy` any (\request -> nativeRequestPath request == "/rubygems/versions" && nativeRequestStatus request >= 400)
                let dependencyRequests = filter (T.isPrefixOf "/rubygems/api/v1/dependencies" . nativeRequestPath) clients
                dependencyRequests `shouldSatisfy` (not . null)
                map nativeRequestStatus dependencyRequests `shouldSatisfy` all (== 404)
                filter (T.isSuffixOf ".gem" . nativeRequestPath) clients `shouldBe` []
                filter (T.isSuffixOf ".gem" . nativeRequestPath) (nativeUpstreamRequests traffic) `shouldBe` []
            proxyStatus proxy named `shouldReturn` 200
