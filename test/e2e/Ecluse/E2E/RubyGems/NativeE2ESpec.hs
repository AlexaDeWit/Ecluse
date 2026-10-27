-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Supported Bundler clients expose real resolution, installation, update and refusal observations.
module Ecluse.E2E.RubyGems.NativeE2ESpec (rubyNativeContract) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.FilePath ((</>))
import Test.Hspec (around, it, shouldBe, shouldReturn, shouldSatisfy)

import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (..))
import Ecluse.E2E.Fixtures.RubyGems
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.RubyGems
import Ecluse.E2E.Harness.RubyGemsGateway
import Ecluse.E2E.Harness.RubyGemsRegistry
import Ecluse.E2E.RubyGems.InstallE2ESpec
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Phase (timedFixture)
import Ecluse.Test.Contract.Runner (DeclaredGroup (..))
import Ecluse.Test.Registry.RubyGems.Fixtures

-- | Each native refusal receives fresh client state and a separate product fixture lifetime.
rubyNativeContract :: DeclaredGroup
rubyNativeContract = DeclaredGroup (fixtureMetadata rubyNativeFixture) $ \recorder ->
    around (timedFixture recorder (withFixture rubyNativeFixture)) $ do
        it (label "minimum-client" "locks, installs, freezes and updates with Bundler 2.6.9") (clientCase minimumBundler)
        it (label "current-client" "locks, installs, freezes and updates with Bundler 4.0.23") (clientCase currentBundler)
        it (label "platforms" "retains the Ruby, Java, Linux musl and Darwin variants in a native lockfile") $ \plane ->
            withObservedRubyProxy defaultE2EConfig plane $ \observed -> rubyConsumer (rubyProxy observed) currentBundler $ \project -> do
                rubyDependency project "ecluse-fixture-platform" "= 1.0.0"
                void (runBundler project ["lock", "--add-platform", "java", "x86_64-linux-musl", "arm64-darwin", "--add-checksums"] >>= shouldSucceed)
                lock <- rubyLockfile project >>= maybe (fail "platform resolution did not create a lockfile") pure
                forM_ ["1.0.0-java", "1.0.0-x86_64-linux-musl", "1.0.0-arm64-darwin"] $ \coordinate -> lock `shouldSatisfy` T.isInfixOf coordinate
                void (rubyInstall project True >>= shouldSucceed)
                installed <- rubyEvaluate project "require 'ecluse_fixture_platform'; puts EcluseFixturePlatform::VALUE" >>= shouldSucceed
                T.strip (crStdout installed) `shouldSatisfy` T.isPrefixOf "ecluse-fixture-platform-1.0.0-"
        it (label "checksum-failure" "refuses a wrong lockfile digest after fetching the admitted native archive") $ \plane ->
            withObservedRubyProxy defaultE2EConfig plane $ \observed -> do
                releases <- loadRubyReleases
                release <- rubyRelease releases rubyRoot rubyVersion "ruby"
                lock <- rubyConsumer (rubyProxy observed) currentBundler $ \control -> do
                    void (rubyLock control >>= shouldSucceed)
                    rubyLockfile control >>= maybe (fail "checksum control did not create a lockfile") pure
                lock `shouldSatisfy` T.isInfixOf (releaseChecksum release)
                rubyConsumer (rubyProxy observed) currentBundler $ \project -> do
                    writeFileText (rubyDirectory project </> "Gemfile.lock") (T.replace (releaseChecksum release) (T.replicate 64 "f") lock)
                    before <- rubyTraffic observed
                    result <- rubyInstall project True >>= shouldFail
                    T.toLower (crStdout result <> crStderr result) `shouldSatisfy` T.isInfixOf "checksum"
                    after <- rubyTraffic observed
                    rubyClientRequests (rubyTrafficSince before after) `shouldSatisfy` any (\request -> rubyRequestPath request == rubyArchiveRoute release && rubyRequestStatus request == 200)
                    tree <- rubyInstalledTree project
                    Map.keys tree `shouldSatisfy` (not . any (T.isSuffixOf "ecluse_fixture_root.rb" . toText))
        it (label "prerelease" "installs an explicitly requested native prerelease") $ \plane ->
            withObservedRubyProxy defaultE2EConfig plane $ \observed -> rubyConsumer (rubyProxy observed) currentBundler $ \project -> do
                rubyDependency project rubyLeaf "= 2.1.0.pre.1"
                void (rubyInstall project False >>= shouldSucceed)
                result <- rubyEvaluate project "require 'ecluse_fixture_leaf'; puts EcluseFixtureLeaf::VALUE" >>= shouldSucceed
                T.strip (crStdout result) `shouldBe` "ecluse-fixture-leaf-2.1.0.pre.1-ruby"
        it (label "ruby-requirement" "refuses a gem requiring Ruby 99 while an ordinary sibling installs") $ \plane ->
            withObservedRubyProxy defaultE2EConfig plane $ \observed -> do
                rubyConsumer (rubyProxy observed) currentBundler $ \control -> void (rubyInstall control False >>= shouldSucceed)
                rubyConsumer (rubyProxy observed) currentBundler $ \project -> do
                    rubyDependency project "ecluse-fixture-future" "= 1.0.0"
                    result <- rubyInstall project False >>= shouldFail
                    (crStdout result <> crStderr result) `shouldSatisfy` T.isInfixOf "99.0"
                    tree <- rubyInstalledTree project
                    tree `shouldBe` mempty
        it (label "direct-denial" "refuses pinned and direct archives without a public artifact fetch") $ \plane -> do
            releases <- loadRubyReleases
            release <- rubyRelease releases rubyRoot rubyVersion "ruby"
            withObservedRubyProxy defaultE2EConfig plane $ \control ->
                proxyStatus (rubyProxy control) (rubyArchiveRoute release) `shouldReturn` 200
            withObservedRubyProxy rubyDeniedConfig plane $ \observed -> do
                before <- rubyTraffic observed
                rubyConsumer (rubyProxy observed) currentBundler $ \project -> void (rubyInstall project False >>= shouldFail)
                proxyStatus (rubyProxy observed) (rubyArchiveRoute release) `shouldReturn` 403
                after <- rubyTraffic observed
                filter (T.isSuffixOf ".gem" . rubyRequestPath) (rubyUpstreamRequests (rubyTrafficSince before after)) `shouldBe` []
        it (label "metadata-no-archives" "reads only named metadata without policy-only archive or compact-spec requests") $ \plane ->
            withObservedRubyProxy defaultE2EConfig plane $ \observed -> do
                before <- rubyTraffic observed
                proxyStatus (rubyProxy observed) "/rubygems/api/v1/dependencies?gems=ecluse-fixture-root,ecluse-fixture-leaf" `shouldReturn` 200
                after <- rubyTraffic observed
                let requests = rubyUpstreamRequests (rubyTrafficSince before after)
                requests `shouldSatisfy` (not . null)
                map rubyRequestPath requests `shouldSatisfy` all (T.isPrefixOf "/info/")
        it (label "private-authority" "keeps authorised private names available while refusing anonymous and wrong callers") $ \plane ->
            withRubyRegistry $ \registry -> do
                releases <- loadRubyReleases
                selected <- traverse (\name -> rubyRelease releases name rubyVersion "ruby") [rubyRoot, rubyLeaf]
                seedRubyRegistry registry "rubygems-private" selected
                setRubyRegistryFault registry "rubygems-private" RubyRegistryRequireRead
                let config = rubyDeniedConfig{ecExtraEnv = ecExtraEnv rubyDeniedConfig <> [("ECLUSE_MOUNTS__RUBYGEMS__PRIVATE_UPSTREAM__REGISTRY__URL", stubUrl RubyGemsPrivate)]}
                withStubFault plane (RubyGemsBackend (rubyRegistryPort registry) False) $
                    withObservedRubyProxy config plane $ \observed -> do
                        let proxy = rubyProxy observed
                        withRubyProject currentBundler proxy (e2eBaseUrl proxy <> "/rubygems/") (Just "ruby:read") $ \project -> do
                            void (rubyInstall project False >>= shouldSucceed)
                            assertRubyContents project "ecluse-fixture-leaf-1.0.0-ruby|root-1.0.0"
                        before <- rubyTraffic observed
                        forM_ [Nothing, Just "ruby:wrong"] $ \credentials ->
                            withRubyProject currentBundler proxy (e2eBaseUrl proxy <> "/rubygems/") credentials $ \project -> do
                                void (rubyInstall project False >>= shouldFail)
                                rubyInstalledTree project `shouldReturn` mempty
                        after <- rubyTraffic observed
                        filter (T.isSuffixOf ".gem" . rubyRequestPath) (rubyUpstreamRequests (rubyTrafficSince before after)) `shouldBe` []
                        observedCredentials <- readTVarIO (rubyRegistryCredentials registry)
                        let public = [credential | (host, _, credential) <- observedCredentials, host == "rubygems-upstream"]
                            private = [credential | (host, _, credential) <- observedCredentials, host == "rubygems-private"]
                        public `shouldSatisfy` (not . null)
                        public `shouldSatisfy` all isNothing
                        private `shouldSatisfy` elem (Just "Basic cnVieTpyZWFk")
        it (label "batch-limits" "rejects malformed and excessive batches before native reads") $ \plane ->
            withObservedRubyProxy defaultE2EConfig plane $ \observed -> do
                let proxy = rubyProxy observed
                proxyStatus proxy "/rubygems/api/v1/dependencies" `shouldReturn` 200
                proxyStatus proxy "/rubygems/api/v1/dependencies?gems=ecluse-fixture-root,ecluse-fixture-leaf" `shouldReturn` 200
                before <- rubyTraffic observed
                forM_ ["?gems=a&gems=b", "?unknown=a", "?gems=" <> T.intercalate "," ["ecluse-batch-" <> show number | number <- [1 .. 101 :: Int]]] $ \query ->
                    proxyStatus proxy ("/rubygems/api/v1/dependencies" <> query) `shouldReturn` 400
                after <- rubyTraffic observed
                rubyUpstreamRequests (rubyTrafficSince before after) `shouldBe` []
  where
    label name = caseLabel ("rubygems/native/" <> name)

clientCase :: BundlerClient -> GlobalDataPlane -> IO ()
clientCase client plane = withObservedRubyProxy defaultE2EConfig plane $ \observed -> rubyConsumer (rubyProxy observed) client $ \project -> do
    before <- rubyTraffic observed
    void (rubyLock project >>= shouldSucceed)
    lock <- rubyLockfile project >>= maybe (fail "Bundler lock did not create its lockfile") pure
    lock `shouldSatisfy` T.isInfixOf "CHECKSUMS"
    releases <- loadRubyReleases
    forM_ [rubyRoot, rubyLeaf] $ \name -> do
        release <- rubyRelease releases name rubyVersion "ruby"
        lock `shouldSatisfy` T.isInfixOf (releaseChecksum release)
    void (rubyInstall project False >>= shouldSucceed)
    assertRubyContents project "ecluse-fixture-leaf-1.0.0-ruby|root-1.0.0"
    void (rubyInstall project True >>= shouldSucceed)
    rubyLockfile project `shouldReturn` Just lock
    rubyDependency project rubyRoot "~> 2.0"
    void (rubyUpdate project >>= shouldSucceed)
    assertRubyContents project "ecluse-fixture-leaf-2.0.0-ruby|root-2.0.0"
    after <- rubyTraffic observed
    let traffic = rubyTrafficSince before after
    assertRubyRouting traffic
    assertRubyAssets project traffic
    rubyClientRequests traffic `shouldSatisfy` any (\request -> rubyRequestPath request == "/rubygems/versions" && rubyRequestStatus request == 404)
    rubyClientRequests traffic `shouldSatisfy` any (T.isPrefixOf "/rubygems/api/v1/dependencies?" . rubyRequestPath)
