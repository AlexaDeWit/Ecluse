-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Actual Bundler installations and archive exchanges supply the canonical installation laws.
module Ecluse.E2E.RubyGems.InstallE2ESpec (
    rubyInstallationFamily,
    rubyConsumer,
    rubyDeniedConfig,
    assertRubyRouting,
    assertRubyContents,
    assertRubyAssets,
    assertRubyClientAssets,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeFileName)
import Test.Hspec (shouldBe, shouldMatchList, shouldSatisfy)

import Ecluse.E2E.Fixtures.Contracts (concreteFamily)
import Ecluse.E2E.Fixtures.RubyGems
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.RubyGems
import Ecluse.E2E.Harness.RubyGemsGateway
import Ecluse.E2E.InstallationE2ESpec (codedArtifact)
import Ecluse.Test.Contract.Ecosystem (FamilyFixture)
import Ecluse.Test.Contract.Install
import Ecluse.Test.Contract.Phase (timedFixture)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Registry.RubyGems.Fixtures

-- | Refused and admitted installs use separate homes, caches and installed trees.
rubyInstallationFamily :: FamilyFixture (InstallationDriver Bool)
rubyInstallationFamily = concreteFamily (rubyReadFixture "installation" ["read", "client-install"]) $ \recorder action plane ->
    timedFixture recorder (withObservedRubyProxy defaultE2EConfig plane) $ \observed -> do
        releases <- loadRubyReleases
        release <- rubyRelease releases rubyRoot rubyVersion "ruby"
        action (installation observed release)

installation :: ObservedRubyProxy -> RubyRelease -> InstallationDriver Bool
installation observed release =
    InstallationDriver
        { installationAccepted = InstallDriver accepted True
        , installationDenied = InstallDriver denied (False, mempty)
        , installationArtifact = (releaseChecksum release,) <$> proxyGetCoded proxy archive
        , installationDigest = hexSha256Of
        , installationHead = proxyHead proxy archive
        , installationCodedArtifact = codedArtifact proxy RubyGemsPublic (T.drop (T.length "/rubygems") archive) archive (pure (releaseChecksum release))
        }
  where
    proxy = rubyProxy observed
    archive = rubyArchiveRoute release
    accepted = rubyConsumer proxy currentBundler $ \project -> do
        before <- rubyTraffic observed
        result <- rubyInstall project False
        when (crExit result == ExitSuccess) $ do
            assertRubyContents project "ecluse-fixture-leaf-1.0.0-ruby|root-1.0.0"
            after <- rubyTraffic observed
            assertRubyRouting (rubyTrafficSince before after)
            assertRubyAssets project (rubyTrafficSince before after)
        pure (result, crExit result == ExitSuccess)
    denied = withObservedRubyProxy rubyDeniedConfig (e2ePlane proxy) $ \refused ->
        rubyConsumer (rubyProxy refused) currentBundler $ \project -> do
            result <- rubyInstall project False
            tree <- rubyInstalledTree project
            pure (result, (crExit result == ExitSuccess, tree))

-- | The client source names only the product's RubyGems mount.
rubyConsumer :: E2E -> BundlerClient -> (RubyProject -> IO a) -> IO a
rubyConsumer proxy client = withRubyProject client proxy (e2eBaseUrl proxy <> "/rubygems/") Nothing

-- | Installed source and the bundled executable must agree on the dependency graph.
assertRubyContents :: RubyProject -> Text -> IO ()
assertRubyContents project expected = do
    moduleResult <- rubyEvaluate project "require 'ecluse_fixture_root'; puts EcluseFixtureRoot::VALUE" >>= shouldSucceed
    T.strip (crStdout moduleResult) `shouldBe` expected
    executable <- runBundler project ["exec", "ecluse-fixture"] >>= shouldSucceed
    T.strip (crStdout executable) `shouldBe` expected

-- | The proxy and TLS edge observe native archives and on-demand info exchanges.
assertRubyRouting :: RubyTraffic -> IO ()
assertRubyRouting traffic = do
    assertRubyClientAssets traffic
    let native request = any (`T.isSuffixOf` rubyRequestPath request) [".gem", ".gemspec.rz"]
        clients = filter native (rubyClientRequests traffic)
        origins = filter ((== "rubygems-upstream") . rubyRequestHost) (rubyUpstreamRequests traffic)
        exchange request = (rubyRequestMethod request, rubyRequestPath request, rubyRequestStatus request)
    origins `shouldSatisfy` any (T.isPrefixOf "/info/" . rubyRequestPath)
    map (\request -> (rubyRequestMethod request, T.drop (T.length "/rubygems") (rubyRequestPath request), rubyRequestStatus request)) clients
        `shouldMatchList` map exchange (filter native origins)

-- | Client archives and info rows traverse the product listener.
assertRubyClientAssets :: RubyTraffic -> IO ()
assertRubyClientAssets traffic = do
    let native request = any (`T.isSuffixOf` rubyRequestPath request) [".gem", ".gemspec.rz"]
        clients = filter native (rubyClientRequests traffic)
        forwarded = filter native (rubyProxyRequests traffic)
        exchange request = (rubyRequestMethod request, rubyRequestPath request, rubyRequestStatus request)
        successful suffix = any (\request -> suffix `T.isSuffixOf` rubyRequestPath request && rubyRequestStatus request == 200)
    clients `shouldSatisfy` successful ".gem"
    rubyClientRequests traffic `shouldSatisfy` any (\request -> "/rubygems/info/" `T.isPrefixOf` rubyRequestPath request && rubyRequestStatus request == 200)
    map rubyRequestScheme clients `shouldSatisfy` all (== "https")
    map rubyRequestPath clients `shouldSatisfy` all (T.isPrefixOf "/rubygems/")
    map exchange clients `shouldMatchList` map exchange forwarded

-- | Every archive retained by Bundler needs a successful exchange at the product listener.
assertRubyAssets :: RubyProject -> RubyTraffic -> IO ()
assertRubyAssets project traffic = do
    tree <- rubyInstalledTree project
    let archives = ordNub [toText (takeFileName path) | path <- Map.keys tree, ".gem" `T.isSuffixOf` toText path]
        received = ordNub [T.takeWhileEnd (/= '/') (rubyRequestPath request) | request <- rubyClientRequests traffic, ".gem" `T.isSuffixOf` rubyRequestPath request, rubyRequestStatus request == 200]
    archives `shouldSatisfy` (not . null)
    archives `shouldMatchList` received

-- | Removing both admission paths proves that artifact checks do not rely on dependency filtering.
rubyDeniedConfig :: E2EConfig
rubyDeniedConfig = defaultE2EConfig{ecExtraEnv = [("ECLUSE_MOUNTS__RUBYGEMS__RULES", "{\"min-age\":{\"enabled\":false},\"remediation-fast-track\":{\"enabled\":false}}")]}
