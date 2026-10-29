-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Public and completed-mirror Bundler installs retain native graph, content and source observations.
module Ecluse.E2E.RubyGems.MirrorInstallE2ESpec (
    RubyCoordinates,
    rubyMirrorFamily,
    rubyMirrorConfig,
    withRubyMirrorFixture,
    rubyObservedGraph,
    rubyPackageTree,
    rubyPublicReachable,
    rubyMirrorComplete,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import System.FilePath (joinPath, splitDirectories)
import Test.Hspec (shouldReturn, shouldSatisfy)
import UnliftIO (finally)

import Ecluse.E2E.Fixtures.Contracts (concreteFamily)
import Ecluse.E2E.Fixtures.RubyGems
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.RubyGems
import Ecluse.E2E.Harness.NativeEdge
import Ecluse.E2E.Harness.RubyGemsRegistry
import Ecluse.E2E.RubyGems.InstallE2ESpec (assertRubyAssets, assertRubyClientAssets, rubyConsumer)
import Ecluse.Test.Contract.Ecosystem (FamilyFixture)
import Ecluse.Test.Contract.Install (TargetExpectation (..), TargetObservation (..), TargetPortability (..))
import Ecluse.Test.Contract.Mirror
import Ecluse.Test.Contract.Phase (timedFixture)
import Ecluse.Test.Registry.RubyGems.Fixtures

-- | Native comparison never replaces exact raw version identities in observed dependency graphs.
type RubyCoordinates = [(Text, Text)]

-- | The private read authority exposes this fixture's mirror target. Queue and state survive product restarts.
rubyMirrorConfig :: E2EConfig
rubyMirrorConfig =
    defaultE2EConfig
        { ecExtraEnv =
            [ ("ECLUSE_MIRROR_STATE__URL", "s3://" <> advisoryBucket <> "/rubygems-mirror")
            , ("ECLUSE_MOUNTS__RUBYGEMS__PRIVATE_UPSTREAM__REGISTRY__URL", stubUrl RubyGemsMirror)
            , ("ECLUSE_MOUNTS__RUBYGEMS__MIRROR_TARGET__REGISTRY__URL", stubUrl RubyGemsMirror)
            , ("ECLUSE_MOUNTS__RUBYGEMS__MIRROR_TARGET__REGISTRY__UPLOAD_URL", T.dropWhileEnd (== '/') (stubUrl RubyGemsMirror))
            , ("ECLUSE_MOUNTS__RUBYGEMS__MIRROR_TARGET__REGISTRY__TOKEN", rubyRegistryToken)
            ]
                <> ministackAwsEnv
        }

-- | Target inventories and original public files survive product restarts within the family.
withRubyMirrorFixture :: GlobalDataPlane -> (RubyRegistry -> [RubyRelease] -> IO a) -> IO a
withRubyMirrorFixture plane action = do
    createAdvisoryBucket plane
    releases <- loadRubyReleases
    withRubyRegistry $ \registry ->
        withStubFault plane (RubyGemsBackend (rubyRegistryPort registry) False) (action registry releases)

-- | Architecture-independent source gems still require actual installed files and a linked executable.
rubyMirrorFamily :: (MirrorFixture RubyCoordinates Text, FamilyFixture (MirrorDriver RubyCoordinates Text))
rubyMirrorFamily =
    ( MirrorFixture coordinates (Right (target, target)) expectedOutput expectedVersions requiredPaths (Right ["/rubygems/"])
    , concreteFamily (rubyWriteFixture "mirror" ["read", "mirror", "client-install"]) $ \recorder action plane ->
        timedFixture recorder (withRubyMirrorFixture plane . curry) $ \(registry, releases) -> do
            selected <- traverse (\name -> rubyRelease releases name rubyVersion "ruby") [rubyRoot, rubyLeaf]
            queue <- sharedMirrorQueue plane
            action (mirrorDriver plane registry selected rubyMirrorConfig{ecQueueUrl = Just queue})
    )
  where
    coordinates = sort [(rubyRoot, rubyVersion), (rubyLeaf, rubyVersion)]
    expectedOutput = "ecluse-fixture-leaf-1.0.0-ruby|root-1.0.0"
    expectedVersions = [(name, [rubyVersion]) | name <- ordNub (map fst coordinates)]
    target = TargetExpectation ArchitectureIndependent [name <> "-1.0.0.gem" | (name, _) <- coordinates] []
    requiredPaths = Set.fromList (["/info/" <> name | (name, _) <- coordinates] <> ["/gems/" <> name <> "-1.0.0.gem" | (name, _) <- coordinates])

mirrorDriver :: GlobalDataPlane -> RubyRegistry -> [RubyRelease] -> E2EConfig -> MirrorDriver RubyCoordinates Text
mirrorDriver plane registry releases config =
    MirrorDriver
        { mirrorPublicInstall = do
            before <- versions
            (observed, target, reachable) <- withObservedNativeProxy config{ecArgs = ["proxy", "--no-worker"]} plane $ \proxy -> do
                reachable <- rubyPublicReachable (observedProxy proxy)
                (\(result, selected, _) -> (result, selected, reachable)) <$> rubyObservedGraph proxy
            rubyRegistryFiles registry "rubygems-mirror" `shouldReturn` []
            withObservedNativeProxy config plane $ \proxy -> do
                awaitRubyRegistry registry "rubygems-mirror" releases `shouldReturn` True
                for_ releases $ \release -> awaitProxyLog (observedProxy proxy) (rubyMirrorComplete (releaseName release) (releaseVersion release)) 120 `shouldReturn` True
            PublicPhase observed target reachable before <$> versions
        , mirrorOfflineInstall = withStubFault plane (RubyGemsBackend (rubyRegistryPort registry) True) $
            withObservedNativeProxy config{ecArgs = ["proxy", "--no-worker"]} plane $ \proxy -> do
                reachable <- rubyPublicReachable (observedProxy proxy)
                before <- nativeTraffic proxy
                (observed, target, sources) <- rubyObservedGraph proxy
                after <- nativeTraffic proxy
                interval <- nativeTrafficSince before after
                let requests = nativeUpstreamRequests interval
                pure
                    ( OfflinePhase
                        observed
                        target
                        Nothing
                        reachable
                        (Set.fromList [nativeRequestPath request | request <- requests, nativeRequestHost request == "rubygems-mirror", nativeRequestStatus request == 200])
                        [show request | request <- requests, nativeRequestHost request == "rubygems-upstream", nativeRequestStatus request /= 444]
                        sources
                    )
        , mirrorEmptyInstall = do
            stored <- rubyRegistryFiles registry "rubygems-mirror"
            for_ releases $ \release -> removeRubyRegistryVersion registry "rubygems-mirror" (releaseName release) (releaseVersion release)
            flip finally (seedRubyRegistry registry "rubygems-mirror" stored) $
                withStubFault plane (RubyGemsBackend (rubyRegistryPort registry) True) $
                    withObservedNativeProxy config{ecArgs = ["proxy", "--no-worker"]} plane $ \proxy -> do
                        reachable <- rubyPublicReachable (observedProxy proxy)
                        rubyConsumer (observedProxy proxy) currentBundler $ \project -> (reachable,) <$> rubyInstall project False
        }
  where
    versions = forM (sort (ordNub (map releaseName releases))) $ \name -> (name,) <$> rubyRegistryVersions registry "rubygems-mirror" name

-- | Graphs, portability and archive names come from installed native specifications, not expected fixture rows.
rubyObservedGraph :: ObservedNativeProxy -> IO (Observed RubyCoordinates Text, TargetObservation RubyCoordinates, [Text])
rubyObservedGraph observed = rubyConsumer proxy currentBundler $ \project -> do
    before <- nativeTraffic observed
    result <- rubyInstall project False >>= shouldSucceed
    facts <- rubyEvaluate project "puts Bundler.load.specs.reject { |s| s.name == 'bundler' }.map { |s| [s.name, s.version.to_s, s.platform.to_s, s.file_name, s.full_gem_path].join(\"\\t\") }" >>= shouldSucceed
    rows <- traverse row (T.lines (T.strip (crStdout facts)))
    exported <- rubyEvaluate project "require 'ecluse_fixture_root'; puts EcluseFixtureRoot::VALUE" >>= shouldSucceed
    executable <- runBundler project ["exec", "ecluse-fixture"] >>= shouldSucceed
    tree <- rubyPackageTree project
    lock <- rubyLockfile project >>= maybe (fail "mirror install did not create a lockfile") pure
    sources <- traverse (maybe (fail "Bundler lockfile names a source outside the product") pure . T.stripPrefix (e2eBaseUrl proxy)) [source | line <- T.lines lock, Just source <- [T.stripPrefix "  remote: " line]]
    sources `shouldSatisfy` (not . null)
    after <- nativeTraffic observed
    interval <- nativeTrafficSince before after
    assertRubyClientAssets interval
    assertRubyAssets project interval
    let graph = sort [(name, version) | (name, version, _, _, _) <- rows]
        archives = sort [filename | (_, _, _, filename, _) <- rows]
        platforms = ordNub [platform | (_, _, platform, _, _) <- rows]
        portability = if platforms == ["ruby"] then ArchitectureIndependent else ArchitectureSpecific (T.intercalate "," platforms)
        foreignPaths = [toString path | (_, _, _, _, path) <- rows, not ((toText (rubyDirectory project) <> "/installed/") `T.isPrefixOf` path)]
        observedResult = Observed (Right graph) (Right (T.strip (crStdout exported))) (Just (Right (T.strip (crStdout executable)))) tree result foreignPaths
    pure (observedResult, TargetObservation portability result (Right graph) (Right archives) tree, ordNub sources)
  where
    proxy = observedProxy observed
    row line = case T.splitOn "\t" line of
        [name, version, platform, filename, path] -> pure (name, version, platform, filename, path)
        _ -> fail "native installed specification observation is malformed"

-- | Native package source files and cached archives exclude transport caches and project paths.
rubyPackageTree :: RubyProject -> IO InstalledTree
rubyPackageTree project = Map.fromList . mapMaybe nativePath . Map.toList <$> rubyInstalledTree project
  where
    nativePath (path, entry) = case dropWhile (`notElem` ["gems", "cache", "bin"]) (splitDirectories path) of
        [] -> Nothing
        components -> Just (joinPath components, entry)

-- | Reuse the existing worker completion observation without duplicating its log parser.
rubyMirrorComplete :: Text -> Text -> Text -> Bool
rubyMirrorComplete = mirrorVersionComplete

-- | An unmirrored name proves whether the fresh product can still reach public source data.
rubyPublicReachable :: E2E -> IO Bool
rubyPublicReachable proxy = (== 200) <$> proxyStatus proxy "/rubygems/info/ecluse-fixture-sentinel"
