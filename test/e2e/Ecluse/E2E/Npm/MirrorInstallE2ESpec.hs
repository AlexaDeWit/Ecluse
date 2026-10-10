-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The install the mirror alone serves, held against the same install served from public. A new
project installs the fixture graph through the public gate, and the worker mirrors every version.
A second proxy and a second new project then install the same root while both public upstreams are
down. The two installs must agree on everything a client can see, so a field the mirror loses shows
as a difference whether or not a case names it.
-}
module Ecluse.E2E.Npm.MirrorInstallE2ESpec (spec) where

import Data.Aeson (Value)
import Data.Set qualified as Set
import Data.Text qualified as T
import Test.Hspec

import Ecluse.E2E.Fixtures.Npm (
    PkgSpec,
    graphDepPkg,
    graphExecutable,
    graphExport,
    graphLeafPkg,
    graphPackages,
    graphPeerPkg,
    graphRootPkg,
    psName,
    psVersion,
 )
import Ecluse.E2E.Harness

-- | Compare a mirror-only install with its control, using a real npm client and local stores.
spec :: Spec
spec = whenE2EAvailable (aroundAll withGlobalDataPlane scenarios)

scenarios :: SpecWith GlobalDataPlane
scenarios = describe "a new project's install while both public upstreams are down" $ do
    aroundAllWith withBothInstalls $ do
        it "resolves the dependency, its own dependency, and the peer, as the install from public did" $ \installs -> do
            obGraph (fromPublic installs) `shouldBe` Right expectedGraph
            obGraph (fromMirror installs) `shouldBe` Right expectedGraph

        it "loads every package's export through the root, as the install from public did" $ \installs -> do
            obExport (fromPublic installs) `shouldBe` Right graphExport
            obExport (fromMirror installs) `shouldBe` Right graphExport

        it "links the root's executable, which prints the root's export, as the install from public did" $ \installs -> do
            obExecutable (fromPublic installs) `shouldBe` Just (Right graphExport)
            obExecutable (fromMirror installs) `shouldBe` Just (Right graphExport)

        it "takes every packument and artifact from the mirror while public answers nothing" $ \installs -> do
            let answered = mirrorPhaseReads installs
            -- The reads ride along, so a failure shows what the stub answered beside what is missing.
            (Set.toList (neededOfMirror `Set.difference` servedByMirror answered), answered) `shouldSatisfy` (null . fst)
            filter answeredByPublic answered `shouldBe` []
            lockedSources (obTree (fromMirror installs))
                `shouldMatchList` [redactedProxy <> npmTarballPath (psName pkg) (psVersion pkg) | pkg <- graphPackages]

        it "leaves the tree the install from public left, apart from the proxy's address" $ \installs -> do
            let control = ("from public", obTree (fromPublic installs))
                mirrored = ("from the mirror", obTree (fromMirror installs))
            case treeDifferences control mirrored of
                [] -> pass
                differences -> expectationFailure (toString (T.unlines differences))

    it "fails the same install when no private store holds the graph" $ \plane ->
        withPublicUpstreamsDown plane . flip (withE2EWith emptyPrivateStore) plane $ \proxy -> do
            npmPublicReachable proxy `shouldReturn` False
            void $ npmInstall proxy rootRequest >>= shouldFail

-- What a client can see of one install: the graph npm resolved, what the root's module exports,
-- what its linked executable prints (Nothing when npm linked none), and the tree on disk.
data Observed = Observed
    { obGraph :: Either Text (Set Resolved)
    , obExport :: Either Text Value
    , obExecutable :: Maybe (Either Text Value)
    , obTree :: InstalledTree
    }

-- The control install, the mirror-only install, and what the stub answered during the second.
data Installs = Installs
    { fromPublic :: Observed
    , fromMirror :: Observed
    , mirrorPhaseReads :: [StubRead]
    }

{- Run both installs once for the group. Each has its own proxy and its own project, and the first
proxy is gone before public goes down, so nothing it fetched or cached can serve the second. -}
withBothInstalls :: (Installs -> IO ()) -> GlobalDataPlane -> IO ()
withBothInstalls action plane = do
    control <- withE2E installFromPublic plane
    (mirrored, answered) <- withPublicUpstreamsDown plane (withE2E installFromMirror plane)
    action Installs{fromPublic = control, fromMirror = mirrored, mirrorPhaseReads = answered}

-- The mirror holds none of the graph before the install, and every version of it once this returns.
installFromPublic :: E2E -> IO Observed
installFromPublic proxy = do
    for_ graphPackages $ \pkg -> verdaccioVersions proxy (psName pkg) `shouldReturn` []
    npmPublicReachable proxy `shouldReturn` True
    observed <- observeInstall proxy
    for_ graphPackages $ \pkg ->
        verdaccioAwaitVersions proxy (psName pkg) [psVersion pkg] `shouldReturn` [psVersion pkg]
    pure observed

-- The reads are the stub's own record of the install, awaited until the mirror's part is complete.
installFromMirror :: E2E -> IO (Observed, [StubRead])
installFromMirror proxy = do
    npmPublicReachable proxy `shouldReturn` False
    seen <- length <$> stubReadsNow plane
    observed <- observeInstall proxy
    answered <- awaitStubReads plane seen ((neededOfMirror `Set.isSubsetOf`) . servedByMirror)
    pure (observed, answered)
  where
    plane = e2ePlane proxy

{- Install the root by exact version in a new project, which holds no cache entry, lockfile, or
@node_modules@ that could supply it. The tree is recorded first, before any later command runs in it. -}
observeInstall :: E2E -> IO Observed
observeInstall proxy = withNpmProject proxy $ \project -> do
    localInstallSources project `shouldReturn` []
    void $ npmInstallIn project rootRequest >>= shouldSucceedThroughProxy proxy
    tree <- installedTree proxy project
    Observed
        <$> resolvedGraph project
        <*> loadedExport project (psName graphRootPkg)
        <*> linkedExecutable project graphExecutable
        <*> pure tree

-- The request names an exact version, because the `latest` of a packument that only the mirror
-- supplied is the proxy's own choice.
rootRequest :: Text
rootRequest = psName graphRootPkg <> "@" <> psVersion graphRootPkg

-- Every edge npm must resolve: the consumer's one request, then the root's dependency and peer.
expectedGraph :: Set Resolved
expectedGraph =
    Set.fromList
        [ consumerName `resolves` graphRootPkg
        , psName graphRootPkg `resolves` graphDepPkg
        , psName graphDepPkg `resolves` graphLeafPkg
        , psName graphRootPkg `resolves` graphPeerPkg
        ]
  where
    dependent `resolves` pkg = Resolved dependent (psName pkg) (psVersion pkg)

-- The two reads an install needs of each package, as the paths the private store serves them at.
neededOfMirror :: Set Text
neededOfMirror = Set.fromList (concatMap readsOf graphPackages)
  where
    readsOf :: PkgSpec -> [Text]
    readsOf pkg = ["/" <> psName pkg, npmArtifactPath (psName pkg) (psVersion pkg)]

-- The paths the mirror's route answered a read of with a success.
servedByMirror :: [StubRead] -> Set Text
servedByMirror answered =
    Set.fromList [srPath r | r <- answered, srRoute r == Mirror, srMethod r == "GET", srStatus r == 200]

-- A proxy whose private upstream answers every read with a 404, so it has no mirror to read back.
emptyPrivateStore :: E2EConfig
emptyPrivateStore =
    defaultE2EConfig{ecExtraEnv = [("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__VERDACCIO__URL", stubUrl EmptyPrivate)]}
