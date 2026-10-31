-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native CodeArtifact repositories and product roles for Cargo maintenance contracts.
module Ecluse.E2E.Harness.CargoDredger (
    CargoSweepPlane (..),
    CargoSweepStore (..),
    withCargoSweepPlane,
    withSweepProxy,
    cacheStore,
    directStore,
    storeInventory,
    storeVersions,
    storeNamesUnder,
    storeArtifactBytes,
    repositoryUrl,
    sweepRoleEnv,
    proxyRoleEnv,
    withSourceWithheld,
    withDeleteRefusal,
    withConsentWithheld,
    withUnsafeMirror,
    setStoredYank,
) where

import Data.Aeson (Value (Bool, Object))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types (status200, status401, statusCode)
import Network.Wai (Application, Request, pathInfo, requestHeaderHost, requestHeaders, requestMethod, responseLBS)
import System.Directory (renameFile)
import System.FilePath ((</>))
import UnliftIO (bracket_)

import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.E2E.Fixtures.Cargo.Dredger (buildDredgerReleases, firstPartyName)
import Ecluse.E2E.Harness.Advisories (createAdvisoryBucket)
import Ecluse.E2E.Harness.CargoRegistry
import Ecluse.E2E.Harness.CodeArtifact
import Ecluse.E2E.Harness.CodeArtifact.Roles (PublicationAuthority (..), codeArtifactProxyRoleEnv, codeArtifactSweepRoleEnv)
import Ecluse.E2E.Harness.Docker (withStubFault)
import Ecluse.E2E.Harness.NativeEdge
import Ecluse.E2E.Harness.Stub (StubFault (CargoCodeArtifactBackend))
import Ecluse.E2E.Harness.Types
import Ecluse.Test.Registry.Cargo.Fixtures

-- | Native files, control-plane evidence and remote mirror records survive product restarts.
data CargoSweepPlane = CargoSweepPlane
    { cspGlobal :: GlobalDataPlane
    , cspControl :: CodeArtifactFixture
    , cspRegistry :: CargoRegistry
    , cspReleases :: [CargoRelease]
    , cspDredgerDeletes :: TVar [(Text, Text)]
    }

-- | Client endpoints remain paired with the physical repository observed by the contract.
data CargoSweepStore = CargoSweepStore
    { cssPlane :: CargoSweepPlane
    , cssObserved :: ObservedNativeProxy
    , cssRepository :: Text
    , cssUrl :: Text
    -- ^ The sparse root that serves reads and receives publication.
    }

-- | Reuse the native registry and shared AWS fixture, with no cloud account or service secrets.
withCargoSweepPlane :: GlobalDataPlane -> (CargoSweepPlane -> IO a) -> IO a
withCargoSweepPlane global action = do
    releases <- buildDredgerReleases (gdpWorkDir global)
    createAdvisoryBucket global
    withCargoRegistry $ \registry -> do
        setCargoRegistryFault registry (CargoRegistryRequireToken True)
        credited <- newTVarIO []
        let repositories = [boundary registry "mirror" [], boundary registry "cache" ["mirror"]]
        withCodeArtifactFixtureFor Cargo cargoRegistryToken repositories $ \control ->
            withStubFault global (CargoCodeArtifactBackend (cafPort control)) $
                action (CargoSweepPlane global control registry releases credited)

boundary :: CargoRegistry -> Text -> [Text] -> RepositoryBoundary
boundary registry repository upstreams =
    RepositoryBoundary
        { repositoryName = repository
        , repositoryInventory = do
            files <- cargoRegistryFiles registry
            pure (Map.map sort (Map.fromListWith (<>) [(name, [version]) | ((host, name, version), _) <- Map.toList files, host == nativeHost repository]))
        , repositoryDelete = \name versions ->
            atomically $
                modifyTVar' (registryFiles registry) (\files -> foldr Map.delete files [(nativeHost repository, name, version) | version <- versions])
        , repositoryApplication = nativeApplication registry repository
        , repositoryUpstreams = upstreams
        }

nativeHost :: Text -> Text
nativeHost repository = if repository == "mirror" then "cargo-mirror" else "cargo-private"

nativeApplication :: CargoRegistry -> Text -> Application
nativeApplication registry repository request respond
    | lookup "Authorization" (requestHeaders request) /= Just (encodeUtf8 cargoRegistryToken) =
        respond (responseLBS status401 [("WWW-Authenticate", "Cargo login_url=\"https://cargo-private/login\"")] "fixture credential required")
    | pathInfo request == ["config.json"] =
        respond (responseLBS status200 [("Content-Type", "application/json")] (LBS.fromStrict (cargoViewDiscovery (repositoryUrl repository) (Just (T.dropWhileEnd (== '/') (repositoryUrl repository))) True)))
    | otherwise = do
        selected <- if repository == "cache" then cacheReader registry request else pure registry
        cargoRegistryApplication selected request{requestHeaderHost = Just (encodeUtf8 (nativeHost repository))} respond

cacheReader :: CargoRegistry -> Request -> IO CargoRegistry
cacheReader registry request = do
    when (requestMethod request == "GET") $ case pathInfo request of
        ["crates", name, version, _] -> atomically $ do
            files <- readTVar (registryFiles registry)
            for_ (Map.lookup ("cargo-mirror", name, version) files) $ \stored ->
                writeTVar (registryFiles registry) (Map.insertWith (\_ current -> current) ("cargo-private", name, version) stored files)
        _ -> pass
    if requestMethod request `elem` ["GET", "HEAD"]
        then do
            files <- cargoRegistryFiles registry
            let upstream = Map.fromList [(("cargo-private", name, version), stored) | (("cargo-mirror", name, version), stored) <- Map.toList files]
            combined <- newTVarIO (Map.union files upstream)
            pure registry{registryFiles = combined}
        else pure registry

-- | Reads and publication share the product's one Cargo registry.
withSweepProxy :: CargoSweepPlane -> [(Text, Text)] -> Maybe Text -> [String] -> (CargoSweepStore -> IO a) -> IO a
withSweepProxy plane extra queue args action =
    withObservedNativeProxy config (cspGlobal plane) $ \observed ->
        action (CargoSweepStore plane observed "mirror" (e2eBaseUrl (observedProxy observed) <> "/cargo/"))
  where
    config = defaultE2EConfig{ecExtraEnv = proxyRoleEnv <> extra, ecQueueUrl = queue, ecArgs = args}

-- | Direct cache observations bypass upstream aggregation and managed mirror records.
cacheStore :: CargoSweepStore -> CargoSweepStore
cacheStore store = directStore store{cssRepository = "cache"}

-- | Direct native publication uses the fixture listener at the same repository coordinate.
directStore :: CargoSweepStore -> CargoSweepStore
directStore store = store{cssUrl = base}
  where
    base = "http://127.0.0.1:" <> show (cafPort (cspControl (cssPlane store))) <> "/cargo/" <> cssRepository store <> "/"

-- | The URL retains the repository and Cargo format required by production configuration parsing.
repositoryUrl :: Text -> Text
repositoryUrl = codeArtifactRepositoryUrl Cargo

-- | Observe repository-local native inventory through production CodeArtifact response parsing.
storeInventory :: CargoSweepStore -> IO (Map Text [Text])
storeInventory store = codeArtifactInventory (cspControl (cssPlane store)) (cssRepository store)

-- | Native versions come from actual physical files, independently of retained mirror state.
storeVersions :: CargoSweepStore -> Text -> IO [Text]
storeVersions store = codeArtifactVersions (cspControl (cssPlane store)) (cssRepository store)

-- | The shared observer runs CodeArtifact's actual package-prefix listing.
storeNamesUnder :: CargoSweepStore -> Text -> IO [Text]
storeNamesUnder store = codeArtifactNamesUnder (cspControl (cssPlane store)) (cssRepository store)

-- | Read exact physical bytes from the selected repository without public fallback.
storeArtifactBytes :: CargoSweepStore -> Text -> Text -> IO (Int, LBS.ByteString)
storeArtifactBytes store name version = do
    let registry = cspRegistry (cssPlane store)
        path = "http://127.0.0.1:" <> show (registryPort registry) <> "/" <> cargoArchivePathFor name version
    base <- Http.parseRequest (toString path)
    let request = base{Http.requestHeaders = [("Host", encodeUtf8 (nativeHost (cssRepository store))), ("Authorization", encodeUtf8 cargoRegistryToken)]}
    response <- Http.httpLbs request (e2eManager (observedProxy (cssObserved store)))
    pure (statusCode (Http.responseStatus response), Http.responseBody response)

-- | Each Dredger process receives separate mirror and cache authority with the native control plane.
sweepRoleEnv :: [(Text, Text)]
sweepRoleEnv = codeArtifactSweepRoleEnv Cargo

-- | Serving and worker roles retain source rows remotely across deletion and process restarts.
proxyRoleEnv :: [(Text, Text)]
proxyRoleEnv = codeArtifactProxyRoleEnv Cargo (PublicationAuthority "" [("TOKEN", cargoRegistryToken)] firstPartyName cargoRegistryToken)

-- | Hide only the requested public archive while preserving its sparse record.
withSourceWithheld :: CargoSweepPlane -> Text -> Text -> IO a -> IO a
withSourceWithheld plane name version action = do
    release <-
        maybe (fail "the requested Cargo source archive is absent from the owned corpus") pure $
            find (\candidate -> crateName (releaseCrate candidate) == name && crateVersion (releaseCrate candidate) == version) (cspReleases plane)
    let path = gdpWorkDir (cspGlobal plane) </> "cargo" </> toString (cargoArchivePath (releaseCrate release))
    bracket_ (renameFile path (path <> ".withheld")) (renameFile (path <> ".withheld") path) action

-- | The cache reports a per-version refusal while its physical archive remains readable.
withDeleteRefusal :: CargoSweepPlane -> IO a -> IO a
withDeleteRefusal plane = withCodeArtifactDeleteRefusal (cspControl plane) "cache"

-- | Removing the mirror consent marker must prevent a destructive request.
withConsentWithheld :: CargoSweepPlane -> IO a -> IO a
withConsentWithheld plane = withCodeArtifactConsentRemoved (cspControl plane) "mirror"

-- | A refill relationship on the mirror must fail the product's target-role checks.
withUnsafeMirror :: CargoSweepPlane -> IO a -> IO a
withUnsafeMirror plane = withCodeArtifactUpstreams (cspControl plane) "mirror" ["cache"]

-- | Native yank changes metadata while retaining the physical archive for subsequent deletion proof.
setStoredYank :: CargoSweepStore -> Text -> Text -> IO ()
setStoredYank store name version =
    atomically $
        modifyTVar' (registryFiles (cspRegistry (cssPlane store))) $
            Map.adjust yank (nativeHost (cssRepository store), name, version)
  where
    yank stored = stored{storedRow = case storedRow stored of Object fields -> Object (KeyMap.insert "yanked" (Bool True) fields); other -> other}
