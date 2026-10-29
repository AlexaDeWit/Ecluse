-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native stores, isolated AWS routing and running product roles for PyPI sweep evidence.
module Ecluse.E2E.Harness.PyPIDredger (
    PyPISweepPlane (..),
    PyPISweepStore (..),
    withPyPISweepPlane,
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
) where

import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types (status401, statusCode)
import Network.Wai (Application, pathInfo, requestHeaders, requestMethod, responseLBS)
import System.Directory (renameFile)
import System.FilePath ((</>))
import UnliftIO (bracket_)

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.E2E.Fixtures.PyPI (pypiWheelFor)
import Ecluse.E2E.Fixtures.PyPI.Dredger (buildDredgerDistributions, firstPartyName)
import Ecluse.E2E.Fixtures.PyPI.Mirror (Distribution (..), builtByCurrentTool)
import Ecluse.E2E.Harness.Advisories (createAdvisoryBucket)
import Ecluse.E2E.Harness.CodeArtifact
import Ecluse.E2E.Harness.CodeArtifact.Roles (PublicationAuthority (PublicationAuthority), codeArtifactProxyRoleEnv, codeArtifactSweepRoleEnv)
import Ecluse.E2E.Harness.Docker (withE2EWith, withStubFault)
import Ecluse.E2E.Harness.PyPIMirror
import Ecluse.E2E.Harness.Stub (StubFault (PyPICodeArtifactBackend))
import Ecluse.E2E.Harness.Types

-- | Product restarts retain only the fixture's native repositories and remote S3 state.
data PyPISweepPlane = PyPISweepPlane
    { pspGlobal :: GlobalDataPlane
    , pspControl :: CodeArtifactFixture
    , pspMirror :: NativeTarget
    , pspCache :: NativeTarget
    , pspDistributions :: [Distribution]
    , pspDredgerDeletes :: TVar [(Text, Text)]
    -- ^ The CodeArtifact delete requests credited to Dredger runs, in request order.
    }

-- | Direct inventory observations stay attached to the repository behind a client endpoint.
data PyPISweepStore = PyPISweepStore
    { pssPlane :: PyPISweepPlane
    , pssProxy :: E2E
    , pssRepository :: Text
    , pssReadUrl :: Text
    , pssPublishUrl :: Text
    , pssUploads :: [Distribution]
    -- ^ The files a publish sends here. The proxy's relay takes only the names a current build tool writes.
    }

-- | Both AWS hosts resolve to the test TLS stub, which forwards only to this local fixture.
withPyPISweepPlane :: GlobalDataPlane -> (PyPISweepPlane -> IO a) -> IO a
withPyPISweepPlane global action = do
    distributions <- buildDredgerDistributions (gdpWorkDir global)
    createAdvisoryBucket global
    withConfiguredNativeTarget (targetConfig "mirror") distributions $ \mirror ->
        withConfiguredNativeTarget (targetConfig "cache") distributions $ \cache -> do
            let repositories =
                    [ boundary distributions "mirror" mirror [] (nativeApplication (targetConfig "mirror") distributions mirror)
                    , boundary distributions "cache" cache ["mirror"] (cacheApplication distributions mirror cache)
                    ]
            credited <- newTVarIO []
            withCodeArtifactFixture repositories $ \control ->
                withStubFault global (PyPICodeArtifactBackend (cafPort control)) $
                    action (PyPISweepPlane global control mirror cache distributions credited)

targetConfig :: Text -> NativeTargetConfig
targetConfig repository = NativeTargetConfig (repositoryUrl repository) "Basic YXdzOmZpeHR1cmUtdG9rZW4=" PreserveUploadFields

boundary :: [Distribution] -> Text -> NativeTarget -> [Text] -> Application -> RepositoryBoundary
boundary distributions name target upstreams application =
    RepositoryBoundary
        { repositoryName = name
        , repositoryInventory = physicalInventory distributions target
        , repositoryDelete = \project versions ->
            atomically $
                modifyTVar' (ntFiles target) (\files -> foldr Map.delete files [distributionFilename d | d <- distributions, distributionProject d == project, distributionVersion d `elem` versions])
        , repositoryApplication = authorised application
        , repositoryUpstreams = upstreams
        }

authorised :: Application -> Application
authorised application request respond
    | lookup "Authorization" (requestHeaders request) == Just "Basic YXdzOmZpeHR1cmUtdG9rZW4=" = application request respond
    | otherwise = respond (responseLBS status401 [("WWW-Authenticate", "Basic realm=fixture")] "fixture credential required")

cacheApplication :: [Distribution] -> NativeTarget -> NativeTarget -> Application
cacheApplication distributions mirror cache request respond = do
    let segments = filter (not . T.null) (pathInfo request)
    remote <- targetFiles mirror
    case segments of
        ["simple", project, filename]
            | requestMethod request == "GET"
            , Map.member filename remote ->
                for_ (find ((== filename) . distributionFilename) distributions) $ \selected ->
                    atomically (modifyTVar' (ntFiles cache) (Map.union (Map.restrictKeys remote (fromList [distributionFilename d | d <- distributions, distributionProject d == project, distributionVersion d == distributionVersion selected]))))
        _ -> pass
    cachedFiles <- targetFiles cache
    case segments of
        ["simple", _] | requestMethod request `elem` ["GET", "HEAD"] -> do
            combined <- newTVarIO (Map.union cachedFiles remote)
            nativeApplication (targetConfig "cache") distributions cache{ntFiles = combined} request respond
        _ -> nativeApplication (targetConfig "cache") distributions cache request respond

physicalInventory :: [Distribution] -> NativeTarget -> IO (Map Text [Text])
physicalInventory distributions target = do
    files <- targetFiles target
    pure (Map.map (sort . ordNub) (Map.fromListWith (<>) [(distributionProject d, [distributionVersion d]) | d <- distributions, Map.member (distributionFilename d) files]))

-- | Read the native mirror through the production proxy and its declared private cache.
withSweepProxy :: PyPISweepPlane -> [(Text, Text)] -> Maybe Text -> [String] -> (PyPISweepStore -> IO a) -> IO a
withSweepProxy plane extra queue args action =
    withE2EWith config (\proxy -> action (PyPISweepStore plane proxy "mirror" (e2eBaseUrl proxy <> "/pypi/") (e2eBaseUrl proxy <> "/pypi/legacy/") relayed)) (pspGlobal plane)
  where
    config = defaultE2EConfig{ecExtraEnv = proxyRoleEnv <> extra, ecQueueUrl = queue, ecArgs = args}
    relayed = filter builtByCurrentTool (pspDistributions plane)

-- | A direct client reaches the cache's physical native registry, bypassing its upstreams.
cacheStore :: PyPISweepStore -> PyPISweepStore
cacheStore store = directStore store{pssRepository = "cache"}

-- | Native publication and byte observations bypass the product under test.
directStore :: PyPISweepStore -> PyPISweepStore
directStore store = store{pssReadUrl = localUrl target, pssPublishUrl = localUrl target <> "legacy/", pssUploads = pspDistributions (pssPlane store)}
  where
    target = if pssRepository store == "mirror" then pspMirror (pssPlane store) else pspCache (pssPlane store)

localUrl :: NativeTarget -> Text
localUrl target = "http://127.0.0.1:" <> show (ntPort target) <> "/"

-- | The endpoint path preserves the CodeArtifact repository identity at boot.
repositoryUrl :: Text -> Text
repositoryUrl = codeArtifactRepositoryUrl PyPI

-- | Enumerate each actual repository with the production CodeArtifact adapter.
storeInventory :: PyPISweepStore -> IO (Map Text [Text])
storeInventory store = codeArtifactInventory (pspControl (pssPlane store)) (pssRepository store)

-- | Read native versions without consulting managed mirror records.
storeVersions :: PyPISweepStore -> Text -> IO [Text]
storeVersions store = codeArtifactVersions (pspControl (pssPlane store)) (pssRepository store)

-- | Use the adapter's bucket walk, including the empty prefix.
storeNamesUnder :: PyPISweepStore -> Text -> IO [Text]
storeNamesUnder store = codeArtifactNamesUnder (pspControl (pssPlane store)) (pssRepository store)

-- | Read stored bytes directly so retained metadata cannot stand in for native files.
storeArtifactBytes :: PyPISweepStore -> Text -> Text -> IO (Int, LBS.ByteString)
storeArtifactBytes store name version = do
    base <- Http.parseRequest (toString (pssReadUrl (directStore store) <> "simple/" <> name <> "/" <> pypiWheelFor name version))
    response <- Http.httpLbs (Http.applyBasicAuth "aws" "fixture-token" base) (e2eManager (pssProxy store))
    pure (statusCode (Http.responseStatus response), Http.responseBody response)

-- | Dredger uses the two native stores without an inactive writer or state requirement.
sweepRoleEnv :: [(Text, Text)]
sweepRoleEnv = codeArtifactSweepRoleEnv PyPI

-- | Proxy and worker reads retain remote mirror records across deletion and restart.
proxyRoleEnv :: [(Text, Text)]
proxyRoleEnv = codeArtifactProxyRoleEnv PyPI (PublicationAuthority "legacy/" [("USERNAME", "aws"), ("TOKEN", codeArtifactToken)] firstPartyName codeArtifactToken)

-- | Remove only the requested public version's physical files for the action.
withSourceWithheld :: PyPISweepPlane -> Text -> Text -> IO a -> IO a
withSourceWithheld plane name version action = foldr hide action paths
  where
    paths = [gdpWorkDir (pspGlobal plane) </> "pypi" </> "simple" </> toString name </> toString (distributionFilename d) | d <- pspDistributions plane, distributionProject d == name, distributionVersion d == version]
    hide path = bracket_ (renameFile path (path <> ".withheld")) (renameFile (path <> ".withheld") path)

-- | The cache reports a native per-version refusal while its files remain readable.
withDeleteRefusal :: PyPISweepPlane -> IO a -> IO a
withDeleteRefusal plane = withCodeArtifactDeleteRefusal (pspControl plane) "cache"

-- | Removing the mirror's repository marker must stop the product before any deletion.
withConsentWithheld :: PyPISweepPlane -> IO a -> IO a
withConsentWithheld plane = withCodeArtifactConsentRemoved (pspControl plane) "mirror"
