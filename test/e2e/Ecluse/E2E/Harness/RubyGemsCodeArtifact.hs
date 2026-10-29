-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Physical RubyGems repositories reuse the production CodeArtifact control transport in isolated fixtures.
module Ecluse.E2E.Harness.RubyGemsCodeArtifact (
    RubySweepPlane (..),
    RubySweepStore (..),
    withRubySweepPlane,
    selectSweepSource,
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
    storeRequest,
) where

import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types (status401, statusCode)
import Network.Wai qualified as Wai

import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Server.Path (encodeComponent)
import Ecluse.E2E.Harness.Advisories (createAdvisoryBucket)
import Ecluse.E2E.Harness.CodeArtifact
import Ecluse.E2E.Harness.CodeArtifact.Roles (PublicationAuthority (..), codeArtifactProxyRoleEnv, codeArtifactSweepRoleEnv)
import Ecluse.E2E.Harness.Docker (withStubFault)
import Ecluse.E2E.Harness.RubyGemsCodeArtifact.Fixtures (buildSweepAdvisories, firstPartyName)
import Ecluse.E2E.Harness.NativeEdge (ObservedNativeProxy, observedProxy, withObservedNativeProxy)
import Ecluse.E2E.Harness.RubyGemsRegistry
import Ecluse.E2E.Harness.Stub (StubFault (RubyGemsCodeArtifactBackend))
import Ecluse.E2E.Harness.Types
import Ecluse.Test.Contract.Sweep.Types (PackageFixture (..))
import Ecluse.Test.Registry.RubyGems.Fixtures

-- | Remote state, native artifacts and delete observations survive product-role restarts.
data RubySweepPlane = RubySweepPlane
    { rspGlobal :: GlobalDataPlane
    , rspControl :: CodeArtifactFixture
    , rspRegistry :: RubyRegistry
    , rspReleases :: [RubyRelease]
    , rspDredgerDeletes :: TVar [(Text, Text)]
    }

-- | The client endpoint and directly observed physical repository remain paired.
data RubySweepStore = RubySweepStore
    { rssPlane :: RubySweepPlane
    , rssObserved :: ObservedNativeProxy
    , rssRepository :: Text
    , rssReadUrl :: Text
    , rssPublishUrl :: Text
    , rssDirect :: Bool
    }

-- | The fixture serves native AWS data/control paths without a cloud account or IAM claim.
withRubySweepPlane :: GlobalDataPlane -> (RubySweepPlane -> IO a) -> IO a
withRubySweepPlane global action = do
    releases <- loadRubyReleases
    buildSweepAdvisories (gdpWorkDir global)
    createAdvisoryBucket global
    withRubyRegistry $ \registry -> do
        credited <- newTVarIO []
        let repositories = [boundary registry "mirror" [], boundary registry "cache" ["mirror"]]
        withCodeArtifactFixtureFor RubyGems rubyRegistryToken repositories $ \control ->
            withStubFault global (RubyGemsCodeArtifactBackend (cafPort control) (rubyRegistryPort registry)) $
                action (RubySweepPlane global control registry releases credited)

-- | Independent mutable lifetimes expose only the coordinates required by their own scenarios.
selectSweepSource :: RubySweepPlane -> [PackageFixture] -> IO ()
selectSweepSource plane packages = do
    let registry = rspRegistry plane
        selected release = any (\package -> releaseName release == packageName package && releaseVersion release `elem` packageVersions package) packages
    previous <- rubyRegistryFiles registry "rubygems-upstream"
    for_ previous $ \release -> removeRubyRegistryVersion registry "rubygems-upstream" (releaseName release) (releaseVersion release)
    seedRubyRegistry registry "rubygems-upstream" (filter selected (rspReleases plane))

boundary :: RubyRegistry -> Text -> [Text] -> RepositoryBoundary
boundary registry repository upstreams =
    RepositoryBoundary
        { repositoryName = repository
        , repositoryInventory = do
            files <- rubyRegistryFiles registry (nativeHost repository)
            pure (Map.map (sort . ordNub) (Map.fromListWith (<>) [(releaseName release, [releaseVersion release]) | release <- files]))
        , repositoryDelete = traverse_ . removeRubyRegistryVersion registry (nativeHost repository)
        , repositoryApplication = nativeApplication registry repository
        , repositoryUpstreams = upstreams
        }

nativeHost :: Text -> Text
nativeHost repository = if repository == "mirror" then "rubygems-mirror" else "rubygems-private"

nativeApplication :: RubyRegistry -> Text -> Wai.Application
nativeApplication registry repository request respond
    | not (authorised request) = respond (Wai.responseLBS status401 [("WWW-Authenticate", "Basic realm=\"CodeArtifact Ruby fixture\"")] "declared native service credential required")
    | otherwise = do
        selected <- if repository == "cache" then cacheReader registry request else pure registry
        let headers = if Wai.requestMethod request == "POST" then ("Authorization", encodeUtf8 rubyRegistryToken) : filter ((/= "Authorization") . fst) (Wai.requestHeaders request) else Wai.requestHeaders request
        rubyRegistryApplication selected request{Wai.requestHeaderHost = Just (encodeUtf8 (nativeHost repository)), Wai.requestHeaders = headers} respond
  where
    authorised incoming = lookup "Authorization" (Wai.requestHeaders incoming) == Just (if Wai.requestMethod incoming == "POST" then "Bearer " <> encodeUtf8 rubyRegistryToken else basicCredential)

basicCredential :: ByteString
basicCredential = "Basic " <> convertToBase Base64 (encodeUtf8 ("aws:" <> rubyRegistryToken) :: ByteString)

cacheReader :: RubyRegistry -> Wai.Request -> IO RubyRegistry
cacheReader registry request = do
    remote <- rubyRegistryFiles registry "rubygems-mirror"
    when (Wai.requestMethod request == "GET") $ case Wai.pathInfo request of
        ["gems", filename] -> for_ (find ((== filename) . releaseFilename) remote) $ \selected ->
            seedRubyRegistry registry "rubygems-private" (filter (\release -> releaseName release == releaseName selected && releaseVersion release == releaseVersion selected) remote)
        _ -> pass
    if Wai.requestMethod request `elem` ["GET", "HEAD"]
        then do
            files <- readTVarIO (rubyRegistryInventory registry)
            let upstream = Map.fromList [(("rubygems-private", releaseFilename release), release) | release <- remote]
            combined <- newTVarIO (Map.union files upstream)
            pure registry{rubyRegistryInventory = combined}
        else pure registry

-- | Every ordinary role uses the actual product listener behind the observed native TLS edge.
withSweepProxy :: RubySweepPlane -> [(Text, Text)] -> Maybe Text -> [String] -> (RubySweepStore -> IO a) -> IO a
withSweepProxy plane extra queue args action = withObservedNativeProxy config (rspGlobal plane) $ \observed -> do
    let base = e2eBaseUrl (observedProxy observed) <> "/rubygems/"
    action (RubySweepStore plane observed "mirror" base base False)
  where
    config = defaultE2EConfig{ecExtraEnv = proxyRoleEnv <> extra, ecQueueUrl = queue, ecArgs = args}

-- | Direct cache observations cannot borrow mirror or public visibility.
cacheStore :: RubySweepStore -> RubySweepStore
cacheStore store = directStore store{rssRepository = "cache"}

-- | Native direct operations retain the declared CodeArtifact service presentation.
directStore :: RubySweepStore -> RubySweepStore
directStore store = store{rssReadUrl = base, rssPublishUrl = base, rssDirect = True}
  where
    base = "http://127.0.0.1:" <> show (cafPort (rspControl (rssPlane store))) <> "/ruby/" <> rssRepository store <> "/"

-- | The native Ruby endpoint path must agree with the production CodeArtifact format mapping.
repositoryUrl :: Text -> Text
repositoryUrl = codeArtifactRepositoryUrl RubyGems

-- | Inventory comes from production CodeArtifact response readers over actual physical files.
storeInventory :: RubySweepStore -> IO (Map Text [Text])
storeInventory store = codeArtifactInventory (rspControl (rssPlane store)) (rssRepository store)

-- | Whole native versions are independent of the product's retained mirror records.
storeVersions :: RubySweepStore -> Text -> IO [Text]
storeVersions store = codeArtifactVersions (rspControl (rssPlane store)) (rssRepository store)

-- | Native package-prefix inventory uses the production backend walker.
storeNamesUnder :: RubySweepStore -> Text -> IO [Text]
storeNamesUnder store = codeArtifactNamesUnder (rspControl (rssPlane store)) (rssRepository store)

-- | Fetch exact physical repository bytes, bypassing its configured upstream chain.
storeArtifactBytes :: RubySweepStore -> Text -> Text -> IO (Int, LBS.ByteString)
storeArtifactBytes store name version = do
    release <- rubyRelease (rspReleases (rssPlane store)) name version "ruby"
    let registry = rspRegistry (rssPlane store)
        url = "http://127.0.0.1:" <> show (rubyRegistryPort registry) <> "/gems/" <> encodeComponent (releaseFilename release)
    initial <- Http.parseRequest (toString url)
    response <- Http.httpLbs initial{Http.requestHeaders = [("Host", encodeUtf8 (nativeHost (rssRepository store)))]} (e2eManager (observedProxy (rssObserved store)))
    pure (statusCode (Http.responseStatus response), Http.responseBody response)

-- | Native client requests preserve method, actual response bytes and target-specific credentials.
storeRequest :: RubySweepStore -> Text -> ByteString -> Maybe ByteString -> IO (Int, LBS.ByteString)
storeRequest store path method body = do
    initial <- Http.parseRequest (toString ((if method == "POST" then rssPublishUrl store else rssReadUrl store) <> path))
    let credential = if method == "POST" then (if rssDirect store then "Bearer " else "") <> encodeUtf8 rubyRegistryToken else basicCredential
        request = initial{Http.method = method, Http.requestHeaders = [("Authorization", credential), ("Content-Type", "application/octet-stream")], Http.requestBody = maybe (Http.RequestBodyBS "") Http.RequestBodyBS body, Http.redirectCount = 0}
    response <- Http.httpLbs request (e2eManager (observedProxy (rssObserved store)))
    pure (statusCode (Http.responseStatus response), Http.responseBody response)

-- | Service roles retain separate mirror/cache coordinates, credentials and TLS roots.
sweepRoleEnv :: [(Text, Text)]
sweepRoleEnv = codeArtifactSweepRoleEnv RubyGems

-- | Source rows persist remotely across independently restarted product roles.
proxyRoleEnv :: [(Text, Text)]
proxyRoleEnv = codeArtifactProxyRoleEnv RubyGems (PublicationAuthority "" [("TOKEN", "Bearer " <> rubyRegistryToken)] firstPartyName rubyRegistryToken)

-- | Withhold only the named native archive while leaving its public metadata available.
withSourceWithheld :: RubySweepPlane -> Text -> Text -> IO a -> IO a
withSourceWithheld plane name version action = do
    release <- rubyRelease (rspReleases plane) name version "ruby"
    let registry = rspRegistry plane
    faults <- readTVarIO (rubyRegistryFaults registry)
    withFixtureValue (rubyRegistryFaults registry) (Map.insert "rubygems-upstream" (RubyRegistryMissingArchive (releaseFilename release)) faults) action

-- | A per-version backend refusal preserves cache bytes until a later authorised sweep.
withDeleteRefusal :: RubySweepPlane -> IO a -> IO a
withDeleteRefusal plane = withCodeArtifactDeleteRefusal (rspControl plane) "cache"

-- | Consent refusal must leave both physical repositories unchanged.
withConsentWithheld :: RubySweepPlane -> IO a -> IO a
withConsentWithheld plane = withCodeArtifactConsentRemoved (rspControl plane) "mirror"

-- | A refillable target must fail adoption checks before native version deletion.
withUnsafeMirror :: RubySweepPlane -> IO a -> IO a
withUnsafeMirror plane = withCodeArtifactUpstreams (rspControl plane) "mirror" ["cache"]
