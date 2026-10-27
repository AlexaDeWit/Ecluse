-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Deterministic CodeArtifact HTTP operations over the same native files the product serves.
module Ecluse.E2E.Harness.CodeArtifact (
    RepositoryBoundary (..),
    CodeArtifactFixture (..),
    withCodeArtifactFixture,
    withCodeArtifactFixtureFor,
    codeArtifactDomain,
    codeArtifactOwner,
    codeArtifactToken,
    repositoryArn,
    codeArtifactNamesUnder,
    codeArtifactVersions,
    withFixtureValue,
    codeArtifactDeleteRequests,
    creditCodeArtifactDeletes,
    withCodeArtifactDeleteAudit,
) where

import Amazonka qualified as AWS
import Amazonka.Auth (fromKeys)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe, (.:))
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Types (Status, status200, status400, status403, status404)
import Network.Wai (Application, Request, Response, getRequestBodyChunk, pathInfo, queryString, requestHeaders, requestMethod, responseLBS)
import Test.Hspec (shouldReturn)
import UnliftIO (bracket)
import UnliftIO.STM (swapTVar)

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, PyPI, RubyGems))
import Ecluse.Core.Package (mkPackageName, renderPackageName)
import Ecluse.Core.Registry.Maintenance (StoreObservation (..), StoredVersion (storedVersion), collectPages)
import Ecluse.Core.Registry.Maintenance.NameSpace (mkNameAlphabet)
import Ecluse.Core.Registry.PyPI.Project (pypiNameLeadChars)
import Ecluse.Core.Security.Limits (BodyLimit (PublishRequestBodyLimit), boundedRead)
import Ecluse.Core.Version (renderVersion)
import Ecluse.E2E.Harness.Container (withBridgeApplication)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (CodeArtifactStore (..), codeArtifactFormat, formatToken)
import Ecluse.Runtime.Maintenance.CodeArtifact.Internal (ControlPlane (cpRead), controlPlaneFor, observationFor)
import Ecluse.Test.Contract.Sweep.Types (SweepRun)
import Ecluse.Test.Maintenance (withBucket)
import Ecluse.Test.Support (expectRightText)

-- | Inventory and deletion address physical registry contents, never the product's managed state.
data RepositoryBoundary = RepositoryBoundary
    { repositoryName :: Text
    , repositoryInventory :: IO (Map Text [Text])
    , repositoryDelete :: Text -> [Text] -> IO ()
    , repositoryApplication :: Application
    , repositoryUpstreams :: [Text]
    }

-- | Faults and observations survive product restarts within one contract resource lifetime.
data CodeArtifactFixture = CodeArtifactFixture
    { cafEcosystem :: Ecosystem
    , cafCredential :: Text
    , cafPort :: Int
    , cafUpstreams :: TVar (Map Text [Text])
    , cafTags :: TVar (Map Text (Map Text Text))
    , cafRefusedDeletes :: TVar [Text]
    , cafRequests :: TVar [(Text, Text)]
    }

-- | The fixture domain resolves only through the isolated TLS route.
codeArtifactDomain :: Text
codeArtifactDomain = "contract-domain"

-- | A synthetic account identifier used only on the isolated test network.
codeArtifactOwner :: Text
codeArtifactOwner = "123456789012"

-- | The deterministic mint and native registry share this test credential.
codeArtifactToken :: Text
codeArtifactToken = "fixture-token"

-- | The ARN scopes repository tags to their actual target.
repositoryArn :: Text -> Text
repositoryArn name = "arn:aws:codeartifact:us-east-1:" <> codeArtifactOwner <> ":repository/" <> codeArtifactDomain <> "/" <> name

-- | Expose the documented REST paths to the running product's ordinary AWS adapters.
withCodeArtifactFixture :: [RepositoryBoundary] -> (CodeArtifactFixture -> IO a) -> IO a
withCodeArtifactFixture = withCodeArtifactFixtureFor PyPI codeArtifactToken

-- | Reuse the same control plane with a native ecosystem and its minted test token.
withCodeArtifactFixtureFor :: Ecosystem -> Text -> [RepositoryBoundary] -> (CodeArtifactFixture -> IO a) -> IO a
withCodeArtifactFixtureFor ecosystem token repositories action = do
    when (isNothing (codeArtifactFormat ecosystem)) (fail "fixture ecosystem has no native CodeArtifact format")
    tags <- newTVarIO (Map.fromList [(repositoryName repository, Map.singleton "ecluse-dredger-consent" "true") | repository <- repositories])
    upstreams <- newTVarIO (Map.fromList [(repositoryName repository, repositoryUpstreams repository) | repository <- repositories])
    refused <- newTVarIO []
    requests <- newTVarIO []
    let fixture = CodeArtifactFixture ecosystem token 0 upstreams tags refused requests
    withBridgeApplication (codeArtifactApplication repositories fixture) $ \port ->
        action fixture{cafPort = port}

-- | Traverse the production adapter's prefix listing over the HTTP fixture.
codeArtifactNamesUnder :: CodeArtifactFixture -> Text -> Text -> IO [Text]
codeArtifactNamesUnder fixture repository prefix = do
    observer <- fixtureObservation fixture repository
    result <- withBucket prefix (collectPages . obListPackagesIn observer)
    sort . map renderPackageName <$> expectRightText (first show result)

-- | Enumerate native versions through the production CodeArtifact response reader.
codeArtifactVersions :: CodeArtifactFixture -> Text -> Text -> IO [Text]
codeArtifactVersions fixture repository name = do
    observer <- fixtureObservation fixture repository
    result <- obEnumerateVersions observer (mkPackageName (cafEcosystem fixture) Nothing name)
    sort . map (renderVersion . storedVersion) <$> expectRightText (first show result)

fixtureObservation :: CodeArtifactFixture -> Text -> IO StoreObservation
fixtureObservation fixture repository = do
    base <- AWS.newEnv (pure . fromKeys (AWS.AccessKey "test") (AWS.SecretKey "test"))
    let env = AWS.overrideService (AWS.setEndpoint False "127.0.0.1" (cafPort fixture)) base{AWS.region = AWS.Region' "us-east-1"}
    plane <- cpRead <$> controlPlaneFor env
    format <- maybe (fail "fixture ecosystem has no CodeArtifact format") pure (codeArtifactFormat (cafEcosystem fixture))
    let store = CodeArtifactStore codeArtifactDomain codeArtifactOwner "us-east-1" repository format
        alphabet = case cafEcosystem fixture of
            Cargo -> "-0123456789_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
            RubyGems -> ".-0123456789_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
            _ -> pypiNameLeadChars
    pure (observationFor (mkNameAlphabet alphabet) (\_ -> fail "direct inventory must not read package metadata") store plane)

codeArtifactApplication :: [RepositoryBoundary] -> CodeArtifactFixture -> Application
codeArtifactApplication repositories fixture request respond = case pathInfo request of
    format : name : rest | Just format == nativeFormat fixture -> case find ((== name) . repositoryName) repositories of
        Just repository -> repositoryApplication repository request{pathInfo = rest} respond
        Nothing -> respond missingRepository
    _ -> do
        atomically (modifyTVar' (cafRequests fixture) (<> [(decodeUtf8 (requestMethod request), T.intercalate "/" (pathInfo request))]))
        if not (signedFixtureRequest request)
            then respond (fault status403 "AccessDeniedException" "expected the fixture's signed test identity")
            else controlRequest repositories fixture request >>= respond

signedFixtureRequest :: Request -> Bool
signedFixtureRequest request =
    maybe False (("AWS4-HMAC-SHA256 Credential=test/" `T.isPrefixOf`) . decodeUtf8) (lookup "Authorization" (requestHeaders request))

controlRequest :: [RepositoryBoundary] -> CodeArtifactFixture -> Request -> IO Response
controlRequest repositories fixture request
    | requestMethod request == "POST"
    , pathInfo request == ["v1", "authorization-token"] =
        if validDomain request
            then do
                expires <- (+ 3600) <$> getPOSIXTime
                pure (json (object ["authorizationToken" .= cafCredential fixture, "expiration" .= (realToFrac expires :: Double)]))
            else pure invalidCoordinates
    | pathInfo request `elem` [["v1", "tags"], ["v1", "tag"], ["v1", "untag"]] =
        case find (\repository -> parameter "resourceArn" request == Just (repositoryArn (repositoryName repository))) repositories of
            Nothing -> pure invalidCoordinates
            Just repository -> tagRequest fixture (repositoryName repository) request
    | validDomain request
    , Just repository <- find (\candidate -> Just (repositoryName candidate) == parameter "repository" request) repositories =
        repositoryRequest fixture repository request
    | otherwise = pure invalidCoordinates

repositoryRequest :: CodeArtifactFixture -> RepositoryBoundary -> Request -> IO Response
repositoryRequest fixture repository request = case (requestMethod request, pathInfo request) of
    ("GET", ["v1", "repository"]) -> do
        upstreams <- Map.findWithDefault [] (repositoryName repository) <$> readTVarIO (cafUpstreams fixture)
        pure (json (repositoryDescription repository{repositoryUpstreams = upstreams}))
    ("POST", ["v1", "packages"]) | validFormat fixture request -> do
        names <- Map.keys <$> repositoryInventory repository
        let prefix = fromMaybe "" (parameter "package-prefix" request)
        pure (json (object ["packages" .= [object ["format" .= nativeFormat fixture, "package" .= name] | name <- names, prefix `T.isPrefixOf` name]]))
    ("POST", ["v1", "package", "versions"])
        | validFormat fixture request
        , Just name <- parameter "package" request -> do
            versions <- Map.findWithDefault [] name <$> repositoryInventory repository
            pure (json (object ["format" .= nativeFormat fixture, "package" .= name, "versions" .= [object ["version" .= version, "status" .= ("Published" :: Text)] | version <- versions]]))
    ("POST", ["v1", "package", "versions", "delete"])
        | validFormat fixture request
        , Just name <- parameter "package" request ->
            deleteVersions fixture repository name request
    _ -> pure invalidCoordinates

repositoryDescription :: RepositoryBoundary -> Value
repositoryDescription repository =
    object
        [ "repository"
            .= object
                [ "name" .= repositoryName repository
                , "domainName" .= codeArtifactDomain
                , "domainOwner" .= codeArtifactOwner
                , "arn" .= repositoryArn (repositoryName repository)
                , "externalConnections" .= ([] :: [Value])
                , "upstreams" .= [object ["repositoryName" .= name] | name <- repositoryUpstreams repository]
                ]
        ]

deleteVersions :: CodeArtifactFixture -> RepositoryBoundary -> Text -> Request -> IO Response
deleteVersions fixture repository name request =
    requestObject request >>= \case
        Just body
            | Just versions <- parseMaybe (.: "versions") body
            , parseMaybe (.: "expectedStatus") body == Just ("Published" :: Text)
            , not (null versions)
            , length versions <= 100 -> do
                refused <- elem (repositoryName repository) <$> readTVarIO (cafRefusedDeletes fixture)
                if refused
                    then pure (deleteReply "failedVersions" versions (object ["errorCode" .= ("NOT_ALLOWED" :: Text), "errorMessage" .= ("fixture refuses cache deletion" :: Text)]))
                    else do
                        repositoryDelete repository name versions
                        pure (deleteReply "successfulVersions" versions (object ["status" .= ("Deleted" :: Text)]))
        _ -> pure invalidCoordinates

deleteReply :: Aeson.Key -> [Text] -> Value -> Response
deleteReply field versions result = json (object [field .= Object (KeyMap.fromList [(Key.fromText version, result) | version <- versions])])

tagRequest :: CodeArtifactFixture -> Text -> Request -> IO Response
tagRequest fixture name request = case (requestMethod request, pathInfo request) of
    ("POST", ["v1", "tags"]) -> do
        tags <- Map.findWithDefault Map.empty name <$> readTVarIO (cafTags fixture)
        pure (json (object ["tags" .= [object ["key" .= key, "value" .= value] | (key, value) <- Map.toList tags]]))
    ("POST", ["v1", "tag"]) ->
        requestObject request >>= \case
            Just body
                | Just values <- parseMaybe (.: "tags") body
                , Just tags <- traverse tagPair values -> do
                    atomically (modifyTVar' (cafTags fixture) (Map.adjust (Map.union (Map.fromList tags)) name))
                    pure (responseLBS status200 [] "")
            _ -> pure invalidCoordinates
    ("POST", ["v1", "untag"]) ->
        requestObject request >>= \case
            Just body | Just keys <- parseMaybe (.: "tagKeys") body -> do
                atomically (modifyTVar' (cafTags fixture) (Map.adjust (\tags -> foldr Map.delete tags (keys :: [Text])) name))
                pure (responseLBS status200 [] "")
            _ -> pure invalidCoordinates
    _ -> pure invalidCoordinates

tagPair :: Value -> Maybe (Text, Text)
tagPair = parseMaybe (Aeson.withObject "tag" (\value -> (,) <$> value .: "key" <*> value .: "value"))

requestObject :: Request -> IO (Maybe Aeson.Object)
requestObject request =
    boundedRead (PublishRequestBodyLimit 65536) (getRequestBodyChunk request) >>= \case
        Left _ -> pure Nothing
        Right (_, bytes) -> pure (Aeson.decodeStrict' bytes)

parameter :: ByteString -> Request -> Maybe Text
parameter name request = decodeUtf8 <$> join (lookup name (queryString request))

validDomain :: Request -> Bool
validDomain request = parameter "domain" request == Just codeArtifactDomain && parameter "domain-owner" request == Just codeArtifactOwner

validFormat :: CodeArtifactFixture -> Request -> Bool
validFormat fixture request = maybe False (\format -> parameter "format" request == Just format) (nativeFormat fixture) && isNothing (parameter "namespace" request) && isNothing (parameter "next-token" request)

nativeFormat :: CodeArtifactFixture -> Maybe Text
nativeFormat fixture = formatToken <$> codeArtifactFormat (cafEcosystem fixture)

json :: Value -> Response
json = responseLBS status200 [("Content-Type", "application/json")] . Aeson.encode

fault :: Status -> Text -> Text -> Response
fault status kind message = responseLBS status [("Content-Type", "application/json"), ("x-amzn-ErrorType", encodeUtf8 kind)] (Aeson.encode (object ["message" .= message]))

invalidCoordinates :: Response
invalidCoordinates = fault status400 "ValidationException" "unexpected CodeArtifact fixture request"

missingRepository :: Response
missingRepository = fault status404 "ResourceNotFoundException" "unknown fixture repository"

-- | Restore fixture state even when a product role or assertion fails.
withFixtureValue :: TVar a -> a -> IO b -> IO b
withFixtureValue variable value action = bracket (atomically (swapTVar variable value)) (atomically . writeTVar variable) (const action)

-- | Physical version-delete requests exclude native yank and metadata changes.
codeArtifactDeleteRequests :: CodeArtifactFixture -> IO [(Text, Text)]
codeArtifactDeleteRequests fixture = filter ((== "v1/package/versions/delete") . snd) <$> readTVarIO (cafRequests fixture)

-- | Attribute physical deletion requests to the Dredger process that issued them.
creditCodeArtifactDeletes :: CodeArtifactFixture -> TVar [(Text, Text)] -> IO SweepRun -> IO SweepRun
creditCodeArtifactDeletes fixture credited run = do
    prior <- length <$> codeArtifactDeleteRequests fixture
    swept <- run
    made <- drop prior <$> codeArtifactDeleteRequests fixture
    atomically (modifyTVar' credited (<> made))
    pure swept

-- | A worker may issue no physical deletion beyond the concurrent Dredger's credited calls.
withCodeArtifactDeleteAudit :: CodeArtifactFixture -> TVar [(Text, Text)] -> IO a -> IO a
withCodeArtifactDeleteAudit fixture credited action = do
    prior <- codeArtifactDeleteRequests fixture
    count <- length <$> readTVarIO credited
    result <- action
    made <- drop count <$> readTVarIO credited
    codeArtifactDeleteRequests fixture `shouldReturn` prior <> made
    pure result
