-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native sparse registries retain exact uploaded bytes across product restarts.
module Ecluse.E2E.Harness.CargoRegistry (
    CargoRegistry (..),
    CargoStored (..),
    CargoRegistryFault (..),
    withCargoRegistry,
    cargoRegistryApplication,
    cargoRegistryFiles,
    cargoRegistryVersions,
    awaitCargoRegistry,
    setCargoRegistryFault,
    seedCargoRegistry,
    cargoRegistryToken,
) where

import Data.Aeson (Value (Array, Null, Object, String), encode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Types (ResponseHeaders, Status, status200, status400, status401, status404, status409, status413, status503, statusCode)
import Network.Wai (Application, Request, Response, ResponseReceived, getRequestBodyChunk, pathInfo, requestHeaderHost, requestHeaders, requestMethod, responseLBS, responseRaw)

import Ecluse.Core.Security.Limits (BodyLimit (PublishRequestBodyLimit), boundedRead)
import Ecluse.E2E.Harness.Container (withBridgeApplication)
import Ecluse.E2E.Harness.NativeEdge (NativeRequest (..))
import Ecluse.E2E.Harness.Stub (StubRoute (CargoMirror, CargoPrivate, CargoPublic), stubUrl)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Poll (pollUntil)
import Ecluse.Test.Registry.Cargo.Fixtures
import Ecluse.Test.Registry.Cargo.Reference (UploadFrame (..), referenceUpload)

-- | Physical archive bytes and the target's native row are independently observable.
data CargoStored = CargoStored
    { storedArchive :: ByteString
    , storedRow :: Value
    }
    deriving stock (Eq, Show)

-- | Faults affect real exchanges after the target receives the request.
data CargoRegistryFault = CargoRegistryHealthy | CargoRegistryUnavailable | CargoRegistryLoseReply | CargoRegistryRequireToken Bool | CargoRegistryMissingArchive Text Text
    deriving stock (Eq, Show)

-- | Two native store roles share one fixture server and retain separate inventories.
data CargoRegistry = CargoRegistry
    { registryPort :: Int
    , registryFiles :: TVar (Map (Text, Text, Text) CargoStored)
    , registryRequests :: TVar [NativeRequest]
    , registryUploads :: TVar [ByteString]
    , registryTokens :: TVar [(Text, Maybe ByteString)]
    , registryFault :: TVar CargoRegistryFault
    }

-- | The fixture token is native Authorization content, without a scheme prefix.
cargoRegistryToken :: Text
cargoRegistryToken = "cargo-fixture-token"

-- | Hold native read and write roles behind the existing TLS stub.
withCargoRegistry :: (CargoRegistry -> IO a) -> IO a
withCargoRegistry action = do
    files <- newTVarIO Map.empty
    requests <- newTVarIO []
    uploads <- newTVarIO []
    tokens <- newTVarIO []
    fault <- newTVarIO CargoRegistryHealthy
    let registry = CargoRegistry 0 files requests uploads tokens fault
    withBridgeApplication (cargoRegistryApplication registry) (\port -> action registry{registryPort = port})

-- | Observe the target's complete physical inventory without consulting product state.
cargoRegistryFiles :: CargoRegistry -> IO (Map (Text, Text, Text) CargoStored)
cargoRegistryFiles = readTVarIO . registryFiles

-- | List only physically present versions in one native store.
cargoRegistryVersions :: CargoRegistry -> Text -> Text -> IO [Text]
cargoRegistryVersions registry host name =
    sort . map (\(_, _, version) -> version) . filter (\(origin, package, _) -> origin == host && package == name) . Map.keys <$> cargoRegistryFiles registry

-- | Await the exact expected archives at the mirror role.
awaitCargoRegistry :: CargoRegistry -> [CargoRelease] -> IO Bool
awaitCargoRegistry registry releases = pollUntil 120 500000 complete (cargoRegistryFiles registry) <&> complete
  where
    complete files = all (\release -> maybe False ((== releaseArchive release) . storedArchive) (Map.lookup ("cargo-mirror", crateName (releaseCrate release), crateVersion (releaseCrate release)) files)) releases

-- | Apply one native backend fault until the caller replaces it.
setCargoRegistryFault :: CargoRegistry -> CargoRegistryFault -> IO ()
setCargoRegistryFault registry = atomically . writeTVar (registryFault registry)

-- | Populate a declared private source without a product write.
seedCargoRegistry :: CargoRegistry -> Text -> [CargoRelease] -> IO ()
seedCargoRegistry registry host releases = atomically $ modifyTVar' (registryFiles registry) $ \files ->
    foldl' (\held release -> Map.insert (host, crateName (releaseCrate release), crateVersion (releaseCrate release)) (CargoStored (releaseArchive release) (cargoIndexRecord release)) held) files releases

-- | Speak sparse discovery, records, atomic archives and native publication.
cargoRegistryApplication :: CargoRegistry -> Application
cargoRegistryApplication registry request respond = do
    fault <- readTVarIO (registryFault registry)
    atomically (modifyTVar' (registryTokens registry) (<> [(host, lookup "Authorization" (requestHeaders request))]))
    let answer status headers body = do
            atomically (modifyTVar' (registryRequests registry) (<> [NativeRequest "http" host (decodeUtf8 (requestMethod request)) path (statusCode status)]))
            respond (responseLBS status headers (if requestMethod request == "HEAD" then "" else body))
        unauthorised = answer status401 [("WWW-Authenticate", "Cargo login_url=\"https://cargo-private/login\"")] "credential required"
        authenticated = lookup "Authorization" (requestHeaders request) == Just (encodeUtf8 cargoRegistryToken)
    case fault of
        CargoRegistryUnavailable -> answer status503 [] "unavailable"
        CargoRegistryRequireToken requireDiscovery
            | not authenticated && (requireDiscovery || pathInfo request /= ["config.json"]) -> unauthorised
        _
            | requestMethod request == "PUT" && pathInfo request == ["api", "v1", "crates", "new"] ->
                if authenticated then upload registry request host fault answer respond else unauthorised
            | requestMethod request `elem` ["GET", "HEAD"] -> do
                files <- cargoRegistryFiles registry
                let nativeBase = case host of
                        "cargo-private" -> stubUrl CargoPrivate
                        "cargo-upstream" -> stubUrl CargoPublic
                        _ -> stubUrl CargoMirror
                    rows = [(name, version, stored) | ((origin, name, version), stored) <- Map.toList files, origin == host]
                case pathInfo request of
                    ["config.json"] -> answer status200 [("Content-Type", "application/json")] (LBS.fromStrict (cargoViewDiscovery nativeBase (if host == "cargo-upstream" then Nothing else Just (T.dropWhileEnd (== '/') nativeBase)) (case fault of CargoRegistryRequireToken _ -> True; _ -> False)))
                    ["crates", name, version, filename]
                        | filename == name <> "-" <> version <> ".crate"
                        , Just stored <- Map.lookup (host, name, version) files ->
                            if fault == CargoRegistryMissingArchive name version then answer status404 [] "missing archive" else answer status200 [] (LBS.fromStrict (storedArchive stored))
                    _ -> case [storedRow stored | (name, _, stored) <- rows, cargoShard name == T.intercalate "/" (pathInfo request)] of
                        [] -> answer status404 [] "missing"
                        records -> answer status200 [("Content-Type", "application/octet-stream")] (foldMap ((<> "\n") . encode) records)
            | otherwise -> answer status404 [] "missing"
  where
    host = decodeUtf8 (fromMaybe "cargo-private" (requestHeaderHost request))
    path = "/" <> T.intercalate "/" (pathInfo request)

upload :: CargoRegistry -> Request -> Text -> CargoRegistryFault -> (Status -> ResponseHeaders -> LByteString -> IO ResponseReceived) -> (Response -> IO ResponseReceived) -> IO ResponseReceived
upload registry request host fault answer respond =
    boundedRead (PublishRequestBodyLimit (16 * 1024 * 1024)) (getRequestBodyChunk request) >>= \case
        Left _ -> answer status413 [] "upload too large"
        Right (_, bytes) -> do
            atomically (modifyTVar' (registryUploads registry) (<> [bytes]))
            case referenceUpload bytes of
                Left reason -> answer status400 [] (LBS.fromStrict (encodeUtf8 reason))
                Right framed -> case (KeyMap.lookup "name" (uploadMetadata framed), KeyMap.lookup "vers" (uploadMetadata framed)) of
                    (Just (String name), Just (String version)) -> do
                        let key = (host, name, version)
                            release = (minimalCargoRelease name version){releaseArchive = uploadArchive framed, releaseChecksum = hexSha256Of (uploadArchive framed)}
                            -- Targets can reset source time and yank, so durable source proof must restore both.
                            row = case cargoIndexRecord release of
                                Object fields -> Object (KeyMap.delete "ecluse-fixture" (KeyMap.insert "links" (fromMaybe Null (KeyMap.lookup "links" (uploadMetadata framed))) (KeyMap.insert "pubtime" (String "2099-01-01T00:00:00Z") (KeyMap.insert "deps" (nativeDependencies (uploadMetadata framed)) (KeyMap.insert "features2" (fromMaybe (Object mempty) (KeyMap.lookup "features" (uploadMetadata framed))) fields)))))
                                other -> other
                        exists <- atomically $ do
                            files <- readTVar (registryFiles registry)
                            if Map.member key files then pure True else writeTVar (registryFiles registry) (Map.insert key (CargoStored (uploadArchive framed) row) files) >> pure False
                        if exists
                            then answer status409 [] "already published"
                            else
                                if fault == CargoRegistryLoseReply
                                    then setCargoRegistryFault registry CargoRegistryHealthy >> respond (responseRaw (\_ _ -> pass) (responseLBS status503 [] "lost reply"))
                                    else answer status200 [("Content-Type", "application/json")] "{\"ok\":true}"
                    _ -> answer status400 [] "upload lacks native name or version"

nativeDependencies :: KeyMap.KeyMap Value -> Value
nativeDependencies metadata = case KeyMap.lookup "deps" metadata of
    Just (Array dependencies) -> Array (fmap indexDependency dependencies)
    _ -> Array mempty
  where
    indexDependency (Object fields) =
        let published = fromMaybe Null (KeyMap.lookup "name" fields)
            alias = case KeyMap.lookup "explicit_name_in_toml" fields of
                Just (String name) -> String name
                _ -> published
            package = if alias == published then Null else published
         in Object (KeyMap.insert "package" package (KeyMap.insert "name" alias (KeyMap.insert "req" (fromMaybe Null (KeyMap.lookup "version_req" fields)) (KeyMap.delete "explicit_name_in_toml" (KeyMap.delete "version_req" fields)))))
    indexDependency other = other
