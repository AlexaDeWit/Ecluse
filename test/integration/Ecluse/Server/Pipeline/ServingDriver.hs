-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native documents and independent response readers for the shared serving laws.
module Ecluse.Server.Pipeline.ServingDriver (
    npmServingDriver,
    pypiServingDriver,
    cargoServingDriver,
    NativeObservation (..),
    nativeByteDriver,
    responseEvidence,
    responseBytes,
) where

import Codec.Compression.GZip qualified as GZip
import Control.Monad (foldM)
import Data.Aeson (Value (Array, Object, String), encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Network.HTTP.Types (Method, RequestHeaders, hContentLength, methodHead, mkStatus, status200, status204, status205, status304, status404, statusCode, statusIsInformational)
import Network.Wai (rawPathInfo, responseLBS)
import Network.Wai qualified as Wai
import Network.Wai.Test (SResponse (..))

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI))
import Ecluse.Core.Mirror.State (StateRead (..), extendProjectRecord, stateTarget)
import Ecluse.Core.Mirror.State.Internal (StateRevision (..), retainVerifiedIdentity)
import Ecluse.Core.Package (mkPackageName)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataRead))
import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterMetadata))
import Ecluse.Core.Registry.CachedDocument (cargoCached)
import Ecluse.Core.Registry.Cargo.Mirror (cargoMirrorDocuments, cargoStateFormat)
import Ecluse.Core.Registry.Metadata (VersionDoc (..), VersionRead (..))
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (prepareVersion)
import Ecluse.Core.Registry.MirrorUpload (MirrorReadData (CargoMirrorRead), checkedMirrorInventory, inventoryFiles, verifyMirrorBytes)
import Ecluse.Core.Registry.Npm.Adapter (npmAdapter)
import Ecluse.Core.Registry.Origin (OriginClient, originClient)
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter)
import Ecluse.Core.Rules (PreparedRule, prepare)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Core.Server.Encoding (CodingChoice (ServeCoding), ContentCoding (GzipCoding), MetadataCompression (GzipWhenAccepted), chooseCoding, gzipEncodingHeader)
import Ecluse.Core.Server.MirrorView (MirrorView)
import Ecluse.Core.Server.Upstream (MirrorServePlan (NoMirrorWrite))
import Ecluse.Core.Version (mkVersion)
import Ecluse.Server.Pipeline.TestSupport (newTestEnvWithQueue, packument, proxyApplication, publishedDaysAgo, requestAt, withUpstreams)
import Ecluse.Test.Contract.Serving (OriginFault (..), ServingDriver (..), ServingFaults (..), ServingRelease (..), ServingResponse (..), ServingSession (..), ServingSetup (..))
import Ecluse.Test.Deadline (untracedClient)
import Ecluse.Test.Json (fieldAt, textAtPath)
import Ecluse.Test.Mirror.State (mirrorViewWith)
import Ecluse.Test.Package (hexSha256Of, sriSha256Of)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Registry.Cargo.Fixtures qualified as Cargo
import Ecluse.Test.Registry.Cargo.Reference qualified as Reference
import Ecluse.Test.Registry.Npm (VersionSpec (..), versionSpec, versionValue)
import Ecluse.Test.Rules (atDefaultPrecedence, inertRuleDeps)
import Ecluse.Test.Server.Mount (requiredAdapter, serveDepsFor)
import Ecluse.Test.Support (expectRight)
import Ecluse.Test.Wai (closedResponse, decodedBody, localhost, selfBaseUrl)

-- | npm's native JSON and tarball paths.
npmServingDriver :: ServingDriver
npmServingDriver = nativeDriver Npm npmAdapter "/npm" "/thing" npmArtifact npmDocument npmObservation sriSha256Of

-- | PyPI's Simple JSON and distribution paths.
pypiServingDriver :: ServingDriver
pypiServingDriver = nativeDriver PyPI pypiAdapter "/pypi" "/simple/thing" pypiArtifact pypiDocument pypiObservation hexSha256Of

-- | Cargo uses the production registration, native LF records and checksum observations.
cargoServingDriver :: ServingDriver
cargoServingDriver = cargoDriver

nativeDriver :: Ecosystem -> RegistryAdapter -> ByteString -> ByteString -> (Text -> ByteString) -> (Text -> [ServingRelease] -> Value) -> (SResponse -> ServingResponse) -> (ByteString -> Text) -> ServingDriver
nativeDriver ecosystem adapter mount metadataPath artifactPath document observe =
    nativeByteDriver ecosystem (pure adapter) mount metadataPath metadataPath artifactPath (\base -> toStrict . encode . document base) (NativeObservation (\_ _ -> pure . observe) (\_ -> pure . observe))

-- | Metadata may address native artifacts while artifact observations remain independent wire responses.
data NativeObservation = NativeObservation
    { observeNativeMetadata :: (Method -> RequestHeaders -> Text -> IO SResponse) -> Method -> SResponse -> IO ServingResponse
    , observeNativeArtifact :: Method -> SResponse -> IO ServingResponse
    }

-- | Native formats reuse the same upstream ownership while supplying independent wire observations.
nativeByteDriver :: Ecosystem -> IO RegistryAdapter -> ByteString -> ByteString -> ByteString -> (Text -> ByteString) -> (Text -> [ServingRelease] -> ByteString) -> NativeObservation -> (ByteString -> Text) -> ServingDriver
nativeByteDriver ecosystem acquireAdapter mount clientMetadataPath upstreamMetadataPath artifactPath document observe digest =
    ServingDriver
        { withOrigins = origins
        , servingIdentity = ("thing@" <>)
        , servingDigest = digest
        }
  where
    origins :: ServingFaults -> [ServingRelease] -> [ServingRelease] -> ((ServingSetup -> IO ServingSession) -> IO a) -> IO a
    origins faults privateReleases publicReleases use = do
        adapter <- acquireAdapter
        withUpstreams (faulted (privateFault faults) (upstream privateReleases)) (faulted (publicFault faults) (upstream publicReleases)) $ \privatePort publicPort deadlines manager ->
            use $ \setup -> do
                env <- newTestMemoryQueue >>= \queue -> newTestEnvWithQueue deadlines queue manager
                deps <- setupDeps setup (serveDepsFor adapter (Just (loopbackRegistryUrl (localhost privatePort))) (loopbackRegistryUrl (localhost publicPort)) NoMirrorWrite)
                let app = proxyApplication ecosystem env deps{pdMountBaseUrl = "http://ecluse.test" <> decodeUtf8 mount}
                    raw method path headers = requestAt method (mount <> path) Wai.defaultRequest{Wai.requestHeaders = headers} app
                    artifact method headers version = raw method (artifactPath version) headers
                pure
                    ServingSession
                        { requestMetadata = \method headers -> raw method clientMetadataPath headers >>= observeNativeMetadata observe artifact method
                        , requestArtifact = \method headers version -> artifact method headers version >>= observeNativeArtifact observe method
                        }
    upstream releases request respond
        | BS.dropWhileEnd (== 47) (rawPathInfo request) == upstreamMetadataPath =
            respond (responseLBS status200 [] (toLazy (document (selfBaseUrl request) releases)))
        | Just release <- find ((== rawPathInfo request) . artifactPath . releaseVersion) releases =
            respond (artifactResponse (Wai.requestHeaders request) (releaseBytes release))
        | otherwise = respond (responseLBS status404 [] "")

-- A mount over loopback origins, configured by the setup, at the fixed clock.
setupDeps :: ServingSetup -> ([PreparedRule] -> IO UTCTime -> PackumentDeps) -> IO PackumentDeps
setupDeps setup depsOver = do
    prepared <- prepare inertRuleDeps (map atDefaultPrecedence (setupRules setup))
    pure
        (depsOver prepared (pure (UTCTime (fromGregorian 2026 6 20) 0)))
            { pdEgressUrl = Right . loopbackRegistryUrl
            , pdMetadataCompression = setupCompression setup
            , pdMinIntegrity = setupPublicFloor setup
            , pdMinTrustedIntegrity = setupTrustedFloor setup
            }

-- Answer every request with the fault, or leave the origin as it is.
faulted :: Maybe OriginFault -> Wai.Application -> Wai.Application
faulted fault app = case fault of
    Nothing -> app
    Just (FaultStatus code) -> \_ respond -> respond (responseLBS (mkStatus code "") [] "")
    Just FaultClosed -> \_ respond -> respond closedResponse
    Just FaultUnconfigured -> app

-- The private origin the mount configures, unless the faults leave it out.
configuredPrivate :: ServingFaults -> Int -> Maybe RegistryUrl
configuredPrivate faults port
    | privateFault faults == Just FaultUnconfigured = Nothing
    | otherwise = Just (loopbackRegistryUrl (localhost port))

-- A CDN can gzip archive responses. HEAD needs an explicit length because Warp cannot infer it.
artifactResponse :: RequestHeaders -> ByteString -> Wai.Response
artifactResponse headers bytes = case chooseCoding GzipWhenAccepted headers of
    ServeCoding GzipCoding -> sized (GZip.compress (toLazy bytes)) [gzipEncodingHeader]
    _ -> sized (toLazy bytes) []
  where
    sized body coding = responseLBS status200 ((hContentLength, show (LBS.length body)) : coding) body

npmArtifact :: Text -> ByteString
npmArtifact version = encodeUtf8 ("/thing/-/thing-" <> version <> ".tgz")

pypiArtifact :: Text -> ByteString
pypiArtifact version = encodeUtf8 ("/simple/thing/thing-" <> version <> ".tar.gz")

npmDocument :: Text -> [ServingRelease] -> Value
npmDocument base releases =
    packument
        [ ( releaseVersion release
          , versionValue
                ( (versionSpec "thing" (releaseVersion release) (base <> decodeUtf8 (npmArtifact (releaseVersion release))))
                    { vsIntegrity = if releaseHasIntegrity release then Just (sriSha256Of (releaseBytes release)) else Nothing
                    , vsShasum = Nothing
                    }
                )
          )
        | release <- releases
        ]
        (maybe "0.0.0" releaseVersion (listToMaybe (reverse releases)))
        [(releaseVersion release, publishedDaysAgo (releaseAgeDays release)) | release <- releases]

pypiDocument :: Text -> [ServingRelease] -> Value
pypiDocument base releases =
    object
        [ "name" .= ("thing" :: Text)
        , "meta" .= object ["api-version" .= ("1.4" :: Text)]
        , "files"
            .= [ object
                    [ "filename" .= ("thing-" <> releaseVersion release <> ".tar.gz")
                    , "url" .= (base <> decodeUtf8 (pypiArtifact (releaseVersion release)))
                    , "hashes" .= object ["sha256" .= hexSha256Of (releaseBytes release) | releaseHasIntegrity release]
                    , "upload-time" .= publishedDaysAgo (releaseAgeDays release)
                    ]
               | release <- releases
               ]
        ]

npmObservation :: SResponse -> ServingResponse
npmObservation response = responseEvidence response versions digests
  where
    entries = case fieldAt "versions" (decodedBody response) of
        Just (Object values) -> KeyMap.toList values
        _ -> []
    versions = sort (map (Key.toText . fst) entries)
    digests = Map.fromList [(Key.toText key, maybeToList (textAtPath ["dist", "integrity"] value)) | (key, value) <- entries]

pypiObservation :: SResponse -> ServingResponse
pypiObservation response = responseEvidence response versions digests
  where
    versions = case fieldAt "versions" (decodedBody response) of
        Just (Array values) -> sort [version | String version <- toList values]
        _ -> []
    files = case fieldAt "files" (decodedBody response) of
        Just (Array values) -> toList values
        _ -> []
    digests =
        Map.fromListWith
            (<>)
            [ (version, [digest])
            | file <- files
            , Just filename <- [textAtPath ["filename"] file]
            , Just version <- [T.stripPrefix "thing-" filename >>= T.stripSuffix ".tar.gz"]
            , Just digest <- [textAtPath ["hashes", "sha256"] file]
            ]

-- | Preserve actual wire status, bytes and headers beside independently decoded native facts.
responseEvidence :: SResponse -> [Text] -> Map Text [Text] -> ServingResponse
responseEvidence response versions digests =
    ServingResponse
        { responseStatus = statusCode (simpleStatus response)
        , responseBody = simpleBody response
        , responseHeaders = simpleHeaders response
        , responseVersions = versions
        , responseDigests = digests
        }

cargoArtifact :: Text -> ByteString
cargoArtifact version = encodeUtf8 ("/crates/thing/" <> version <> "/thing-" <> version <> ".crate")

cargoDocument :: Text -> [ServingRelease] -> ByteString
cargoDocument _ = BS.intercalate "\n" . map row
  where
    row release = toStrict (encode (Object (KeyMap.insert "pubtime" (String (publishedDaysAgo (releaseAgeDays release))) (KeyMap.insert "cksum" (String checksum) fields))))
      where
        checksum = if releaseHasIntegrity release then hexSha256Of (releaseBytes release) else ""
        fields = case Cargo.cargoIndexRecord (Cargo.minimalCargoRelease "thing" (releaseVersion release)) of
            Object values -> values
            _ -> KeyMap.empty

cargoObservation :: Method -> SResponse -> ServingResponse
cargoObservation method response = responseEvidence response versions digests
  where
    values = [fields | row <- Reference.referenceIndex bytes, Right fields <- [Reference.indexValue row]]
    versions = sort [version | fields <- values, Just (String version) <- [KeyMap.lookup "vers" fields]]
    digests = Map.fromList [(version, [digest]) | fields <- values, Just (String version) <- [KeyMap.lookup "vers" fields], Just (String digest) <- [KeyMap.lookup "cksum" fields]]
    bytes = responseBytes method response

-- | Decode only representations that carry a body, preserving HEAD and conditional-response emptiness.
responseBytes :: Method -> SResponse -> ByteString
responseBytes method response
    | method == methodHead || statusIsInformational status || status `elem` [status204, status205, status304] = ""
    | lookup "Content-Encoding" (simpleHeaders response) == Just "gzip" = toStrict (GZip.decompress (simpleBody response))
    | otherwise = toStrict (simpleBody response)
  where
    status = simpleStatus response

cargoDriver :: ServingDriver
cargoDriver =
    ServingDriver
        { withOrigins = origins
        , servingIdentity = ("thing@" <>)
        , servingDigest = hexSha256Of
        }
  where
    origins :: ServingFaults -> [ServingRelease] -> [ServingRelease] -> ((ServingSetup -> IO ServingSession) -> IO a) -> IO a
    origins faults privateReleases publicReleases use =
        withUpstreams (faulted (privateFault faults) (cargoUpstream privateReleases)) (faulted (publicFault faults) (cargoUpstream publicReleases)) $ \privatePort publicPort deadlines manager -> do
            adapter <- requiredAdapter Cargo
            let privateUrl = loopbackRegistryUrl (localhost privatePort)
                publicUrl = loopbackRegistryUrl (localhost publicPort)
                origin = originClient defaultLimits (untracedClient manager deadlines) publicUrl Nothing
            view <- cargoManagedView adapter origin (localhost privatePort) privateReleases publicReleases
            use $ \setup -> do
                env <- newTestMemoryQueue >>= \queue -> newTestEnvWithQueue deadlines queue manager
                deps <- setupDeps setup (serveDepsFor adapter (Just privateUrl) publicUrl NoMirrorWrite)
                let app = proxyApplication Cargo env deps{pdMountBaseUrl = "http://ecluse.test/cargo", pdMirrorState = Just view}
                    request method path headers = cargoObservation method <$> requestAt method ("/cargo" <> path) Wai.defaultRequest{Wai.requestHeaders = headers} app
                pure
                    ServingSession
                        { requestMetadata = (`request` "/th/in/thing")
                        , requestArtifact = \method headers version -> request method (cargoArtifact version) headers
                        }

cargoUpstream :: [ServingRelease] -> Wai.Application
cargoUpstream releases request respond
    | rawPathInfo request == "/config.json" =
        respond (responseLBS status200 [] (toLazy (Cargo.cargoViewDiscovery (selfBaseUrl request) Nothing False)))
    | rawPathInfo request == "/th/in/thing" =
        respond (responseLBS status200 [] (toLazy (cargoDocument (selfBaseUrl request) releases)))
    | Just release <- find ((== rawPathInfo request) . cargoArtifact . releaseVersion) releases =
        respond (artifactResponse (Wai.requestHeaders request) (releaseBytes release))
    | otherwise = respond (responseLBS status404 [] "")

cargoManagedView :: RegistryAdapter -> OriginClient -> Text -> [ServingRelease] -> [ServingRelease] -> IO MirrorView
cargoManagedView adapter origin targetUrl private public = do
    record <- foldM copy Nothing copies
    mirrorViewWith cargoStateFormat cargoMirrorDocuments target (\_ _ -> pure (Right (maybe StateAbsent (StateLoaded (StateRevision target name "complete")) record)))
  where
    target = stateTarget targetUrl
    name = mkPackageName Cargo Nothing "thing"
    copies = [release | release <- public, releaseHasIntegrity release, any (sameIdentity release) private]
    sameIdentity publicRelease privateRelease = releaseVersion publicRelease == releaseVersion privateRelease && releaseBytes publicRelease == releaseBytes privateRelease && releaseHasIntegrity privateRelease
    copy previous release = do
        let version = mkVersion Cargo (releaseVersion release)
        result <- expectRight =<< prepareVersion (metadataRead (adapterMetadata adapter)) passthroughTracingPort origin name version
        doc <- maybe (fail "completed fixture has no admitted source version") pure (vrVersion result)
        source <- maybe (fail "completed fixture has no native source records") pure (vdRaw doc >>= snd cargoCached)
        inventory <- expectRight (checkedMirrorInventory name version doc (CargoMirrorRead source))
        expected <- expectRight (extendProjectRecord cargoStateFormat defaultLimits target inventory previous)
        let file :| _ = inventoryFiles inventory
        verified <- expectRight (verifyMirrorBytes file (releaseBytes release))
        Just <$> expectRight (retainVerifiedIdentity version verified expected)
