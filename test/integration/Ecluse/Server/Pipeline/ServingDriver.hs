-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native documents and independent response readers for the shared serving laws.
module Ecluse.Server.Pipeline.ServingDriver (npmServingDriver, pypiServingDriver) where

import Codec.Compression.GZip qualified as GZip
import Data.Aeson (Value (Array, Object, String), encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (RequestHeaders, hContentLength, status200, status404, statusCode)
import Network.Wai (rawPathInfo, responseLBS)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Test (SResponse (..))

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter)
import Ecluse.Core.Registry.Npm.Adapter (npmAdapter)
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Core.Server.Encoding (CodingChoice (ServeCoding), ContentCoding (GzipCoding), MetadataCompression (GzipWhenAccepted), chooseCoding, gzipEncodingHeader)
import Ecluse.Core.Server.Upstream (MirrorServePlan (NoMirrorWrite))
import Ecluse.Server.Pipeline.TestSupport (newTestEnvWithQueue, packument, proxyApplication, publishedDaysAgo, requestAt)
import Ecluse.Test.Contract.Serving (ServingDriver (..), ServingRelease (..), ServingResponse (..), ServingSession (..))
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Json (fieldAt, textAtPath)
import Ecluse.Test.Package (hexSha256Of, sriSha256Of)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Registry.Npm (VersionSpec (..), versionSpec, versionValue)
import Ecluse.Test.Rules (atDefaultPrecedence, inertRuleDeps)
import Ecluse.Test.Server.Mount (serveDepsFor)
import Ecluse.Test.Wai (decodedBody, localhost, selfBaseUrl)

-- | npm's native JSON and tarball paths.
npmServingDriver :: ServingDriver
npmServingDriver = nativeDriver Npm npmAdapter "/npm" "/thing" npmArtifact npmDocument npmObservation sriSha256Of

-- | PyPI's Simple JSON and distribution paths.
pypiServingDriver :: ServingDriver
pypiServingDriver = nativeDriver PyPI pypiAdapter "/pypi" "/simple/thing" pypiArtifact pypiDocument pypiObservation hexSha256Of

nativeDriver :: Ecosystem -> RegistryAdapter -> ByteString -> ByteString -> (Text -> ByteString) -> (Text -> [ServingRelease] -> Value) -> (SResponse -> ServingResponse) -> (ByteString -> Text) -> ServingDriver
nativeDriver ecosystem adapter mount metadataPath artifactPath document observe digest =
    ServingDriver
        { withServing = \compression rules privateReleases publicReleases action ->
            testWithApplication (pure (upstream privateReleases)) $ \privatePort ->
                testWithApplication (pure (upstream publicReleases)) $ \publicPort -> withSweeper $ \deadlines -> do
                    queue <- newTestMemoryQueue
                    manager <- newManager defaultManagerSettings
                    env <- newTestEnvWithQueue deadlines queue manager
                    prepared <- prepare inertRuleDeps (map atDefaultPrecedence rules)
                    let deps =
                            (serveDepsFor adapter (Just (loopbackRegistryUrl (localhost privatePort))) (loopbackRegistryUrl (localhost publicPort)) NoMirrorWrite prepared (pure now))
                                { pdMountBaseUrl = "http://ecluse.test" <> decodeUtf8 mount
                                , pdEgressUrl = Right . loopbackRegistryUrl
                                , pdMetadataCompression = compression
                                }
                        app = proxyApplication ecosystem env deps
                        request method path headers = observe <$> requestAt method (mount <> path) Wai.defaultRequest{Wai.requestHeaders = headers} app
                    action
                        ServingSession
                            { requestMetadata = (`request` metadataPath)
                            , requestArtifact = \method headers version -> request method (artifactPath version) headers
                            }
        , servingIdentity = ("thing@" <>)
        , servingDigest = digest
        }
  where
    now = UTCTime (fromGregorian 2026 6 20) 0
    upstream releases request respond
        | BS.dropWhileEnd (== 47) (rawPathInfo request) == metadataPath =
            respond (responseLBS status200 [] (encode (document (selfBaseUrl request) releases)))
        | Just release <- find ((== rawPathInfo request) . artifactPath . releaseVersion) releases =
            respond (artifactResponse (Wai.requestHeaders request) (releaseBytes release))
        | otherwise = respond (responseLBS status404 [] "")

{- Answer as a CDN does: gzip the artifact for a request that permits it. A proxy that relays those
bytes breaks the advertised digest. Warp does not infer a builder response's length for HEAD.
-}
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

responseEvidence :: SResponse -> [Text] -> Map Text [Text] -> ServingResponse
responseEvidence response versions digests =
    ServingResponse
        { responseStatus = statusCode (simpleStatus response)
        , responseBody = simpleBody response
        , responseHeaders = simpleHeaders response
        , responseVersions = versions
        , responseDigests = digests
        }
