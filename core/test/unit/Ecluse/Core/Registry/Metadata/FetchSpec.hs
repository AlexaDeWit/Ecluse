-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The read driver over every registered ecosystem: HTTP outcomes reach a read before body
projection, a full read pays for its bytes, and held chunks read as a response does.
-}
module Ecluse.Core.Registry.Metadata.FetchSpec (spec) where

import Prelude hiding (universe)

import Data.Aeson (Value, encode, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Universe.Class (Universe (universe))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (mkStatus, status200)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Package (PackageInfo (infoVersions), PackageName, mkPackageName)
import Ecluse.Core.Registry (FetchFault (FetchBoundExceeded))
import Ecluse.Core.Registry.Adapter (adapterFor)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataRead))
import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterEcosystem, adapterMetadata))
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Metadata (
    ContentDigest,
    Manifest (..),
    MetadataClient (fetchFullManifest, fetchVersionMetadata),
    MetadataError (MetadataAbsent, MetadataAuthorisationFailure, MetadataFetch, MetadataHttpFailure),
    VersionRead (vrBodyBytes, vrVersion),
 )
import Ecluse.Core.Registry.Metadata.Fetch (EcosystemRead, ReadTerms (..), fetchManifest, fetchVersion, readManifest, readVersion)
import Ecluse.Core.Registry.Npm.Adapter (npmAdapter)
import Ecluse.Core.Registry.Origin (OriginClient (ocLimits), chargingFullReads, originBaseUrl, perCallerOrigin)
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge), Limits (maxMetadataBytes), defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Metadata (ecosystemMetadataReads, privateMetadataClient)
import Ecluse.Core.Telemetry.Span (TracingPort (spanMetadataDecode, spanMetadataFetch))
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Port (noopMetricsPort, passthroughTracingPort)
import Ecluse.Test.Registry.JsonStream (heldBody)
import Ecluse.Test.Registry.Npm (packumentValue, versionSpec, versionValue)
import Ecluse.Test.Registry.PyPI (filesNamed, simpleIndex)
import Ecluse.Test.Snapshot (digestOf)
import Ecluse.Test.Stub (Stub, headerValue, stubBaseUrl, stubConfig, withRoutedStub)

spec :: Spec
spec = do
    it "holds documents for every ecosystem with an adapter" $
        map rdEcosystem registered `shouldBe` filter (isJust . adapterFor) universe
    responseReadsSpec
    heldReadsSpec

responseReadsSpec :: Spec
responseReadsSpec = describe "reads from an origin" $ do
    for_ registered $ \eco ->
        for_ statusOutcomes $ \(code, expected) ->
            it (label eco <> " preserves HTTP " <> show code <> " over a valid manifest body on full and version reads") $
                testWithApplication (pure (\_ respond -> respond (responseLBS (mkStatus code "test") [] (rdEmpty eco)))) $ \port -> do
                    manager <- newManager defaultManagerSettings
                    events <- newIORef ([] :: [(Text, PackageName)])
                    let name = thing eco
                        record phase who action = modifyIORef' events (<> [(phase, who)]) >> action
                        tracing = passthroughTracingPort{spanMetadataFetch = record "fetch", spanMetadataDecode = record "decode"}
                        origin = perCallerOrigin defaultLimits manager (loopbackRegistryUrl ("http://localhost:" <> show port)) Nothing
                        client = privateMetadataClient (ecosystemMetadataReads (rdRead eco) tracing noopMetricsPort (\_ _ -> pass) (\_ _ -> pass) (const pass) origin)
                    full <- fetchFullManifest client name
                    void full `shouldBe` expected
                    single <- fetchVersionMetadata client name (firstVersion eco)
                    void single `shouldBe` expected
                    readIORef events `shouldReturn` concat (replicate 2 ([("fetch", name)] <> [("decode", name) | isRight expected]))
                    when (isRight expected) $ do
                        fmap manifestBodyBytes full `shouldBe` Right (fromIntegral (BL.length (rdEmpty eco)))
                        fmap manifestDigest full `shouldBe` Right (digestOf (toStrict (rdEmpty eco)))
                        fmap vrBodyBytes single `shouldBe` Right (fromIntegral (BL.length (rdEmpty eco)))
    for_ registered $ \eco ->
        it (label eco <> " charges a full read for every source byte and a selected read for none") $
            testWithApplication (pure (\_ respond -> respond (responseLBS (mkStatus 200 "ok") [] (rdEmpty eco)))) $ \port -> do
                manager <- newManager defaultManagerSettings
                charges <- newIORef (0 :: Int)
                let name = thing eco
                    origin = chargingFullReads (\n -> modifyIORef' charges (+ n)) (perCallerOrigin defaultLimits manager (loopbackRegistryUrl ("http://localhost:" <> show port)) Nothing)
                    client = privateMetadataClient (ecosystemMetadataReads (rdRead eco) passthroughTracingPort noopMetricsPort (\_ _ -> pass) (\_ _ -> pass) (const pass) origin)
                _ <- fetchVersionMetadata client name (firstVersion eco)
                readIORef charges `shouldReturn` 0
                _ <- fetchFullManifest client name
                readIORef charges `shouldReturn` fromIntegral (BL.length (rdEmpty eco))

heldReadsSpec :: Spec
heldReadsSpec = describe "reads over held chunks" $
    for_ registered $ \eco -> do
        it (label eco <> " gives the response's manifest and version read, and pays for the full read alone") $
            withRelease eco $ \stub document -> do
                origin <- stubConfig loopbackRegistryUrl stub
                charges <- newIORef (0 :: Int)
                let terms = ReadTerms{rtLimits = defaultLimits, rtBaseUrl = originBaseUrl origin, rtChargeFullRead = \n -> modifyIORef' charges (+ n)}
                    (front, back) = BS.splitAt (BS.length document `div` 3) document
                heldVersion <- readVersion (rdRead eco) passthroughTracingPort terms (thing eco) (firstVersion eco) (heldBody [front, back])
                readIORef charges `shouldReturn` 0
                held <- readManifest (rdRead eco) passthroughTracingPort terms (thing eco) (heldBody [front, back])
                readIORef charges `shouldReturn` BS.length document
                fetched <- fetchManifest (rdRead eco) passthroughTracingPort origin (thing eco)
                fmap manifestFields held `shouldBe` fmap manifestFields fetched
                fmap (Map.keys . infoVersions . manifestInfo) held `shouldBe` Right ["1.2.3"]
                fetchedVersion <- fetchVersion (rdRead eco) passthroughTracingPort origin (thing eco) (firstVersion eco)
                heldVersion `shouldBe` fetchedVersion
                fmap (isJust . vrVersion) heldVersion `shouldBe` Right True

        it (label eco <> " refuses a body over the limit as a response's") $
            withRelease eco $ \stub document -> do
                origin <- stubConfig loopbackRegistryUrl stub
                let tight = defaultLimits{maxMetadataBytes = 8}
                    terms = ReadTerms{rtLimits = tight, rtBaseUrl = originBaseUrl origin, rtChargeFullRead = const pass}
                    refused = Left (MetadataFetch (FetchBoundExceeded (BodyTooLarge (MetadataBodyLimit 8))))
                held <- readManifest (rdRead eco) passthroughTracingPort terms (thing eco) (heldBody [document])
                void held `shouldBe` refused
                fetched <- fetchManifest (rdRead eco) passthroughTracingPort origin{ocLimits = tight} (thing eco)
                void fetched `shouldBe` refused

-- One registered ecosystem: the read its adapter holds, and documents of its own format.
data Registered = Registered
    { rdEcosystem :: Ecosystem
    , rdRead :: EcosystemRead
    , rdEmpty :: BL.ByteString
    -- ^ A valid document for @thing@ with no release.
    , rdRelease :: Text -> Value
    -- ^ A document for @thing@ with release 1.2.3, whose artifact an origin at the base URL honours.
    }

registered :: [Registered]
registered =
    [ Registered
        { rdEcosystem = adapterEcosystem npmAdapter
        , rdRead = metadataRead (adapterMetadata npmAdapter)
        , rdEmpty = "{\"name\":\"thing\",\"versions\":{}}"
        , rdRelease = \base -> packumentValue "thing" "1.2.3" [("1.2.3", versionValue (versionSpec "thing" "1.2.3" (base <> "/thing/-/thing-1.2.3.tgz")))] ["1.2.3" .= ("2020-01-01T00:00:00.000Z" :: Text)] []
        }
    , Registered
        { rdEcosystem = adapterEcosystem pypiAdapter
        , rdRead = metadataRead (adapterMetadata pypiAdapter)
        , rdEmpty = "{\"meta\":{\"api-version\":\"1.0\"},\"name\":\"thing\",\"files\":[]}"
        , rdRelease = const (simpleIndex "thing" (filesNamed ["thing-1.2.3.tar.gz"]))
        }
    ]

label :: Registered -> String
label = show . rdEcosystem

thing :: Registered -> PackageName
thing eco = mkPackageName (rdEcosystem eco) Nothing "thing"

firstVersion :: Registered -> Version
firstVersion eco = mkVersion (rdEcosystem eco) "1.2.3"

manifestFields :: Manifest -> (PackageInfo, CachedDoc, Int, ContentDigest)
manifestFields manifest = (manifestInfo manifest, manifestRaw manifest, manifestBodyBytes manifest, manifestDigest manifest)

-- Serve the ecosystem's one-release document from a stub, located under the stub's own base URL.
withRelease :: Registered -> (Stub -> ByteString -> IO a) -> IO a
withRelease eco action =
    withRoutedStub (\captured -> (status200, [], encode (rdRelease eco ("http://" <> maybe "" decodeUtf8 (headerValue "Host" captured))))) $ \stub ->
        action stub (toStrict (encode (rdRelease eco (stubBaseUrl stub))))

statusOutcomes :: [(Int, Either MetadataError ())]
statusOutcomes =
    [(code, Right ()) | code <- [200, 201, 299]]
        <> [(code, Left (MetadataAuthorisationFailure code)) | code <- [401, 403]]
        <> [(404, Left MetadataAbsent)]
        <> [(code, Left (MetadataHttpFailure code)) | code <- [301, 304, 400, 408, 410, 429, 500, 503, 599]]
