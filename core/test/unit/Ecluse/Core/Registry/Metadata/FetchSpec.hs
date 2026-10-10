-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The read driver over every registered ecosystem: what a read reports for each upstream status,
body bound and request fault, and that held chunks read as a response does.
-}
module Ecluse.Core.Registry.Metadata.FetchSpec (spec) where

import Prelude hiding (universe)

import Codec.Compression.GZip qualified as GZip
import Data.Aeson (Value, encode, (.=))
import Data.Aeson.Key qualified as Key
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Universe.Class (Universe (universe))
import Network.HTTP.Client (Request, defaultManagerSettings, newManager, parseRequest)
import Network.HTTP.Types (hLocation, mkStatus, status200, status302)
import Network.HTTP.Types.Header (hContentEncoding)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Package (PackageInfo (infoVersions), PackageName, mkPackageName)
import Ecluse.Core.Registry (FetchFault (FetchBoundExceeded, FetchUrlUnformable), UrlFormationError (EmptyBaseUrl))
import Ecluse.Core.Registry.Adapter (adapterFor)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataRead))
import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterEcosystem, adapterMetadata))
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Metadata (
    ContentDigest,
    Manifest (..),
    MetadataClient (fetchFullManifest, fetchVersionMetadata),
    MetadataError (MetadataAbsent, MetadataAuthorisationFailure, MetadataBoundExceeded, MetadataFetch, MetadataHttpFailure, MetadataNameMismatch, MetadataUndecodable),
    VersionRead (vrBodyBytes, vrVersion),
 )
import Ecluse.Core.Registry.Metadata.Fetch (fetchManifest, fetchVersion, readManifest, readVersion)
import Ecluse.Core.Registry.Metadata.Fetch.Types (EcosystemRead (erRequest), ReadTerms (..))
import Ecluse.Core.Registry.Npm.Adapter (npmAdapter)
import Ecluse.Core.Registry.Origin (OriginClient (ocChargeFullRead, ocLimits), chargingFullReads, originBaseUrl, originClient, perCallerOrigin)
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge, TooManyVersions), Limits (maxMetadataBytes, maxVersionCount), defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Metadata (ecosystemMetadataReads, privateMetadataClient)
import Ecluse.Core.Telemetry.Span (TracingPort (spanMetadataDecode, spanMetadataFetch))
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Port (noopMetricsPort, passthroughTracingPort)
import Ecluse.Test.Registry (isBoundExceededFetch, isTransportFetch)
import Ecluse.Test.Registry.JsonStream (heldBody)
import Ecluse.Test.Registry.Npm (packumentValue, versionSpec, versionValue)
import Ecluse.Test.Registry.PyPI (filesNamed, simpleIndex)
import Ecluse.Test.Snapshot (digestOf)
import Ecluse.Test.Stub (Captured (capPath), allCaptured, headerValue, stubBaseUrl, stubConfig, withRoutedStub, withStub, withStubHeaders)

spec :: Spec
spec = do
    it "holds documents for every ecosystem with an adapter" $
        map rdEcosystem registered `shouldBe` filter (isJust . adapterFor) universe
    for_ registered $ \eco ->
        describe (show (rdEcosystem eco)) $ do
            statusSpec eco
            responseBoundSpec eco
            requestSpec eco
            heldSpec eco

statusSpec :: Registered -> Spec
statusSpec eco = describe "upstream statuses" $ do
    for_ statusOutcomes $ \(code, expected) ->
        it ("preserves HTTP " <> show code <> " over a valid manifest body on full and version reads") $
            withStub (mkStatus code "test") (toLazy document) $ \stub -> do
                manager <- newManager defaultManagerSettings
                events <- newIORef ([] :: [(Text, PackageName)])
                let record phase who action = modifyIORef' events (<> [(phase, who)]) >> action
                    tracing = passthroughTracingPort{spanMetadataFetch = record "fetch", spanMetadataDecode = record "decode"}
                    origin = perCallerOrigin defaultLimits manager (loopbackRegistryUrl (stubBaseUrl stub)) Nothing
                    client = privateMetadataClient (ecosystemMetadataReads (rdRead eco) tracing noopMetricsPort (\_ _ -> pass) (\_ _ -> pass) (const pass) origin)
                full <- fetchFullManifest client (thing eco)
                void full `shouldBe` expected
                single <- fetchVersionMetadata client (thing eco) (releaseOf eco)
                void single `shouldBe` expected
                -- The decode span opens only after success headers.
                readIORef events `shouldReturn` concat (replicate 2 ([("fetch", thing eco)] <> [("decode", thing eco) | isRight expected]))
                when (isRight expected) $ do
                    fmap manifestBodyBytes full `shouldBe` Right (BS.length document)
                    fmap manifestDigest full `shouldBe` Right (digestOf document)
                    fmap vrBodyBytes single `shouldBe` Right (BS.length document)

    it "charges a full read for every source byte and a selected read for none" $
        withStub status200 (toLazy document) $ \stub -> do
            manager <- newManager defaultManagerSettings
            charges <- newIORef (0 :: Int)
            let origin = chargingFullReads (\n -> modifyIORef' charges (+ n)) (perCallerOrigin defaultLimits manager (loopbackRegistryUrl (stubBaseUrl stub)) Nothing)
                client = privateMetadataClient (ecosystemMetadataReads (rdRead eco) passthroughTracingPort noopMetricsPort (\_ _ -> pass) (\_ _ -> pass) (const pass) origin)
            _ <- fetchVersionMetadata client (thing eco) (releaseOf eco)
            readIORef charges `shouldReturn` 0
            _ <- fetchFullManifest client (thing eco)
            readIORef charges `shouldReturn` BS.length document

    for_ [401, 403] $ \code ->
        it ("retains HTTP " <> show code <> " before an oversized error body") $
            withStub (mkStatus code "test") (toLazy (rdDocument eco "thing" 256)) $ \stub -> do
                origin <- stubConfig loopbackRegistryUrl stub
                outcome <- fetchManifest (rdRead eco) passthroughTracingPort origin{ocLimits = defaultLimits{maxMetadataBytes = 64}} (thing eco)
                void outcome `shouldBe` Left (MetadataAuthorisationFailure code)
  where
    document = rdDocument eco "thing" 0

responseBoundSpec :: Registered -> Spec
responseBoundSpec eco = describe "the body bound on a response" $ do
    it "digests the complete source body within maxMetadataBytes" $
        withStub status200 (toLazy small) $ \stub -> do
            origin <- stubConfig loopbackRegistryUrl stub
            manifest <- fetchManifest (rdRead eco) passthroughTracingPort origin{ocLimits = defaultLimits{maxMetadataBytes = BS.length small}} (thing eco)
            fmap manifestDigest manifest `shouldBe` Right (digestOf small)

    it "reports decompressed bytes for an accepted gzip body" $
        withGzipped padded $ \origin -> do
            manifest <- fetchManifest (rdRead eco) passthroughTracingPort origin{ocLimits = defaultLimits{maxMetadataBytes = BS.length padded}} (thing eco)
            fmap manifestBodyBytes manifest `shouldBe` Right (BS.length padded)
            fmap manifestDigest manifest `shouldBe` Right (digestOf padded)

    it "charges a full read for every decompressed byte it hands the parser" $
        withGzipped padded $ \origin -> do
            charges <- newIORef []
            _ <- fetchManifest (rdRead eco) passthroughTracingPort origin{ocLimits = defaultLimits{maxMetadataBytes = BS.length padded}, ocChargeFullRead = \n -> modifyIORef' charges (n :)} (thing eco)
            recorded <- readIORef charges
            sum recorded `shouldBe` BS.length padded

    it "bounds decompressed size: a small gzip body that inflates past the cap is refused" $ do
        -- The compressed body is under the cap, so only the decompressed-size bound explains a refusal.
        BS.length (toStrict (GZip.compress (toLazy inflating))) `shouldSatisfy` (< 1024)
        withGzipped inflating $ \origin -> do
            outcome <- fetchManifest (rdRead eco) passthroughTracingPort origin{ocLimits = defaultLimits{maxMetadataBytes = 1024}} (thing eco)
            fetchFault outcome `shouldSatisfy` isBoundExceededFetch
  where
    small = rdDocument eco "thing" 0
    padded = rdDocument eco "thing" 256
    inflating = rdDocument eco "thing" 65536
    withGzipped body action =
        withStubHeaders status200 [(hContentEncoding, "gzip")] (GZip.compress (toLazy body)) (stubConfig loopbackRegistryUrl >=> action)

requestSpec :: Registered -> Spec
requestSpec eco = describe "the request" $ do
    for_ ["", "http://127.0.0.1:1"] $ \url ->
        it ("does not open a decode span for request failure at " <> toString url) $ do
            manager <- newManager defaultManagerSettings
            count <- newIORef (0 :: Int)
            let tracing = passthroughTracingPort{spanMetadataDecode = \_ action -> modifyIORef' count (+ 1) >> action}
            outcome <- fetchManifest (rdRead eco) tracing (originClient defaultLimits manager (loopbackRegistryUrl url) Nothing) (thing eco)
            void outcome `shouldSatisfy` isLeft
            readIORef count `shouldReturn` 0

    it "reports an empty base URL as a FetchUrlUnformable value, never thrown" $ do
        manager <- newManager defaultManagerSettings
        outcome <- fetchManifest (rdRead eco) passthroughTracingPort (originClient defaultLimits manager (loopbackRegistryUrl "") Nothing) (thing eco)
        void outcome `shouldBe` Left (MetadataFetch (FetchUrlUnformable EmptyBaseUrl))

    it "reports a refused connection as a FetchTransport value, never thrown" $ do
        -- Port 1 on the loopback is privileged and unbound, so the kernel refuses the connect.
        manager <- newManager defaultManagerSettings
        outcome <- fetchManifest (rdRead eco) passthroughTracingPort (originClient defaultLimits manager (loopbackRegistryUrl "http://127.0.0.1:1") Nothing) (thing eco)
        fetchFault outcome `shouldSatisfy` isTransportFetch

    it "follows no redirect when the ecosystem's request builder did not seal its request" $
        withRoutedStub redirectOnce $ \stub -> do
            origin <- stubConfig loopbackRegistryUrl stub
            outcome <- fetchManifest (rdRead eco){erRequest = unsealedRequest} passthroughTracingPort origin (thing eco)
            void outcome `shouldBe` Left (MetadataHttpFailure 302)
            map capPath <$> allCaptured stub `shouldReturn` ["/"]
  where
    -- http-client's own parse leaves redirect following on, which a sealed request turns off.
    unsealedRequest base _ _ = first (const EmptyBaseUrl) (parseRequest (toString base) :: Either SomeException Request)
    redirectOnce captured
        | capPath captured == "/moved" = (status200, [], toLazy (rdDocument eco "thing" 0))
        | otherwise = (status302, [(hLocation, "/moved")], "")

heldSpec :: Registered -> Spec
heldSpec eco = describe "held chunks, against the response" $ do
    it "gives the response's manifest and version read, and pays for the full read alone" $ do
        charges <- newIORef (0 :: Int)
        (full, selected) <- bothWays eco defaultLimits (\n -> modifyIORef' charges (+ n)) (\base -> toStrict (encode (rdReleases eco base ["1.2.3"])))
        fmap (\(info, _, _, _) -> Map.keys (infoVersions info)) full `shouldBe` Right ["1.2.3"]
        fmap (isJust . vrVersion) selected `shouldBe` Right True
        paid <- readIORef charges
        fmap (\(_, _, bytes, _) -> bytes) full `shouldBe` Right paid

    it "refuses a body over the limit, on a full and a selected read" $ do
        (full, selected) <- bothWays eco defaultLimits{maxMetadataBytes = 64} (const pass) (const (rdDocument eco "thing" 256))
        void full `shouldBe` Left (MetadataFetch (FetchBoundExceeded (BodyTooLarge (MetadataBodyLimit 64))))
        void selected `shouldBe` Left (MetadataFetch (FetchBoundExceeded (BodyTooLarge (MetadataBodyLimit 64))))

    it "refuses a malformed body as undecodable" $ do
        (full, selected) <- bothWays eco defaultLimits (const pass) (const (BS.take 12 (rdDocument eco "thing" 0)))
        void full `shouldBe` Left MetadataUndecodable
        void selected `shouldBe` Left MetadataUndecodable

    it "refuses another package's document by the name it reports" $ do
        (full, selected) <- bothWays eco defaultLimits (const pass) (const (rdDocument eco "other" 0))
        void full `shouldBe` Left (MetadataNameMismatch "other")
        void selected `shouldBe` Left (MetadataNameMismatch "other")

    it "reports a version count over the limit as a bound the walk raised" $ do
        (full, _) <- bothWays eco defaultLimits{maxVersionCount = 1} (const pass) (\base -> toStrict (encode (rdReleases eco base ["1.2.3", "2.3.4"])))
        void full `shouldBe` Left (MetadataBoundExceeded (TooManyVersions 2 1))

{- The full and selected reads of one served document under the limits, from the response and from
the same bytes held in three chunks. The held reads must equal the response's. -}
bothWays :: Registered -> Limits -> (Int -> IO ()) -> (Text -> ByteString) -> IO (Either MetadataError ManifestFields, Either MetadataError VersionRead)
bothWays eco limits charge document =
    withRoutedStub (\captured -> (status200, [], toLazy (document ("http://" <> maybe "" decodeUtf8 (headerValue "Host" captured))))) $ \stub -> do
        served <- stubConfig loopbackRegistryUrl stub
        let origin = served{ocLimits = limits}
            terms = ReadTerms{rtLimits = limits, rtBaseUrl = originBaseUrl origin, rtChargeFullRead = charge}
            bytes = document (stubBaseUrl stub)
            (front, rest) = BS.splitAt (BS.length bytes `div` 3) bytes
            (middle, back) = BS.splitAt (BS.length rest `div` 2) rest
            held = heldBody [front, middle, back]
        fetched <- fetchManifest (rdRead eco) passthroughTracingPort origin (thing eco)
        heldFull <- readManifest (rdRead eco) passthroughTracingPort terms (thing eco) held
        fmap manifestFields heldFull `shouldBe` fmap manifestFields fetched
        fetchedVersion <- fetchVersion (rdRead eco) passthroughTracingPort origin (thing eco) (releaseOf eco)
        heldVersion <- readVersion (rdRead eco) passthroughTracingPort terms (thing eco) (releaseOf eco) held
        heldVersion `shouldBe` fetchedVersion
        pure (fmap manifestFields heldFull, heldVersion)

type ManifestFields = (PackageInfo, CachedDoc, Int, ContentDigest)

manifestFields :: Manifest -> ManifestFields
manifestFields manifest = (manifestInfo manifest, manifestRaw manifest, manifestBodyBytes manifest, manifestDigest manifest)

fetchFault :: Either MetadataError a -> Either FetchFault ()
fetchFault = \case
    Left (MetadataFetch fault) -> Left fault
    _ -> Right ()

-- One registered ecosystem: the read its adapter holds, and documents of its own format.
data Registered = Registered
    { rdEcosystem :: Ecosystem
    , rdRead :: EcosystemRead
    , rdDocument :: ByteString -> Int -> ByteString
    -- ^ A valid document with no release that reports the name, padded by that many bytes no read keeps.
    , rdReleases :: Text -> [Text] -> Value
    -- ^ A document for @thing@ with these releases, whose artifacts an origin at the base URL honours.
    }

registered :: [Registered]
registered =
    [ Registered
        { rdEcosystem = adapterEcosystem npmAdapter
        , rdRead = metadataRead (adapterMetadata npmAdapter)
        , rdDocument = \name padding -> "{\"name\":\"" <> name <> "\",\"versions\":{}" <> paddingMember padding
        , rdReleases = \base versions ->
            packumentValue
                "thing"
                "1.2.3"
                [(version, versionValue (versionSpec "thing" version (base <> "/thing/-/thing-" <> version <> ".tgz"))) | version <- versions]
                [Key.fromText version .= ("2020-01-01T00:00:00.000Z" :: Text) | version <- versions]
                []
        }
    , Registered
        { rdEcosystem = adapterEcosystem pypiAdapter
        , rdRead = metadataRead (adapterMetadata pypiAdapter)
        , rdDocument = \name padding -> "{\"meta\":{\"api-version\":\"1.0\"},\"name\":\"" <> name <> "\",\"files\":[]" <> paddingMember padding
        , rdReleases = \_ versions -> simpleIndex "thing" (filesNamed ["thing-" <> version <> ".tar.gz" | version <- versions])
        }
    ]
  where
    paddingMember padding = ",\"_padding\":\"" <> BS.replicate padding 0x78 <> "\"}"

thing :: Registered -> PackageName
thing eco = mkPackageName (rdEcosystem eco) Nothing "thing"

-- The release every selected read asks for, which both ecosystems key as written.
releaseOf :: Registered -> Version
releaseOf eco = mkVersion (rdEcosystem eco) "1.2.3"

statusOutcomes :: [(Int, Either MetadataError ())]
statusOutcomes =
    [(code, Right ()) | code <- [200, 201, 299]]
        <> [(code, Left (MetadataAuthorisationFailure code)) | code <- [401, 403]]
        <> [(404, Left MetadataAbsent)]
        <> [(code, Left (MetadataHttpFailure code)) | code <- [301, 304, 400, 408, 410, 429, 500, 503, 599]]
