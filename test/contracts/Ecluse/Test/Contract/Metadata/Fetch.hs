-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Metadata transport laws shared by every registered ecosystem.
module Ecluse.Test.Contract.Metadata.Fetch (ReadFixture (..), metadataFetchContract) where

import Codec.Compression.GZip qualified as GZip
import Control.Exception (throwIO)
import Data.Aeson (Value, encode)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))
import Network.HTTP.Client (Request, defaultManagerSettings, newManager, parseRequest)
import Network.HTTP.Types (hLocation, mkStatus, status200, status302)
import Network.HTTP.Types.Header (hContentEncoding)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Package (PackageInfo (infoVersions), PackageName, mkPackageName)
import Ecluse.Core.Registry (FetchFault (FetchBoundExceeded, FetchUrlUnformable), UrlFormationError (EmptyBaseUrl))
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Metadata (
    ContentDigest,
    Manifest (..),
    MetadataClient (fetchFullManifest, fetchVersionMetadata),
    MetadataError (MetadataAbsent, MetadataAuthorisationFailure, MetadataBoundExceeded, MetadataFetch, MetadataHttpFailure, MetadataNameMismatch, MetadataUndecodable),
    VersionRead (vrBodyBytes, vrVersion),
 )
import Ecluse.Core.Registry.Metadata.Fetch (chargingSelectedReads, fetchManifest, fetchVersion, readManifest, readVersion)
import Ecluse.Core.Registry.Metadata.Fetch.Types (EcosystemRead (erRequest), ReadTerms (..))
import Ecluse.Core.Registry.Origin (OriginClient (ocChargeFullRead, ocLimits), chargingFullReads, originBaseUrl, originClient, perCallerOrigin)
import Ecluse.Core.Registry.Request (baseRequestUrl, mkBaseRequest)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge, TooManyVersions), Limits (maxMetadataBytes, maxVersionCount), defaultLimits)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Metadata (ecosystemMetadataReads, privateMetadataClient)
import Ecluse.Core.Telemetry.Span (TracingPort (spanMetadataDecode, spanMetadataFetch))
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Deadline (withExchangeClient, withSweeper)
import Ecluse.Test.Port (noopMetricsPort, passthroughTracingPort)
import Ecluse.Test.Registry (isBoundExceededFetch, isTransportFetch)
import Ecluse.Test.Registry.Metadata.Fetch (heldBody)
import Ecluse.Test.Snapshot (digestOf)
import Ecluse.Test.Stub (Captured (capPath), allCaptured, headerValue, stubBaseUrl, stubConfig, withRoutedStub, withStub, withStubHeaders)

-- | Check response, held-body, status and bound behaviour with native documents.
metadataFetchContract :: ReadFixture -> Spec
metadataFetchContract eco = do
    statusSpec eco
    responseBoundSpec eco
    requestSpec eco
    heldSpec eco

statusSpec :: ReadFixture -> Spec
statusSpec eco = describe "upstream statuses" $ do
    for_ statusOutcomes $ \(code, expected) ->
        fetchCase eco ("status-" <> show code) ("preserves HTTP " <> show code <> " over a valid manifest body on full and version reads") $
            withStub (mkStatus code "test") (toLazy document) $ \stub -> do
                withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
                    events <- newIORef ([] :: [(Text, PackageName)])
                    let record phase who action = modifyIORef' events (<> [(phase, who)]) >> action
                        tracing = passthroughTracingPort{spanMetadataFetch = record "fetch", spanMetadataDecode = record "decode"}
                        origin = perCallerOrigin defaultLimits exchanges (mkBaseRequest (loopbackRegistryUrl (stubBaseUrl stub))) Nothing
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

    fetchCase eco "full-charge" "charges a full read for every source byte and a selected read for none" $
        withStub status200 (toLazy document) $ \stub -> do
            withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
                charges <- newIORef (0 :: Int)
                let origin = chargingFullReads (\n -> modifyIORef' charges (+ n)) (perCallerOrigin defaultLimits exchanges (mkBaseRequest (loopbackRegistryUrl (stubBaseUrl stub))) Nothing)
                    client = privateMetadataClient (ecosystemMetadataReads (rdRead eco) passthroughTracingPort noopMetricsPort (\_ _ -> pass) (\_ _ -> pass) (const pass) origin)
                _ <- fetchVersionMetadata client (thing eco) (releaseOf eco)
                readIORef charges `shouldReturn` 0
                _ <- fetchFullManifest client (thing eco)
                readIORef charges `shouldReturn` BS.length document

    for_ [401, 403] $ \code ->
        fetchCase eco ("oversized-auth-" <> show code) ("retains HTTP " <> show code <> " before an oversized error body") $
            withStub (mkStatus code "test") (toLazy (rdDocument eco "thing" 256)) $ \stub -> withSweeper $ \deadlines -> do
                origin <- stubConfig deadlines loopbackRegistryUrl stub
                outcome <- fetchManifest (rdRead eco) passthroughTracingPort origin{ocLimits = defaultLimits{maxMetadataBytes = 64}} (thing eco)
                void outcome `shouldBe` Left (MetadataAuthorisationFailure code)
  where
    document = rdDocument eco "thing" 0

responseBoundSpec :: ReadFixture -> Spec
responseBoundSpec eco = describe "the body bound on a response" $ do
    fetchCase eco "exact-byte-bound" "digests the complete source body within maxMetadataBytes" $
        withStub status200 (toLazy small) $ \stub -> withSweeper $ \deadlines -> do
            origin <- stubConfig deadlines loopbackRegistryUrl stub
            manifest <- fetchManifest (rdRead eco) passthroughTracingPort origin{ocLimits = defaultLimits{maxMetadataBytes = BS.length small}} (thing eco)
            fmap manifestDigest manifest `shouldBe` Right (digestOf small)

    fetchCase eco "gzip-bytes" "reports decompressed bytes for an accepted gzip body" $
        withGzipped padded $ \origin -> do
            manifest <- fetchManifest (rdRead eco) passthroughTracingPort origin{ocLimits = defaultLimits{maxMetadataBytes = BS.length padded}} (thing eco)
            fmap manifestBodyBytes manifest `shouldBe` Right (BS.length padded)
            fmap manifestDigest manifest `shouldBe` Right (digestOf padded)

    fetchCase eco "gzip-charge" "charges a full read for every decompressed byte it hands the parser" $
        withGzipped padded $ \origin -> do
            charges <- newIORef []
            _ <- fetchManifest (rdRead eco) passthroughTracingPort origin{ocLimits = defaultLimits{maxMetadataBytes = BS.length padded}, ocChargeFullRead = \n -> modifyIORef' charges (n :)} (thing eco)
            recorded <- readIORef charges
            sum recorded `shouldBe` BS.length padded

    fetchCase eco "gzip-bound" "bounds decompressed size: a small gzip body that inflates past the cap is refused" $ do
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
        withSweeper $ \deadlines -> withStubHeaders status200 [(hContentEncoding, "gzip")] (GZip.compress (toLazy body)) (stubConfig deadlines loopbackRegistryUrl >=> action)

requestSpec :: ReadFixture -> Spec
requestSpec eco = describe "the request" $ do
    for_ [("empty-url", ""), ("refused-connection", "http://127.0.0.1:1")] $ \(identifier, url) ->
        fetchCase eco ("decode-span-" <> identifier) ("does not open a decode span for request failure at " <> url) $ do
            withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
                count <- newIORef (0 :: Int)
                let tracing = passthroughTracingPort{spanMetadataDecode = \_ action -> modifyIORef' count (+ 1) >> action}
                outcome <- fetchManifest (rdRead eco) tracing (originClient defaultLimits exchanges (loopbackRegistryUrl url) Nothing) (thing eco)
                void outcome `shouldSatisfy` isLeft
                readIORef count `shouldReturn` 0

    fetchCase eco "empty-url-fault" "reports an empty base URL as a FetchUrlUnformable value, never thrown" $ do
        withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
            outcome <- fetchManifest (rdRead eco) passthroughTracingPort (originClient defaultLimits exchanges (loopbackRegistryUrl "") Nothing) (thing eco)
            void outcome `shouldBe` Left (MetadataFetch (FetchUrlUnformable EmptyBaseUrl))

    fetchCase eco "transport-fault" "reports a refused connection as a FetchTransport value, never thrown" $ do
        -- Port 1 on the loopback is privileged and unbound, so the kernel refuses the connect.
        withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
            outcome <- fetchManifest (rdRead eco) passthroughTracingPort (originClient defaultLimits exchanges (loopbackRegistryUrl "http://127.0.0.1:1") Nothing) (thing eco)
            fetchFault outcome `shouldSatisfy` isTransportFetch

    fetchCase eco "no-redirect" "follows no redirect when the ecosystem's request builder did not seal its request" $
        withRoutedStub redirectOnce $ \stub -> withSweeper $ \deadlines -> do
            origin <- stubConfig deadlines loopbackRegistryUrl stub
            outcome <- fetchManifest (rdRead eco){erRequest = unsealedRequest} passthroughTracingPort origin (thing eco)
            void outcome `shouldBe` Left (MetadataHttpFailure 302)
            map capPath <$> allCaptured stub `shouldReturn` ["/"]
  where
    -- http-client's own parse leaves redirect following on, which a sealed request turns off.
    unsealedRequest base _ _ = first (const EmptyBaseUrl) (parseRequest (toString (registryUrlText (baseRequestUrl base))) :: Either SomeException Request)
    redirectOnce captured
        | capPath captured == "/moved" = (status200, [], toLazy (rdDocument eco "thing" 0))
        | otherwise = (status302, [(hLocation, "/moved")], "")

heldSpec :: ReadFixture -> Spec
heldSpec eco = describe "held chunks, against the response" $ do
    fetchCase eco "selected-worker-charge" "charges an explicitly wrapped selected read as an absolute per-read requirement" $ do
        paid <- newIORef (0 :: Int)
        let base = "https://registry.example.test"
            bytes = toStrict (encode (rdReleases eco base ["1.2.3"]))
            (front, back) = BS.splitAt (BS.length bytes `div` 2) bytes
            terms = ReadTerms defaultLimits base (const pass)
            charged = chargingSelectedReads BS.length (writeIORef paid) (rdRead eco)
        outcome <- readVersion charged passthroughTracingPort terms (thing eco) (releaseOf eco) (heldBody [front, back])
        fmap (isJust . vrVersion) outcome `shouldBe` Right True
        readIORef paid `shouldReturn` BS.length bytes
    fetchCase eco "selected-worker-refusal" "pays before parsing and propagates a payment refusal" $ do
        let rejected = TestContractEscape "selected read payment refused"
            charged = chargingSelectedReads BS.length (const (throwIO rejected)) (rdRead eco)
            terms = ReadTerms defaultLimits "https://registry.example.test" (const pass)
        readVersion charged passthroughTracingPort terms (thing eco) (releaseOf eco) (heldBody ["invalid JSON"])
            `shouldThrow` (== rejected)

    fetchCase eco "held-parity" "gives the response's manifest and version read, and pays for the full read alone" $ do
        charges <- newIORef (0 :: Int)
        (full, selected) <- bothWays eco defaultLimits (\n -> modifyIORef' charges (+ n)) (\base -> toStrict (encode (rdReleases eco base ["1.2.3"])))
        fmap (\(info, _, _, _) -> Map.keys (infoVersions info)) full `shouldBe` Right ["1.2.3"]
        fmap (isJust . vrVersion) selected `shouldBe` Right True
        paid <- readIORef charges
        fmap (\(_, _, bytes, _) -> bytes) full `shouldBe` Right paid

    fetchCase eco "held-byte-bound" "refuses a body over the limit, on a full and a selected read" $ do
        (full, selected) <- bothWays eco defaultLimits{maxMetadataBytes = 64} (const pass) (const (rdDocument eco "thing" 256))
        void full `shouldBe` Left (MetadataFetch (FetchBoundExceeded (BodyTooLarge (MetadataBodyLimit 64))))
        void selected `shouldBe` Left (MetadataFetch (FetchBoundExceeded (BodyTooLarge (MetadataBodyLimit 64))))

    fetchCase eco "held-malformed" "refuses a malformed body as undecodable" $ do
        (full, selected) <- bothWays eco defaultLimits (const pass) (const (BS.take 12 (rdDocument eco "thing" 0)))
        void full `shouldBe` Left MetadataUndecodable
        void selected `shouldBe` Left MetadataUndecodable

    fetchCase eco "held-name-mismatch" "refuses another package's document by the name it reports" $ do
        (full, selected) <- bothWays eco defaultLimits (const pass) (const (rdDocument eco "other" 0))
        void full `shouldBe` Left (MetadataNameMismatch "other")
        void selected `shouldBe` Left (MetadataNameMismatch "other")

    fetchCase eco "held-version-bound" "reports a version count over the limit as a bound the walk raised" $ do
        (full, _) <- bothWays eco defaultLimits{maxVersionCount = 1} (const pass) (\base -> toStrict (encode (rdReleases eco base ["1.2.3", "2.3.4"])))
        void full `shouldBe` Left (MetadataBoundExceeded (TooManyVersions 2 1))

{- The full and selected reads of one served document under the limits, from the response and from
the same bytes held in three chunks. The held reads must equal the response's. -}
bothWays :: ReadFixture -> Limits -> (Int -> IO ()) -> (Text -> ByteString) -> IO (Either MetadataError ManifestFields, Either MetadataError VersionRead)
bothWays eco limits charge document =
    withRoutedStub (\captured -> (status200, [], toLazy (document ("http://" <> maybe "" decodeUtf8 (headerValue "Host" captured))))) $ \stub -> withSweeper $ \deadlines -> do
        served <- stubConfig deadlines loopbackRegistryUrl stub
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

-- | A native read implementation and documents with controlled names and releases.
data ReadFixture = ReadFixture
    { rdEcosystem :: Ecosystem
    , rdRead :: EcosystemRead
    , rdDocument :: ByteString -> Int -> ByteString
    -- ^ A valid document with no release that reports the name, padded by that many bytes no read keeps.
    , rdReleases :: Text -> [Text] -> Value
    -- ^ A document for @thing@ with these releases, whose artifacts an origin at the base URL honours.
    }

thing :: ReadFixture -> PackageName
thing eco = mkPackageName (rdEcosystem eco) Nothing "thing"

-- The release every selected read asks for, which both ecosystems key as written.
releaseOf :: ReadFixture -> Version
releaseOf eco = mkVersion (rdEcosystem eco) "1.2.3"

statusOutcomes :: [(Int, Either MetadataError ())]
statusOutcomes =
    [(code, Right ()) | code <- [200, 201, 299]]
        <> [(code, Left (MetadataAuthorisationFailure code)) | code <- [401, 403]]
        <> [(404, Left MetadataAbsent)]
        <> [(code, Left (MetadataHttpFailure code)) | code <- [301, 304, 400, 408, 410, 429, 500, 503, 599]]

fetchCase :: ReadFixture -> Text -> Text -> IO () -> Spec
fetchCase eco identifier description = it (caseLabel ("core.metadata.fetch." <> T.toLower (show (rdEcosystem eco)) <> "." <> identifier) description)
