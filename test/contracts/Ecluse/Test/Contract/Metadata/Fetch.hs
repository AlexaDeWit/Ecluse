-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Metadata transport laws shared by every registered ecosystem.
module Ecluse.Test.Contract.Metadata.Fetch (
    ReadFixture (..),
    metadataFetchContract,
    metadataStatusLaw,
) where

import Codec.Compression.GZip qualified as GZip
import Control.Exception (throwIO)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Client (Request, defaultManagerSettings, newManager, parseRequest, path)
import Network.HTTP.Types (ResponseHeaders, Status, hLocation, mkStatus, status200, status302)
import Network.HTTP.Types.Header (hContentEncoding)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Package (PackageInfo (infoInvalidEntries, infoVersions), PackageName, mkPackageName)
import Ecluse.Core.Registry (FetchFault (FetchBoundExceeded, FetchUrlUnformable), UrlFormationError (EmptyBaseUrl, UnparseableUrl))
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Metadata (
    ContentDigest,
    Manifest (..),
    MetadataClient (fetchFullManifest, fetchVersionMetadata),
    MetadataError (MetadataAbsent, MetadataAuthorisationFailure, MetadataBoundExceeded, MetadataFetch, MetadataHttpFailure, MetadataNameMismatch, MetadataUndecodable),
    VersionRead (vrBodyBytes, vrVersion),
 )
import Ecluse.Core.Registry.Metadata.Fetch (chargingSelectedReads, readManifest, readVersion)
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (PrepareRead (..), prepareManifest, prepareRead, prepareVersion)
import Ecluse.Core.Registry.Metadata.Fetch.Types (EcosystemRead (erRequest), ReadTerms (..))
import Ecluse.Core.Registry.Origin (OriginClient (ocChargeFullRead, ocLimits), chargingFullReads, originBaseUrl, originClient, perCallerOrigin)
import Ecluse.Core.Registry.Request (baseRequestUrl, mkBaseRequest)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge, TooManyVersions), Limits (maxMetadataBytes, maxVersionCount), defaultLimits)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.MemoryModel (bufferedChunkCharge)
import Ecluse.Core.Server.Metadata (ecosystemMetadataReads, privateMetadataClient)
import Ecluse.Core.Telemetry.Span (TracingPort (spanMetadataDecode, spanMetadataFetch))
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Metadata.Projection (MetadataFraming (..))
import Ecluse.Test.Deadline (withExchangeClient, withSweeper)
import Ecluse.Test.Port (noopMetricsPort, passthroughTracingPort)
import Ecluse.Test.Registry (isBoundExceededFetch, isTransportFetch)
import Ecluse.Test.Registry.Metadata.Fetch (heldBody)
import Ecluse.Test.Snapshot (digestOf)
import Ecluse.Test.Stub (Captured (capPath), Stub, allCaptured, headerValue, stubBaseUrl, stubConfig, withRoutedStub)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape), expectRight)

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
            metadataStatusLaw eco (code, expected)

    fetchCase eco "full-charge" "charges preparation and full source bytes while selected source bytes add no charge" $
        withReadStub eco status200 [] (const (toLazy document)) $ \stub -> do
            withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
                charges <- newIORef (0 :: Int)
                let origin = chargingFullReads (\n -> modifyIORef' charges (+ n)) (perCallerOrigin defaultLimits exchanges (mkBaseRequest (loopbackRegistryUrl (stubBaseUrl stub))) Nothing)
                    client = privateMetadataClient (ecosystemMetadataReads (rdPrepareRead eco) passthroughTracingPort noopMetricsPort (\_ _ -> pass) (\_ _ -> pass) (const pass) origin)
                _ <- fetchVersionMetadata client (thing eco) (releaseOf eco)
                let preparation = preparationCharge eco (stubBaseUrl stub)
                readIORef charges `shouldReturn` preparation
                _ <- fetchFullManifest client (thing eco)
                readIORef charges `shouldReturn` (2 * preparation + BS.length document)

    for_ [401, 403] $ \code ->
        fetchCase eco ("oversized-auth-" <> show code) ("retains HTTP " <> show code <> " before an oversized error body") $
            withReadStub eco (mkStatus code "test") [] (const (toLazy (rdDocument eco "thing" 256))) $ \stub -> withSweeper $ \deadlines -> do
                origin <- stubConfig deadlines loopbackRegistryUrl stub
                outcome <- prepareManifest (rdPrepareRead eco) passthroughTracingPort origin{ocLimits = defaultLimits{maxMetadataBytes = 64}} (thing eco)
                void outcome `shouldBe` Left (MetadataAuthorisationFailure code)
  where
    document = rdDocument eco "thing" 0

-- | Apply the canonical full and selected status checks to a registered reader factory.
metadataStatusLaw :: ReadFixture -> (Int, Either MetadataError ()) -> IO ()
metadataStatusLaw eco (code, expected) =
    withReadStub eco (mkStatus code "test") [] (const (toLazy document)) $ \stub -> do
        withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
            events <- newIORef ([] :: [(Text, PackageName)])
            let record phase who action = modifyIORef' events (<> [(phase, who)]) >> action
                tracing = passthroughTracingPort{spanMetadataFetch = record "fetch", spanMetadataDecode = record "decode"}
                origin = perCallerOrigin defaultLimits exchanges (mkBaseRequest (loopbackRegistryUrl (stubBaseUrl stub))) Nothing
                client = privateMetadataClient (ecosystemMetadataReads (rdPrepareRead eco) tracing noopMetricsPort (\_ _ -> pass) (\_ _ -> pass) (const pass) origin)
            full <- fetchFullManifest client (thing eco)
            single <- fetchVersionMetadata client (thing eco) (releaseOf eco)
            void full `shouldBe` expected
            void single `shouldBe` expected
            -- The decode span opens only after success headers.
            readIORef events `shouldReturn` concat (replicate 2 ([("fetch", thing eco)] <> [("decode", thing eco) | isRight expected]))
            when (isRight expected) $ do
                fmap manifestBodyBytes full `shouldBe` Right (BS.length document)
                fmap manifestDigest full `shouldBe` Right (digestOf document)
                fmap vrBodyBytes single `shouldBe` Right (BS.length document)
  where
    document = rdDocument eco "thing" 0

responseBoundSpec :: ReadFixture -> Spec
responseBoundSpec eco = describe "the body bound on a response" $ do
    fetchCase eco "exact-byte-bound" "digests the complete source body within maxMetadataBytes" $
        withReadStub eco status200 [] (const (toLazy small)) $ \stub -> withSweeper $ \deadlines -> do
            origin <- stubConfig deadlines loopbackRegistryUrl stub
            manifest <- prepareManifest (rdPrepareRead eco) passthroughTracingPort origin{ocLimits = defaultLimits{maxMetadataBytes = BS.length small}} (thing eco)
            fmap manifestDigest manifest `shouldBe` Right (digestOf small)

    fetchCase eco "gzip-bytes" "reports decompressed bytes for an accepted gzip body" $
        withGzipped padded $ \origin -> do
            manifest <- prepareManifest (rdPrepareRead eco) passthroughTracingPort origin{ocLimits = defaultLimits{maxMetadataBytes = BS.length padded}} (thing eco)
            fmap manifestBodyBytes manifest `shouldBe` Right (BS.length padded)
            fmap manifestDigest manifest `shouldBe` Right (digestOf padded)

    fetchCase eco "gzip-charge" "charges a full read for every decompressed byte it hands the parser" $
        withGzipped padded $ \origin -> do
            charges <- newIORef []
            _ <- prepareManifest (rdPrepareRead eco) passthroughTracingPort origin{ocLimits = defaultLimits{maxMetadataBytes = BS.length padded}, ocChargeFullRead = \n -> modifyIORef' charges (n :)} (thing eco)
            recorded <- readIORef charges
            sum recorded `shouldBe` (BS.length padded + preparationCharge eco (originBaseUrl origin))

    fetchCase eco "gzip-bound" "bounds decompressed size: a small gzip body that inflates past the cap is refused" $ do
        -- The compressed body is under the cap, so only the decompressed-size bound explains a refusal.
        BS.length (toStrict (GZip.compress (toLazy inflating))) `shouldSatisfy` (< 1024)
        withGzipped inflating $ \origin -> do
            outcome <- prepareManifest (rdPrepareRead eco) passthroughTracingPort origin{ocLimits = defaultLimits{maxMetadataBytes = 1024}} (thing eco)
            fetchFault outcome `shouldSatisfy` isBoundExceededFetch
  where
    small = rdDocument eco "thing" 0
    padded = rdDocument eco "thing" 256
    inflating = rdDocument eco "thing" 65536
    withGzipped body action =
        withSweeper $ \deadlines -> withReadStub eco status200 [(hContentEncoding, "gzip")] (const (GZip.compress (toLazy body))) (stubConfig deadlines loopbackRegistryUrl >=> action)

requestSpec :: ReadFixture -> Spec
requestSpec eco = describe "the request" $ do
    for_ [("empty-url", ""), ("refused-connection", "http://127.0.0.1:1")] $ \(identifier, url) ->
        fetchCase eco ("decode-span-" <> identifier) ("does not open a decode span for request failure at " <> url) $ do
            withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
                count <- newIORef (0 :: Int)
                let tracing = passthroughTracingPort{spanMetadataDecode = \_ action -> modifyIORef' count (+ 1) >> action}
                outcome <- prepareManifest (rdPrepareRead eco) tracing (originClient defaultLimits exchanges (loopbackRegistryUrl url) Nothing) (thing eco)
                void outcome `shouldSatisfy` isLeft
                readIORef count `shouldReturn` 0

    fetchCase eco "empty-url-fault" "reports an empty base URL as a FetchUrlUnformable value, never thrown" $ do
        withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
            outcome <- prepareManifest (rdPrepareRead eco) passthroughTracingPort (originClient defaultLimits exchanges (loopbackRegistryUrl "") Nothing) (thing eco)
            void outcome `shouldBe` Left (MetadataFetch (FetchUrlUnformable EmptyBaseUrl))

    fetchCase eco "transport-fault" "reports a refused connection as a FetchTransport value, never thrown" $ do
        -- Port 1 on the loopback is privileged and unbound, so the kernel refuses the connect.
        withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
            outcome <- prepareManifest (rdPrepareRead eco) passthroughTracingPort (originClient defaultLimits exchanges (loopbackRegistryUrl "http://127.0.0.1:1") Nothing) (thing eco)
            fetchFault outcome `shouldSatisfy` isTransportFetch

    fetchCase eco "no-redirect" "follows no redirect when the ecosystem's request builder did not seal its request" $
        withRoutedStub redirectOnce $ \stub -> withSweeper $ \deadlines -> do
            origin <- stubConfig deadlines loopbackRegistryUrl stub
            let factory = PrepareRead (fmap (fmap unseal) . prepareRead (rdPrepareRead eco))
            outcome <- prepareManifest factory passthroughTracingPort origin (thing eco)
            void outcome `shouldBe` Left (MetadataHttpFailure 302)
            map capPath <$> allCaptured stub `shouldReturn` (["/config.json" | isJust (rdDiscovery eco)] <> ["/"])
  where
    unseal nativeRead = nativeRead{erRequest = unsealedRequest}
    -- http-client's own parse leaves redirect following on, which a sealed request turns off.
    unsealedRequest base _ _ = first (const EmptyBaseUrl) (parseRequest (toString (registryUrlText (baseRequestUrl base))) :: Either SomeException Request)
    redirectOnce captured
        | capPath captured == "/config.json", Just discovery <- rdDiscovery eco = (status200, [], toLazy (discovery (captureBase captured)))
        | capPath captured == "/moved" = (status200, [], toLazy (rdDocument eco "thing" 0))
        | otherwise = (status302, [(hLocation, "/moved")], "")

heldSpec :: ReadFixture -> Spec
heldSpec eco = describe "held chunks, against the response" $ do
    fetchCase eco "selected-worker-charge" "charges an explicitly wrapped selected read as an absolute per-read requirement" $ do
        paid <- newIORef (0 :: Int)
        let base = "https://registry.example.test"
            bytes = rdReleases eco base ["1.2.3"]
            (front, back) = BS.splitAt (BS.length bytes `div` 2) bytes
            terms = ReadTerms defaultLimits base (const pass)
        nativeRead <- expectRight (rdRead eco base)
        let charged = chargingSelectedReads BS.length (writeIORef paid) nativeRead
        outcome <- readVersion charged passthroughTracingPort terms (thing eco) (releaseOf eco) (heldBody [front, back])
        fmap (isJust . vrVersion) outcome `shouldBe` Right True
        readIORef paid `shouldReturn` BS.length bytes
    fetchCase eco "selected-worker-refusal" "pays before parsing and propagates a payment refusal" $ do
        let rejected = TestContractEscape "selected read payment refused"
            terms = ReadTerms defaultLimits "https://registry.example.test" (const pass)
        nativeRead <- expectRight (rdRead eco "https://registry.example.test")
        let charged = chargingSelectedReads BS.length (const (throwIO rejected)) nativeRead
        readVersion charged passthroughTracingPort terms (thing eco) (releaseOf eco) (heldBody ["invalid JSON"])
            `shouldThrow` (== rejected)

    fetchCase eco "held-parity" "gives the response's manifest and version read, and pays for the full read alone" $ do
        charges <- newIORef (0 :: Int)
        (full, selected) <- bothWays eco defaultLimits (\n -> modifyIORef' charges (+ n)) (\base -> rdReleases eco base ["1.2.3"])
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
        assertHeldFailure eco MetadataUndecodable full selected

    case rdFraming eco of
        CallerBoundRecords -> fetchCase eco "held-origin-mismatch" "binds the requested name and rejects another source origin" $ do
            let base = "https://registry.example.test/"
                foreignBase = "https://other.example.test/"
                terms = ReadTerms defaultLimits foreignBase (const pass)
                bytes = rdReleases eco base ["1.2.3"]
            nativeRead <- expectRight (rdRead eco base)
            let own = mkBaseRequest (loopbackRegistryUrl base)
                foreignRequest = mkBaseRequest (loopbackRegistryUrl foreignBase)
            request <- expectRight (erRequest nativeRead own Nothing (thing eco))
            expectedPath <- maybe (fail "caller-bound fixture has no expected native request path") pure (rdCallerBoundPath eco)
            path request `shouldBe` expectedPath
            case erRequest nativeRead foreignRequest Nothing (thing eco) of
                Left (UnparseableUrl _) -> pass
                _ -> expectationFailure "caller-bound reader accepted another request origin"
            full <- readManifest nativeRead passthroughTracingPort terms (thing eco) (heldBody [bytes])
            selected <- readVersion nativeRead passthroughTracingPort terms (thing eco) (releaseOf eco) (heldBody [bytes])
            void full `shouldBe` Left MetadataUndecodable
            void selected `shouldBe` Left MetadataUndecodable
        _ -> fetchCase eco "held-name-mismatch" "refuses another package's document by the name it reports" $ do
            (full, selected) <- bothWays eco defaultLimits (const pass) (const (rdDocument eco "other" 0))
            assertHeldFailure eco (MetadataNameMismatch "other") full selected

    fetchCase eco "held-version-bound" "reports a version count over the limit as a bound the walk raised" $ do
        (full, _) <- bothWays eco defaultLimits{maxVersionCount = 1} (const pass) (\base -> rdReleases eco base ["1.2.3", "2.3.4"])
        void full `shouldBe` Left (MetadataBoundExceeded (TooManyVersions 2 1))

{- The full and selected reads of one served document under the limits, from the response and from
the same bytes held in three chunks. The held reads must equal the response's. -}
bothWays :: ReadFixture -> Limits -> (Int -> IO ()) -> (Text -> ByteString) -> IO (Either MetadataError ManifestFields, Either MetadataError VersionRead)
bothWays eco limits charge document =
    withReadStub eco status200 [] (toLazy . document) $ \stub -> withSweeper $ \deadlines -> do
        served <- stubConfig deadlines loopbackRegistryUrl stub
        let origin = served{ocLimits = limits}
            terms = ReadTerms{rtLimits = limits, rtBaseUrl = originBaseUrl origin, rtChargeFullRead = charge}
            bytes = document (stubBaseUrl stub)
            (front, rest) = BS.splitAt (BS.length bytes `div` 3) bytes
            (middle, back) = BS.splitAt (BS.length rest `div` 2) rest
            held = heldBody [front, middle, back]
        nativeRead <- expectRight (rdRead eco (stubBaseUrl stub))
        fetched <- prepareManifest (rdPrepareRead eco) passthroughTracingPort origin (thing eco)
        heldFull <- readManifest nativeRead passthroughTracingPort terms (thing eco) held
        fmap manifestFields heldFull `shouldBe` fmap manifestFields fetched
        fetchedVersion <- prepareVersion (rdPrepareRead eco) passthroughTracingPort origin (thing eco) (releaseOf eco)
        heldVersion <- readVersion nativeRead passthroughTracingPort terms (thing eco) (releaseOf eco) held
        heldVersion `shouldBe` fetchedVersion
        pure (fmap manifestFields heldFull, heldVersion)

type ManifestFields = (PackageInfo, CachedDoc, Int, ContentDigest)

manifestFields :: Manifest -> ManifestFields
manifestFields manifest = (manifestInfo manifest, manifestRaw manifest, manifestBodyBytes manifest, manifestDigest manifest)

fetchFault :: Either MetadataError a -> Either FetchFault ()
fetchFault = \case
    Left (MetadataFetch fault) -> Left fault
    _ -> Right ()

-- | A registered factory, held-body reader, and documents with controlled names and releases.
data ReadFixture = ReadFixture
    { rdEcosystem :: Ecosystem
    , rdPrepareRead :: PrepareRead
    -- ^ The registered adapter factory used by every network read.
    , rdRead :: Text -> Either MetadataError EcosystemRead
    -- ^ The explicit reader used only for held bodies.
    , rdFraming :: MetadataFraming
    , rdCallerBoundPath :: Maybe ByteString
    -- ^ The native checked-name route is required for a caller-bound provider.
    , rdDiscovery :: Maybe (Text -> ByteString)
    -- ^ Native preparation bytes served only from the selected origin's config route.
    , rdDocument :: ByteString -> Int -> ByteString
    -- ^ A valid named document with that many padding bytes.
    , rdReleases :: Text -> [Text] -> ByteString
    -- ^ A document for @thing@ with these releases, whose artifacts an origin at the base URL honours.
    }

thing :: ReadFixture -> PackageName
thing eco = mkPackageName (rdEcosystem eco) Nothing "thing"

-- The release every selected read asks for, which every ecosystem keys as written.
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

withReadStub :: ReadFixture -> Status -> ResponseHeaders -> (Text -> LByteString) -> (Stub -> IO a) -> IO a
withReadStub eco status headers document = withRoutedStub $ \captured ->
    let base = captureBase captured
     in case rdDiscovery eco of
            Just discovery | capPath captured == "/config.json" -> (status200, [], toLazy (discovery base))
            _ -> (status, headers, document base)

captureBase :: Captured -> Text
captureBase captured = "http://" <> maybe "" decodeUtf8 (headerValue "Host" captured)

preparationCharge :: ReadFixture -> Text -> Int
preparationCharge eco base = maybe 0 (bufferedChunkCharge . BS.length . ($ base)) (rdDiscovery eco)

assertHeldFailure :: ReadFixture -> MetadataError -> Either MetadataError ManifestFields -> Either MetadataError VersionRead -> Expectation
assertHeldFailure eco failure full selected = case rdFraming eco of
    SingleDocument -> do
        void full `shouldBe` Left failure
        void selected `shouldBe` Left failure
    CallerBoundRecords -> invalidRecords
    RecordStream -> invalidRecords
  where
    invalidRecords = do
        (info, _, _, _) <- expectRight full
        infoVersions info `shouldBe` Map.empty
        infoInvalidEntries info `shouldSatisfy` not . null
        vrVersion <$> selected `shouldBe` Right Nothing
