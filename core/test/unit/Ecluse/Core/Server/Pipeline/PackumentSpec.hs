-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Validators, coding negotiation, first-party private misses, and the output charge's basis.
module Ecluse.Core.Server.Pipeline.PackumentSpec (spec) where

import Codec.Compression.GZip qualified as GZip
import Control.Concurrent.MVar qualified as MVar
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (Value (String), encode, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian, nominalDay)
import Network.HTTP.Types (Header, Method, methodGet, methodHead, status200, status403, status404, statusCode)
import Network.HTTP.Types.Header (RequestHeaders, hAcceptEncoding, hAuthorization, hContentEncoding, hContentLength, hETag, hIfNoneMatch, hVary)
import Network.Wai (Application, Request (rawPathInfo, requestHeaders, requestMethod), Response, defaultRequest, responseHeaders, responseLBS, responseStatus)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
import UnliftIO.Async (cancel, race_, wait, withAsync)
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName, pkgEcosystem)
import Ecluse.Core.Package.Entry (EntryKey (..))
import Ecluse.Core.Package.Merge (Provenance (GatedSource, TrustedSource), mergePackuments)
import Ecluse.Core.Registry.Metadata (ContentDigest, Manifest (manifestDigest, manifestInfo, manifestRaw))
import Ecluse.Core.Registry.Npm.Credential (npmCredential)
import Ecluse.Core.Registry.Npm.Route (npmRouter)
import Ecluse.Core.Registry.PyPI.Credential (pypiCredential)
import Ecluse.Core.Registry.PyPI.Route (pypiRouter)
import Ecluse.Core.Registry.Request (CredentialMapping)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Rules.Types (Rule (AllowIfOlderThan))
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission (newServeAdmissionTuned)
import Ecluse.Core.Server.Admission.Meter (MeterSettings (..), meterSnapshot, newMemoryMeter, takeLargestCharge)
import Ecluse.Core.Server.Admission.Types (MeterSnapshot (snChargedBytes))
import Ecluse.Core.Server.Conditional (ETag, mkStrongETag, renderETag)
import Ecluse.Core.Server.Context (
    MountBinding (..),
    MountRouter,
    PackumentDeps (pdInboundToken, pdMetadataCompression, pdMountBaseUrl, pdRules),
    ResponseAction (RunPipeline),
    RouteAction (RouteAction),
    ServeRuntime (srMemoryMeter),
 )
import Ecluse.Core.Server.Encoding (ContentCoding (GzipCoding, IdentityCoding), MetadataCompression (GzipWhenAccepted, IdentityOnly))
import Ecluse.Core.Server.MetadataGzip (gzipBufferChargeBytes, gzipWorkspaceBytes)
import Ecluse.Core.Server.Pipeline.Internal (denialLabels, packumentServeDecision)
import Ecluse.Core.Server.Pipeline.Origin (Contribution (..), OriginMiss (MissAbsent, MissUnresolved))
import Ecluse.Core.Server.Pipeline.Packument (
    PackumentReplies (..),
    codedETag,
    firstPartyMissDecision,
    firstPartyMissReply,
    outputBasisBytes,
    packumentETag,
 )
import Ecluse.Core.Server.Response (
    RejectReason (Unavailable),
    ServeDecision,
    Transience (WillResolve),
 )
import Ecluse.Core.Server.Upstream (MirrorServePlan (NoMirrorWrite))
import Ecluse.Core.Snapshot (Snapshot (Snapshot))
import Ecluse.Core.Telemetry.Metrics (CacheResult (Hit, Miss))
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (mpAssembledCacheRequest))
import Ecluse.Core.Version (Version)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Package (npmVersion, pypiVersion, requestsName, sampleManifest, sriSha512Of, thingName)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Registry.Npm (VersionSpec (vsIntegrity), packumentValue, versionSpec, versionValue)
import Ecluse.Test.Registry.PyPI (simpleFile, simpleIndex, withFileKeys)
import Ecluse.Test.Rules (atDefaultPrecedence, inertRuleDeps)
import Ecluse.Test.Server.Handler (MissingFixtureResponse (MissingFixtureResponse), deliverServe, responseBytes, testServeRuntime, testServeRuntimeWith)
import Ecluse.Test.Server.Mount (npmServeDeps, pypiServeDeps, withPrivateBaseUrl)
import Ecluse.Test.Server.Response (reasonOf)
import Ecluse.Test.Snapshot (digestOf)
import Ecluse.Test.Wai (selfBaseUrl)

spec :: Spec
spec = do
    packumentETagSpec
    codedETagSpec
    metadataGzipSpec
    firstPartyMissSpec
    outputBasisSpec

codedETagSpec :: Spec
codedETagSpec = describe "codedETag -- one validator per content coding" $ do
    it "keeps the identity tag for identity coding" $
        codedETag IdentityCoding (tagWith base) `shouldBe` tagWith base
    it "pins a weak gzip tag over the framed identity tag" $
        -- SHA-256 of "ecluse:packument-etag:gzip:v1\0" and the length-framed quoted identity tag.
        renderETag (codedETag GzipCoding (mkStrongETag (hash ("identity" :: ByteString) :: Digest SHA256)))
            `shouldBe` "W/\"34d4ae5c45518e8b9b30722d9a46d3628e64aca9df195e4351b9f3a702d44b3a\""
    it "changes the gzip tag with the identity tag" $
        codedETag GzipCoding (tagWith base{publicSurvivors = ["1.0.0"]}) `shouldNotBe` codedETag GzipCoding (tagWith base)
    it "never shares the identity tag's opaque value" $
        T.drop 2 (renderETag (codedETag GzipCoding (tagWith base))) `shouldNotBe` renderETag (tagWith base)

metadataGzipSpec :: Spec
metadataGzipSpec = describe "native metadata gzip over the live listing path" $
    for_ listings $ \listing -> describe (listingLabel listing) $ do
        it "gzips the identity listing's exact bytes for a client that accepts gzip" $
            withListing listing $ \serve -> do
                plain <- serve GzipWhenAccepted methodGet []
                packed <- serve GzipWhenAccepted methodGet [(hAcceptEncoding, "gzip")]
                map statusOf [plain, packed] `shouldBe` [200, 200]
                header hContentEncoding plain `shouldBe` Nothing
                header hContentEncoding packed `shouldBe` Just "gzip"
                map (header hVary) [plain, packed] `shouldBe` [Just "Accept-Encoding", Just "Accept-Encoding"]
                plainBytes <- responseBytes plain
                packedBytes <- responseBytes packed
                GZip.decompress packedBytes `shouldBe` plainBytes
                header hContentLength packed `shouldBe` Just (show (LBS.length packedBytes))
                header hETag packed `shouldSatisfy` maybe False (BS.isPrefixOf "W/")
                header hETag plain `shouldSatisfy` maybe False (BS.isPrefixOf "\"")
        it "states a gzip HEAD's exact length and validator without a body" $
            withListing listing $ \serve -> do
                packed <- serve GzipWhenAccepted methodGet [(hAcceptEncoding, "gzip")]
                headed <- serve GzipWhenAccepted methodHead [(hAcceptEncoding, "gzip")]
                statusOf headed `shouldBe` 200
                responseBytes headed `shouldReturn` ""
                packedBytes <- responseBytes packed
                header hContentLength headed `shouldBe` Just (show (LBS.length packedBytes))
                map (header hETag) [headed, packed] `shouldBe` replicate 2 (header hETag packed)
                header hContentEncoding headed `shouldBe` Just "gzip"
        it "answers 304 only to the selected coding's validator" $
            withListing listing $ \serve -> do
                plainTag <- validator =<< serve GzipWhenAccepted methodGet []
                gzipTag <- validator =<< serve GzipWhenAccepted methodGet [(hAcceptEncoding, "gzip")]
                for_ [methodGet, methodHead] $ \method -> do
                    matched <- serve GzipWhenAccepted method [(hAcceptEncoding, "gzip"), (hIfNoneMatch, gzipTag)]
                    statusOf matched `shouldBe` 304
                    responseBytes matched `shouldReturn` ""
                    header hETag matched `shouldBe` Just gzipTag
                    header hVary matched `shouldBe` Just "Accept-Encoding"
                crossed <- serve GzipWhenAccepted methodGet [(hAcceptEncoding, "gzip"), (hIfNoneMatch, plainTag)]
                (statusOf crossed, header hContentEncoding crossed) `shouldBe` (200, Just "gzip")
                unpacked <- serve GzipWhenAccepted methodGet [(hIfNoneMatch, gzipTag)]
                (statusOf unpacked, header hContentEncoding unpacked) `shouldBe` (200, Nothing)
        it "refuses with 406 a request that excludes both codings" $
            withListing listing $ \serve -> do
                refused <- serve GzipWhenAccepted methodGet [(hAcceptEncoding, "gzip;q=0, identity;q=0")]
                statusOf refused `shouldBe` 406
                header hVary refused `shouldBe` Just "Accept-Encoding"
        it "keeps the identity response unchanged when the operator opts out" $
            withListing listing $ \serve -> do
                plain <- serve IdentityOnly methodGet []
                plainBytes <- responseBytes plain
                for_ [[(hAcceptEncoding, "gzip")], [(hAcceptEncoding, "*;q=0")]] $ \offered -> do
                    response <- serve IdentityOnly methodGet offered
                    statusOf response `shouldBe` 200
                    responseHeaders response `shouldBe` responseHeaders plain
                    responseBytes response `shouldReturn` plainBytes
                header hVary plain `shouldBe` Nothing
        it "serves both codings from one identity entry in the assembled cache" $ do
            results <- newIORef []
            rt <- testServeRuntime noopMetricsPort{mpAssembledCacheRequest = \result -> modifyIORef' results (result :)}
            withListingIn rt listing $ \serve -> do
                packed <- serve GzipWhenAccepted methodGet [(hAcceptEncoding, "gzip")]
                plain <- serve GzipWhenAccepted methodGet []
                plainBytes <- responseBytes plain
                (GZip.decompress <$> responseBytes packed) `shouldReturn` plainBytes
                reverse <$> readIORef results `shouldReturn` [Miss, Hit]
        it "charges a gzip response its workspace and buffer, and returns every charge" $ do
            meter <- newMemoryMeter MeterSettings{msBudgetBytes = maxBound `div` 2, msStepBytes = 1, msEntryRoom = 1000, msEntryWaitMicros = 0}
            unmetered <- testServeRuntime noopMetricsPort
            withListingIn unmetered{srMemoryMeter = meter} listing $ \serve -> do
                void (serve GzipWhenAccepted methodGet [])
                void (takeLargestCharge meter)
                void (serve GzipWhenAccepted methodGet [])
                plainCharge <- takeLargestCharge meter
                void (serve GzipWhenAccepted methodGet [(hAcceptEncoding, "gzip")])
                gzipCharge <- takeLargestCharge meter
                gzipCharge - plainCharge `shouldBe` gzipWorkspaceBytes + gzipBufferChargeBytes
                snChargedBytes <$> meterSnapshot meter `shouldReturn` 0
        it "keeps edge authentication, private authorisation and policy ahead of a cached gzip validator" $ do
            rt <- testServeRuntime noopMetricsPort
            privateState <- newIORef PrivateServes
            testWithApplication (pure (listingOrigin listing)) $ \publicPort ->
                testWithApplication (pure (privateOrigin privateState (listingOrigin listing))) $ \privatePort -> do
                    public <- listingDeps listing publicPort
                    let publicOnly = public{pdInboundToken = Just (mkSecret "edge-token")}
                        withPrivate = withPrivateBaseUrl (Just (loopbackRegistryUrl ("http://localhost:" <> show privatePort))) publicOnly
                        serve deps headers = captureListing rt listing deps methodGet (gzipOffer <> headers)
                        signed = [listingEdgeCredential listing]
                    tag <- validator =<< serve withPrivate signed
                    statusOf <$> serve withPrivate [(hIfNoneMatch, tag)] `shouldReturn` 401
                    statusOf <$> serve withPrivate ((hIfNoneMatch, tag) : signed) `shouldReturn` 304
                    writeIORef privateState PrivateRefuses
                    statusOf <$> serve withPrivate ((hIfNoneMatch, tag) : signed) `shouldReturn` 403
                    publicTag <- validator =<< serve publicOnly signed
                    statusOf <$> serve publicOnly ((hIfNoneMatch, publicTag) : signed) `shouldReturn` 304
                    statusOf <$> serve publicOnly{pdRules = []} ((hIfNoneMatch, publicTag) : signed) `shouldReturn` 403
        it "returns its admission slot and charges when cancelled while it responds" $ do
            admission <- newServeAdmissionTuned 1 0 0
            rt <- testServeRuntimeWith admission noopMetricsPort
            testWithApplication (pure (listingOrigin listing)) $ \port -> do
                deps <- listingDeps listing port
                ready <- MVar.newEmptyMVar
                blocked <- MVar.newEmptyMVar
                let stalled = runListing rt listing deps methodGet [(hAcceptEncoding, "gzip")] (\_ -> MVar.putMVar ready () >> MVar.takeMVar blocked)
                withAsync stalled $ \worker -> do
                    race_ (MVar.takeMVar ready) (wait worker)
                    cancel worker
                snChargedBytes <$> meterSnapshot (srMemoryMeter rt) `shouldReturn` 0
                next <- captureListing rt listing deps methodGet [(hAcceptEncoding, "gzip")]
                statusOf next `shouldBe` 200
  where
    statusOf = statusCode . responseStatus
    header name = lookup name . responseHeaders
    validator response = maybe (throwIO MissingFixtureResponse) pure (header hETag response)
    gzipOffer = [(hAcceptEncoding, "gzip")]

-- What the private origin answers for the listing.
data PrivateState = PrivateServes | PrivateRefuses

privateOrigin :: IORef PrivateState -> Application -> Application
privateOrigin current origin request respond =
    readIORef current >>= \case
        PrivateServes -> origin request respond
        PrivateRefuses -> respond (responseLBS status403 [] "")

packumentETagSpec :: Spec
packumentETagSpec = describe "packumentETag -- the input-derived validator" $ do
    it "pins the v4 byte framing for every entry constructor" $ do
        let sources =
                [ (TrustedSource, privateDigest base, [("1\0é", [ArrayEntry 0, ArrayEntry 10, ArrayEntry (-1), ObjectEntry "é\0x", SingletonEntry])])
                , (GatedSource, publicDigest base, [])
                ]
        renderETag (packumentETag mountBase [] thingName sources)
            `shouldBe` "\"4319a6866882aedd076a71e0f8158f81138c10f4f18ff475446cd63a307a5fe6\""

    it "changes when the origin URL changes the source metadata pointer" $
        packumentETag mountBase ["https://source-one.example"] thingName (piecesOf base)
            `shouldNotBe` packumentETag mountBase ["https://source-two.example"] thingName (piecesOf base)

    it "changes when an origin body changes (same survivors)" $
        tagWith base{publicDigest = digestOf "public-bytes-v2"} `shouldNotBe` tagWith base

    it "changes when the private origin's body changes" $
        tagWith base{privateDigest = digestOf "private-bytes-v2"} `shouldNotBe` tagWith base

    it "changes when a version drops out of the survivor set" $
        tagWith base{publicSurvivors = ["1.0.0"]} `shouldNotBe` tagWith base

    it "changes when a version joins the survivor set" $
        tagWith base{publicSurvivors = ["1.0.0", "2.0.0", "3.0.0"]} `shouldNotBe` tagWith base

    it "changes when the mount base URL changes (rewritten tarball URLs differ)" $
        packumentETag "https://other.example/npm" [] thingName (piecesOf base)
            `shouldNotBe` tagWith base

    it "changes across packages" $
        packumentETag mountBase [] (mkPackageName Npm Nothing "other-thing") (piecesOf base)
            `shouldNotBe` tagWith base

    it "distinguishes provenance: the same digest as trusted vs gated" $
        tagWith base{privateProvenance = GatedSource} `shouldNotBe` tagWith base

    it "distinguishes source order (merge precedence is positional)" $
        packumentETag mountBase [] thingName (reverse (piecesOf base)) `shouldNotBe` tagWith base

    it "does not collide survivor lists on concatenation framing" $ do
        -- ["1.0", "0.2.0"] vs ["1.0.0", "2.0"] concatenate to the same characters.
        -- The per-field terminator must keep them distinct.
        tagWith base{publicSurvivors = ["1.0", "0.2.0"]}
            `shouldNotBe` tagWith base{publicSurvivors = ["1.0.0", "2.0"]}

    it "does not collide a survivor moved across the source boundary" $
        -- The same flat multiset of survivors, split differently between the two
        -- sources, must not collide. The source-block terminator keeps them apart.
        tagWith base{privateSurvivors = ["9.0.0", "1.0.0"], publicSurvivors = ["2.0.0"]}
            `shouldNotBe` tagWith base{privateSurvivors = ["9.0.0"], publicSurvivors = ["1.0.0", "2.0.0"]}

    it "changes when a whole source appears or disappears" $
        packumentETag mountBase [] thingName [publicPiece base] `shouldNotBe` tagWith base

    for_ [TrustedSource, GatedSource] $ \provenance -> do
        let tag entries = packumentETag mountBase [] thingName [(provenance, publicDigest base, [("1.0.0", entries)])]
        it ("tracks exact admitted coordinates for " <> show provenance) $
            tag [ArrayEntry 0, ArrayEntry 1] `shouldNotBe` tag [ArrayEntry 1]
        it ("distinguishes entry constructors for " <> show provenance) $ do
            tag [ArrayEntry 0] `shouldNotBe` tag [ObjectEntry "0"]
            tag [SingletonEntry] `shouldNotBe` tag [ObjectEntry "s"]
        it ("frames arbitrary object keys for " <> show provenance) $
            tag [ObjectEntry "a", ObjectEntry "b"] `shouldNotBe` tag [ObjectEntry "a\0b"]

firstPartyMissSpec :: Spec
firstPartyMissSpec = describe "a first-party name whose private origin yielded nothing" $ do
    it "renders an origin that answered 404 as a 404" $
        replyFor MissAbsent `shouldBe` "not-found"

    it "renders an origin that was never read as a 503" $
        replyFor MissUnresolved `shouldBe` "unavailable"

    it "counts an absence as a denial the first-party rule decided" $ do
        packumentServeDecision [decisionFor MissAbsent] `shouldBe` Metric.Deny
        fmap denialLabels (reasonOf (decisionFor MissAbsent))
            `shouldBe` Just (Just "first-party", Metric.ReasonPolicy)

    it "counts an unread origin as an outage, suggesting no delay" $ do
        packumentServeDecision [decisionFor MissUnresolved] `shouldBe` Metric.Unavailable
        reasonOf (decisionFor MissUnresolved) `shouldBe` Just (Unavailable (WillResolve Nothing))

outputBasisSpec :: Spec
outputBasisSpec = describe "outputBasisBytes -- the source bytes a listing's output charge scales" $
    forM_ [(thingName, npmVersion), (requestsName, pypiVersion)] $ \(name, version) ->
        describe (show (pkgEcosystem name)) $ do
            let source = listingSource name version
            it "is a single source's size" $
                basisOf [source GatedSource 900 ["1.0.0", "2.0.0"]] `shouldBe` Just 900

            it "counts one copy of sources that hold the same versions" $
                basisOf [source TrustedSource 900 ["1.0.0", "2.0.0"], source GatedSource 900 ["1.0.0", "2.0.0"]] `shouldBe` Just 900

            it "counts both sources when they share no version" $
                basisOf [source TrustedSource 400 ["1.0.0"], source GatedSource 900 ["2.0.0", "3.0.0"]] `shouldBe` Just 1300

            it "adds the smaller source's share for the versions the larger lacks" $
                basisOf [source TrustedSource 600 ["1.0.0", "2.0.0", "3.0.0"], source GatedSource 900 ["2.0.0", "3.0.0", "4.0.0"]] `shouldBe` Just 1100

            it "takes the larger source whole on either side of the merge" $
                basisOf [source TrustedSource 900 ["1.0.0", "2.0.0", "3.0.0"], source GatedSource 300 ["3.0.0", "4.0.0", "5.0.0"]] `shouldBe` Just 1100

            it "rounds a share up, anchored on the largest source when it gives more" $
                basisOf [source TrustedSource 100 ["1.0.0", "2.0.0", "3.0.0"], source GatedSource 900 ["2.0.0", "3.0.0", "4.0.0"]] `shouldBe` Just 934

            it "anchors on the base document when a larger source holds all its versions" $
                basisOf [source TrustedSource 900 ["1.0.0"], source GatedSource 1000 thousandVersions] `shouldBe` Just 1899

            it "anchors on the trusted source wherever it sits in the merge" $
                basisOf [source GatedSource 1000 thousandVersions, source TrustedSource 900 ["1.0.0"]] `shouldBe` Just 1899

-- A 1,000-version document that includes 1.0.0.
thousandVersions :: [Text]
thousandVersions = [show major <> ".0.0" | major <- [1 .. 1000 :: Int]]

-- A merge source holding the given versions, whose body is the given number of bytes.
listingSource :: PackageName -> (Text -> Version) -> Provenance -> Int -> [Text] -> Contribution
listingSource name version provenance bytes keys =
    Contribution provenance (manifestInfo manifest) (manifestRaw manifest) (manifestDigest manifest) bytes
  where
    manifest = sampleManifest name (map version keys)

basisOf :: [Contribution] -> Maybe Int
basisOf sources = (`outputBasisBytes` sources) <$> mergePackuments [(srcProvenance s, Snapshot (srcDigest s) (srcInfo s)) | s <- sources]

-- Each reply factory answers its own name, so a case reads back which one the pipeline chose.
namedReplies :: PackumentReplies Text
namedReplies =
    PackumentReplies
        { packumentOk = \_ _ -> "ok"
        , packumentNotModified = const "not-modified"
        , packumentUnauthorised = \_ _ -> "unauthorised"
        , packumentForbidden = \_ _ -> "forbidden"
        , packumentNotFound = \_ _ -> "not-found"
        , packumentNotAcceptable = \_ _ -> "not-acceptable"
        , packumentInternal = \_ _ -> "internal"
        , packumentBadGateway = \_ _ -> "bad-gateway"
        , packumentUnavailable = \_ _ -> "unavailable"
        }

replyFor :: OriginMiss -> Text
replyFor = firstPartyMissReply namedReplies Nothing thingName

decisionFor :: OriginMiss -> ServeDecision
decisionFor = firstPartyMissDecision thingName

-- The fixture: a private (trusted) and a public (gated) source with distinct
-- bodies and survivor sets, varied one field at a time by each case.
data Fixture = Fixture
    { privateProvenance :: Provenance
    , privateDigest :: ContentDigest
    , privateSurvivors :: [Text]
    , publicDigest :: ContentDigest
    , publicSurvivors :: [Text]
    }

base :: Fixture
base =
    Fixture
        { privateProvenance = TrustedSource
        , privateDigest = digestOf "private-bytes-v1"
        , privateSurvivors = ["9.0.0"]
        , publicDigest = digestOf "public-bytes-v1"
        , publicSurvivors = ["1.0.0", "2.0.0"]
        }

piecesOf :: Fixture -> [(Provenance, ContentDigest, [(Text, [EntryKey])])]
piecesOf f = [(privateProvenance f, privateDigest f, map (,[SingletonEntry]) (privateSurvivors f)), publicPiece f]

publicPiece :: Fixture -> (Provenance, ContentDigest, [(Text, [EntryKey])])
publicPiece f = (GatedSource, publicDigest f, map (,[SingletonEntry]) (publicSurvivors f))

tagWith :: Fixture -> ETag
tagWith f = packumentETag mountBase [] thingName (piecesOf f)

mountBase :: Text
mountBase = "https://proxy.example/npm"

-- One ecosystem's listing route, mount, and the public origin that serves its document.
data Listing = Listing
    { listingLabel :: String
    , listingRouter :: MountRouter
    , listingPath :: [Text]
    , listingPrefix :: Text
    , listingCredential :: CredentialMapping
    , listingEdgeCredential :: Header
    -- ^ The edge token @edge-token@ in the ecosystem's own presentation.
    , listingDeps :: Int -> IO PackumentDeps
    , listingOrigin :: Application
    }

listings :: [Listing]
listings =
    [ Listing "npm" npmRouter ["leftpad"] "npm" npmCredential (hAuthorization, "Bearer edge-token") (agedDeps npmServeDeps (UTCTime (fromGregorian 2020 1 1) 0)) npmOrigin
    , -- HTTP Basic, user:edge-token.
      Listing "PyPI" pypiRouter ["simple", "requests"] "pypi" pypiCredential (hAuthorization, "Basic dXNlcjplZGdlLXRva2Vu") (agedDeps pypiServeDeps (UTCTime (fromGregorian 2026 6 20) 0)) pypiOrigin
    ]
  where
    agedDeps serveDeps now port = do
        prepared <- prepare inertRuleDeps [atDefaultPrecedence (AllowIfOlderThan (7 * nominalDay))]
        pure (serveDeps Nothing (loopbackRegistryUrl ("http://localhost:" <> show port)) NoMirrorWrite prepared (pure now)){pdMountBaseUrl = "http://proxy.test"}

npmOrigin :: Application
npmOrigin request respond
    | rawPathInfo request == "/leftpad" = respond (responseLBS status200 [] (encode document))
    | otherwise = respond (responseLBS status404 [] "")
  where
    tarball = selfBaseUrl request <> "/leftpad/-/leftpad-1.0.0.tgz"
    document = packumentValue "leftpad" "1.0.0" [("1.0.0", versionValue (versionSpec "leftpad" "1.0.0" tarball){vsIntegrity = Just (sriSha512Of "leftpad bytes")})] ["1.0.0" .= ("2019-01-01T00:00:00.000Z" :: Text)] []

pypiOrigin :: Application
pypiOrigin request respond
    | rawPathInfo request == "/simple/requests/" = respond (responseLBS status200 [] (encode index))
    | otherwise = respond (responseLBS status404 [] "")
  where
    index = simpleIndex "requests" [withFileKeys [("url", String (selfBaseUrl request <> "/files/requests-1.0.0.tar.gz"))] (simpleFile "requests-1.0.0.tar.gz")]

-- Serve each request through a fresh runtime over one origin, choosing the mount's coding policy.
withListing :: Listing -> ((MetadataCompression -> Method -> RequestHeaders -> IO Response) -> IO a) -> IO a
withListing listing use = do
    rt <- testServeRuntime noopMetricsPort
    withListingIn rt listing use

withListingIn :: ServeRuntime -> Listing -> ((MetadataCompression -> Method -> RequestHeaders -> IO Response) -> IO a) -> IO a
withListingIn rt listing use =
    testWithApplication (pure (listingOrigin listing)) $ \port -> do
        deps <- listingDeps listing port
        use (\policy -> captureListing rt listing deps{pdMetadataCompression = policy})

captureListing :: ServeRuntime -> Listing -> PackumentDeps -> Method -> RequestHeaders -> IO Response
captureListing rt listing deps method headers = do
    captured <- newIORef Nothing
    runListing rt listing deps method headers (writeIORef captured . Just)
    maybe (throwIO MissingFixtureResponse) pure =<< readIORef captured

-- The listing's route under its own router, so a HEAD takes the bodiless contract.
runListing :: ServeRuntime -> Listing -> PackumentDeps -> Method -> RequestHeaders -> (Response -> IO ()) -> IO ()
runListing rt listing deps method headers deliver =
    case listingRouter listing method headers (listingPath listing) of
        RouteAction contract (RunPipeline _ action) -> do
            logEnv <- newTestLogEnv
            deliverServe logEnv contract rt binding (action request) deliver
        _ -> throwIO MissingFixtureResponse
  where
    binding = MountBinding (listingPrefix listing :| []) (listingRouter listing) (listingCredential listing) deps Nothing
    request = defaultRequest{requestMethod = method, requestHeaders = headers}
