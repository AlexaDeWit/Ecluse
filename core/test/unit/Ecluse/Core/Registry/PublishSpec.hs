-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Target credential, charging, and transport contracts for mirror publication.
module Ecluse.Core.Registry.PublishSpec (spec) where

import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types (Header, Status)
import Network.HTTP.Types.Header (hLocation)
import Network.HTTP.Types.Status (status200, status302)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)

import Ecluse.Core.BuildIdentity (userAgent)
import Ecluse.Core.Credential (bareCredential, mkSecret)
import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Registry (
    FetchFault (FetchBoundExceeded, FetchUrlUnformable),
    PublishFault (PublishFetch),
    UrlFormationError (UnparseableUrl),
 )
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataFetch, MetadataUndecodable))
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (PrepareRead (..), staticRead)
import Ecluse.Core.Registry.Metadata.Fetch.Types (EcosystemRead (erRequest))
import Ecluse.Core.Registry.MirrorUpload (inventoryFiles, verifyMirrorBytes)
import Ecluse.Core.Registry.Npm.Metadata (npmRead)
import Ecluse.Core.Registry.Npm.Publish (npmPublishCodec, prepareNpmSource, prepareNpmUpload)
import Ecluse.Core.Registry.Origin (OriginClient (ocChargeFullRead, ocToken), originBaseUrl)
import Ecluse.Core.Registry.Publish (
    MirrorPublish (mpProbeInventory),
    MirrorTransport (MirrorTransport, ptClient, ptLimits, ptMintCredential, ptTracing),
    PublishCodec (..),
    PublishPlan (PublishPlan, ppLatest),
    newMirrorPublish,
 )
import Ecluse.Core.Registry.Request (baseRequestUrl, nativePresentation)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge), Limits (maxMetadataBytes), defaultLimits)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission.Budget (scaleCharge)
import Ecluse.Test.Deadline (untracedClient, withExchangeClient, withSweeper)
import Ecluse.Test.Package (v1_0_0)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Registry (isBoundExceededPublish, isTransportFetch, isUrlUnformableFetch, isUrlUnformablePublish)
import Ecluse.Test.Registry.Npm (dummyArtifact, isOdd, isOddVersionDoc, mirrorVersionDoc)
import Ecluse.Test.Registry.Publish (anonymousPublishAt, publishOnlyFile)
import Ecluse.Test.Stub (
    Captured (capPath),
    Stub,
    allCaptured,
    headerValue,
    lastCaptured,
    stubBaseUrl,
    withRoutedStub,
    withStub,
 )
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    describe "newMirrorPublish (the codec-over-transport marriage)" $ do
        it "mints the bearer per probe and attaches it to the wire" $
            withStub status200 "{\"name\":\"is-odd\"}" $ \stub -> withSweeper $ \deadlines -> do
                (publish, mints) <- mintCountingPublish deadlines stub
                _ <- mpProbeInventory publish (const (pure ())) isOdd
                captured <- lastCaptured stub
                headerValue "Authorization" captured `shouldBe` Just "Bearer minted-token"
                _ <- mpProbeInventory publish (const (pure ())) isOdd
                minted <- readIORef mints
                minted `shouldBe` 2

        it "prepares a target after minting with the overlapping-inventory charge" $
            withStub status200 "{}" $ \stub ->
                withExchangeClient (Client.newManager Client.defaultManagerSettings) $ \exchanges -> do
                    events <- newIORef ([] :: [Text])
                    charges <- newIORef []
                    let credential = Just (bareCredential (mkSecret "service-token"))
                        target = loopbackRegistryUrl (stubBaseUrl stub)
                        transport =
                            MirrorTransport
                                { ptClient = exchanges
                                , ptMintCredential = modifyIORef' events (<> ["mint"]) $> credential
                                , ptLimits = defaultLimits
                                , ptTracing = passthroughTracingPort
                                }
                        factory = PrepareRead $ \origin -> do
                            modifyIORef' events (<> ["prepare"])
                            ocToken origin `shouldBe` credential
                            originBaseUrl origin `shouldBe` registryUrlText target
                            ocChargeFullRead origin 13
                            pure (Left MetadataUndecodable)
                        codec = (npmPublishCodec nativePresentation){pcTargetRead = factory}
                        publish = newMirrorPublish transport target target codec
                    mpProbeInventory publish (\bytes -> modifyIORef' charges (<> [bytes])) isOdd `shouldReturn` Left MetadataUndecodable
                    readIORef events `shouldReturn` ["mint", "prepare"]
                    readIORef charges `shouldReturn` [scaleCharge (2 * pcTargetReadPermille (npmPublishCodec nativePresentation)) 13]
                    allCaptured stub `shouldReturn` []

        it "mints the bearer per publish and attaches it to the wire" $
            withStub status200 "{}" $ \stub -> withSweeper $ \deadlines -> do
                (publish, mints) <- mintCountingPublish deadlines stub
                _ <- publishFixture publish
                captured <- lastCaptured stub
                headerValue "Authorization" captured `shouldBe` Just "Bearer minted-token"
                minted <- readIORef mints
                minted `shouldBe` 1

        it "reports an unformable probe URL as a FetchFault value, never thrown" $ withSweeper $ \deadlines -> do
            publish <- publishAt deadlines ""
            outcome <- mpProbeInventory publish (const (pure ())) isOdd
            outcome `shouldSatisfy` isProbeUrlFault

        it "reports an unformable publish URL as a PublishFault value, never thrown" $ withSweeper $ \deadlines -> do
            publish <- publishAt deadlines ""
            outcome <- publishFixture publish
            outcome `shouldSatisfy` isUrlUnformablePublish

        it "reports a probe transport failure as a FetchTransport value, never thrown" $ withSweeper $ \deadlines -> do
            publish <- publishAt deadlines "http://127.0.0.1:1"
            outcome <- mpProbeInventory publish (const (pure ())) isOdd
            outcome `shouldSatisfy` isProbeTransportFault

        it "refuses an over-cap probe body fail-closed as a FetchBoundExceeded value" $
            withStub status200 "0123456789 more than sixteen bytes" $ \stub -> do
                withExchangeClient (Client.newManager Client.defaultManagerSettings) $ \exchanges -> do
                    let transport =
                            MirrorTransport
                                { ptClient = exchanges
                                , ptMintCredential = pure Nothing
                                , ptLimits = defaultLimits{maxMetadataBytes = 16}
                                , ptTracing = passthroughTracingPort
                                }
                        publish = newMirrorPublish transport (loopbackRegistryUrl (stubBaseUrl stub)) (loopbackRegistryUrl (stubBaseUrl stub)) (npmPublishCodec nativePresentation)
                    outcome <- mpProbeInventory publish (const (pure ())) isOdd
                    outcome `shouldBe` Left (MetadataFetch (FetchBoundExceeded (BodyTooLarge (MetadataBodyLimit 16))))

        it "refuses an over-cap publish answer fail-closed as a PublishFetch bound breach" $
            withStub status200 "0123456789 more than sixteen bytes" $ \stub -> do
                withExchangeClient (Client.newManager Client.defaultManagerSettings) $ \exchanges -> do
                    let transport =
                            MirrorTransport
                                { ptClient = exchanges
                                , ptMintCredential = pure Nothing
                                , ptLimits = defaultLimits{maxMetadataBytes = 16}
                                , ptTracing = passthroughTracingPort
                                }
                        publish = newMirrorPublish transport (loopbackRegistryUrl (stubBaseUrl stub)) (loopbackRegistryUrl (stubBaseUrl stub)) (npmPublishCodec nativePresentation)
                    outcome <- publishFixture publish
                    outcome `shouldSatisfy` isBoundExceededPublish

    describe "the codec's request formers (credential invariants per married client)" $ do
        it "the probe request attaches the bearer at the single attach point with redirects disabled" $
            case pcProbeRequest (npmPublishCodec nativePresentation) "https://mirror.test" (Just (bareCredential (mkSecret "tok"))) isOdd of
                Left err -> fail ("expected a formed probe request, got " <> show err)
                Right request -> do
                    Client.redirectCount request `shouldBe` 0
                    lookup "Authorization" (Client.requestHeaders request) `shouldBe` Just "Bearer tok"

        it "the publish request attaches the bearer with redirects disabled" $
            case npmPublishCodec nativePresentation of
                PublishCodec{pcPrepareSource, pcPrepareUpload, pcPublishRequest} -> withSweeper $ \deadlines -> do
                    (inventory, source) <- expectRight (pcPrepareSource isOdd v1_0_0 (mirrorVersionDoc isOdd v1_0_0 isOddVersionDoc dummyArtifact))
                    let file :| _ = inventoryFiles inventory
                    verified <- expectRight (verifyMirrorBytes file "")
                    upload <- pcPrepareUpload deadlines defaultLimits (const pass) inventory source file verified >>= expectRight
                    request <- expectRight (pcPublishRequest "https://mirror.test" (Just (bareCredential (mkSecret "tok"))) planV1 upload)
                    Client.redirectCount request `shouldBe` 0
                    lookup "Authorization" (Client.requestHeaders request) `shouldBe` Just "Bearer tok"

    describe "the transport seals whatever a codec hands it" $ do
        it "refuses to chase a redirect the codec's own probe request would have followed" $
            -- A codec formed outside the shared entry keeps http-client's default
            -- redirectCount of 10, so only the transport's re-seal stops the hop.
            withRoutedStub (redirectFrom "/probe") $ \stub -> withSweeper $ \deadlines -> do
                publish <- unsealedPublishAt deadlines stub
                _ <- mpProbeInventory publish (const (pure ())) isOdd
                servedPaths stub `shouldReturn` ["/probe"]

        it "refuses to chase a redirect on the write leg, where the bearer rides" $
            withRoutedStub (redirectFrom "/write") $ \stub -> withSweeper $ \deadlines -> do
                publish <- unsealedPublishAt deadlines stub
                _ <- publishFixture publish
                servedPaths stub `shouldReturn` ["/write"]

        it "identifies the proxy on the wire, whatever headers the codec set" $
            withStub status200 "{}" $ \stub -> withSweeper $ \deadlines -> do
                publish <- unsealedPublishAt deadlines stub
                _ <- mpProbeInventory publish (const (pure ())) isOdd
                captured <- lastCaptured stub
                headerValue "User-Agent" captured `shouldBe` Just userAgent

{- A codec that forms its requests with @http-client@ directly, as a second ecosystem's author
may. It composes no shared attach, so its requests arrive unsealed. -}
unsealedCodec :: PublishCodec
unsealedCodec =
    PublishCodec
        { pcProbeRequest = \targetUrl _token _name -> unsealed (targetUrl <> "/probe")
        , pcVersionListWalk = pcVersionListWalk (npmPublishCodec nativePresentation)
        , pcMirrorCompanion = Nothing
        , pcNativeComplete = pcNativeComplete (npmPublishCodec nativePresentation)
        , pcTargetReadPermille = pcTargetReadPermille (npmPublishCodec nativePresentation)
        , pcTargetRead = staticRead npmRead{erRequest = \target _ _ -> unsealed (registryUrlText (baseRequestUrl target) <> "/probe")}
        , pcPrepareTarget = pcPrepareTarget (npmPublishCodec nativePresentation)
        , pcPrepareSource = prepareNpmSource
        , pcPrepareUpload = \_ _ _ inventory source file bytes -> pure (prepareNpmUpload inventory source file bytes)
        , pcPublishRequest = \targetUrl _token _plan _upload ->
            first (PublishFetch . FetchUrlUnformable) (unsealed (targetUrl <> "/write"))
        , pcPublishOutcome = \_ _ -> Right ()
        }
  where
    unsealed url = maybe (Left (UnparseableUrl url)) Right (Client.parseRequest (toString url))

planV1 :: PublishPlan
planV1 = PublishPlan{ppLatest = v1_0_0}

publishFixture :: MirrorPublish -> IO (Either PublishFault ())
publishFixture publish =
    publishOnlyFile publish (mirrorVersionDoc isOdd v1_0_0 isOddVersionDoc dummyArtifact) planV1 ""

isProbeUrlFault :: Either MetadataError a -> Bool
isProbeUrlFault = \case
    Left (MetadataFetch fault) -> isUrlUnformableFetch (Left fault)
    _ -> False

isProbeTransportFault :: Either MetadataError a -> Bool
isProbeTransportFault = \case
    Left (MetadataFetch fault) -> isTransportFetch (Left fault)
    _ -> False

unsealedPublishAt :: Deadlines -> Stub -> IO MirrorPublish
unsealedPublishAt deadlines stub = anonymousPublishAt deadlines unsealedCodec (loopbackRegistryUrl (stubBaseUrl stub))

-- Answer one path with a 302 elsewhere on the same stub, so a followed hop shows up as a
-- second served path.
redirectFrom :: ByteString -> Captured -> (Status, [Header], LBS.ByteString)
redirectFrom source cap
    | capPath cap == source = (status302, [(hLocation, "/redirected")], "")
    | otherwise = (status200, [], "{}")

servedPaths :: Stub -> IO [ByteString]
servedPaths stub = fmap capPath <$> allCaptured stub

mintCountingPublish :: Deadlines -> Stub -> IO (MirrorPublish, IORef Int)
mintCountingPublish deadlines stub = do
    exchanges <- untracedClient <$> Client.newManager Client.defaultManagerSettings <*> pure deadlines
    mints <- newIORef (0 :: Int)
    let mint = do
            atomicModifyIORef' mints (\n -> (n + 1, ()))
            pure (Just (bareCredential (mkSecret "minted-token")))
        transport = MirrorTransport{ptClient = exchanges, ptMintCredential = mint, ptLimits = defaultLimits, ptTracing = passthroughTracingPort}
    pure (newMirrorPublish transport (loopbackRegistryUrl (stubBaseUrl stub)) (loopbackRegistryUrl (stubBaseUrl stub)) (npmPublishCodec nativePresentation), mints)

-- The anonymous marriage of npm's codec against an arbitrary target URL.
publishAt :: Deadlines -> Text -> IO MirrorPublish
publishAt deadlines = anonymousPublishAt deadlines (npmPublishCodec nativePresentation) . loopbackRegistryUrl
