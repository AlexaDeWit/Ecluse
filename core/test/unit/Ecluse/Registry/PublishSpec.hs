-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Registry.PublishSpec (spec) where

import Data.List (lookup)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types.Status (status200)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Data.List.NonEmpty qualified as NE
import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (HashAlg (SHA1), PackageName, mkPackageName)
import Ecluse.Core.Registry (
    FetchFault (FetchBoundExceeded, FetchTransport, FetchUrlUnformable),
    MirrorArtifact (MirrorArtifact, maFilename, maHashes, maSize),
    PublishFault (PublishUrlUnformable),
 )
import Ecluse.Core.Registry.Npm.Publish (npmPublishCodec)
import Ecluse.Core.Registry.Publish (
    MirrorPublish (mpProbeMetadata, mpPublishArtifact),
    MirrorTransport (MirrorTransport, ptLimits, ptManager, ptMintToken),
    PublishCodec (pcProbeRequest, pcPublishRequest),
    newMirrorPublish,
 )
import Ecluse.Core.Security (Limits (maxBodyBytes), defaultLimits)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Package (unsafeHash, validSha1)
import Ecluse.Test.Stub (Stub, headerValue, lastCaptured, stubBaseUrl, withStub)

spec :: Spec
spec = do
    describe "newMirrorPublish (the codec-over-transport marriage)" $ do
        it "mints the bearer per probe and attaches it to the wire" $
            -- A managed mirror (CodeArtifact) requires auth on reads as on writes,
            -- so the probe must present a current token: minted per call through
            -- the transport, never cached in the marriage.
            withStub status200 "{\"name\":\"is-odd\"}" $ \stub -> do
                (publish, mints) <- mintCountingPublish stub
                _ <- mpProbeMetadata publish isOdd
                captured <- lastCaptured stub
                headerValue "Authorization" captured `shouldBe` Just "Bearer minted-token"
                _ <- mpProbeMetadata publish isOdd
                minted <- readIORef mints
                minted `shouldBe` 2

        it "mints the bearer per publish and attaches it to the wire" $
            withStub status200 "{}" $ \stub -> do
                (publish, mints) <- mintCountingPublish stub
                _ <- mpPublishArtifact publish isOdd v1 dummyArtifact "bytes"
                captured <- lastCaptured stub
                headerValue "Authorization" captured `shouldBe` Just "Bearer minted-token"
                minted <- readIORef mints
                minted `shouldBe` 1

        it "reports an unformable probe URL as a FetchFault value, never thrown" $ do
            publish <- publishAt ""
            outcome <- mpProbeMetadata publish isOdd
            outcome `shouldSatisfy` isUrlUnformableFetch

        it "reports an unformable publish URL as a PublishFault value, never thrown" $ do
            publish <- publishAt ""
            outcome <- mpPublishArtifact publish isOdd v1 dummyArtifact "bytes"
            outcome `shouldSatisfy` isUrlUnformablePublish

        it "reports a probe transport failure as a FetchTransport value, never thrown" $ do
            publish <- publishAt "http://127.0.0.1:1"
            outcome <- mpProbeMetadata publish isOdd
            outcome `shouldSatisfy` isTransportFetch

        it "refuses an over-cap probe body fail-closed as a FetchBoundExceeded value" $
            -- The probe reads the mirror's answer bounded: a body past the budget is
            -- refused as a value rather than buffered whole.
            withStub status200 "0123456789 more than sixteen bytes" $ \stub -> do
                manager <- Client.newManager Client.defaultManagerSettings
                let transport =
                        MirrorTransport
                            { ptManager = manager
                            , ptMintToken = pure Nothing
                            , ptLimits = defaultLimits{maxBodyBytes = 16}
                            }
                    publish = newMirrorPublish transport (stubBaseUrl stub) npmPublishCodec
                outcome <- mpProbeMetadata publish isOdd
                outcome `shouldSatisfy` isBoundExceededFetch

    describe "the codec's request formers (credential invariants per married client)" $ do
        it "the probe request attaches the bearer at the single attach point with redirects disabled" $
            -- The transport hands the minted bearer to the codec's former, which
            -- attaches it through the shared single attach point: redirectCount 0,
            -- so a credential-bearing probe never follows a redirect.
            case pcProbeRequest npmPublishCodec "https://mirror.test" (Just (mkSecret "tok")) isOdd of
                Left err -> fail ("expected a formed probe request, got " <> show err)
                Right request -> do
                    Client.redirectCount request `shouldBe` 0
                    lookup "Authorization" (Client.requestHeaders request) `shouldBe` Just "Bearer tok"

        it "the publish request attaches the bearer at the single attach point with redirects disabled" $
            case pcPublishRequest npmPublishCodec "https://mirror.test" (Just (mkSecret "tok")) isOdd v1 dummyArtifact "bytes" of
                Left err -> fail ("expected a formed publish request, got " <> show err)
                Right request -> do
                    Client.redirectCount request `shouldBe` 0
                    lookup "Authorization" (Client.requestHeaders request) `shouldBe` Just "Bearer tok"

-- The marriage against a stub with a mint that counts its calls and answers a
-- fixed token, so the per-call mint is observable.
mintCountingPublish :: Stub -> IO (MirrorPublish, IORef Int)
mintCountingPublish stub = do
    manager <- Client.newManager Client.defaultManagerSettings
    mints <- newIORef (0 :: Int)
    let mint = do
            atomicModifyIORef' mints (\n -> (n + 1, ()))
            pure (Just (mkSecret "minted-token"))
        transport = MirrorTransport{ptManager = manager, ptMintToken = mint, ptLimits = defaultLimits}
    pure (newMirrorPublish transport (stubBaseUrl stub) npmPublishCodec, mints)

-- The anonymous marriage against an arbitrary target URL.
publishAt :: Text -> IO MirrorPublish
publishAt targetUrl = do
    manager <- Client.newManager Client.defaultManagerSettings
    let transport = MirrorTransport{ptManager = manager, ptMintToken = pure Nothing, ptLimits = defaultLimits}
    pure (newMirrorPublish transport targetUrl npmPublishCodec)

isOdd :: PackageName
isOdd = mkPackageName Npm Nothing "is-odd"

v1 :: Version
v1 = mkVersion Npm "1.0.0"

dummyArtifact :: MirrorArtifact
dummyArtifact =
    MirrorArtifact
        { maFilename = "is-odd-1.0.0.tgz"
        , maHashes = NE.singleton (unsafeHash SHA1 validSha1)
        , maSize = Nothing
        }

isUrlUnformableFetch :: Either FetchFault a -> Bool
isUrlUnformableFetch = \case
    Left (FetchUrlUnformable _) -> True
    _ -> False

isTransportFetch :: Either FetchFault a -> Bool
isTransportFetch = \case
    Left (FetchTransport _) -> True
    _ -> False

isBoundExceededFetch :: Either FetchFault a -> Bool
isBoundExceededFetch = \case
    Left (FetchBoundExceeded _) -> True
    _ -> False

isUrlUnformablePublish :: Either PublishFault a -> Bool
isUrlUnformablePublish = \case
    Left (PublishUrlUnformable _) -> True
    _ -> False
