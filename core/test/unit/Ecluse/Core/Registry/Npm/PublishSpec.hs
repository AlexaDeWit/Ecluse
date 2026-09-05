-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Registry.Npm.PublishSpec (spec) where

import Data.Text qualified as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types.Status (status200, status404, status409, status500)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)

import Ecluse.Core.Fault (TransportFault (tfDetail))
import Ecluse.Core.Registry (
    FetchFault (FetchTransport),
    MirrorArtifact (maSize),
    PublishError (publishErrorMessage),
    PublishFault (PublishFetch, PublishRejected),
 )
import Ecluse.Core.Registry.Npm.Publish (npmPublishCodec, npmPublishDocument)
import Ecluse.Core.Registry.Publish (
    MirrorPublish (mpPublishArtifact),
    MirrorTransport (MirrorTransport, ptLimits, ptManager, ptMintToken),
    newMirrorPublish,
 )
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Test.Package (v1_0_0, validSha1)
import Ecluse.Test.Registry.Npm (dummyArtifact, isOdd)

import Ecluse.Test.Stub (
    Stub,
    capBody,
    capMethod,
    capPath,
    headerValue,
    lastCaptured,
    stubBaseUrl,
    withStub,
 )

spec :: Spec
spec = publishSpec

{- | The npm mirror write, driven exactly as production runs it: 'npmPublishCodec'
married to the shared transport ('newMirrorPublish') against a recording stub.
-}
publishSpec :: Spec
publishSpec = describe "the npm mirror write (codec over the shared transport)" $ do
    it "PUTs the publish document to the package path" $
        withStub status200 "{}" $ \stub -> do
            publish <- stubPublish stub
            _ <- mpPublishArtifact publish isOdd v1_0_0 sizedArtifact dummyTarballBytes
            cap <- lastCaptured stub
            capMethod cap `shouldBe` "PUT"
            capPath cap `shouldBe` "/is-odd"
            capBody cap `shouldBe` publishDoc
            -- The publish must declare a content type of application/json: a
            -- spec-compliant registry (e.g. Verdaccio) 415s a publish that omits it.
            headerValue "content-type" cap `shouldBe` Just "application/json"

    it "treats a 2xx as success" $
        withStub status200 "{}" $ \stub -> do
            publish <- stubPublish stub
            mpPublishArtifact publish isOdd v1_0_0 sizedArtifact dummyTarballBytes `shouldReturn` Right ()

    it "treats a 409 Conflict as idempotent success (the immutable version is already present)" $
        withStub status409 "{\"error\":\"version already exists\"}" $ \stub -> do
            publish <- stubPublish stub
            mpPublishArtifact publish isOdd v1_0_0 sizedArtifact dummyTarballBytes `shouldReturn` Right ()

    it "reports a 404 as a publish error naming the status (so the mirror job is retried)" $
        withStub status404 "{\"error\":\"Not found\"}" $ \stub -> do
            publish <- stubPublish stub
            outcome <- mpPublishArtifact publish isOdd v1_0_0 sizedArtifact dummyTarballBytes
            -- Force the error message so the failure carries the status it saw.
            leftMessage outcome `shouldSatisfy` maybe False (T.isInfixOf "404")

    it "reports a 500 as a publish error" $
        withStub status500 "boom" $ \stub -> do
            publish <- stubPublish stub
            outcome <- mpPublishArtifact publish isOdd v1_0_0 sizedArtifact dummyTarballBytes
            outcome `shouldSatisfy` isLeft

    it "reports a transport failure as a PublishFetch value, never thrown" $ do
        -- No server listens on this port, so the write throws a connection failure. The transport
        -- must fold it into a retryable PublishFetch value, never throw.
        publish <- publishAt "http://127.0.0.1:1"
        outcome <- mpPublishArtifact publish isOdd v1_0_0 sizedArtifact dummyTarballBytes
        outcome `shouldSatisfy` isTransport

-- The production marriage against a stub's endpoint: anonymous mint, a no-TLS
-- manager, and the secure-default response bounds.
stubPublish :: Stub -> IO MirrorPublish
stubPublish stub = publishAt (stubBaseUrl stub)

publishAt :: Text -> IO MirrorPublish
publishAt targetUrl = do
    manager <- newManager defaultManagerSettings
    let transport = MirrorTransport{ptManager = manager, ptMintToken = pure Nothing, ptLimits = defaultLimits}
    pure (newMirrorPublish transport (loopbackRegistryUrl targetUrl) npmPublishCodec)

-- | The shared descriptor, sized: the npm codec reports the attachment length it declares.
sizedArtifact :: MirrorArtifact
sizedArtifact = dummyArtifact{maSize = Just 1234}

dummyTarballBytes :: ByteString
dummyTarballBytes = "tarball-bytes"

-- | The expected publish document assembled by the codec.
publishDoc :: ByteString
publishDoc = npmPublishDocument isOdd v1_0_0 "is-odd-1.0.0.tgz" Nothing (Just validSha1) dummyTarballBytes

{- | The (forced) error message of a publish 'Left', or 'Nothing' on a 'Right'.
Forcing the message exercises the error-construction path.
-}
leftMessage :: Either PublishFault a -> Maybe Text
leftMessage outcome = case outcome of
    Left (PublishRejected err) -> Just (publishErrorMessage err)
    Left (PublishFetch (FetchTransport fault)) -> Just (tfDetail fault)
    Left (PublishFetch _) -> Nothing
    Right _ -> Nothing

-- | Whether a publish outcome is the retryable transport fault (a value, not a throw).
isTransport :: Either PublishFault a -> Bool
isTransport = \case
    Left (PublishFetch (FetchTransport _)) -> True
    _ -> False
