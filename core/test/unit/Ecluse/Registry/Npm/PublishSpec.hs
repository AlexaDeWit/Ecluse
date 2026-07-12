-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Registry.Npm.PublishSpec (spec) where

import Data.Text qualified as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types.Status (status200, status404, status409, status500)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)

import Data.List.NonEmpty qualified as NE
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (HashAlg (..), PackageName, mkPackageName)
import Ecluse.Core.Registry (
    MirrorArtifact (..),
    PublishError (publishErrorMessage),
    PublishFault (PublishRejected, PublishTransport, PublishUrlUnformable),
 )
import Ecluse.Core.Registry.Npm.Publish (npmPublishCodec, npmPublishDocument)
import Ecluse.Core.Registry.Publish (
    MirrorPublish (mpPublishArtifact),
    MirrorTransport (MirrorTransport, ptLimits, ptManager, ptMintToken),
    newMirrorPublish,
 )
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Package (unsafeHash, validSha1)

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
            _ <- mpPublishArtifact publish isOdd v1 dummyArtifact dummyTarballBytes
            cap <- lastCaptured stub
            capMethod cap `shouldBe` "PUT"
            capPath cap `shouldBe` "/is-odd"
            capBody cap `shouldBe` publishDoc
            -- The publish body must be declared application/json: a spec-compliant
            -- registry (e.g. Verdaccio) 415s a publish that omits it.
            headerValue "content-type" cap `shouldBe` Just "application/json"

    it "treats a 2xx as success" $
        withStub status200 "{}" $ \stub -> do
            publish <- stubPublish stub
            mpPublishArtifact publish isOdd v1 dummyArtifact dummyTarballBytes `shouldReturn` Right ()

    it "treats a 409 Conflict as idempotent success (the immutable version is already present)" $
        withStub status409 "{\"error\":\"version already exists\"}" $ \stub -> do
            publish <- stubPublish stub
            mpPublishArtifact publish isOdd v1 dummyArtifact dummyTarballBytes `shouldReturn` Right ()

    it "reports a 404 as a publish error naming the status (so the mirror job is retried)" $
        withStub status404 "{\"error\":\"Not found\"}" $ \stub -> do
            publish <- stubPublish stub
            outcome <- mpPublishArtifact publish isOdd v1 dummyArtifact dummyTarballBytes
            -- Force the error message so the failure carries the status it saw.
            leftMessage outcome `shouldSatisfy` maybe False (T.isInfixOf "404")

    it "reports a 500 as a publish error" $
        withStub status500 "boom" $ \stub -> do
            publish <- stubPublish stub
            outcome <- mpPublishArtifact publish isOdd v1 dummyArtifact dummyTarballBytes
            outcome `shouldSatisfy` isLeft

    it "reports a transport failure as a PublishTransport value, never thrown" $ do
        -- No server listens on this port, so the write throws a connection failure;
        -- the transport must fold it into a PublishTransport value (a retryable
        -- fault), honouring its total, never-thrown contract so the worker's
        -- retry-vs-drop match stays exhaustive.
        publish <- publishAt "http://127.0.0.1:1"
        outcome <- mpPublishArtifact publish isOdd v1 dummyArtifact dummyTarballBytes
        outcome `shouldSatisfy` isTransport

-- The production marriage against a stub's endpoint: anonymous mint, a no-TLS
-- manager, and the secure-default response bounds.
stubPublish :: Stub -> IO MirrorPublish
stubPublish stub = publishAt (stubBaseUrl stub)

publishAt :: Text -> IO MirrorPublish
publishAt targetUrl = do
    manager <- newManager defaultManagerSettings
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
        , maSize = Just 1234
        }

dummyTarballBytes :: ByteString
dummyTarballBytes = "tarball-bytes"

-- | The expected publish document assembled by the codec.
publishDoc :: ByteString
publishDoc = npmPublishDocument isOdd v1 "is-odd-1.0.0.tgz" Nothing (Just validSha1) dummyTarballBytes

{- | The (forced) error message of a publish 'Left', or 'Nothing' on a 'Right'.
Forcing the message exercises the error-construction path.
-}
leftMessage :: Either PublishFault a -> Maybe Text
leftMessage outcome = case outcome of
    Left (PublishRejected err) -> Just (publishErrorMessage err)
    Left (PublishTransport detail) -> Just detail
    Left (PublishUrlUnformable _) -> Nothing
    Right _ -> Nothing

-- | Whether a publish outcome is the retryable transport fault (a value, not a throw).
isTransport :: Either PublishFault a -> Bool
isTransport = \case
    Left (PublishTransport _) -> True
    _ -> False
