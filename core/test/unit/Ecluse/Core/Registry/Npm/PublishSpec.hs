-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Exercise npm mirror publication through a recording transport stub.
Integrity cases connect worker verification to the published document and attachment.
-}
module Ecluse.Core.Registry.Npm.PublishSpec (spec) where

import Data.Aeson (Object, Value (String), toJSON, (.:), (.:?))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.ByteArray.Encoding (Base (Base64), convertFromBase)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types.Status (status200, status404, status409, status500)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)

import Ecluse.Core.Fault (TransportFault (tfDetail))
import Ecluse.Core.Package (HashAlg (SHA1, SRI), mkHash, mkSriHashes)
import Ecluse.Core.Registry (
    FetchFault (FetchTransport),
    MirrorArtifact (maHashes, maSize),
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
import Ecluse.Core.Worker.Integrity (IntegrityResult (IntegrityVerified), verifyIntegrity)
import Ecluse.Test.Package (hexSha1Of, sriSha256Of, sriSha512Of, unsafeHash, v1_0_0, validSha1)
import Ecluse.Test.Registry.Npm (dummyArtifact, isOdd)
import Ecluse.Test.Support (decodeJsonOrFail, expectRight)

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

-- | npm publication outcomes and the integrity carried with attached bytes.
spec :: Spec
spec = do
    publishSpec
    integritySpec

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
            leftMessage outcome `shouldSatisfy` maybe False (T.isInfixOf "404")

    it "reports a 500 as a publish error" $
        withStub status500 "boom" $ \stub -> do
            publish <- stubPublish stub
            outcome <- mpPublishArtifact publish isOdd v1_0_0 sizedArtifact dummyTarballBytes
            outcome `shouldSatisfy` isLeft

    it "reports a transport failure as a PublishFetch value, never thrown" $ do
        publish <- publishAt "http://127.0.0.1:1"
        outcome <- mpPublishArtifact publish isOdd v1_0_0 sizedArtifact dummyTarballBytes
        outcome `shouldSatisfy` isTransport

integritySpec :: Spec
integritySpec = describe "verified integrity in the published document" $ do
    let matching = sriSha512Of dummyTarballBytes
        nonmatching = sriSha512Of "other tarball bytes"
        weaker = sriSha256Of dummyTarballBytes
    for_
        [ ("keeps a nonmatching alternative before a matching alternative", [nonmatching, matching], Just (nonmatching <> " " <> matching))
        , ("keeps a matching alternative before a nonmatching alternative", [matching, nonmatching], Just (matching <> " " <> nonmatching))
        , ("drops a weaker first token and keeps every strongest alternative", [weaker, nonmatching, matching], Just (nonmatching <> " " <> matching))
        , ("preserves a single SRI token", [matching], Just matching)
        , ("preserves a SHA1-only artifact without adding SRI", [], Nothing)
        ]
        $ \(label, tokens, expectedIntegrity) ->
            it label (assertPublishedIntegrity tokens expectedIntegrity)

assertPublishedIntegrity :: [Text] -> Maybe Text -> Expectation
assertPublishedIntegrity tokens expectedIntegrity =
    withStub status200 "{}" $ \stub -> do
        let shasum = T.toUpper (hexSha1Of dummyTarballBytes)
            hashes = unsafeHash SHA1 shasum :| map (unsafeHash SRI) tokens
            artifact = sizedArtifact{maHashes = hashes}
        verifyIntegrity hashes dummyTarballBytes `shouldBe` IntegrityVerified
        publish <- stubPublish stub
        mpPublishArtifact publish isOdd v1_0_0 artifact dummyTarballBytes `shouldReturn` Right ()
        cap <- lastCaptured stub
        document <- decodeJsonOrFail (capBody cap) :: IO Object
        manifest <- expectRight (parseEither (\o -> o .: "versions" >>= (.: "1.0.0")) document)
        dist <- expectRight (parseEither (.: "dist") manifest)
        attachment <- expectRight (parseEither (\o -> o .: "_attachments" >>= (.: "is-odd-1.0.0.tgz")) document)
        tags <- expectRight (parseEither (.: "dist-tags") document)
        KeyMap.lookup "_id" document `shouldBe` Just (String "is-odd")
        KeyMap.lookup "name" document `shouldBe` Just (String "is-odd")
        KeyMap.lookup "latest" tags `shouldBe` Just (String "1.0.0")
        KeyMap.lookup "name" manifest `shouldBe` Just (String "is-odd")
        KeyMap.lookup "version" manifest `shouldBe` Just (String "1.0.0")
        KeyMap.lookup "tarball" dist `shouldBe` Just (String "is-odd-1.0.0.tgz")
        KeyMap.lookup "integrity" dist `shouldBe` (String <$> expectedIntegrity)
        KeyMap.lookup "shasum" dist `shouldBe` Just (String shasum)
        KeyMap.lookup "content_type" attachment `shouldBe` Just (String "application/octet-stream")
        KeyMap.lookup "length" attachment `shouldBe` Just (toJSON (BS.length dummyTarballBytes))
        encoded <- expectRight (parseEither (.: "data") attachment) :: IO Text
        bytes <- expectRight (convertFromBase Base64 (encodeUtf8 encoded :: ByteString))
        bytes `shouldBe` dummyTarballBytes
        integrity <- expectRight (parseEither (.:? "integrity") dist)
        case integrity of
            Just carrier -> do
                publishedHashes <- expectRight (mkSriHashes carrier)
                verifyIntegrity publishedHashes bytes `shouldBe` IntegrityVerified
            Nothing -> do
                raw <- expectRight (parseEither (.: "shasum") dist)
                publishedHash <- expectRight (mkHash SHA1 raw)
                verifyIntegrity (publishedHash :| []) bytes `shouldBe` IntegrityVerified

stubPublish :: Stub -> IO MirrorPublish
stubPublish stub = publishAt (stubBaseUrl stub)

publishAt :: Text -> IO MirrorPublish
publishAt targetUrl = do
    manager <- newManager defaultManagerSettings
    let transport = MirrorTransport{ptManager = manager, ptMintToken = pure Nothing, ptLimits = defaultLimits}
    pure (newMirrorPublish transport (loopbackRegistryUrl targetUrl) npmPublishCodec)

sizedArtifact :: MirrorArtifact
sizedArtifact = dummyArtifact{maSize = Just 1234}

dummyTarballBytes :: ByteString
dummyTarballBytes = "tarball-bytes"

publishDoc :: ByteString
publishDoc = npmPublishDocument isOdd v1_0_0 "is-odd-1.0.0.tgz" Nothing (Just validSha1) dummyTarballBytes

leftMessage :: Either PublishFault a -> Maybe Text
leftMessage outcome = case outcome of
    Left (PublishRejected err) -> Just (publishErrorMessage err)
    Left (PublishFetch (FetchTransport fault)) -> Just (tfDetail fault)
    Left (PublishFetch _) -> Nothing
    Right _ -> Nothing

isTransport :: Either PublishFault a -> Bool
isTransport = \case
    Left (PublishFetch (FetchTransport _)) -> True
    _ -> False
