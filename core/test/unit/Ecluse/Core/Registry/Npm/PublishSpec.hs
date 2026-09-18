-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Exercise npm mirror publication through a recording transport stub.
Integrity cases connect worker verification to the published document and attachment, and the
field-rewrite cases pin what the published version object keeps, replaces, and strips.
-}
module Ecluse.Core.Registry.Npm.PublishSpec (spec) where

import Data.Aeson (Object, Value (Bool, Number, Object, String), object, toJSON, (.:), (.:?), (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.ByteArray.Encoding (Base (Base64), convertFromBase)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types.Status (status200, status404, status409, status500)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Fault (TransportFault (tfDetail))
import Ecluse.Core.Package (HashAlg (SHA1, SRI), mkHash, mkSriHashes)
import Ecluse.Core.Registry (
    FetchFault (FetchTransport),
    MirrorArtifact (maHashes, maSize),
    PublishError (publishErrorMessage),
    PublishFault (PublishFetch, PublishRejected),
 )
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached, pypiSimpleCached)
import Ecluse.Core.Registry.Npm.Publish (npmPublishCodec, npmPublishDocument)
import Ecluse.Core.Registry.Publish (
    MirrorPublish (mpPublishArtifact),
    MirrorTransport (MirrorTransport, ptLimits, ptManager, ptMintToken),
    PublishPlan (PublishPlan, ppLatest, ppMetadata, ppVersion),
    newMirrorPublish,
 )
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Version (mkVersion)
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
    fieldRewriteSpec

publishSpec :: Spec
publishSpec = describe "the npm mirror write (codec over the shared transport)" $ do
    it "PUTs the publish document to the package path" $
        withStub status200 "{}" $ \stub -> do
            publish <- stubPublish stub
            _ <- mpPublishArtifact publish isOdd planV1 sizedArtifact dummyTarballBytes
            cap <- lastCaptured stub
            capMethod cap `shouldBe` "PUT"
            capPath cap `shouldBe` "/is-odd"
            capBody cap `shouldBe` publishDoc
            headerValue "content-type" cap `shouldBe` Just "application/json"

    it "declares the plan's latest, not the version it publishes" $ do
        let plan = planV1{ppLatest = mkVersion Npm "2.0.0"}
        document <- decodeJsonOrFail (npmPublishDocument isOdd plan "is-odd-1.0.0.tgz" Nothing (Just validSha1) dummyTarballBytes) :: IO Object
        tags <- expectRight (parseEither (.: "dist-tags") document)
        versions <- expectRight (parseEither (.: "versions") document) :: IO Object
        KeyMap.lookup "latest" tags `shouldBe` Just (String "2.0.0")
        KeyMap.keys versions `shouldBe` ["1.0.0"]

    it "treats a 2xx as success" $
        withStub status200 "{}" $ \stub -> do
            publish <- stubPublish stub
            mpPublishArtifact publish isOdd planV1 sizedArtifact dummyTarballBytes `shouldReturn` Right ()

    it "treats a 409 Conflict as idempotent success (the immutable version is already present)" $
        withStub status409 "{\"error\":\"version already exists\"}" $ \stub -> do
            publish <- stubPublish stub
            mpPublishArtifact publish isOdd planV1 sizedArtifact dummyTarballBytes `shouldReturn` Right ()

    it "reports a 404 as a publish error naming the status (so the mirror job is retried)" $
        withStub status404 "{\"error\":\"Not found\"}" $ \stub -> do
            publish <- stubPublish stub
            outcome <- mpPublishArtifact publish isOdd planV1 sizedArtifact dummyTarballBytes
            leftMessage outcome `shouldSatisfy` maybe False (T.isInfixOf "404")

    it "reports a 500 as a publish error" $
        withStub status500 "boom" $ \stub -> do
            publish <- stubPublish stub
            outcome <- mpPublishArtifact publish isOdd planV1 sizedArtifact dummyTarballBytes
            outcome `shouldSatisfy` isLeft

    it "reports a transport failure as a PublishFetch value, never thrown" $ do
        publish <- publishAt "http://127.0.0.1:1"
        outcome <- mpPublishArtifact publish isOdd planV1 sizedArtifact dummyTarballBytes
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
        mpPublishArtifact publish isOdd planV1 artifact dummyTarballBytes `shouldReturn` Right ()
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

fieldRewriteSpec :: Spec
fieldRewriteSpec = describe "the field-rewrite contract on the published version object" $ do
    it "keeps what the author wrote: dependencies, executables, policy inputs, and an unknown field" $ do
        manifest <- publishedManifest (Just sourceVersion)
        forM_ ["dependencies", "bin", "scripts", "engines", "license", "gitHead"] $ \field ->
            KeyMap.lookup field manifest `shouldBe` KeyMap.lookup field sourceObject

    it "keeps a deprecation notice verbatim" $ do
        manifest <- publishedManifest (Just sourceVersion)
        KeyMap.lookup "deprecated" manifest `shouldBe` Just (String "use is-even instead")

    it "rewrites only the validated name and version under local authority" $ do
        manifest <- publishedManifest (Just sourceVersion)
        KeyMap.lookup "name" manifest `shouldBe` Just (String "is-odd")
        KeyMap.lookup "version" manifest `shouldBe` Just (String "1.0.0")

    it "replaces the dist location and digests with the verified ones and keeps the rest of dist" $ do
        dist <- distOf <$> publishedManifest (Just sourceVersion)
        KeyMap.lookup "tarball" dist `shouldBe` Just (String "is-odd-1.0.0.tgz")
        KeyMap.lookup "integrity" dist `shouldBe` Just (String verifiedSri)
        KeyMap.lookup "shasum" dist `shouldBe` Just (String validSha1)
        KeyMap.lookup "unpackedSize" dist `shouldBe` Just (Number 4096)
        KeyMap.lookup "fileCount" dist `shouldBe` Just (Number 3)

    it "strips dist.signatures and dist.attestations, which reference the public registry's own keys" $ do
        dist <- distOf <$> publishedManifest (Just sourceVersion)
        KeyMap.lookup "signatures" dist `shouldBe` Nothing
        KeyMap.lookup "attestations" dist `shouldBe` Nothing

    it "strips every underscore-prefixed registry bookkeeping field" $ do
        manifest <- publishedManifest (Just sourceVersion)
        filter (T.isPrefixOf "_" . Key.toText) (KeyMap.keys manifest) `shouldBe` []

    it "never lets an unverified source digest survive the absence of a verified one" $ do
        document <- decodeJsonOrFail (npmPublishDocument isOdd (planWith (Just (fst npmCached sourceVersion))) "is-odd-1.0.0.tgz" Nothing Nothing dummyTarballBytes) :: IO Object
        dist <- distOf <$> (expectRight (parseEither (\o -> o .: "versions" >>= (.: "1.0.0")) document) :: IO Object)
        KeyMap.lookup "integrity" dist `shouldBe` Nothing
        KeyMap.lookup "shasum" dist `shouldBe` Nothing
        KeyMap.lookup "tarball" dist `shouldBe` Just (String "is-odd-1.0.0.tgz")

    it "declares the minimum when no version object was carried" $ do
        manifest <- publishedManifest Nothing
        sort (KeyMap.keys manifest) `shouldBe` ["dist", "name", "version"]

    it "declares the minimum for a document another ecosystem injected, or one that is not an object" $ do
        injected <- publishedManifestOf (Just (fst pypiSimpleCached sourceVersion))
        scalar <- publishedManifestOf (Just (fst npmCached (String "not an object")))
        sort (KeyMap.keys injected) `shouldBe` ["dist", "name", "version"]
        sort (KeyMap.keys scalar) `shouldBe` ["dist", "name", "version"]
  where
    publishedManifest :: Maybe Value -> IO Object
    publishedManifest = publishedManifestOf . fmap (fst npmCached)
    publishedManifestOf :: Maybe CachedDoc -> IO Object
    publishedManifestOf raw = do
        document <- decodeJsonOrFail (npmPublishDocument isOdd (planWith raw) "is-odd-1.0.0.tgz" (Just verifiedSri) (Just validSha1) dummyTarballBytes) :: IO Object
        expectRight (parseEither (\o -> o .: "versions" >>= (.: "1.0.0")) document)
    planWith raw = planV1{ppMetadata = raw}
    distOf :: Object -> Object
    distOf manifest = case KeyMap.lookup "dist" manifest of
        Just (Object dist) -> dist
        _ -> mempty
    verifiedSri = sriSha512Of dummyTarballBytes

-- The version object as the public registry serves it: author fields, registry bookkeeping, and a
-- name, version, and dist that must not reach the mirror as written.
sourceVersion :: Value
sourceVersion = Object sourceObject

sourceObject :: Object
sourceObject =
    KeyMap.fromList
        [ "name" .= ("shadowed-name" :: Text)
        , "version" .= ("9.9.9" :: Text)
        , "dist"
            .= object
                [ "tarball" .= ("https://registry.npmjs.org/is-odd/-/is-odd-1.0.0.tgz" :: Text)
                , "integrity" .= ("sha512-forged" :: Text)
                , "shasum" .= ("0000000000000000000000000000000000000000" :: Text)
                , "signatures" .= [object ["keyid" .= ("SHA256:npm" :: Text), "sig" .= ("MEUC" :: Text)]]
                , "attestations" .= object ["url" .= ("https://registry.npmjs.org/-/npm/v1/attestations/is-odd@1.0.0" :: Text)]
                , "unpackedSize" .= (4096 :: Int)
                , "fileCount" .= (3 :: Int)
                ]
        , "dependencies" .= object ["is-number" .= ("^6.0.0" :: Text)]
        , "bin" .= object ["is-odd" .= ("cli.js" :: Text)]
        , "scripts" .= object ["test" .= ("jest" :: Text)]
        , "engines" .= object ["node" .= (">=18" :: Text)]
        , "license" .= ("MIT" :: Text)
        , "deprecated" .= ("use is-even instead" :: Text)
        , "gitHead" .= ("0123456789abcdef0123456789abcdef01234567" :: Text)
        , "_id" .= ("is-odd@1.0.0" :: Text)
        , "_npmUser" .= object ["name" .= ("publisher" :: Text)]
        , "_nodeVersion" .= ("20.11.0" :: Text)
        , "_hasShrinkwrap" .= Bool False
        ]

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

-- A write of @1.0.0@ that also declares it latest, the shape most cases here do not vary.
planV1 :: PublishPlan
planV1 = PublishPlan{ppVersion = v1_0_0, ppLatest = v1_0_0, ppMetadata = Nothing}

publishDoc :: ByteString
publishDoc = npmPublishDocument isOdd planV1 "is-odd-1.0.0.tgz" Nothing (Just validSha1) dummyTarballBytes

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
