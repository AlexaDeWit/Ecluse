-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TupleSections #-}

-- | The native multipart writer exercised through the shared HTTP transport.
module Ecluse.Core.Registry.PyPI.PublishSpec (spec) where

import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.List (lookup)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types.Status (status200)
import Network.Wai.Parse qualified as Wai
import Test.Hspec

import Data.Aeson qualified as Aeson
import Ecluse.Core.Credential (ClientCredential (..), mkSecret)
import Ecluse.Core.Package (PackageInfo (infoVersions))
import Ecluse.Core.Registry (PublishFault (PublishSourceUnavailable))
import Ecluse.Core.Registry.CachedDocument (pypiSimpleCached)
import Ecluse.Core.Registry.Metadata (VersionDoc (..))
import Ecluse.Core.Registry.MirrorUpload (inventoryFiles, inventoryPackage, inventoryVersion, verifyMirrorBytes)
import Ecluse.Core.Registry.Publish (MirrorPublish (..), MirrorTransport (..), PublishCodec (..), PublishPlan (..), newMirrorPublish, pmvInventory, pmvPrepareUpload, publishPrepared)
import Ecluse.Core.Registry.PyPI.Publish (classifyPublish, pypiPublishCodec)
import Ecluse.Core.Registry.Request (nativePresentation)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Security.Limits (defaultLimits)
import Ecluse.Test.Deadline (withExchangeClient, withSweeper)
import Ecluse.Test.Osv (osvZipOf)
import Ecluse.Test.Package (pypiVersion, unscopedPyPI)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIIndex)
import Ecluse.Test.Stub (capBody, capMethod, capPath, headerValue, lastCaptured, stubBaseUrl, withStub)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    describe "PyPI mirror HTTP upload" $
        it "posts exact verified bytes, native fields, and the supplied Basic pair to the declared endpoint" $
            withStub status200 "ok" $ \stub -> do
                bytes <- BSL.toStrict <$> osvZipOf [("example-1.dist-info/METADATA", "Metadata-Version: 2.4\nName: example\nVersion: 1\nRequires-Dist: dependency>=2; extra == 'test'\nProvides-Extra: test\n\nA description.\n")]
                snapshot <- sourceSnapshot bytes
                withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
                    let target = loopbackRegistryUrl (stubBaseUrl stub <> "/custom-upload")
                        transport = MirrorTransport exchanges (pure (Just (ClientCredential (Just "publisher") (mkSecret "password")))) defaultLimits passthroughTracingPort
                        writer = newMirrorPublish transport (loopbackRegistryUrl (stubBaseUrl stub)) target (pypiPublishCodec nativePresentation)
                    version <- expectRight (mpPrepareVersion writer (unscopedPyPI "example") (pypiVersion "1") snapshot)
                    let inventory = pmvInventory version
                        file = NE.head (inventoryFiles inventory)
                    inventoryPackage inventory `shouldBe` unscopedPyPI "example"
                    inventoryVersion inventory `shouldBe` pypiVersion "1"
                    verified <- expectRight (verifyMirrorBytes file bytes)
                    prepared <- pmvPrepareUpload version (const (pure ())) file verified >>= expectRight
                    publishPrepared prepared (PublishPlan (pypiVersion "1")) `shouldReturn` Right ()
                    captured <- lastCaptured stub
                    capMethod captured `shouldBe` "POST"
                    capPath captured `shouldBe` "/custom-upload"
                    headerValue "authorization" captured `shouldBe` Just "Basic cHVibGlzaGVyOnBhc3N3b3Jk"
                    contentType <- maybe (fail "missing multipart content type") pure (headerValue "content-type" captured)
                    let (_, attributes) = Wai.parseContentType contentType
                    boundary <- maybe (fail "missing multipart boundary") pure (lookup "boundary" attributes)
                    ref <- newIORef (capBody captured)
                    (fields, files) <- Wai.sinkRequestBody Wai.lbsBackEnd (Wai.Multipart boundary) (atomicModifyIORef' ref (BS.empty,))
                    lookup ":action" fields `shouldBe` Just "file_upload"
                    lookup "protocol_version" fields `shouldBe` Just "1"
                    lookup "filetype" fields `shouldBe` Just "bdist_wheel"
                    lookup "pyversion" fields `shouldBe` Just "py3"
                    lookup "sha256_digest" fields `shouldBe` Just (encodeUtf8 (sha256 bytes))
                    lookup "requires_dist" fields `shouldBe` Just "dependency>=2; extra == 'test'"
                    map (BSL.toStrict . Wai.fileContent . snd) files `shouldBe` [bytes]
    describe "PyPI upload preparation" $
        for_ ["example-1-py3-none-any.whl", "example-1.zip"] $ \filename ->
            it ("refuses verified malformed archive bytes for " <> toString filename) $ do
                let bytes = "not an archive"
                snapshot <- sourceSnapshotFor filename bytes
                case pypiPublishCodec nativePresentation of
                    PublishCodec{pcPrepareSource = prepareSource, pcPrepareUpload = prepareUpload} -> withSweeper $ \deadlines -> do
                        (inventory, source) <- expectRight (prepareSource (unscopedPyPI "example") (pypiVersion "1") snapshot)
                        let file = NE.head (inventoryFiles inventory)
                        verified <- expectRight (verifyMirrorBytes file bytes)
                        prepared <- prepareUpload deadlines defaultLimits (const (pure ())) inventory source file verified
                        void prepared `shouldBe` Left (PublishSourceUnavailable "missing or ambiguous ZIP end record")
    describe "classifyPublish" $ do
        it "recognises duplicate filename responses but preserves unrelated 400 failures" $ do
            classifyPublish 400 "File already exists. See https://pypi.org/help/#file-name-reuse" `shouldBe` Right ()
            classifyPublish 400 "A file with the same name already exists" `shouldBe` Right ()
            classifyPublish 400 "File already exists ('example.whl', with blake2_256 hash 'abc'). See https://pypi.org/help/#file-name-reuse" `shouldBe` Right ()
            classifyPublish 400 "Invalid metadata" `shouldSatisfy` isLeft
            classifyPublish 403 "File already exists." `shouldSatisfy` isLeft
            classifyPublish 500 "File already exists." `shouldSatisfy` isLeft

sourceSnapshot :: ByteString -> IO VersionDoc
sourceSnapshot = sourceSnapshotFor "example-1-py3-none-any.whl"

sourceSnapshotFor :: Text -> ByteString -> IO VersionDoc
sourceSnapshotFor filename bytes = do
    let index =
            object
                [ "meta" .= object ["api-version" .= ("1.0" :: Text)]
                , "name" .= ("example" :: Text)
                , "files"
                    .= [ object
                            [ "filename" .= filename
                            , "url" .= ("https://files.example.test/" <> filename)
                            , "hashes" .= object ["sha256" .= sha256 bytes]
                            ]
                       ]
                ]
    (info, document) <- expectRight (projectPyPIIndex defaultLimits (unscopedPyPI "example") (BSL.toStrict (Aeson.encode index)))
    details <- maybe (fail "fixture has no projected version") pure (Map.lookup "1" (infoVersions info))
    pure (VersionDoc details (Just (fst pypiSimpleCached document)))

sha256 :: ByteString -> Text
sha256 bytes = show (hash bytes :: Digest SHA256)
