-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | PyPI mirror uploads bind archive metadata to the same verified distribution bytes.
module Ecluse.Core.Registry.PyPI.Publish (
    pypiPublishCodec,
    publishRequest,
    classifyPublish,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import Data.Text qualified as T
import Network.HTTP.Client (Request (method, requestBody, requestHeaders), RequestBody (RequestBodyLBS))
import Network.HTTP.Types.Header (hContentType)

import Ecluse.Core.Credential (ClientCredential, credUsername)
import Ecluse.Core.Package (Artifact (artFilename), hashValue)
import Ecluse.Core.Registry (FetchFault (FetchUrlUnformable), ParseError (parseErrorMessage), PublishError (..), PublishFault (..), isSuccessStatus)
import Ecluse.Core.Registry.CachedDocument (pypiSimpleCached)
import Ecluse.Core.Registry.Metadata (VersionDoc (vdRaw))
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (presentedRead, staticRead)
import Ecluse.Core.Registry.MirrorUpload
import Ecluse.Core.Registry.MirrorUpload.Internal (checkedMirrorUploadData)
import Ecluse.Core.Registry.Publish (PublishCodec (..))
import Ecluse.Core.Registry.PyPI.Archive (archiveMetadata)
import Ecluse.Core.Registry.PyPI.Credential (pypiCredential)
import Ecluse.Core.Registry.PyPI.Metadata (pypiChargeFactors, pypiRead)
import Ecluse.Core.Registry.PyPI.Request (simpleIndexRequest)
import Ecluse.Core.Registry.PyPI.UploadMetadata (PyPIUploadMetadata, parseUploadMetadata, uploadFields, uploadMetadataMemoryBytes)
import Ecluse.Core.Registry.Request (CredentialMapping, ServicePresentation, StoreOperation (StoreRead, StoreWrite), attachCredential, parseRequestEither, presentedMapping, presentedRequest)
import Ecluse.Core.Security.Limits (Limits (maxMetadataBytes))
import Ecluse.Core.Server.Admission.Types (ChargeFactors (cfFullReadPermille))

-- | Read native target inventories and prepare one checked multipart upload per distribution.
pypiPublishCodec :: ServicePresentation -> PublishCodec
pypiPublishCodec presentation =
    PublishCodec
        { pcPrepareSource = \package version doc -> do
            source <- maybeToRight (PublishSourceUnavailable "PyPI snapshot has no paired source metadata") (vdRaw doc >>= snd pypiSimpleCached)
            inventory <- checkedMirrorInventory package version doc (PyPIMirrorRead source)
            pure (inventory, ())
        , pcPrepareUpload = \_ limits charge inventory () file bytes -> do
            extracted <- archiveMetadata limits charge (inventoryPackage inventory) (inventoryVersion inventory) (artFilename (expectedArtifact file)) (verifiedBytes bytes)
            case first (PublishSourceUnavailable . parseErrorMessage) extracted of
                Left fault -> pure (Left fault)
                Right raw -> case first (PublishSourceUnavailable . parseErrorMessage) (uploadMetadataMemoryBytes raw) of
                    Left fault -> pure (Left fault)
                    Right weight -> do
                        charge weight
                        pure $ do
                            metadata <- first (PublishSourceUnavailable . parseErrorMessage) (parseUploadMetadata (maxMetadataBytes limits) (inventoryPackage inventory) (inventoryVersion inventory) raw)
                            checkedMirrorUploadData inventory file bytes metadata
        , pcMirrorCompanion = Nothing
        , pcNativeComplete = \_ _ _ -> False
        , pcTargetRead = presentedRead readAs (staticRead pypiRead)
        , pcTargetReadPermille = cfFullReadPermille pypiChargeFactors
        , pcProbeRequest = \target credential name -> presentedRequest readAs credential (\anonymous -> simpleIndexRequest target anonymous name)
        , pcVersionListWalk = Nothing
        , pcPrepareTarget = \_ target -> pure (Right target)
        , pcPublishRequest = \target credential _ -> publishRequest (presentedMapping presentation StoreWrite pypiCredential) target credential
        , pcPublishOutcome = classifyPublish
        }
  where
    readAs = presentedMapping presentation StoreRead pypiCredential

-- | Post to the exact configured endpoint using the supplied credential pair, rendered under @writeAs@, and verified bytes.
publishRequest :: CredentialMapping -> Text -> Maybe ClientCredential -> MirrorUploadData PyPIUploadMetadata -> Either PublishFault Request
publishRequest writeAs target credential upload = do
    for_ credential $ \presented ->
        when (isNothing (credUsername presented)) $
            Left (PublishSourceUnavailable "PyPI upload requires an explicit credential username")
    base <- first (PublishFetch . FetchUrlUnformable) (parseRequestEither target)
    let filename = artFilename (expectedArtifact (uploadFile upload))
        bytes = uploadBytes upload
        digest = hashValue (uploadTargetHash upload)
        boundary = "ecluse-" <> encodeUtf8 (T.take 48 digest)
    python <- pythonTag filename
    let fields = [(":action", "file_upload"), ("protocol_version", "1"), ("filetype", if ".whl" `T.isSuffixOf` filename then "bdist_wheel" else "sdist"), ("pyversion", python), ("sha256_digest", digest)] <> uploadFields (uploadMetadata upload)
    when (T.any (`elem` ['\r', '\n', '"', '\\']) filename) (Left (PublishSourceUnavailable "unsafe multipart filename"))
    when (BS.isInfixOf boundary bytes || any (BS.isInfixOf boundary . encodeUtf8 . snd) fields) (Left (PublishSourceUnavailable "multipart boundary collides with upload content"))
    let body = foldMap (fieldPart boundary) fields <> filePart boundary filename bytes <> byteString ("--" <> boundary <> "--\r\n")
    pure . attachCredential writeAs credential $
        base
            { method = "POST"
            , requestHeaders = (hContentType, "multipart/form-data; boundary=" <> boundary) : requestHeaders base
            , requestBody = RequestBodyLBS (toLazyByteString body)
            }

pythonTag :: Text -> Either PublishFault Text
pythonTag filename = case T.stripSuffix ".whl" filename of
    Nothing -> Right "source"
    Just stem -> case reverse (T.splitOn "-" stem) of
        _platform : _abi : python : _ | not (T.null python) -> Right python
        _ -> Left (PublishSourceUnavailable "wheel filename has no Python tag")

fieldPart :: ByteString -> (Text, Text) -> Builder
fieldPart boundary (name, value) = byteString ("--" <> boundary <> "\r\nContent-Disposition: form-data; name=\"" <> encodeUtf8 name <> "\"\r\n\r\n" <> encodeUtf8 value <> "\r\n")

filePart :: ByteString -> Text -> ByteString -> Builder
filePart boundary filename bytes =
    byteString ("--" <> boundary <> "\r\nContent-Disposition: form-data; name=\"content\"; filename=\"" <> encodeUtf8 filename <> "\"\r\nContent-Type: application/octet-stream\r\n\r\n")
        <> byteString bytes
        <> byteString "\r\n"

-- | A duplicate-file response still requires the worker's independent target digest proof.
classifyPublish :: Int -> ByteString -> Either PublishFault ()
classifyPublish status body
    | isSuccessStatus status = Right ()
    | status == 400 && duplicateFile body = Right ()
    | otherwise = Left (PublishRejected (PublishError ("PyPI upload failed with HTTP status " <> show status)))

duplicateFile :: ByteString -> Bool
duplicateFile body =
    "File already exists." `BS.isInfixOf` body
        || ("File already exists (" `BS.isInfixOf` body && "with blake2_256 hash" `BS.isInfixOf` body)
        || "A file with the same name already exists" `BS.isInfixOf` body
