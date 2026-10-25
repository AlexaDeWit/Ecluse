-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native Cargo upload framing rejects ambiguous JSON and retains the exact archive bytes.
module Ecluse.Core.Registry.Cargo.Publish.Body (
    CargoUploadBody,
    decodeUpload,
    readUpload,
    uploadMetadataValue,
    uploadName,
    uploadVersion,
    uploadArchive,
    encodeUpload,
) where

import Data.Aeson (Value (..), encode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Builder (byteString, lazyByteString, toLazyByteString, word32LE)
import Data.Bits (shiftL, (.|.))

import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.Cargo.Project (projectName)
import Ecluse.Core.Registry.Cargo.Wire (decodeCargoRecord)
import Ecluse.Core.Security.Limits (Limits (..))
import Ecluse.Core.Version.Cargo (parseCargo)

-- | The decoded claims and archive remain paired with one fully consumed native body.
data CargoUploadBody = CargoUploadBody Value Text Text ByteString
    deriving stock (Eq, Show)

-- | Publication metadata excludes both length prefixes and the compressed archive.
uploadMetadataValue :: CargoUploadBody -> Value
uploadMetadataValue (CargoUploadBody value _ _ _) = value

-- | Preserve the declared published spelling for archive agreement.
uploadName :: CargoUploadBody -> Text
uploadName (CargoUploadBody _ name _ _) = name

-- | Preserve the declared raw native version, including build metadata.
uploadVersion :: CargoUploadBody -> Text
uploadVersion (CargoUploadBody _ _ version _) = version

-- | These bytes are a slice of the validated original body.
uploadArchive :: CargoUploadBody -> ByteString
uploadArchive (CargoUploadBody _ _ _ bytes) = bytes

-- | Decode strict native framing and JSON under the configured request and metadata bounds.
decodeUpload :: Limits -> ByteString -> Either ParseError CargoUploadBody
decodeUpload limits bytes = splitUpload limits bytes >>= uncurry (decodeSections limits)

-- | Charge the JSON tree before decoding. The caller separately reserves the original body.
readUpload :: Limits -> (Int -> IO ()) -> ByteString -> IO (Either ParseError CargoUploadBody)
readUpload limits charge bytes = case splitUpload limits bytes of
    Left fault -> pure (Left fault)
    Right (metadata, archive) -> do
        charge (metadataCharge metadata)
        pure (decodeSections limits metadata archive)

metadataCharge :: ByteString -> Int
metadataCharge bytes = fromInteger (min (toInteger (maxBound :: Int)) (65536 + 64 * toInteger (BS.length bytes)))

splitUpload :: Limits -> ByteString -> Either ParseError (ByteString, ByteString)
splitUpload limits bytes = do
    when (BS.length bytes > maxPublishRequestBytes limits) (bad "Cargo upload exceeds its request byte bound")
    (metadata, rest) <- section (maxMetadataBytes limits) bytes
    (archive, trailing) <- section (maxMirrorArtifactBytes limits) rest
    unless (BS.null trailing) (bad "Cargo upload contains trailing bytes")
    when (BS.null archive) (bad "Cargo upload has no archive")
    pure (metadata, archive)

section :: Int -> ByteString -> Either ParseError (ByteString, ByteString)
section cap bytes = do
    when (BS.length bytes < 4) (bad "Cargo upload has a truncated length prefix")
    let declared = foldl' (.|.) 0 [toInteger (BS.index bytes offset) `shiftL` (8 * offset) | offset <- [0 .. 3]]
        available = BS.drop 4 bytes
    when (cap <= 0 || declared > toInteger cap) (bad "Cargo upload section exceeds its byte bound")
    when (declared > toInteger (BS.length available)) (bad "Cargo upload has a truncated section")
    pure (BS.splitAt (fromInteger declared) available)

decodeSections :: Limits -> ByteString -> ByteString -> Either ParseError CargoUploadBody
decodeSections limits metadata archive = do
    value <- first (const (ParseError "Cargo upload metadata is malformed or exceeds its nesting bound")) (decodeCargoRecord (maxNestingDepth limits) metadata)
    fields <- case value of
        Object objectFields -> Right objectFields
        _ -> bad "Cargo upload metadata is not an object"
    name <- textField "name" fields
    version <- textField "vers" fields
    void (first (const (ParseError "Cargo upload name is invalid")) (projectName name))
    void (maybe (bad "Cargo upload version is invalid") Right (parseCargo version))
    for_ ["deps", "authors", "keywords", "categories"] $ \key -> case KeyMap.lookup key fields of
        Just (Array _) -> Right ()
        _ -> bad "Cargo upload lacks a required metadata array"
    case KeyMap.lookup "features" fields of
        Just (Object features) -> for_ features $ \case
            Array entries | all isText entries -> Right ()
            _ -> bad "Cargo upload has invalid feature declarations"
        _ -> bad "Cargo upload lacks its feature map"
    pure (CargoUploadBody value name version archive)
  where
    textField key fields = case KeyMap.lookup key fields of
        Just (String value) -> Right value
        _ -> bad "Cargo upload lacks a native identity field"
    isText = \case
        String _ -> True
        _ -> False

-- | Encode native uint32 framing without changing either the JSON value or verified archive bytes.
encodeUpload :: Value -> ByteString -> Either ParseError LByteString
encodeUpload metadata archive = do
    let json = encode metadata
    unless (toInteger (LBS.length json) <= toInteger (maxBound :: Word32) && toInteger (BS.length archive) <= toInteger (maxBound :: Word32)) (bad "Cargo upload cannot fit uint32 framing")
    pure . toLazyByteString $ word32LE (fromIntegral (LBS.length json)) <> lazyByteString json <> word32LE (fromIntegral (BS.length archive)) <> byteString archive

bad :: Text -> Either ParseError a
bad = Left . ParseError
