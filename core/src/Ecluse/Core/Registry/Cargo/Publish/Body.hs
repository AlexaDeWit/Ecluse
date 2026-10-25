-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native Cargo upload framing rejects ambiguous JSON and retains the exact archive bytes.
module Ecluse.Core.Registry.Cargo.Publish.Body (
    CargoUploadBody,
    decodeUpload,
    readUpload,
    uploadName,
    uploadVersion,
    uploadArchive,
    encodeUpload,
) where

import Data.Aeson (Value, encode, withObject, (.:), (.:?))
import Data.Aeson.Types (Parser, parseEither)
import Data.Bits (shiftL, (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Builder (byteString, lazyByteString, toLazyByteString, word32LE)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.Cargo.Project (projectName)
import Ecluse.Core.Registry.Cargo.Wire (decodeCargoRecord)
import Ecluse.Core.Security.Limits (Limits (..))
import Ecluse.Core.Version.Cargo (parseCargo)

-- | The decoded claims and archive remain paired with one fully consumed native body.
data CargoUploadBody = CargoUploadBody Text Text ByteString
    deriving stock (Eq, Show)

-- | Preserve the declared published spelling for archive agreement.
uploadName :: CargoUploadBody -> Text
uploadName (CargoUploadBody name _ _) = name

-- | Preserve the declared raw native version, including build metadata.
uploadVersion :: CargoUploadBody -> Text
uploadVersion (CargoUploadBody _ version _) = version

-- | These bytes are a slice of the validated original body.
uploadArchive :: CargoUploadBody -> ByteString
uploadArchive (CargoUploadBody _ _ bytes) = bytes

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
    (name, version) <- first (ParseError . toText) (parseEither metadataIdentity value)
    pure (CargoUploadBody name version archive)

metadataIdentity :: Value -> Parser (Text, Text)
metadataIdentity = withObject "Cargo upload metadata" $ \fields -> do
    name <- fields .: "name"
    version <- fields .: "vers"
    nativeName name
    unless (isJust (parseCargo version)) (fail "Cargo upload version is invalid")
    dependencies <- fields .: "deps"
    traverse_ uploadDependency (dependencies :: [Value])
    void (fields .: "features" :: Parser (Map Text [Text]))
    for_ ["authors", "keywords", "categories"] $ \key ->
        void (fields .: key :: Parser [Text])
    for_ ["description", "documentation", "homepage", "readme", "readme_file", "license", "license_file", "repository", "links"] $ \key ->
        void (fields .:? key :: Parser (Maybe Text))
    void (fields .:? "badges" :: Parser (Maybe (Map Text (Map Text Text))))
    rustVersion <- fields .:? "rust_version"
    traverse_ rustRelease rustVersion
    pure (name, version)

uploadDependency :: Value -> Parser ()
uploadDependency = withObject "Cargo upload dependency" $ \fields -> do
    fields .: "name" >>= nativeName
    void (fields .: "version_req" :: Parser Text)
    void (fields .: "features" :: Parser [Text])
    for_ ["optional", "default_features"] $ \key ->
        void (fields .: key :: Parser Bool)
    for_ ["target", "registry"] $ \key ->
        void (fields .:? key :: Parser (Maybe Text))
    alias <- fields .:? "explicit_name_in_toml"
    traverse_ nativeName alias
    kind <- fields .:? "kind"
    for_ kind $ \value ->
        unless (value `elem` ["normal", "dev", "build" :: Text]) (fail "Cargo upload dependency kind is invalid")

nativeName :: Text -> Parser ()
nativeName = either (const (fail "Cargo upload name is invalid")) (const (pure ())) . projectName

rustRelease :: Text -> Parser ()
rustRelease raw = do
    let components = T.splitOn "." raw
        complete = T.intercalate "." (components <> replicate (max 0 (3 - length components)) "0")
    unless
        (length components `elem` [1, 2, 3] && not (T.any (`elem` ['-', '+']) raw) && isJust (parseCargo complete))
        (fail "Cargo upload Rust version is invalid")

-- | Encode native uint32 framing without changing either the JSON value or verified archive bytes.
encodeUpload :: Value -> ByteString -> Either ParseError LByteString
encodeUpload metadata archive = do
    let json = encode metadata
    unless (toInteger (LBS.length json) <= toInteger (maxBound :: Word32) && toInteger (BS.length archive) <= toInteger (maxBound :: Word32)) (bad "Cargo upload cannot fit uint32 framing")
    pure . toLazyByteString $ word32LE (fromIntegral (LBS.length json)) <> lazyByteString json <> word32LE (fromIntegral (BS.length archive)) <> byteString archive

bad :: Text -> Either ParseError a
bad = Left . ParseError
