-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Extract every upload identity from bounded multipart bytes without copying file contents.
module Ecluse.Core.Registry.PyPI.Multipart (declaredNames) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Char (toLower)
import Data.Text.Encoding qualified as Text
import Network.Wai.Parse qualified as Multipart

import Ecluse.Core.Registry.Adapter.Capability (PublishBody (..))

-- | Read every name using the declared boundary. Incomplete or ambiguous parts yield no names.
declaredNames :: PublishBody -> IO [Text]
declaredNames body = pure . fromMaybe [] $ do
    boundary <- publishContentType body >>= declaredBoundary
    firstPart <- BS.stripPrefix ("--" <> boundary <> "\r\n") (publishBytes body)
    readParts ("\r\n--" <> boundary) [] firstPart

declaredBoundary :: ByteString -> Maybe ByteString
declaredBoundary header = do
    (mediaType, attributes) <- parameters header
    guard (mediaType == "multipart/form-data")
    boundary <- uniqueValue "boundary" attributes
    guard (not (BS.null boundary) && BS.length boundary <= maxBoundaryBytes)
    guard (BS.all boundaryCharacter boundary)
    pure boundary

maxBoundaryBytes :: Int
maxBoundaryBytes = 70

maxPartHeaders :: Int
maxPartHeaders = 32

maxHeaderParameters :: Int
maxHeaderParameters = 32

boundaryCharacter :: Word8 -> Bool
boundaryCharacter c = alphaNumeric c || c `BS.elem` "'()+_,-./:=?"

-- wai-extra can stop at blank headers and treats inline markers as delimiters. Traverse complete parts here.
readParts :: ByteString -> [Text] -> ByteString -> Maybe [Text]
readParts delimiter names remaining = do
    (part, next) <- splitPart delimiter remaining
    claim <- partName part
    let names' = maybe names (: names) claim
    maybe (pure (reverse names')) (readParts delimiter names') next

splitPart :: ByteString -> ByteString -> Maybe (ByteString, Maybe ByteString)
splitPart delimiter bytes = scan 0 bytes
  where
    scan offset remaining = do
        let (before, marker) = BS.breakSubstring delimiter remaining
            at = offset + BS.length before
        after <- BS.stripPrefix delimiter marker
        case after of
            "--" -> pure (BS.take at bytes, Nothing)
            "--\r\n" -> pure (BS.take at bytes, Nothing)
            _ | Just next <- BS.stripPrefix "\r\n" after -> pure (BS.take at bytes, Just next)
            _ | "--\r\n" `BS.isPrefixOf` after -> Nothing
            _ -> scan (at + BS.length delimiter) after

partName :: ByteString -> Maybe (Maybe Text)
partName part = do
    let (rawHeaders, separator) = BS.breakSubstring "\r\n\r\n" part
    content <- BS.stripPrefix "\r\n\r\n" separator
    headers <- partHeaders rawHeaders
    guard (all ((/= "content-transfer-encoding") . fst) headers)
    disposition <- uniqueValue "content-disposition" headers
    (kind, attributes) <- parameters disposition
    guard (kind == "form-data" && all ((`elem` ["name", "filename"]) . fst) attributes)
    field <- uniqueValue "name" attributes
    guard (not (BS.null field))
    if field == "name"
        then do
            guard (all ((/= "filename") . fst) attributes)
            Just <$> rightToMaybe (Text.decodeUtf8' content)
        else pure Nothing

partHeaders :: ByteString -> Maybe [(ByteString, ByteString)]
partHeaders bytes = do
    guard (not (BS.null bytes))
    lines' <- traverse (BS.stripSuffix "\r") (BSC.split '\n' (bytes <> "\r"))
    guard (length lines' <= maxPartHeaders)
    headers <- traverse headerPair lines'
    guard (distinctKeys headers)
    pure headers
  where
    headerPair line = do
        let (key, rest) = BSC.break (== ':') line
        value <- BS.stripPrefix ":" rest
        guard (not (BS.null key) && BS.all tokenCharacter key)
        guard (BS.all headerCharacter value)
        pure (lower key, trim value)

-- wai-extra supplies the media token. Its parameter decoder does not handle quoted pairs or semicolons.
parameters :: ByteString -> Maybe (ByteString, [(ByteString, ByteString)])
parameters header = do
    guard (BS.all headerCharacter header)
    let (mediaType, _) = Multipart.parseContentType header
    guard (not (BS.null (trim mediaType)))
    attributes <- readParameters 0 [] (BS.drop (BS.length mediaType) header)
    guard (distinctKeys attributes)
    pure (lower (trim mediaType), attributes)

readParameters :: Int -> [(ByteString, ByteString)] -> ByteString -> Maybe [(ByteString, ByteString)]
readParameters count attributes remaining
    | BS.null (trim remaining) = Just (reverse attributes)
    | count >= maxHeaderParameters = Nothing
    | otherwise = do
        next <- BS.stripPrefix ";" (trim remaining)
        let (key, afterKey) = BS.span tokenCharacter (trim next)
        guard (not (BS.null key))
        afterEquals <- BS.stripPrefix "=" (trim afterKey)
        (value, rest) <- parameterValue (trim afterEquals)
        readParameters (count + 1) ((lower key, value) : attributes) rest

parameterValue :: ByteString -> Maybe (ByteString, ByteString)
parameterValue bytes = case BS.stripPrefix "\"" bytes of
    Just quoted -> quotedValue [] quoted
    Nothing -> do
        let (value, rest) = BS.span tokenCharacter bytes
        guard (not (BS.null value))
        pure (value, rest)

quotedValue :: [ByteString] -> ByteString -> Maybe (ByteString, ByteString)
quotedValue pieces bytes = do
    let (piece, special) = BS.break (`BS.elem` "\"\\") bytes
    (marker, rest) <- BS.uncons special
    case marker of
        34 -> pure (BS.concat (reverse (piece : pieces)), rest)
        _ -> do
            (escaped, next) <- BS.uncons rest
            guard (headerCharacter escaped)
            quotedValue (BS.singleton escaped : piece : pieces) next

distinctKeys :: [(ByteString, a)] -> Bool
distinctKeys pairs = length pairs == length (ordNub (map fst pairs))

uniqueValue :: ByteString -> [(ByteString, a)] -> Maybe a
uniqueValue key pairs = case [value | (candidate, value) <- pairs, candidate == key] of
    [value] -> Just value
    _ -> Nothing

trim :: ByteString -> ByteString
trim = BSC.dropWhile horizontalSpace . BSC.dropWhileEnd horizontalSpace
  where
    horizontalSpace c = c == ' ' || c == '\t'

lower :: ByteString -> ByteString
lower = BSC.map toLower

headerCharacter :: Word8 -> Bool
headerCharacter c = c == 9 || (c >= 32 && c /= 127)

tokenCharacter :: Word8 -> Bool
tokenCharacter c = alphaNumeric c || c `BS.elem` "!#$%&'*+-.^_`|~"

alphaNumeric :: Word8 -> Bool
alphaNumeric c = (c >= 48 && c <= 57) || (c >= 65 && c <= 90) || (c >= 97 && c <= 122)
