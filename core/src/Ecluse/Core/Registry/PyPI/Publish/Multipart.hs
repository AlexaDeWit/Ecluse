-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}

-- | Bounded declared-name decoding for the unchanged PyPI publication relay body.
module Ecluse.Core.Registry.PyPI.Publish.Multipart (declaredNames) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Char (toLower)
import Data.Text.Encoding qualified as TE
import Network.Wai.Parse qualified as Wai
import UnliftIO.Exception (tryAny)

import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.PyPI.Project (canonicalName, projectName)
import Ecluse.Core.Security.Limits (Limits (..))

-- | Read every claim using the declared Content-Type boundary. Conflicting or missing claims refuse the relay.
declaredNames :: Limits -> Maybe ByteString -> ByteString -> IO (Either ParseError [Text])
declaredNames limits contentType body = case checkedClaims limits contentType body >>= traverse decodeName of
    Left fault -> pure (Left fault)
    Right claims -> do
        let values = map (TE.encodeUtf8 . canonicalName) claims
            projectedSize = sum (map (\value -> toInteger (BS.length value) + 100) values) + 100
        if projectedSize > toInteger (maxPublishRequestBytes limits)
            then pure (bad "multipart name projection exceeds its byte bound")
            else do
                chunks <- newIORef (nameProjection values)
                let readChunk = atomicModifyIORef' chunks (\remaining -> let (chunk, rest) = BS.splitAt 32768 remaining in (rest, chunk))
                outcome <- tryAny (Wai.sinkRequestBodyEx options discardFile (Wai.Multipart projectionBoundary) readChunk)
                pure $ case outcome of
                    Left _ -> bad "malformed or oversized multipart upload"
                    Right (params, files) -> do
                        unless (null files && params == map ("name",) values) (bad "multipart name parsers disagree")
                        case claims of
                            [] -> bad "multipart upload has no name"
                            name : remaining
                                | all (== name) remaining -> Right [canonicalName name]
                                | otherwise -> bad "multipart upload declares conflicting names"
  where
    options = Wai.setMaxRequestParmsSize (maxMetadataBytes limits) Wai.defaultParseRequestBodyOptions
    decodeName bytes = first (const (ParseError "multipart name is not UTF-8")) (TE.decodeUtf8' bytes) >>= projectName

-- A checked package name cannot contain a colon, so this delimiter cannot occur in projected values.
projectionBoundary :: ByteString
projectionBoundary = "ecluse:name:projection"

nameProjection :: [ByteString] -> ByteString
nameProjection values = BS.concat (map part values) <> "--" <> projectionBoundary <> "--\r\n"
  where
    part value = "--" <> projectionBoundary <> "\r\nContent-Disposition: form-data; name=\"name\"\r\n\r\n" <> value <> "\r\n"

discardFile :: Wai.BackEnd ()
discardFile _ _ readChunk = drain
  where
    drain = readChunk >>= \chunk -> unless (BS.null chunk) drain

checkedClaims :: Limits -> Maybe ByteString -> ByteString -> Either ParseError [ByteString]
checkedClaims limits contentType body = do
    when (BS.length body > maxPublishRequestBytes limits) (bad "multipart upload exceeds its byte bound")
    header <- maybe (bad "missing multipart Content-Type") Right contentType
    when (BS.length header > 8192) (bad "multipart Content-Type exceeds its bound")
    let (mediaType, attributes) = Wai.parseContentType header
    unless (mediaType == "multipart/form-data") (bad "upload is not multipart/form-data")
    boundary <- case [value | ("boundary", value) <- attributes] of
        [value] | not (BS.null value), BS.length value <= 70, BS.all boundaryByte value -> Right value
        _ -> bad "missing, malformed, or ambiguous multipart boundary"
    unless (BS.isPrefixOf ("--" <> boundary <> "\r\n") body) (bad "multipart opening boundary is missing")
    checkParts limits boundary 0 (BS.drop (BS.length boundary + 4) body)

boundaryByte :: Word8 -> Bool
boundaryByte byte = byte >= 33 && byte <= 126 && byte /= 34 && byte /= 92

-- Wai ignores malformed part headers, so reject ambiguity before it selects a disposition claim.
checkParts :: Limits -> ByteString -> Int -> ByteString -> Either ParseError [ByteString]
checkParts limits boundary count bytes = do
    when (count >= maxArtifactCount limits) (bad "multipart part count exceeds its bound")
    let (headers, headerSuffix) = BS.breakSubstring "\r\n\r\n" bytes
    when (BS.null headerSuffix || BS.length headers > 16384) (bad "malformed multipart headers")
    named <- checkHeaders headers
    (value, suffix) <- splitPart boundary (BS.drop 4 headerSuffix)
    later <- case suffix of
        "--\r\n" -> Right []
        "--" -> Right []
        remaining | Just following <- BS.stripPrefix "\r\n" remaining -> checkParts limits boundary (count + 1) following
        _ -> bad "malformed multipart boundary delimiter"
    pure ([value | named] <> later)

splitPart :: ByteString -> ByteString -> Either ParseError (ByteString, ByteString)
splitPart boundary bytes = go 0 bytes
  where
    marker = "\r\n--" <> boundary
    go consumed remaining = case BS.breakSubstring marker remaining of
        (_, suffix) | BS.null suffix -> bad "multipart closing boundary is missing"
        (prefix, suffix) ->
            let after = BS.drop (BS.length marker) suffix
                offset = consumed + BS.length prefix
             in if BS.isPrefixOf "\r\n" after || BS.isPrefixOf "--" after
                    then Right (BS.take offset bytes, after)
                    else go (offset + BS.length marker) after

checkHeaders :: ByteString -> Either ParseError Bool
checkHeaders bytes = do
    headers <- traverse parseHeader (BSC.lines bytes)
    disposition <- case [value | ("content-disposition", value) <- headers] of
        [value] -> Right value
        _ -> bad "missing or ambiguous multipart disposition"
    when (BS.elem 92 disposition) (bad "escaped multipart disposition attributes are unsupported")
    let (kind, attributes) = Wai.parseContentType disposition
        names = [value | ("name", value) <- attributes]
    when (any (BS.elem 34 . snd) attributes) (bad "malformed quoted multipart disposition attribute")
    unless (kind == "form-data" && length names == 1 && not (any BS.null names)) (bad "malformed multipart disposition name")
    unless (length [() | ("filename", _) <- attributes] <= 1) (bad "ambiguous multipart filename")
    when (any (\(key, _) -> key `notElem` ["name", "filename"]) attributes) (bad "unsupported multipart disposition attribute")
    let named = names == ["name"]
    when (named && any ((== "filename") . fst) attributes) (bad "multipart name is a file part")
    pure named
  where
    parseHeader line = do
        let clean = fromMaybe line (BS.stripSuffix "\r" line)
            (key, rest) = BS.break (== 58) clean
        when (BS.null key || BS.null rest || BS.any (\c -> c <= 32 || c >= 127) key) (bad "malformed multipart header")
        pure (BSC.map toLower key, BSC.dropWhile (== ' ') (BS.drop 1 rest))

bad :: Text -> Either ParseError a
bad = Left . ParseError
