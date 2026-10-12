-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

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
declaredNames limits contentType body = case checkedBoundary limits contentType body of
    Left fault -> pure (Left fault)
    Right boundary -> do
        chunks <- newIORef body
        let readChunk = atomicModifyIORef' chunks (\remaining -> let (chunk, rest) = BS.splitAt 32768 remaining in (rest, chunk))
        outcome <- tryAny (Wai.sinkRequestBodyEx options discardFile (Wai.Multipart boundary) readChunk)
        pure $ case outcome of
            Left _ -> bad "malformed or oversized multipart upload"
            Right (params, files) -> do
                when (any ((== "name") . fst) files) (bad "multipart name is a file part")
                claims <- traverse decodeName [value | ("name", value) <- params]
                case claims of
                    [] -> bad "multipart upload has no name"
                    name : remaining
                        | all (== name) remaining -> Right [canonicalName name]
                        | otherwise -> bad "multipart upload declares conflicting names"
  where
    options =
        Wai.setMaxRequestKeyLength 4096
            . Wai.setMaxRequestNumFiles (maxArtifactCount limits)
            . Wai.setMaxRequestFileSize (fromIntegral (maxPublishRequestBytes limits))
            . Wai.setMaxRequestFilesSize (fromIntegral (maxPublishRequestBytes limits))
            . Wai.setMaxRequestParmsSize (maxMetadataBytes limits)
            . Wai.setMaxHeaderLines 16
            . Wai.setMaxHeaderLineLength 8192
            $ Wai.defaultParseRequestBodyOptions
    decodeName bytes = first (const (ParseError "multipart name is not UTF-8")) (TE.decodeUtf8' bytes) >>= projectName

discardFile :: Wai.BackEnd ()
discardFile _ _ readChunk = drain
  where
    drain = readChunk >>= \chunk -> unless (BS.null chunk) drain

checkedBoundary :: Limits -> Maybe ByteString -> ByteString -> Either ParseError ByteString
checkedBoundary limits contentType body = do
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
    pure boundary

boundaryByte :: Word8 -> Bool
boundaryByte byte = byte >= 33 && byte <= 126 && byte /= 34 && byte /= 92

-- Wai ignores malformed part headers, so reject ambiguity before it selects a disposition claim.
checkParts :: Limits -> ByteString -> Int -> ByteString -> Either ParseError ()
checkParts limits boundary count bytes = do
    when (count >= maxArtifactCount limits) (bad "multipart part count exceeds its bound")
    let (headers, headerSuffix) = BS.breakSubstring "\r\n\r\n" bytes
    when (BS.null headerSuffix || BS.length headers > 16384) (bad "malformed multipart headers")
    checkHeaders headers
    let (_, suffix) = BS.breakSubstring ("\r\n--" <> boundary) (BS.drop 4 headerSuffix)
    when (BS.null suffix) (bad "multipart closing boundary is missing")
    case BS.drop (BS.length boundary + 4) suffix of
        "--\r\n" -> Right ()
        "--" -> Right ()
        remaining | Just following <- BS.stripPrefix "\r\n" remaining -> checkParts limits boundary (count + 1) following
        _ -> bad "malformed multipart boundary delimiter"

checkHeaders :: ByteString -> Either ParseError ()
checkHeaders bytes = do
    headers <- traverse parseHeader (BSC.lines bytes)
    disposition <- case [value | ("content-disposition", value) <- headers] of
        [value] -> Right value
        _ -> bad "missing or ambiguous multipart disposition"
    let (kind, attributes) = Wai.parseContentType disposition
        names = [value | ("name", value) <- attributes]
    unless (kind == "form-data" && length names == 1 && not (any BS.null names)) (bad "malformed multipart disposition name")
    unless (length [() | ("filename", _) <- attributes] <= 1) (bad "ambiguous multipart filename")
    when (any (\(key, _) -> key `notElem` ["name", "filename"]) attributes) (bad "unsupported multipart disposition attribute")
  where
    parseHeader line = do
        let clean = fromMaybe line (BS.stripSuffix "\r" line)
            (key, rest) = BS.break (== 58) clean
        when (BS.null key || BS.null rest || BS.any (\c -> c <= 32 || c >= 127) key) (bad "malformed multipart header")
        pure (BSC.map toLower key, BSC.dropWhile (== ' ') (BS.drop 1 rest))

bad :: Text -> Either ParseError a
bad = Left . ParseError
