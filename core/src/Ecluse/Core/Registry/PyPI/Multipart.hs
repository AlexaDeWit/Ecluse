-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Extract upload identities from already bounded multipart bytes without retaining file contents.
module Ecluse.Core.Registry.PyPI.Multipart (declaredNames) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Char (toLower)
import Data.Text.Encoding qualified as Text
import Network.Wai.Parse qualified as Multipart
import UnliftIO.Exception (catch)

import Ecluse.Core.Registry.Adapter.Capability (PublishBody (..))

-- | Read every name field using the declared boundary. Invalid framing or encoding yields no names.
declaredNames :: PublishBody -> IO [Text]
declaredNames body = case publishContentType body >>= declaredBoundary of
    Just boundary
        | validFraming boundary (publishBytes body) ->
            extractNames boundary body `catch` (\(_ :: Multipart.RequestParseException) -> pure [])
    _ -> pure []

declaredBoundary :: ByteString -> Maybe ByteString
declaredBoundary header = do
    let (mediaType, attributes) = Multipart.parseContentType header
    guard (BSC.map toLower mediaType == "multipart/form-data")
    guard (all (\(key, value) -> not (BS.null key || BS.null value)) attributes)
    boundary <- case [value | (key, value) <- attributes, BSC.map toLower key == "boundary"] of
        [value] -> Just value
        _ -> Nothing
    guard (not (BS.null boundary) && BS.length boundary <= maxBoundaryBytes)
    guard (BS.all boundaryCharacter boundary)
    pure boundary

maxBoundaryBytes :: Int
maxBoundaryBytes = 70

boundaryCharacter :: Word8 -> Bool
boundaryCharacter c =
    (c >= 48 && c <= 57) || (c >= 65 && c <= 90) || (c >= 97 && c <= 122) || c `BS.elem` "'()+_,-./:=?"

-- wai-extra accepts unterminated parts and ignores opening delimiters. Refuse ambiguous framing before parsing.
validFraming :: ByteString -> ByteString -> Bool
validFraming boundary bytes =
    maybe False delimiters (BS.stripPrefix (marker <> "\r\n") bytes)
  where
    marker = "--" <> boundary
    delimiters remaining =
        let (before, suffix) = BS.breakSubstring marker remaining
         in "\r\n" `BS.isSuffixOf` before
                && case BS.stripPrefix marker suffix of
                    Just "--\r\n" -> True
                    Just "--" -> True
                    Just next | Just rest <- BS.stripPrefix "\r\n" next -> delimiters rest
                    _ -> False

extractNames :: ByteString -> PublishBody -> IO [Text]
extractNames boundary body = do
    remaining <- newIORef bytes
    (fields, files) <- Multipart.sinkRequestBodyEx options discardFile (Multipart.Multipart boundary) (atomicModifyIORef' remaining (BS.empty,))
    pure $ if any ((== "name") . fst) files then [] else fromMaybe [] (traverse decodeName [value | (key, value) <- fields, key == "name"])
  where
    bytes = publishBytes body
    byteCount = BS.length bytes
    -- The caller's byte cap bounds headers and fields too. No file payload is retained by this decoder.
    options =
        Multipart.clearMaxHeaderLineLength
            . Multipart.setMaxRequestParmsSize byteCount
            . Multipart.setMaxRequestKeyLength byteCount
            . Multipart.setMaxRequestNumFiles byteCount
            $ Multipart.defaultParseRequestBodyOptions
    decodeName = rightToMaybe . Text.decodeUtf8'

discardFile :: Multipart.BackEnd ()
discardFile _ _ next = drain
  where
    drain = do
        chunk <- next
        unless (BS.null chunk) drain
