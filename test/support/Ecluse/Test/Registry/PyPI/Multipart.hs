-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Independent native multipart observations for PyPI protocol fixtures.
module Ecluse.Test.Registry.PyPI.Multipart (decodeUpload) where

import Data.ByteString qualified as BS
import Data.List (lookup)
import Network.Wai.Parse (FileInfo, RequestBodyType (Multipart), lbsBackEnd, parseContentType, sinkRequestBody)

-- | Callers bound the body before decoding exactly one native content part.
decodeUpload :: Maybe ByteString -> ByteString -> IO (Either Text ([(ByteString, ByteString)], FileInfo LByteString))
decodeUpload contentType body =
    case parseContentType <$> contentType of
        Just ("multipart/form-data", attributes)
            | Just boundary <- lookup "boundary" attributes
            , not (BS.null boundary)
            , BS.length boundary <= 256 -> do
                chunks <- newIORef body
                (fields, files) <- sinkRequestBody lbsBackEnd (Multipart boundary) (atomicModifyIORef' chunks (BS.empty,))
                pure $ case files of
                    [("content", file)] -> Right (fields, file)
                    _ -> Left "expected one content part"
        _ -> pure (Left "expected bounded multipart boundary")
