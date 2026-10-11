-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native multipart upload fixtures shared by decoder and publish-handler specs.
module Ecluse.Test.Registry.PyPI.Multipart (
    multipartUpload,
    multipartField,
    multipartFile,
    multipartContentType,
    multipartBoundary,
) where

import Data.ByteString qualified as BS

import Ecluse.Core.Registry.Adapter.Capability (PublishBody (..))

-- | The boundary declared by the fixture's Content-Type.
multipartBoundary :: ByteString
multipartBoundary = "upload-boundary"

-- | The unquoted Content-Type paired with 'multipartUpload'.
multipartContentType :: ByteString
multipartContentType = "multipart/form-data; boundary=" <> multipartBoundary

-- | Add the closing delimiter to a list of complete parts.
multipartUpload :: [ByteString] -> PublishBody
multipartUpload parts = PublishBody (Just multipartContentType) (BS.concat parts <> "--" <> multipartBoundary <> "--\r\n")

-- | A text part, including its opening delimiter and trailing CRLF.
multipartField :: ByteString -> ByteString -> ByteString
multipartField name value = "--" <> multipartBoundary <> "\r\nContent-Disposition: form-data; name=\"" <> name <> "\"\r\n\r\n" <> value <> "\r\n"

-- | A file part whose bytes remain opaque to identity extraction.
multipartFile :: ByteString -> ByteString -> ByteString
multipartFile name bytes = "--" <> multipartBoundary <> "\r\nContent-Disposition: form-data; name=\"" <> name <> "\"; filename=\"upload.whl\"\r\nContent-Type: application/octet-stream\r\n\r\n" <> bytes <> "\r\n"
