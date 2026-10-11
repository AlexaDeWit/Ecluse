-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native multipart upload fixtures shared by decoder and publish-handler specs.
module Ecluse.Test.Registry.PyPI.Multipart (
    multipartUpload,
    multipartField,
    multipartFile,
    malformedMultipartParts,
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

-- | Parts that must not hide a later identity claim from the guard.
malformedMultipartParts :: [ByteString]
malformedMultipartParts =
    [ "--" <> multipartBoundary <> "\r\n\r\nignored\r\n"
    , part "Content-Disposition: form-data; name=\"ignored\"\r\nContent-Disposition: form-data; name=\"name\""
    , part "Content-Disposition: form-data; name=\"ignored\"; name=\"name\""
    , part "Content-Disposition: form-data; name=\"name\"; Name=\"ignored\""
    , part "Content-Disposition: form-data; name=\"ignored\"; name*=utf-8''name"
    , part "Content-Disposition: form-data; name=\"\""
    , part "Content-Disposition: form-data; name=\"name\\\""
    , part "Content-Type: text/plain"
    , part "Content-Disposition: form-data; name=\"ignored\"\r\nX-Broken-Header"
    , part "Content-Disposition: form-data; name=\"ignored\"\r\nContent-Transfer-Encoding: base64"
    ]
  where
    part headers = "--" <> multipartBoundary <> "\r\n" <> headers <> "\r\n\r\npublic-project\r\n"
