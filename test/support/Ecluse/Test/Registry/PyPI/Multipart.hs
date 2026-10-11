-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native multipart upload fixtures shared by decoder and publish-handler specs.
module Ecluse.Test.Registry.PyPI.Multipart (
    multipartUpload,
    multipartField,
    multipartFile,
    malformedMultipartParts,
    compatibleMultipartBodies,
    malformedMultipartEnvelopes,
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

-- | Legal MIME envelopes containing exactly two name parts, despite name-like text outside their delimiters.
compatibleMultipartBodies :: [PublishBody]
compatibleMultipartBodies =
    [ envelope "upload boundary" "\"upload boundary\"" "" "" ""
    , envelope "upload boundary" "\"upload\\ boundary\"" "" "" ""
    , envelope multipartBoundary multipartBoundary " \t" "" ""
    , envelope multipartBoundary multipartBoundary "" preamble ""
    , envelope multipartBoundary multipartBoundary "" "" epilogue
    , envelope "upload boundary" "\"upload boundary\"" " \t" preamble epilogue
    ]
  where
    preamble = "Content-Disposition: form-data; name=\"name\"\r\n\r\npublic-project\r\n"
    epilogue = multipartField "name" "public-project"

-- | Invalid declared boundaries and delimiter padding, each paired with matching wire bytes.
malformedMultipartEnvelopes :: [PublishBody]
malformedMultipartEnvelopes =
    [ envelope "upload boundary " "\"upload boundary \"" "" "" ""
    , envelope "upload\tboundary" "\"upload\tboundary\"" "" "" ""
    , envelope "upload boundary" "upload boundary" "" "" ""
    , envelope multipartBoundary multipartBoundary "\v" "" ""
    ]

envelope :: ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> PublishBody
envelope boundary parameter padding before after =
    PublishBody
        (Just ("multipart/form-data; boundary=" <> parameter))
        (before <> part <> part <> "--" <> boundary <> "--" <> padding <> "\r\n" <> after)
  where
    part = "--" <> boundary <> padding <> "\r\nContent-Disposition: form-data; name=\"name\"\r\n\r\nrequests\r\n"
