-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | S3-compatible conditional object reads for the worker fixtures' durable mirror state.
module Ecluse.BenchLoad.ObjectStore (objectRevision, conditionalObjectRead) where

import Data.List (lookup)
import Network.HTTP.Types (hContentType, status200, status304, status404)
import Network.HTTP.Types.Header (hETag, hIfNoneMatch)
import Network.Wai (Request, Response, requestHeaders, responseLBS)

import Ecluse.Test.Package (hexSha256Of)

-- | A quoted content digest, used as the object's opaque entity tag.
objectRevision :: ByteString -> ByteString
objectRevision bytes = "\"" <> encodeUtf8 (hexSha256Of bytes) <> "\""

-- | Answer S3's NoSuchKey, an empty 304 for a matching revision, or the stored bytes under their revision.
conditionalObjectRead :: Request -> Maybe (ByteString, ByteString) -> Response
conditionalObjectRead request = \case
    Nothing -> responseLBS status404 [(hContentType, "application/xml")] "<Error><Code>NoSuchKey</Code></Error>"
    Just (revision, bytes)
        | lookup hIfNoneMatch (requestHeaders request) == Just revision -> responseLBS status304 [(hETag, revision)] ""
        | otherwise -> responseLBS status200 [(hETag, revision), (hContentType, "application/json")] (fromStrict bytes)
