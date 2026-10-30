-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The compiled advisory database a load proxy syncs, served from a stub S3 bucket.
module Ecluse.BenchLoad.AdvisoryStore (
    advisoryBucket,
    advisoryStoreReply,
) where

import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Network.HTTP.Types (Header, Status, hContentType, status200, status404)
import Network.HTTP.Types.Header (hETag, hLastModified)

import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Osv.Schema (osvDbFileName)
import Ecluse.Test.Package (hexSha1OfLazy)
import Ecluse.Test.Stub (Captured (capPath))

-- | The bucket the proxy syncs its advisory database from.
advisoryBucket :: Text
advisoryBucket = "ecluse-bench-advisories"

-- | Answer the artifact's HEAD or GET with its ETag and publication time, and other paths with 404.
advisoryStoreReply :: Ecosystem -> UTCTime -> LByteString -> Captured -> (Status, [Header], LByteString)
advisoryStoreReply ecosystem publishedAt artifact request
    | capPath request == artifactPath = (status200, headers, artifact)
    | otherwise = (status404, [], "")
  where
    artifactPath = encodeUtf8 ("/" <> advisoryBucket <> "/" <> toText (osvDbFileName (ecosystemName ecosystem)))
    headers =
        [ (hETag, encodeUtf8 ("\"" <> hexSha1OfLazy artifact <> "\""))
        , (hLastModified, encodeUtf8 (formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" publishedAt))
        , (hContentType, "application/octet-stream")
        ]
