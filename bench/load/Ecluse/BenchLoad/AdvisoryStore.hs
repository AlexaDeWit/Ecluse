-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Advisory objects share a fixture endpoint with independently prepared native mirror state.
module Ecluse.BenchLoad.AdvisoryStore (
    AdvisoryObject (..),
    advisoryBucket,
    advisoryStoreReply,
    advisoryStoreApplication,
) where

import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Network.HTTP.Types (Header, Status, hContentType, status200, status404)
import Network.HTTP.Types.Header (hETag, hLastModified)
import Network.Wai (Application, pathInfo)

import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Osv.Schema (osvDbFileName)
import Ecluse.Test.Package (hexSha1OfLazy)
import Ecluse.Test.Stub (Captured (capPath), routedApplication)

-- | A compiled native artifact retains its original publication time and ecosystem.
data AdvisoryObject = AdvisoryObject Ecosystem UTCTime LByteString
    deriving stock (Eq, Show)

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

-- | Keep advisory bucket replies separate from native state replies on one owned endpoint.
advisoryStoreApplication :: AdvisoryObject -> Application -> Application
advisoryStoreApplication (AdvisoryObject ecosystem publishedAt artifact) native = \request respond ->
    case pathInfo request of
        bucket : _ | bucket == advisoryBucket -> advisory request respond
        _ -> native request respond
  where
    advisory = routedApplication (pure . advisoryStoreReply ecosystem publishedAt artifact)
