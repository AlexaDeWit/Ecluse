-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Conditional object writes support the existing native worker fixtures.
module Ecluse.BenchLoad.WorkerState (stateAppFor) where

import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Network.HTTP.Types (hContentType, status200, status304, status400, status404, status412)
import Network.Wai (Application, rawPathInfo, requestHeaders, requestMethod, responseLBS, strictRequestBody)

import Ecluse.BenchLoad.Fixture (delayUpstream)
import Ecluse.Test.Package (hexSha256Of)

-- | Opaque revisions and conditional writes retain S3's compare-and-swap behaviour.
stateAppFor :: IORef (Map ByteString (ByteString, ByteString)) -> IORef (Map Text Int) -> Int -> Application
stateAppFor objectsRef countsRef latency request respond = do
    delayUpstream latency
    count "state"
    let key = rawPathInfo request
        headers = requestHeaders request
    objects <- readIORef objectsRef
    case (requestMethod request, Map.lookup key objects) of
        ("GET", Nothing) -> respond (responseLBS status404 [(hContentType, "application/xml")] "<Error><Code>NoSuchKey</Code><Message>missing</Message></Error>")
        ("GET", Just (revision, bytes))
            | lookup "If-None-Match" headers == Just revision -> respond (responseLBS status304 [("ETag", revision)] "")
            | otherwise -> respond (responseLBS status200 [("ETag", revision)] (LBS.fromStrict bytes))
        ("PUT", previous) -> do
            bytes <- LBS.toStrict <$> strictRequestBody request
            let condition = case previous of
                    Nothing -> lookup "If-None-Match" headers == Just "*"
                    Just (storedRevision, _) -> lookup "If-Match" headers == Just storedRevision
                nextRevision = "\"" <> encodeUtf8 (hexSha256Of bytes) <> "\""
            if condition
                then do
                    modifyIORef' objectsRef (Map.insert key (nextRevision, bytes))
                    count "state-write"
                    respond (responseLBS status200 [("ETag", nextRevision)] "")
                else respond (responseLBS status412 [(hContentType, "application/xml")] "<Error><Code>PreconditionFailed</Code></Error>")
        _ -> respond (responseLBS status400 [] "unexpected object request")
  where
    count name = atomicModifyIORef' countsRef (\counts -> (Map.insertWith (+) name 1 counts, ()))
