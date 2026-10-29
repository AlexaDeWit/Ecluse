-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Target copies, upload gating and conditional object writes shared by the native worker fixtures.
module Ecluse.BenchLoad.WorkerState (
    WorkerTarget (..),
    newWorkerTarget,
    resetWorkerTarget,
    countRequest,
    acceptUpload,
    targetStateApp,
    stateAppFor,
) where

import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Network.HTTP.Types (hContentType, status200, status304, status400, status404, status412, status503)
import Network.Wai (Application, Response, rawPathInfo, requestHeaders, requestMethod, responseLBS, strictRequestBody)

import Ecluse.BenchLoad.Fixture (delayUpstream)
import Ecluse.Test.Package (hexSha256Of)

-- | One run's target copy, durable objects, request counts and injected upload rejection.
data WorkerTarget = WorkerTarget
    { wtFiles :: IORef (Map Text ByteString)
    , wtObjects :: IORef (Map ByteString (ByteString, ByteString))
    , wtCounts :: IORef (Map Text Int)
    , wtReject :: IORef Bool
    }

-- | An empty target that accepts uploads.
newWorkerTarget :: IO WorkerTarget
newWorkerTarget = WorkerTarget <$> newIORef mempty <*> newIORef mempty <*> newIORef mempty <*> newIORef False

-- | Clear the copy, durable objects and counts together. The rejection switch keeps its setting.
resetWorkerTarget :: WorkerTarget -> IO ()
resetWorkerTarget target = do
    writeIORef (wtFiles target) mempty
    writeIORef (wtObjects target) mempty
    writeIORef (wtCounts target) mempty

-- | Record one request under its label.
countRequest :: WorkerTarget -> Text -> IO ()
countRequest target label = atomicModifyIORef' (wtCounts target) (\counts -> (Map.insertWith (+) label 1 counts, ()))

-- | Count the attempt, honour an injected rejection, and store the bytes only once durable inventory exists.
acceptUpload :: WorkerTarget -> Text -> ByteString -> Response -> IO Response
acceptUpload target filename bytes accepted = do
    countRequest target "upload-attempt"
    rejected <- readIORef (wtReject target)
    inventory <- readIORef (wtObjects target)
    if rejected
        then pure (responseLBS status503 [] "injected upload rejection")
        else
            if Map.null inventory
                then pure (responseLBS status400 [] "upload preceded durable inventory")
                else do
                    modifyIORef' (wtFiles target) (Map.insert filename bytes)
                    countRequest target "upload"
                    pure accepted

-- | Serve the target's durable objects under the shared conditional object semantics.
targetStateApp :: Int -> WorkerTarget -> Application
targetStateApp latency target = stateAppFor (wtObjects target) (wtCounts target) latency

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
