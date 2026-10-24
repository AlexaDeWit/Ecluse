-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Request deadlines, connection closure, and control-plane health responses.
module Ecluse.Runtime.Server.Middleware (
    -- * The request timeout
    requestTimeout,

    -- * HTTP/1 connection closure
    closeResponseConnections,
    ignoreResponseClose,

    -- * Drain-aware going-away header
    goingAwayMiddleware,

    -- * Control-plane health probes
    probeApplication,

    -- * Neutral response shapes
    jsonResponse,
) where

import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Map.Strict qualified as Map
import Network.HTTP.Types (Status, hConnection, hContentType, http20, status200, status404, status503)
import Network.Wai (Application, Middleware, Request, Response, httpVersion, mapResponseHeaders, modifyResponse, pathInfo, responseHeaders, responseLBS)
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Deadline (Deadlines, timeout)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Server.FieldValue (fieldValues, listMembers, lowerAscii)
import Ecluse.Core.Server.Readiness (
    MountReadiness (MountAwaitingFirstSync, MountReady),
    Readiness (AwaitingMounts, Latched, Routable),
    routable,
 )
import Ecluse.Core.Worker (Liveness (liveHealthy, liveLastPoll))
import Ecluse.Runtime.Server.Drain (DrainSignal, isDraining)

-- | Answer an empty @503@ about one sweep after the deadline, even if a response started.
requestTimeout :: Deadlines -> Int -> Middleware
requestTimeout deadlines seconds app request respond =
    timeout deadlines (seconds * 1_000_000) (app request respond) >>= maybe (respond (responseLBS status503 [] "")) pure

-- | Honour response close after the inner application returns. Install outside request observation.
closeResponseConnections :: Middleware
closeResponseConnections app request respond
    | httpVersion request >= http20 = app request respond
    | otherwise = do
        closing <- newIORef False
        received <- app request $ \response -> do
            sent <- respond response
            when (responseClosesConnection response) (writeIORef closing True)
            pure sent
        -- Warp 3.4.9 closes a completed response through its exception path.
        whenM (readIORef closing) (throwIO ResponseClose)
        pure received

-- | Keep the transport close signal out of Warp's application-failure observer.
ignoreResponseClose :: (Maybe Request -> SomeException -> IO ()) -> Maybe Request -> SomeException -> IO ()
ignoreResponseClose observe request exception =
    case fromException exception of
        Just ResponseClose -> pass
        Nothing -> observe request exception

data ResponseClose = ResponseClose
    deriving stock (Show)

instance Exception ResponseClose

responseClosesConnection :: Response -> Bool
responseClosesConnection =
    any (any ((== "close") . lowerAscii) . listMembers) . fieldValues hConnection . responseHeaders

-- | Mark every response for connection closure while the instance drains.
goingAwayMiddleware :: DrainSignal -> Middleware
goingAwayMiddleware drain app request respond = do
    draining <- isDraining drain
    if draining
        then modifyResponse closeConnection app request respond
        else app request respond

closeConnection :: Response -> Response
closeConnection = mapResponseHeaders ((hConnection, "close") :)

-- | Answer liveness and readiness above every mount. Other paths receive @404@.
probeApplication :: DrainSignal -> IO Readiness -> IO Liveness -> Application
probeApplication drain checkReady checkLiveness request respond =
    case pathInfo request of
        ["livez"] -> checkLiveness >>= respond . livenessResponse
        ["readyz"] -> readiness drain checkReady >>= respond
        _ -> respond notFound

-- The last progress time lets worker orchestrators detect stalled polling.
livenessResponse :: Liveness -> Response
livenessResponse liveness
    | liveHealthy liveness = body status200 "live"
    | otherwise = body status503 "liveness check failed"
  where
    body :: Status -> Text -> Response
    body status label =
        jsonResponse status (encode (object ["status" .= label, "lastPoll" .= liveLastPoll liveness]))

-- Public outages do not remove a pod that can still serve private registries.
readiness :: DrainSignal -> IO Readiness -> IO Response
readiness drain checkReady =
    isDraining drain >>= \case
        True -> pure (statusOnly status503 "draining")
        False -> readinessResponse <$> checkReady

-- Name each mount so operators can identify the advisory database still loading.
readinessResponse :: Readiness -> Response
readinessResponse verdict = case verdict of
    Routable mounts -> withMounts readyLabel mounts
    AwaitingMounts mounts -> withMounts awaitingLabel mounts
    Latched -> statusOnly status "halted"
  where
    -- 'routable' alone decides the status, and the match decides only what the body reports.
    status = bool status503 status200 (routable verdict)
    withMounts label mounts =
        jsonResponse status (encode (object ["status" .= label, "mounts" .= mountsOf mounts]))

-- A mount reports the two words the whole verdict reports, under its configured ecosystem key.
mountsOf :: Map.Map Ecosystem MountReadiness -> Value
mountsOf mounts =
    object [Key.fromText (ecosystemName eco) .= mountLabel mount | (eco, mount) <- Map.toList mounts]

mountLabel :: MountReadiness -> Text
mountLabel = \case
    MountReady -> readyLabel
    MountAwaitingFirstSync -> awaitingLabel

readyLabel, awaitingLabel :: Text
readyLabel = "ready"
-- Only a mount that denies on the advisory database waits, so the label names what is missing and
-- which role publishes it rather than reporting a generic startup delay.
awaitingLabel = "awaiting the advisory database that ecluse pilot publishes"

-- A probe body carrying the verdict alone, for a state no mount detail explains.
statusOnly :: Status -> Text -> Response
statusOnly status label = jsonResponse status (encode (object ["status" .= label]))

-- This tier sits above the mounts, so no ecosystem shapes the body of an unmounted path.
notFound :: Response
notFound =
    responseLBS status404 [(hContentType, "text/plain; charset=utf-8")] "Not Found\n"

-- | A JSON response with the given status and body, tagged @application\/json@.
jsonResponse :: Status -> LByteString -> Response
jsonResponse status =
    responseLBS status [(hContentType, "application/json")]
