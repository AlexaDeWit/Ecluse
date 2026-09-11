-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The front door's cross-cutting middleware pieces and the control-plane health
endpoints. Those are the drain-aware going-away header, the per-request timeout knob,
and the @\/livez@ \/ @\/readyz@ probe application. "Ecluse.Runtime.Server"'s @serverMiddleware@
composes the pieces around the proxy 'Application'. Its dispatch answers the probes
through 'probeApplication'. The request-body cap is not here: it is a route concern,
enforced at the read site by the only body-consuming route (publish).
-}
module Ecluse.Runtime.Server.Middleware (
    -- * Drain-aware going-away header
    goingAwayMiddleware,

    -- * Per-request timeout
    timeoutSeconds,

    -- * Control-plane health probes
    probeApplication,

    -- * Neutral response shapes
    jsonResponse,
) where

import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Map.Strict qualified as Map
import Network.HTTP.Types (Status, hConnection, hContentType, status200, status404, status503)
import Network.Wai (Application, Middleware, Response, mapResponseHeaders, modifyResponse, pathInfo, responseLBS)

import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Server.Readiness (
    MountReadiness (MountAwaitingFirstSync, MountReady),
    Readiness (AwaitingMounts, Latched, Routable),
 )
import Ecluse.Core.Worker (Liveness (liveHealthy, liveLastPoll))
import Ecluse.Runtime.Server.Drain (DrainSignal, isDraining)

{- | While the instance is draining, stamp @Connection: close@ on every response. A keep-alive
client or a mesh connection pool then stops reusing the socket on a closing instance.
-}
goingAwayMiddleware :: DrainSignal -> Middleware
goingAwayMiddleware drain app request respond = do
    draining <- isDraining drain
    if draining
        then modifyResponse closeConnection app request respond
        else app request respond
  where
    -- Add @Connection: close@ to the response's header set. A streaming response keeps
    -- streaming: only its headers are rewritten.
    closeConnection :: Response -> Response
    closeConnection = mapResponseHeaders ((hConnection, "close") :)

{- | The per-request timeout, in seconds. Generous enough for a large packument
fetch, bounded so a stuck upstream cannot pin a handler indefinitely.
-}
timeoutSeconds :: Int
timeoutSeconds = 60

{- | The control-plane health probes, answered above any mount: @\/livez@ from the injected
liveness check, @\/readyz@ from the drain signal and startup gate. Any other path is a @404@.
-}
probeApplication :: DrainSignal -> IO Readiness -> IO Liveness -> Application
probeApplication drain checkReady checkLiveness request respond =
    case pathInfo request of
        ["livez"] -> checkLiveness >>= respond . livenessResponse
        ["readyz"] -> readiness drain checkReady >>= respond
        _ -> respond notFound

{- The @\/livez@ body carries the loop's last recorded progress beside the verdict, so a
dedicated worker fleet's orchestrator can judge staleness rather than only pass or fail. -}
livenessResponse :: Liveness -> Response
livenessResponse liveness
    | liveHealthy liveness = body status200 "live"
    | otherwise = body status503 "liveness check failed"
  where
    body :: Status -> Text -> Response
    body status label =
        jsonResponse status (encode (object ["status" .= label, "lastPoll" .= liveLastPoll liveness]))

{- Readiness stays lenient about public-upstream reachability, because the proxy still serves
private-upstream hits when public is down. A blip must not flap a healthy pod out of rotation.
-}
readiness :: DrainSignal -> IO Readiness -> IO Response
readiness drain checkReady =
    isDraining drain >>= \case
        True -> pure (statusOnly status503 "draining")
        False -> readinessResponse <$> checkReady

{- The body names every configured mount beside the verdict, so an operator sees which ecosystem
awaits its advisory database while the others keep serving. -}
readinessResponse :: Readiness -> Response
readinessResponse = \case
    Routable mounts -> withMounts status200 readyLabel mounts
    AwaitingMounts mounts -> withMounts status503 awaitingLabel mounts
    Latched -> statusOnly status503 "halted"
  where
    withMounts status label mounts =
        jsonResponse status (encode (object ["status" .= label, "mounts" .= mountsOf mounts]))

-- A mount reports the two words the whole verdict reports, under its configured ecosystem key.
mountsOf :: Map.Map Ecosystem MountReadiness -> Value
mountsOf mounts =
    object [Key.fromText (ecosystemName eco) .= mountLabel state | (eco, state) <- Map.toList mounts]
  where
    mountLabel = \case
        MountReady -> readyLabel
        MountAwaitingFirstSync -> awaitingLabel

readyLabel, awaitingLabel :: Text
readyLabel = "ready"
awaitingLabel = "awaiting startup readiness"

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
