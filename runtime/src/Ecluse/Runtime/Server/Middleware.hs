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

import Network.HTTP.Types (Status, hConnection, hContentType, status200, status404, status503)
import Network.Wai (Application, Middleware, Response, mapResponseHeaders, modifyResponse, pathInfo, responseLBS)

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
probeApplication :: DrainSignal -> IO Bool -> IO Bool -> Application
probeApplication drain checkReady checkLiveness request respond =
    case pathInfo request of
        ["livez"] -> do
            alive <- checkLiveness
            if alive
                then respond (jsonResponse status200 "{\"status\":\"live\"}")
                else respond (jsonResponse status503 "{\"status\":\"liveness check failed\"}")
        ["readyz"] -> readiness drain checkReady >>= respond
        _ -> respond notFound

{- Readiness stays lenient about public-upstream reachability, because the proxy still serves
private-upstream hits when public is down. A blip must not flap a healthy pod out of rotation.
-}
readiness :: DrainSignal -> IO Bool -> IO Response
readiness drain checkReady =
    isDraining drain >>= \case
        True -> pure (jsonResponse status503 "{\"status\":\"draining\"}")
        False ->
            checkReady <&> \case
                False -> jsonResponse status503 "{\"status\":\"awaiting startup readiness\"}"
                True -> jsonResponse status200 "{\"status\":\"ready\"}"

{- This tier sits above the mounts, so no ecosystem shapes the body of an unmounted path.
-}
notFound :: Response
notFound =
    responseLBS status404 [(hContentType, "text/plain; charset=utf-8")] "Not Found\n"

-- | A JSON response with the given status and body, tagged @application\/json@.
jsonResponse :: Status -> LByteString -> Response
jsonResponse status =
    responseLBS status [(hContentType, "application/json")]
