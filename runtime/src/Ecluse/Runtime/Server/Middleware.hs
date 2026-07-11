{- | The front door's cross-cutting middleware pieces and the control-plane health
endpoints: the defensive request-body cap, the drain-aware going-away header, the
per-request timeout knob, and the @\/livez@ \/ @\/readyz@ probe application.
"Ecluse.Runtime.Server"'s @serverMiddleware@ composes the pieces around the
proxy 'Application'; its dispatch answers the probes through 'probeApplication'.
-}
module Ecluse.Runtime.Server.Middleware (
    -- * Request-body cap
    RequestSizeLimit (..),
    defaultRequestSizeLimit,
    sizeLimitMiddleware,

    -- * Drain-aware going-away header
    goingAwayMiddleware,

    -- * Per-request timeout
    timeoutSeconds,

    -- * Control-plane health probes
    probeApplication,

    -- * Neutral response shapes
    pong,
    jsonResponse,
) where

import Network.HTTP.Types (Status, hConnection, hContentType, status200, status404, status503)
import Network.Wai (Application, Middleware, Response, mapResponseHeaders, modifyResponse, pathInfo, responseLBS)
import Network.Wai.Middleware.RequestSizeLimit (defaultRequestSizeLimitSettings, requestSizeLimitMiddleware, setMaxLengthForRequest)

import Ecluse.Runtime.Server.Drain (DrainSignal, isDraining)

{- | The maximum request-body size accepted, in bytes -- a defensive cap so a
hostile or runaway client cannot force the proxy to buffer an unbounded body. A
'newtype' so a raw byte count is not mistaken for some other 'Word64'.
-}
newtype RequestSizeLimit = RequestSizeLimit Word64
    deriving stock (Eq, Show)

{- | The default request-body cap: 25 MiB. Generous for the metadata and small
control-plane bodies the proxy accepts (artifact __downloads__ stream the other
way and are never buffered), while still bounding a hostile upload.
-}
defaultRequestSizeLimit :: RequestSizeLimit
defaultRequestSizeLimit = RequestSizeLimit (25 * 1024 * 1024)

{- | Cap the request body at the configured limit, rejecting an over-cap body
before it is buffered.
-}
sizeLimitMiddleware :: RequestSizeLimit -> Middleware
sizeLimitMiddleware (RequestSizeLimit maxBytes) =
    requestSizeLimitMiddleware
        (setMaxLengthForRequest (\_req -> pure (Just maxBytes)) defaultRequestSizeLimitSettings)

{- | While the instance is draining, stamp @Connection: close@ on every response so a
keep-alive client (or a mesh connection pool) does not reuse the socket on a closing
instance; while serving, pass responses through untouched. The flag is read
per-response -- the same one-way 'DrainSignal' the readiness probe observes -- so the
header appears the moment the drain begins and on every response thereafter.
-}
goingAwayMiddleware :: DrainSignal -> Middleware
goingAwayMiddleware drain app request respond = do
    draining <- isDraining drain
    if draining
        then modifyResponse closeConnection app request respond
        else app request respond
  where
    -- Add @Connection: close@ to the response's header set. A streaming response
    -- keeps streaming -- only its headers are rewritten.
    closeConnection :: Response -> Response
    closeConnection = mapResponseHeaders ((hConnection, "close") :)

{- | The per-request timeout, in seconds. Generous enough for a large packument
fetch, bounded so a stuck upstream cannot pin a handler indefinitely.
-}
timeoutSeconds :: Int
timeoutSeconds = 60

{- | The control-plane health probes, answered above any mount: @\/livez@ from the
injected liveness check (the worker-heartbeat arm folded in by the caller),
@\/readyz@ from the drain signal ANDed with the composition root's startup gate,
and any other unmounted path as the neutral @404@.
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

{- Readiness (@\/readyz@): @200@ when config is loaded and the listener is serving,
@503@ once the instance is __draining__. It is deliberately __lenient about
public-upstream reachability__ -- the proxy still serves private-upstream hits when
public is down -- so readiness must not flap on an upstream blip and pull a healthy
pod from rotation.

The drain flip is the load-balancer signal of a graceful rollover: while the
'DrainSignal' is raised, readiness fails so an upstream LB or service mesh stops
routing __new__ traffic here, while in-flight requests finish (see
@docs\/architecture\/hosting.md@ → "Graceful rollover").

The additional check is the composition root's startup gate (@scCheckReady@):
a one-way flip (today, the advisory database's first sync), so it cannot flap
a pod out of rotation once ready.
-}
readiness :: DrainSignal -> IO Bool -> IO Response
readiness drain checkReady =
    isDraining drain >>= \case
        True -> pure (jsonResponse status503 "{\"status\":\"draining\"}")
        False ->
            checkReady <&> \case
                False -> jsonResponse status503 "{\"status\":\"awaiting startup readiness\"}"
                True -> jsonResponse status200 "{\"status\":\"ready\"}"

{- | @\/-\/ping@: answered locally with @200 {}@, since the client is only
checking that the proxy endpoint it talks to is up. No upstream round-trip.
-}
pong :: Response
pong = jsonResponse status200 "{}"

{- A path matching no configured mount: a generic @404 Not Found@ in @text\/plain@.
This tier sits above the mounts, so there is no ecosystem to shape it -- the body is
kept as readable as possible to whatever client reached an unmounted path.
-}
notFound :: Response
notFound =
    responseLBS status404 [(hContentType, "text/plain; charset=utf-8")] "Not Found\n"

-- | A JSON response with the given status and body, tagged @application\/json@.
jsonResponse :: Status -> LByteString -> Response
jsonResponse status =
    responseLBS status [(hContentType, "application/json")]
