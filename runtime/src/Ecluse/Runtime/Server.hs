-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
-- TupleSections: pairing a matched mount with its router's verdict on the remainder
-- in 'matchMount' ((mount,) . router); see STYLE.md §2.
{-# LANGUAGE TupleSections #-}

{- | The HTTP front door: the raw @wai@ 'Application', its dispatch, the
meta-routes, the middleware stack, and 'runServer'.

The proxy is a passthrough over a small, irregular URL surface, so the front
door is a raw 'Application' rather than a web framework -- matching on @pathInfo@
keeps the encoded-slash handling and the streaming control the proxy depends on
(see @docs\/architecture\/web-layer.md@). Routing is two layers:

* __Mount dispatch__: match a request's leading path segments to a configured
  'MountBinding', strip the prefix, and hand the remainder (an ecosystem-native
  path) to that mount's 'Ecluse.Core.Server.Context.MountRouter'. A binding carries a
  mount's __complete__ ecosystem wiring: its router and serve dependencies. The web layer is closed
  over the agnostic 'Ecluse.Core.Server.Context.RouteAction' vocabulary and holds no
  ecosystem's path grammar or body shape of its own. Every registry is
  __path-mounted__ (e.g. @\/npm@); there is no root mount, so adding an ecosystem
  never changes an existing consumer's URLs. A mount prefix is accepted with or
  without a trailing slash (see @docs\/architecture\/web-layer.md@ → "Multi-ecosystem mounts").

Responses split into __two tiers__:

* __Above the mounts, neutral and server-owned.__ The orchestration health probes
  (@\/livez@, @\/readyz@) are answered at the top level, and a path matching __no__
  configured mount is a generic @404 Not Found@ in @text\/plain@: there is no
  ecosystem to shape it.

* __Within a matched mount.__ The mount's router
  ('Ecluse.Core.Server.Context.MountRouter', supplied by its ecosystem adapter) says what
  the request names, as an 'Ecluse.Core.Server.Context.RouteAction': a route-scoped
  response contract existentially paired with either a pure response value or a data-plane
  handler that can produce only that value type.

This module holds __no route knowledge of its own__. It does not name a route, a path
grammar, or a status: it asks the matched mount's router for an action and either
responds with it or runs it under the request perimeter. Adding an ecosystem adds a
router and changes nothing here.

Cross-cutting concerns are applied as middleware composed around the
'Application' (see @docs\/architecture\/web-layer.md@ → "Middleware"): correct
client-IP recovery behind a load balancer, and a request timeout. The
request-body cap is not cross-cutting -- it is a route concern, enforced at the
read site by the only body-consuming route (publish). The middleware pieces and the health probes
live in "Ecluse.Runtime.Server.Middleware", the graceful-shutdown drain
vocabulary in "Ecluse.Runtime.Server.Drain", and the local-dev quit key in
"Ecluse.Runtime.Server.Halt"; this module composes them and re-exports their
surface. Dispatch builds a per-request
'Ecluse.Core.Server.Context.RequestCtx' -- the request runtime ('serveRuntimeOf')
paired with the matched 'MountBinding' -- and the effectful routes run in the
'Ecluse.Core.Server.Context.Handler' reader over it, so a handler reads its mount's
wiring and the request runtime from context rather than as threaded arguments.
-}
module Ecluse.Runtime.Server (
    -- * The WAI application
    ServerConfig (..),
    mkServerConfig,
    defaultPort,
    MountBinding (..),
    application,
    tracedApplication,

    -- * Running the server
    runWarp,
    probeApplication,

    -- * The typed request perimeter
    perimeterGuard,

    -- * Graceful shutdown
    DrainSignal,
    newDrainSignal,
    neverDraining,
    beginDrain,
    isDraining,
    ShutdownDrainTimeout (..),
    defaultShutdownDrainTimeout,

    -- * Local-dev immediate halt
    InteractiveHalt (..),
    defaultInteractiveHalt,
    withInteractiveHalt,

    -- * Middleware
    serverMiddleware,
) where

import Data.List (dropWhileEnd)
import Katip (Severity (ErrorS), katipAddContext, logFM, sl)
import Network.HTTP.Types (Method, status500)
import Network.Wai (Application, Middleware, Request, Response, ResponseReceived, pathInfo, rawPathInfo, requestMethod)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.RealIp (realIp)
import Network.Wai.Middleware.Timeout (timeout)
import System.Posix.Signals (Handler (CatchOnce), installHandler, sigINT, sigTERM)
import UnliftIO.Exception (catchAny, throwIO)

import Ecluse.Core.Server.Context (
    MountBinding (..),
    RequestCtx (RequestCtx),
    ResponseAction (AnswerLocally, RunPipeline),
    RouteAction (RouteAction),
    ServeRuntime (srMetrics),
    runHandler,
 )
import Ecluse.Core.Server.Contract (responseToWai)
import Ecluse.Core.Server.Fault (RequestFault (rqCause, rqDetail), classifyEscape)
import Ecluse.Core.Telemetry.Record (MetricsPort (mpRequestPerimeterFault))
import Ecluse.Runtime.Env (Env, envDdContext, envLogEnv, envTelemetry, serveRuntimeOf)
import Ecluse.Runtime.Server.Drain (
    DrainSignal,
    ShutdownDrainTimeout (..),
    beginDrain,
    defaultShutdownDrainTimeout,
    isDraining,
    neverDraining,
    newDrainSignal,
 )
import Ecluse.Runtime.Server.Halt (
    InteractiveHalt (..),
    defaultInteractiveHalt,
    withInteractiveHalt,
 )
import Ecluse.Runtime.Server.Middleware (
    goingAwayMiddleware,
    jsonResponse,
    probeApplication,
    timeoutSeconds,
 )
import Ecluse.Runtime.Telemetry.Correlation (ddPayloadNow)
import Ecluse.Runtime.Telemetry.Tracing (telemetryWaiMiddleware)

{- | The server's own settings -- the values the 'Application' and 'runServer'
need that the composition-root 'Env' does not carry: the listen port and the served
mount bindings. Backend selection is a composition-root concern; this is the minimal
shape the web layer needs to route. The request-body cap is not here: it is a route
concern, enforced by the only body-consuming route (publish) against its own
'Ecluse.Core.Server.Context.pubMaxRequestBytes'.
-}
data ServerConfig = ServerConfig
    { scPort :: Int
    -- ^ The TCP port @warp@ listens on.
    , scMounts :: [MountBinding]
    {- ^ The mounts served, tried in order; the first whose prefix matches the
    request's leading segments wins. A deployment with no mounts serves nothing
    beyond the health probes -- every other path is the neutral @404@.
    -}
    , scDrain :: DrainSignal
    {- ^ The shared shutdown-drain flag the front door observes: once raised, the
    readiness probe fails and responses carry @Connection: close@
    (the going-away middleware), so a load balancer stops routing new traffic to
    this instance and clients stop reusing keep-alive sockets to it. Defaults to
    'neverDraining'; 'runServer' replaces it with a live signal it flips on a
    shutdown signal.
    -}
    , scDrainTimeout :: ShutdownDrainTimeout
    {- ^ How long the graceful drain waits for in-flight requests and in-progress
    artifact streams to finish before the process exits ('defaultShutdownDrainTimeout').
    -}
    , scCheckReady :: IO Bool
    {- ^ An additional readiness gate the composition root installs, ANDed with
    the drain check by @\/readyz@. Today it is the advisory database's
    first-sync signal: a one-way flip per configured ecosystem, so readiness
    never flaps on it, and @'pure' True@ (the 'mkServerConfig' default) when no
    advisory bucket is configured. The listener serves regardless, since an
    absent advisory database only ever abstains into deny-by-default; this
    gates what a load balancer routes, not whether the process answers.
    -}
    , scCheckLive :: IO Bool
    {- ^ The liveness check @\/livez@ answers from, beyond the listener itself.
    The composition root wires the mirror worker's consume-loop heartbeat here
    exactly when a worker runs (a mirroring deployment); the 'mkServerConfig'
    default is @'pure' True@ (the listener alone), so a serve-only deployment
    can never go unhealthy over a worker it never started.
    -}
    , scOnException :: Maybe Request -> SomeException -> IO ()
    {- ^ @warp@'s exception hook, fired for a fault that escapes to the server
    itself: a post-commit teardown the request perimeter rethrew, or a fault in
    warp's own connection handling. The composition root wires it to the
    process's structured logger (filtered through
    'Warp.defaultShouldDisplayException', so routine client disconnects stay
    quiet); the 'mkServerConfig' default is inert, so a bare config never
    surprises a test with logging.
    -}
    }

{- | Build a 'ServerConfig' over the given mount bindings, taking the default
listen port ('defaultPort').

The composition root supplies the bindings -- each a mount's complete ecosystem
wiring -- and overrides the port by record update where a deployment needs to. There
is no built-in mount: an ecosystem is served only once its binding is passed here, so
the web layer carries no ecosystem of its own.
-}
mkServerConfig :: [MountBinding] -> ServerConfig
mkServerConfig mounts =
    ServerConfig
        { scPort = defaultPort
        , scMounts = mounts
        , scDrain = neverDraining
        , scDrainTimeout = defaultShutdownDrainTimeout
        , scCheckReady = pure True
        , scCheckLive = pure True
        , scOnException = \_ _ -> pass
        }

-- | The conventional npm proxy listen port (4873), the 'mkServerConfig' default.
defaultPort :: Int
defaultPort = 4873

{- | Build the proxy's WAI 'Application' over a 'ServerConfig' and the
composition-root 'Env', with the middleware stack composed around it.

The bare app dispatches a request: a control-plane health probe (@\/livez@ \/
@\/readyz@) is answered at the top level; otherwise the leading path segment is
matched to a mount, the prefix stripped, and the remainder classified and
rendered. The returned 'Application' has the middleware applied (body cap,
client-IP recovery, timeout).
-}
application :: ServerConfig -> Env -> Application
application cfg env = serverMiddleware cfg (dispatch cfg env)

{- | Build the proxy 'Application' with the OpenTelemetry server-span middleware
wrapped __outermost__ around 'application', so one server span covers the whole
request (the other middlewares included). When telemetry is disabled the wrapper is
'id', so this is exactly 'application' -- additive and inert off (see
"Ecluse.Runtime.Telemetry.Tracing"). 'runServer' serves through this; a caller embedding the
proxy that wants the request trace builds its application here rather than through
the bare 'application'.
-}
tracedApplication :: ServerConfig -> Env -> IO Application
tracedApplication cfg env = do
    traceMiddleware <- telemetryWaiMiddleware (envTelemetry env)
    pure (traceMiddleware (application cfg env))

{- Dispatch a request to its handler. Top-level health probes are answered first,
above any mount. Otherwise the leading path segments are matched to a mount: a
match routes the remainder through that mount's binding; no match is the neutral
@404@, since there is no ecosystem to render it.
-}
dispatch :: ServerConfig -> Env -> Application
dispatch cfg env request respond =
    case matchMount (requestMethod request) (scMounts cfg) (pathInfo request) of
        Just (binding, action) -> serve env binding action request respond
        Nothing -> probeApplication (scDrain cfg) (scCheckReady cfg) (scCheckLive cfg) request respond

{- Carry out the action the matched mount's router named.

The route itself was decided by that router ('bindingRouter', which the mount's
ecosystem adapter supplies); this function knows only the two kinds of action it can
return. An 'AnswerLocally' action is a pure value interpreted through the route contract. A
'RunPipeline' action is discharged to 'IO' under the typed request perimeter, over the
per-request 'RequestCtx' built once here (the request runtime 'serveRuntimeOf' paired
with the matched 'MountBinding'), so the handler reads its mount's serve dependencies
from context rather than as threaded arguments. The deps-or-stub decision
is the handler's: a mount with no packument dependencies answers the
recognised-but-unwired @501@, and one with no publication target answers a publish with
@405@.
-}
serve :: Env -> MountBinding -> RouteAction -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
serve env binding (RouteAction contract action) request respond =
    case action of
        AnswerLocally answer -> send answer
        -- The data-plane handler under the typed request perimeter: it is discharged
        -- through 'run', and the perimeter's observation channel is the bounded
        -- @ecluse.serve.perimeter.faults@ metric plus the audit log line.
        RunPipeline fallback handler -> perimeterGuard observeFault send fallback (run . handler request)
  where
    send value = respond (responseToWai contract value)

    observeFault fault = do
        mpRequestPerimeterFault (srMetrics runtime) (rqCause fault)
        run . katipAddContext (perimeterPayload fault) $
            logFM ErrorS "the request perimeter answered an escaped pre-commit fault with the neutral 500"

    -- The perimeter audit payload: the request path, the bounded classified
    -- cause, and the rendered detail -- mirror fields of the denial audit line,
    -- so an operator triages both surfaces with one vocabulary.
    perimeterPayload fault =
        sl "module" ("Ecluse.Runtime.Server" :: Text)
            <> sl "path" (decodeUtf8 (rawPathInfo request) :: Text)
            <> sl "perimeterCause" (show (rqCause fault) :: Text)
            <> sl "perimeterDetail" (rqDetail fault)

    -- Discharge a 'Handler' to 'IO' over the per-request context, establishing the
    -- @katip@ logging context the application owns: the composition root's 'LogEnv'
    -- (scribes) and the resolved trace-correlation @dd@ object as the initial context,
    -- so every serve-path line carries @dd@. The request runtime the handler reads is
    -- projected from 'Env' ('serveRuntimeOf').
    run handlerAction = do
        dd <- ddPayloadNow (envDdContext env)
        runHandler (envLogEnv env) dd ctx handlerAction

    runtime :: ServeRuntime
    runtime = serveRuntimeOf env

    ctx :: RequestCtx
    ctx = RequestCtx runtime binding

{- | The typed request perimeter over one effectful route: run the handler with a
commit-tracking respond, catching only __synchronous__ escapes (asynchronous
cancellation is not caught and tears the request down like any thread). The
handlers report every routine failure as a value, so what arrives here is an
escape from some dependency's typed contract.

Pre-commit, the escape is classified ('Ecluse.Core.Server.Fault.classifyEscape'),
handed to the injected observation channel (the composition wires the bounded
@ecluse.serve.perimeter.faults@ metric and the audit log line), and answered with
the route's declared neutral 500 -- no fault detail ever reaches the client.
Post-commit -- the wrapped respond has already begun the response -- there is no
second response to give: the escape rethrows, warp tears the connection down, and
the 'scOnException' hook logs it. Exported for its spec; 'serve' wires it per
request.
-}
perimeterGuard ::
    -- | Observe a classified pre-commit fault (the metric and the audit line).
    (RequestFault -> IO ()) ->
    -- | The route-scoped response continuation.
    (response -> IO ResponseReceived) ->
    -- | The route's declared neutral pre-commit fallback.
    response ->
    -- | The route's handler, discharged to 'IO', awaiting the tracked respond.
    ((response -> IO ResponseReceived) -> IO ResponseReceived) ->
    IO ResponseReceived
perimeterGuard observeFault respond fallback handlerOn = do
    committed <- newIORef False
    let respondCommitted response = do
            atomicWriteIORef committed True
            respond response
    handlerOn respondCommitted `catchAny` \escape -> do
        wasCommitted <- readIORef committed
        if wasCommitted
            then throwIO escape
            else do
                observeFault (classifyEscape escape)
                respond fallback

{- Match a request path to a mount: the first binding whose prefix the path begins
with, paired with the action its ecosystem's router names for the remainder.
'Nothing' when no mount's prefix matches, and the caller then answers the neutral
@404@ (a path under no mount has no ecosystem to render it). A mount prefix is
accepted with or without a trailing slash, so @\/npm\/pkg@ and a bare @\/npm@ both
match the @\/npm@ mount.
-}
matchMount :: Method -> [MountBinding] -> [Text] -> Maybe (MountBinding, RouteAction)
matchMount method mounts segments = asum (map match mounts)
  where
    -- The binding whose prefix the path begins with, paired with its router's verdict
    -- on the remainder. The method is threaded through because it is part of the
    -- mapping (npm tells a @PUT@ publish from a @GET@ read over the same path, and a
    -- @HEAD@ from the @GET@ it varies). 'Nothing' for a non-matching prefix.
    match :: MountBinding -> Maybe (MountBinding, RouteAction)
    match binding =
        (binding,) . bindingRouter binding method
            <$> stripPrefixSegments (toList (bindingPrefix binding)) segments

{- Strip a mount's prefix segments off the front of a request path. The root
mount (an empty prefix) consumes nothing and always matches. Trailing empty
segments left after the prefix -- the trailing slash(es) of a bare @\/npm\/@ or
@\/npm\/\/@ -- are dropped so they are not mistaken for empty ecosystem path
components.
-}
stripPrefixSegments :: [Text] -> [Text] -> Maybe [Text]
stripPrefixSegments [] segs = Just (dropTrailingSlashes segs)
stripPrefixSegments (p : ps) (s : ss)
    | p == s = stripPrefixSegments ps ss
stripPrefixSegments _ _ = Nothing

-- Drop every trailing empty segment (the trailing slash(es) of a bare-mount path,
-- e.g. @\/npm\/@ arriving as @["npm",""]@ or @\/npm\/\/@ as @["npm","",""]@), so a
-- run of them normalises to the empty path rather than a spurious empty component.
-- An /internal/ empty segment is left untouched for the router to reject.
dropTrailingSlashes :: [Text] -> [Text]
dropTrailingSlashes = dropWhileEnd (== "")

-- An in-mount error response: the status, with the body shaped by the mount's
-- renderer. The perimeter's neutral @500@ is the one such response the web layer still
-- owns; every route's own body is shaped by its ecosystem's router.

{- | The cross-cutting middleware stack composed around the proxy 'Application':
correct client-IP recovery behind a load balancer (@X-Forwarded-For@ \/ @X-Real-IP@),
and a per-request timeout. The pieces live in "Ecluse.Runtime.Server.Middleware"; this
composes them over the 'ServerConfig'.

The request-body cap is __not__ a middleware. Only one route (publish) consumes a
request body, and it bounds it at the source as a value: a declared Content-Length over
the cap fails closed before a byte is read, and a chunked body is bounded by a counted
read ('Ecluse.Core.Security.boundedRead'), each answered as the route's own @413@. A
body-cap middleware would instead have to wrap the reader and __throw__ across the
request perimeter (untracked control flow), so the bound lives at the read site
('Ecluse.Core.Server.Pipeline.Publish') rather than here.

A third middleware, the __going-away__ header, is active only during a graceful
drain: while the 'ServerConfig''s 'DrainSignal' is raised it stamps @Connection:
close@ on every response so an HTTP\/1.1 keep-alive pool (a client's, or a service
mesh's connection pool) does not reuse a socket on an instance that is shutting down
-- the cause of the 503-on-rollover this guards against (see
@docs\/architecture\/web-layer.md@ → "Graceful shutdown").

Two @wai-extra@ middlewares are deliberately __not__ used. @Autohead@ answers a
HEAD by running the GET handler and discarding the body, which on a tarball route
would open the upstream and stream a whole artifact to nowhere; instead a HEAD on the
tarball or packument route is handled explicitly (in 'serve'), gating exactly as the
GET path does but suppressing the body -- the tarball probing the upstream as a HEAD so
a bodiless HEAD can never trigger a full-artifact upstream fetch, the packument
emitting the same status and headers as the GET with the locally-built body withheld.
@Gzip@
would re-compress already compressed artifacts and fight the streaming backpressure
the serve path relies on.
-}
serverMiddleware :: ServerConfig -> Middleware
serverMiddleware cfg =
    realIp
        . timeout timeoutSeconds
        . goingAwayMiddleware (scDrain cfg)

{- | Serve the proxy's HTTP front door: start @warp@ on the 'ServerConfig''s port
with the 'application' built over it and the composition-root 'Env'. The
'ServerConfig' -- in particular its mount bindings ('scMounts'), each a mount's
complete ecosystem wiring -- is supplied by the composition root, which is where the
served ecosystems are mounted (see @Ecluse@).

__Graceful shutdown.__ A fresh live 'DrainSignal' is allocated per launch and wired
into both the request path (the @application@ reads it through 'scDrain') and the
@warp@ shutdown handler. On @SIGTERM@ or @SIGINT@ the handler raises the drain -- so
the readiness probe begins failing and responses gain @Connection: close@ -- then
closes the listen socket, which puts @warp@ into graceful-shutdown mode: it stops
accepting new connections and waits for in-flight requests __and in-progress
artifact streams__ to finish before the process exits, bounded by 'scDrainTimeout'.
The handler is a 'CatchOnce', so a second signal during the drain hard-stops the
server rather than being swallowed.

__Local-dev quit key.__ The whole run is wrapped in 'withInteractiveHalt', which --
__only when attached to an interactive terminal__ -- arms a watcher that forces an
immediate halt on Ctrl-D (end of standard input), bypassing the drain like a second
Ctrl-C. Outside a TTY (production) no watcher is installed and this changes nothing.
-}
runWarp :: ServerConfig -> IO Application -> IO ()
runWarp cfg0 getApp = do
    drain <- newDrainSignal
    let cfg = cfg0{scDrain = drain}
        ShutdownDrainTimeout timeoutSecs = scDrainTimeout cfg
        settings =
            Warp.setPort (scPort cfg)
                . Warp.setInstallShutdownHandler (installShutdownHandler drain)
                . Warp.setGracefulShutdownTimeout (Just timeoutSecs)
                -- The composition root's exception hook: post-commit teardowns the
                -- request perimeter rethrew, and warp's own connection faults, reach
                -- the structured logger rather than warp's stderr default.
                . Warp.setOnException (scOnException cfg)
                -- Defence-in-depth for a fault with no mount context (middleware,
                -- warp itself): a neutral JSON 500 (no exception detail) rather than
                -- warp's default body. A pre-commit handler escape never reaches
                -- this -- the typed request perimeter ('serve') answers it first,
                -- route-shaped.
                . Warp.setOnExceptionResponse (const onExceptionResponse)
                $ Warp.defaultSettings
    app <- getApp
    withInteractiveHalt defaultInteractiveHalt (Warp.runSettings settings app)

-- The neutral response for a fault that escapes to warp's own handler (see 'runWarp'):
-- a deny-shaped 500 carrying no exception detail.
onExceptionResponse :: Response
onExceptionResponse = jsonResponse status500 "{\"error\":\"internal server error\"}"

{- Install the OS shutdown handler @warp@ asks for: on @SIGTERM@\/@SIGINT@, raise the
drain (flip readiness to @503@ and start stamping @Connection: close@) and then run
@warp@'s @closeSocket@, which begins the graceful drain of in-flight work. Each
signal is caught __once__ ('CatchOnce') so a second signal falls through to the
runtime's default and hard-stops a drain that is taking too long.
-}
installShutdownHandler :: DrainSignal -> IO () -> IO ()
installShutdownHandler drain closeSocket =
    traverse_ install [sigTERM, sigINT]
  where
    install sig = installHandler sig (CatchOnce (beginDrain drain >> closeSocket)) Nothing
