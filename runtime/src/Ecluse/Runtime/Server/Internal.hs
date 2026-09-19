-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The dispatch, the request perimeter and the listener behind "Ecluse.Runtime.Server", which
documents the front door and re-exports the curated surface. Importing this module opts out of
that stability promise, the convention @text@ and @bytestring@ use, so production code imports
the public one.
-}
module Ecluse.Runtime.Server.Internal (
    -- * The WAI application
    ServerConfig (..),
    mkServerConfig,
    defaultPort,
    MountBinding (..),
    application,
    tracedApplication,

    -- * Running the server
    runWarp,
    raceServerAgainstLoop,
    probeApplication,
    probeOnlyApplication,

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
import Katip (Severity (ErrorS), SimpleLogPayload, katipAddContext, logFM, sl)
import Network.HTTP.Types (Method, status500)
import Network.HTTP.Types.Header (RequestHeaders)
import Network.Wai (Application, Middleware, Request, Response, ResponseReceived, pathInfo, rawPathInfo, requestHeaders, requestMethod)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.RealIp (realIp)
import Network.Wai.Middleware.Timeout (timeout)
import System.Posix.Signals qualified as Posix
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (race_)
import UnliftIO.Exception (catchAny, throwIO)

import Ecluse.Core.Server.Context (
    Handler,
    MountBinding (..),
    RequestCtx (RequestCtx, ctxRuntime),
    ResponseAction (AnswerLocally, AnswerRefusal, RunPipeline),
    RouteAction (RouteAction),
    ServeRuntime (srMetrics),
    pdHelp,
    runHandler,
 )
import Ecluse.Core.Server.Contract (responseToWai)
import Ecluse.Core.Server.Fault (RequestFault (rqCause, rqDetail), classifyEscape)
import Ecluse.Core.Server.Readiness (Readiness, alwaysReady)
import Ecluse.Core.Telemetry.Record (MetricsPort (mpRequestPerimeterFault))
import Ecluse.Core.Worker (Liveness, alwaysLive)
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

{- | The settings the web layer needs to serve that the composition-root 'Env' does not carry.
Not the request-body cap: the publish route bounds its own body as a value.
-}
data ServerConfig = ServerConfig
    { scPort :: Int
    -- ^ The TCP port @warp@ listens on.
    , scMounts :: [MountBinding]
    {- ^ The mounts served. The first whose prefix matches the request's leading segments
    wins, and a path under no mount is the neutral @404@.
    -}
    , scDrain :: DrainSignal
    {- ^ The shutdown-drain flag the front door observes. Once raised, the readiness probe
    fails and every response carries @Connection: close@. Defaults to 'neverDraining'.
    -}
    , scDrainTimeout :: ShutdownDrainTimeout
    {- ^ How long the graceful drain waits for in-flight requests and in-progress
    artifact streams to finish before the process exits ('defaultShutdownDrainTimeout').
    -}
    , scCheckReady :: IO Readiness
    {- ^ The readiness verdict the composition root installs, which @\/readyz@ renders and the
    drain check overrides. Each mount's flip is one way, so readiness never flaps a pod out of rotation.
    -}
    , scCheckLive :: IO Liveness
    {- ^ The liveness check @\/livez@ answers from, beyond the listener itself. A worker
    heartbeat is wired here only when a worker runs, so a serve-only deployment stays live.
    -}
    , scOnException :: Maybe Request -> SomeException -> IO ()
    {- ^ @warp@'s exception hook, for a post-commit escape the request perimeter rethrew or a
    fault in warp's own connection handling. The 'mkServerConfig' default is inert.
    -}
    }

{- | Build a 'ServerConfig' over the mount bindings on 'defaultPort'. There is no built-in
mount: the web layer serves only the ecosystems the composition root binds here.
-}
mkServerConfig :: [MountBinding] -> ServerConfig
mkServerConfig mounts =
    ServerConfig
        { scPort = defaultPort
        , scMounts = mounts
        , scDrain = neverDraining
        , scDrainTimeout = defaultShutdownDrainTimeout
        , scCheckReady = pure alwaysReady
        , scCheckLive = pure alwaysLive
        , scOnException = \_ _ -> pass
        }

-- | The conventional npm proxy listen port (4873), the 'mkServerConfig' default.
defaultPort :: Int
defaultPort = 4873

{- | The proxy's WAI 'Application': the request dispatch under the cross-cutting
middleware stack ('serverMiddleware').
-}
application :: ServerConfig -> Env -> Application
application cfg env = serverMiddleware cfg (dispatch cfg env)

{- | The WAI 'Application' of a role that serves the health probes and nothing else. Every
path outside @\/livez@ and @\/readyz@ is the neutral @404@.
-}
probeOnlyApplication :: ServerConfig -> IO Application
probeOnlyApplication cfg = pure (serverMiddleware cfg (probesOf cfg))

-- The health probes over one config's drain signal and injected checks.
probesOf :: ServerConfig -> Application
probesOf cfg = probeApplication (scDrain cfg) (scCheckReady cfg) (scCheckLive cfg)

{- | 'application' with the OpenTelemetry server-span middleware wrapped __outermost__, so
one server span covers the whole request. The wrapper is 'id' when telemetry is off.
-}
tracedApplication :: ServerConfig -> Env -> IO Application
tracedApplication cfg env = do
    traceMiddleware <- telemetryWaiMiddleware (envTelemetry env)
    pure (traceMiddleware (application cfg env))

{- Route a request: the first matching mount wins, and every other path falls to the
health probes, which answer @\/livez@ and @\/readyz@ and give the rest the neutral @404@.
-}
dispatch :: ServerConfig -> Env -> Application
dispatch cfg env request respond =
    case matchMount (requestMethod request) (requestHeaders request) (scMounts cfg) (pathInfo request) of
        Just (binding, action) -> serve env binding action request respond
        Nothing -> probesOf cfg request respond

{- Carry out the action the matched mount's router named. A 'RunPipeline' action runs
under the typed request perimeter, over the 'RequestCtx' this function builds once.
-}
serve :: Env -> MountBinding -> RouteAction -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
serve env binding (RouteAction contract action) request respond =
    case action of
        AnswerLocally answer -> send answer
        -- This is where a route's own refusal meets the mount's help message: the table decides
        -- the refusal, and the binding beside it renders the body.
        AnswerRefusal render -> send (render (pdHelp (bindingPackumentDeps binding)))
        RunPipeline fallback handler ->
            perimeterGuard
                (observePerimeterFault env ctx request)
                send
                fallback
                (runInRequest env ctx . handler request)
  where
    send value = respond (responseToWai contract value)

    ctx :: RequestCtx
    ctx = RequestCtx (serveRuntimeOf env) binding

-- Discharge a 'Handler' to 'IO' over the per-request context. Resolving @dd@ here is what makes
-- every serve-path log line carry its trace correlation.
runInRequest :: Env -> RequestCtx -> Handler a -> IO a
runInRequest env ctx action = do
    dd <- ddPayloadNow (envDdContext env)
    runHandler (envLogEnv env) dd ctx action

-- Record an escaped pre-commit fault on the metric and the audit line, both before the
-- perimeter answers its neutral fallback.
observePerimeterFault :: Env -> RequestCtx -> Request -> RequestFault -> IO ()
observePerimeterFault env ctx request fault = do
    mpRequestPerimeterFault (srMetrics (ctxRuntime ctx)) (rqCause fault)
    runInRequest env ctx . katipAddContext (perimeterPayload request fault) $
        logFM ErrorS "the request perimeter answered an escaped pre-commit fault with the neutral 500"

-- The fields mirror the denial audit line, so an operator triages both surfaces with one
-- vocabulary. The @module@ key names the public module, because operators filter on it.
perimeterPayload :: Request -> RequestFault -> SimpleLogPayload
perimeterPayload request fault =
    sl "module" ("Ecluse.Runtime.Server" :: Text)
        <> sl "path" (decodeUtf8 (rawPathInfo request) :: Text)
        <> sl "perimeterCause" (show (rqCause fault) :: Text)
        <> sl "perimeterDetail" (rqDetail fault)

{- | Run one route's handler behind a commit-tracking respond, catching __synchronous__ escapes
only. Pre-commit one answers the neutral fallback with no detail, post-commit it rethrows.
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

{- Match a request path to a mount: the first binding whose prefix the path begins with, paired
with the action its router names. A prefix matches with or without a trailing slash.
-}
matchMount :: Method -> RequestHeaders -> [MountBinding] -> [Text] -> Maybe (MountBinding, RouteAction)
matchMount method headers mounts segments = asum (map match mounts)
  where
    {- The method and the headers are part of the mapping: the npm router tells a @PUT@ publish
    from a @GET@ over one path, and a media-typed route refuses a client that admits none. -}
    match :: MountBinding -> Maybe (MountBinding, RouteAction)
    match binding =
        (binding,) . bindingRouter binding method headers
            <$> stripPrefixSegments (toList (bindingPrefix binding)) segments

-- Strip a mount's prefix segments off the front of a request path. The first equation is the
-- base case a fully-consumed prefix reaches, which is where the trailing slash is dropped.
stripPrefixSegments :: [Text] -> [Text] -> Maybe [Text]
stripPrefixSegments [] segs = Just (dropTrailingSlashes segs)
stripPrefixSegments (p : ps) (s : ss)
    | p == s = stripPrefixSegments ps ss
stripPrefixSegments _ _ = Nothing

-- A trailing slash arrives as an empty final segment (@\/npm\/@ as @["npm",""]@). An
-- internal empty segment is left untouched for the router to reject.
dropTrailingSlashes :: [Text] -> [Text]
dropTrailingSlashes = dropWhileEnd (== "")

{- | The cross-cutting stack around the proxy 'Application'. The body cap is not a middleware:
it would throw across the request perimeter, and @Autohead@ and @Gzip@ fight streaming.
-}
serverMiddleware :: ServerConfig -> Middleware
serverMiddleware cfg =
    realIp
        . timeout timeoutSeconds
        . goingAwayMiddleware (scDrain cfg)

{- | Serve the front door over one live 'DrainSignal', which the probe, the going-away header,
and the shutdown handler share. @warp@ drains under 'scDrainTimeout', and a TTY adds Ctrl-D.
-}
runWarp :: ServerConfig -> (ServerConfig -> IO Application) -> IO ()
runWarp cfg0 getApp = do
    drain <- newDrainSignal
    let cfg = cfg0{scDrain = drain}
        ShutdownDrainTimeout timeoutSecs = scDrainTimeout cfg
        settings =
            Warp.setPort (scPort cfg)
                . Warp.setInstallShutdownHandler (installShutdownHandler drain)
                . Warp.setGracefulShutdownTimeout (Just timeoutSecs)
                . Warp.setOnException (scOnException cfg)
                -- Defence in depth for a fault with no mount context, from a middleware or
                -- warp itself: a neutral 500 with no detail. A handler escape never gets here.
                . Warp.setOnExceptionResponse (const onExceptionResponse)
                $ Warp.defaultSettings
    app <- getApp cfg
    withInteractiveHalt defaultInteractiveHalt (Warp.runSettings settings app)

-- The neutral response for a fault that escapes to warp's own handler (see 'runWarp'):
-- a deny-shaped 500 carrying no exception detail.
onExceptionResponse :: Response
onExceptionResponse = jsonResponse status500 "{\"error\":\"internal server error\"}"

{- On @SIGTERM@ or @SIGINT@, raise the drain before closing the socket, so readiness fails and
responses carry @Connection: close@ first. 'CatchOnce' leaves the second to the runtime.
-}
installShutdownHandler :: DrainSignal -> IO () -> IO ()
installShutdownHandler drain closeSocket =
    traverse_ install [Posix.sigTERM, Posix.sigINT]
  where
    install sig = Posix.installHandler sig (Posix.CatchOnce (beginDrain drain >> closeSocket)) Nothing

{- | Race a server arm against a never-returning background loop, the single-process shutdown
shape. 'race_' is the invariant: 'concurrently_' would wait forever, brackets un-unwound.
-}
raceServerAgainstLoop :: (MonadUnliftIO m) => m () -> m () -> m ()
raceServerAgainstLoop = race_
