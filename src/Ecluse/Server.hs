-- TupleSections: pairing a matched mount with its classified remainder in
-- 'dispatchMount' ((mount,) . classifier); see STYLE.md §2.
{-# LANGUAGE TupleSections #-}

{- | The HTTP front door: the raw @wai@ 'Application', its dispatch, the
meta-routes, the middleware stack, and 'runServer'.

The proxy is a passthrough over a small, irregular URL surface, so the front
door is a raw 'Application' rather than a web framework — matching on @pathInfo@
keeps the encoded-slash handling and the streaming control the proxy depends on
(see @docs\/architecture\/web-layer.md@). Routing is two layers:

* __Mount dispatch__ — match a request's leading path segments to a configured
  'MountBinding', strip the prefix, and hand the remainder (an ecosystem-native
  path) to that mount's 'Ecluse.Server.Route.Classifier'. A binding carries a
  mount's __complete__ ecosystem wiring — its classifier, its packument-serve
  dependencies, and its error 'Ecluse.Server.Response.MountRenderer' — so the web
  layer is closed over the shared 'Route' set ("Ecluse.Server.Route") and holds no
  ecosystem's path grammar or body shape of its own. Every registry is
  __path-mounted__ (e.g. @\/npm@); there is no root mount, so adding an ecosystem
  never changes an existing consumer's URLs. A mount prefix is accepted with or
  without a trailing slash (see @docs\/architecture\/hosting.md@ → "Dispatch").

Responses split into __two tiers__:

* __Above the mounts — neutral, server-owned.__ The orchestration health probes
  (@\/livez@, @\/readyz@) are answered at the top level, and a path matching __no__
  configured mount is a generic @404 Not Found@ in @text\/plain@ — there is no
  ecosystem to shape it.

* __Within a matched mount — the mount's renderer.__ The classified 'Route'
  renders through that mount's 'Ecluse.Server.Response.MountRenderer', in the
  ecosystem's own error surface: @\/-\/ping@ is answered locally with @200 {}@,
  @\/-\/v1\/search@ is @501@ (search is not an install path), an unrecognised
  in-mount path is @404@ (deny by default), and the package\/artifact routes
  ('Packument', 'Tarball') are recognised but, without serve dependencies wired,
  return an explicit @501 Not Implemented@ rather than a fabricated success — their
  fetch → rules → serve pipeline lives outside this module.

Cross-cutting concerns are applied as middleware composed around the
'Application' (see @docs\/architecture\/web-layer.md@ → "Middleware"): a
defensive request-body size cap, correct client-IP recovery behind a load
balancer, and a request timeout. Dispatch builds a per-request
'Ecluse.Server.Context.RequestCtx' — the composition-root 'Env' paired with the
matched 'MountBinding' — and the effectful routes run in the
'Ecluse.Server.Context.Handler' reader over it, so a handler reads its mount's
wiring and the composition root from context rather than as threaded arguments.
-}
module Ecluse.Server (
    -- * The WAI application
    ServerConfig (..),
    mkServerConfig,
    defaultPort,
    MountBinding (..),
    application,

    -- * Running the server
    runServer,

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

    -- * Request-body cap
    RequestSizeLimit (..),
    defaultRequestSizeLimit,
) where

import Network.HTTP.Types (Status, hConnection, hContentType, status200, status404, status501, status503)
import Network.Wai (Application, Middleware, Request, Response, ResponseReceived, mapResponseHeaders, modifyResponse, pathInfo, responseLBS)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.RealIp (realIp)
import Network.Wai.Middleware.RequestSizeLimit (defaultRequestSizeLimitSettings, requestSizeLimitMiddleware, setMaxLengthForRequest)
import Network.Wai.Middleware.Timeout (timeout)
import System.Exit (ExitCode (ExitFailure))
import System.IO (hIsTerminalDevice, isEOF)
import System.Posix.Process (exitImmediately)
import System.Posix.Signals (Handler (CatchOnce), installHandler, sigINT, sigTERM)
import UnliftIO.Async (withAsync)

import Ecluse.Env (Env)
import Ecluse.Server.Context (
    MountBinding (..),
    RequestCtx (RequestCtx),
    runHandler,
 )
import Ecluse.Server.Pipeline (servePackument, serveTarball)
import Ecluse.Server.Response (MountRenderer, RenderedBody (RenderedBody), renderError)
import Ecluse.Server.Route (Route (..))

-- ── server configuration ─────────────────────────────────────────────────────

{- | The server's own settings — the values the 'Application' and 'runServer'
need that the composition-root 'Env' does not carry: the listen port, the served
mount bindings, and the request-body cap. Backend selection is a composition-root
concern; this is the minimal shape the web layer needs to route.
-}
data ServerConfig = ServerConfig
    { scPort :: Int
    -- ^ The TCP port @warp@ listens on.
    , scMounts :: [MountBinding]
    {- ^ The mounts served, tried in order; the first whose prefix matches the
    request's leading segments wins. A deployment with no mounts serves nothing
    beyond the health probes — every other path is the neutral @404@.
    -}
    , scSizeLimit :: RequestSizeLimit
    -- ^ The defensive cap on request-body size.
    , scDrain :: DrainSignal
    {- ^ The shared shutdown-drain flag the front door observes: once raised, the
    readiness probe fails ('readiness') and responses carry @Connection: close@
    (the going-away middleware), so a load balancer stops routing new traffic to
    this instance and clients stop reusing keep-alive sockets to it. Defaults to
    'neverDraining'; 'runServer' replaces it with a live signal it flips on a
    shutdown signal.
    -}
    , scDrainTimeout :: ShutdownDrainTimeout
    {- ^ How long the graceful drain waits for in-flight requests and in-progress
    artifact streams to finish before the process exits ('defaultShutdownDrainTimeout').
    -}
    }

{- | Build a 'ServerConfig' over the given mount bindings, taking the default
listen port ('defaultPort') and request-body cap ('defaultRequestSizeLimit').

The composition root supplies the bindings — each a mount's complete ecosystem
wiring — and overrides the port or cap by record update where a deployment needs
to. There is no built-in mount: an ecosystem is served only once its binding is
passed here, so the web layer carries no ecosystem of its own.
-}
mkServerConfig :: [MountBinding] -> ServerConfig
mkServerConfig mounts =
    ServerConfig
        { scPort = defaultPort
        , scMounts = mounts
        , scSizeLimit = defaultRequestSizeLimit
        , scDrain = neverDraining
        , scDrainTimeout = defaultShutdownDrainTimeout
        }

-- | The conventional npm proxy listen port (4873), the 'mkServerConfig' default.
defaultPort :: Int
defaultPort = 4873

-- ── request-body cap ─────────────────────────────────────────────────────────

{- | The maximum request-body size accepted, in bytes — a defensive cap so a
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

-- ── graceful shutdown ──────────────────────────────────────────────────────────

{- | The shared shutdown-drain flag the front door observes during a graceful
rollover, as a small handle (a reader plus a one-way raise) rather than a bare
'TVar' — so the same field can hold either a live, flip-once signal ('newDrainSignal')
or the inert 'neverDraining' constant the socket-free tests assemble against, and
nothing downstream can lower it back. It is raised once, on a shutdown signal, and
read on every request by the readiness probe and the going-away middleware.
-}
data DrainSignal = DrainSignal
    { drainState :: STM Bool
    -- ^ Whether the instance is draining: 'False' while serving, 'True' once raised.
    , drainRaise :: STM ()
    -- ^ Raise the flag. Idempotent — a second raise is a no-op.
    }

{- | Allocate a live, lowered shutdown-drain signal backed by a 'TVar'. 'runServer'
allocates one per launch and flips it from the signal handler; the @application@ it
builds reads the very same signal, so the readiness probe and the going-away
middleware see the drain the instant the handler raises it.
-}
newDrainSignal :: IO DrainSignal
newDrainSignal = do
    tvar <- newTVarIO False
    pure
        DrainSignal
            { drainState = readTVar tvar
            , drainRaise = writeTVar tvar True
            }

{- | The inert drain signal: permanently lowered, raising it is a no-op. The
'mkServerConfig' default, so an @application@ assembled for a socket-free test (and
one driven without ever entering shutdown) reports ready and adds no going-away
header. A real launch overrides it with 'newDrainSignal' in 'runServer'.
-}
neverDraining :: DrainSignal
neverDraining =
    DrainSignal
        { drainState = pure False
        , drainRaise = pure ()
        }

-- | Raise a drain signal — the one-way transition into draining. Idempotent.
beginDrain :: DrainSignal -> IO ()
beginDrain = atomically . drainRaise

-- | Read whether a drain signal is raised.
isDraining :: DrainSignal -> IO Bool
isDraining = atomically . drainState

{- | The bound on the graceful drain: how many seconds the server waits for
in-flight requests and in-progress artifact streams to finish after it stops
accepting new connections, before the process exits regardless. A @newtype@ so a
raw seconds count is not mistaken for some other 'Int', and so a non-positive value
cannot be passed where a positive timeout is meant (see 'runServer').
-}
newtype ShutdownDrainTimeout = ShutdownDrainTimeout Int
    deriving stock (Eq, Show)

{- | The default graceful-drain bound: 30 seconds. Long enough for an in-flight
metadata fetch or a moderate artifact stream to complete during a rolling deploy,
short enough that a stuck request cannot pin the old instance indefinitely.
-}
defaultShutdownDrainTimeout :: ShutdownDrainTimeout
defaultShutdownDrainTimeout = ShutdownDrainTimeout 30

-- ── local-dev immediate halt ─────────────────────────────────────────────────

{- | The local-development immediate-halt wiring, as three injectable seams so its
logic is exercised without a real terminal. It exists only to give an interactive
session a "quit now" key: when the server is attached to a TTY, closing standard
input (Ctrl-D) forces an __immediate__ process exit, aborting any in-progress drain
— the same hard-stop a second Ctrl-C gives, but on the dev's deliberate signal.

It is __inert outside an interactive terminal__: in production standard input is a
non-TTY or closed, 'haltOnInteractive' returns 'False', and no watcher is installed,
so the signal-driven graceful lifecycle is completely untouched. The TTY guard is
what enforces that zero-production-impact contract (see 'withInteractiveHalt').
-}
data InteractiveHalt = InteractiveHalt
    { haltOnInteractive :: IO Bool
    {- ^ Whether to arm the halt at all — the production guard. The real wiring is
    "is standard input a terminal?", so a non-interactive process never installs the
    watcher.
    -}
    , awaitHaltSignal :: IO ()
    {- ^ Block until the dev's halt signal. The real wiring reads standard input
    until end-of-input (Ctrl-D); it returns when the watcher should fire.
    -}
    , halt :: IO ()
    {- ^ The halt itself: terminate the process __immediately__, bypassing the drain
    wait. The real wiring is a direct @_exit@ ('exitImmediately'), matching the
    second-Ctrl-C hard stop.
    -}
    }

{- | The real local-dev halt: armed only when standard input is a terminal
('hIsTerminalDevice'), fired by end-of-input on standard input (Ctrl-D), and
halting via 'exitImmediately' — an immediate @_exit@ that bypasses the graceful
drain, mirroring a second Ctrl-C. The exit status (130) is the conventional
"terminated from the terminal" code.
-}
defaultInteractiveHalt :: InteractiveHalt
defaultInteractiveHalt =
    InteractiveHalt
        { haltOnInteractive = hIsTerminalDevice stdin
        , awaitHaltSignal = awaitStdinEof
        , halt = exitImmediately (ExitFailure 130)
        }
  where
    -- Read and discard standard input until end-of-input. On an interactive
    -- terminal this blocks until the dev presses Ctrl-D (or the stream otherwise
    -- closes); typed lines in between are consumed and ignored — the watcher only
    -- cares about the close.
    awaitStdinEof :: IO ()
    awaitStdinEof = go
      where
        go =
            isEOF >>= \case
                True -> pass
                False -> void getLine >> go

{- | Run an action with the local-dev immediate-halt watcher armed __only when
interactive__. If 'haltOnInteractive' is 'True', a watcher runs alongside the action
for exactly its lifetime ('withAsync', so it is torn down when the action returns or
is cancelled — it never lingers); the watcher blocks on 'awaitHaltSignal' and, when
that returns, runs 'halt'. If 'False' — the production case — the action runs alone,
with no watcher and no extra thread, so nothing about the graceful lifecycle changes.
-}
withInteractiveHalt :: InteractiveHalt -> IO a -> IO a
withInteractiveHalt ih action =
    haltOnInteractive ih >>= \case
        False -> action
        True -> withAsync (awaitHaltSignal ih >> halt ih) (const action)

-- ── the application ──────────────────────────────────────────────────────────

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

{- Dispatch a request to its handler. Top-level health probes are answered first,
above any mount. Otherwise the leading path segments are matched to a mount: a
match routes the remainder through that mount's binding; no match is the neutral
@404@, since there is no ecosystem to render it.
-}
dispatch :: ServerConfig -> Env -> Application
dispatch cfg env request respond =
    case pathInfo request of
        ["livez"] -> respond (liveness env)
        ["readyz"] -> readiness (scDrain cfg) >>= respond
        segments -> case matchMount (scMounts cfg) segments of
            Nothing -> respond notFound
            Just (binding, classified) -> serve env binding classified request respond

{- Serve a classified route under its matched mount. Dispatch builds the
per-request 'RequestCtx' once — the composition-root 'Env' paired with the matched
'MountBinding' — and the effectful 'Packument' and 'Tarball' routes run in the
'Handler' reader over it, so the handler reads the mount's serve dependencies and
renderer from context rather than as threaded arguments (the deps-or-@501@ decision
is the handler's). Every other route renders to a pure 'Response' through the
mount's renderer.
-}
serve :: Env -> MountBinding -> Route -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
serve env binding classified request respond =
    case classified of
        Packument name -> runHandler ctx (servePackument name request respond)
        Tarball name version filename -> runHandler ctx (serveTarball name version filename request respond)
        _ -> respond (renderRoute (bindingRenderer binding) classified)
  where
    ctx :: RequestCtx
    ctx = RequestCtx env binding

{- Match a request path to a mount: the first binding whose prefix the path begins
with, paired with the remainder classified through that mount's classifier.
'Nothing' when no mount's prefix matches — the caller then answers the neutral
@404@. A mount prefix is accepted with or without a trailing slash, so @\/npm\/pkg@
and a bare @\/npm@ both match the @\/npm@ mount.
-}
matchMount :: [MountBinding] -> [Text] -> Maybe (MountBinding, Route)
matchMount mounts segments = asum (map match mounts)
  where
    -- The binding whose prefix the path begins with, paired with the classified
    -- remainder. 'Nothing' for a non-matching prefix.
    match :: MountBinding -> Maybe (MountBinding, Route)
    match binding =
        (binding,) . bindingClassifier binding
            <$> stripPrefixSegments (toList (bindingPrefix binding)) segments

{- Strip a mount's prefix segments off the front of a request path. The root
mount (an empty prefix) consumes nothing and always matches. A trailing empty
segment left after the prefix — the trailing slash of a bare @\/npm\/@ — is
dropped so it is not mistaken for an empty ecosystem path component.
-}
stripPrefixSegments :: [Text] -> [Text] -> Maybe [Text]
stripPrefixSegments [] segs = Just (dropTrailingSlash segs)
stripPrefixSegments (p : ps) (s : ss)
    | p == s = stripPrefixSegments ps ss
stripPrefixSegments _ _ = Nothing

-- A single trailing empty segment (a bare-mount trailing slash, e.g. @\/npm\/@
-- arriving as @["npm",""]@) is dropped so the remainder is the empty path, not a
-- spurious empty component. A non-trailing empty segment is left untouched for
-- the router to reject.
dropTrailingSlash :: [Text] -> [Text]
dropTrailingSlash [""] = []
dropTrailingSlash (x : xs) = x : dropTrailingSlash xs
dropTrailingSlash [] = []

-- ── route rendering ──────────────────────────────────────────────────────────

{- Render a non-effectful in-mount classified 'Route' to a pure response through
the mount's renderer. @\/-\/ping@ is answered locally with @200 {}@;
@\/-\/v1\/search@ is a @501@ pointer; an unrecognised in-mount path is a @404@ —
every error in the mount's own surface. The effectful 'Packument' and 'Tarball'
routes are dispatched to the 'Handler' by 'serve' before reaching here; their
branches below are the defensive @501@ fallback should that routing ever change.
-}
renderRoute :: MountRenderer -> Route -> Response
renderRoute renderer = \case
    Ping -> pong
    Search -> renderedError renderer status501 "search is not supported by this proxy; use the public registry's website to discover packages"
    Packument _ -> renderedError renderer status501 notYetServedMessage
    Tarball{} -> renderedError renderer status501 notYetServedMessage
    Unsupported -> renderedError renderer status404 "not found"
  where
    notYetServedMessage :: Text
    notYetServedMessage = "this route is recognised but not yet served by this proxy"

-- An in-mount error response: the status, with the body shaped by the mount's
-- renderer. A meta-route error carries no operator help message.
renderedError :: MountRenderer -> Status -> Text -> Response
renderedError renderer status message =
    let RenderedBody contentType body = renderError renderer Nothing message
     in responseLBS status [(hContentType, contentType)] body

-- ── meta-route responses ─────────────────────────────────────────────────────

{- @\/-\/ping@: answered locally with @200 {}@, since the client is only
checking that the proxy endpoint it talks to is up. No upstream round-trip.
-}
pong :: Response
pong = jsonResponse status200 "{}"

{- A path matching no configured mount: a generic @404 Not Found@ in @text\/plain@.
This tier sits above the mounts, so there is no ecosystem to shape it — the body is
kept as readable as possible to whatever client reached an unmounted path.
-}
notFound :: Response
notFound =
    responseLBS status404 [(hContentType, "text/plain; charset=utf-8")] "Not Found\n"

-- ── health probes ────────────────────────────────────────────────────────────

{- Liveness (@\/livez@): @200@ while the process is responsive. The architecture
folds the mirror worker's consume-loop heartbeat into single-process liveness so a
stalled worker fails it (see @docs\/architecture\/cloud-backends.md@ → "Process model").

Liveness stays @200@ __throughout__ a graceful drain: a draining instance is alive
and finishing its in-flight work, not unhealthy, so an orchestrator must not kill it
prematurely — that is the readiness probe's job (see 'readiness').
-}
liveness :: Env -> Response
liveness _env = jsonResponse status200 "{\"status\":\"live\"}"

{- Readiness (@\/readyz@): @200@ when config is loaded and the listener is serving,
@503@ once the instance is __draining__. It is deliberately __lenient about
public-upstream reachability__ — the proxy still serves private-upstream hits when
public is down — so readiness must not flap on an upstream blip and pull a healthy
pod from rotation.

The drain flip is the load-balancer signal of a graceful rollover: while the
'DrainSignal' is raised, readiness fails so an upstream LB or service mesh stops
routing __new__ traffic here, while in-flight requests finish (see
@docs\/architecture\/hosting.md@ → "Graceful rollover").
-}
readiness :: DrainSignal -> IO Response
readiness drain =
    isDraining drain <&> \case
        True -> jsonResponse status503 "{\"status\":\"draining\"}"
        False -> jsonResponse status200 "{\"status\":\"ready\"}"

-- ── response helpers ─────────────────────────────────────────────────────────

-- A JSON response with the given status and body, tagged @application\/json@.
jsonResponse :: Status -> LByteString -> Response
jsonResponse status =
    responseLBS status [(hContentType, "application/json")]

-- ── middleware ───────────────────────────────────────────────────────────────

{- | The cross-cutting middleware stack composed around the proxy 'Application': a
defensive request-body size cap (rejecting an over-cap body with @413@ once a
handler reads it), correct client-IP recovery behind a load balancer
(@X-Forwarded-For@ \/ @X-Real-IP@), and a per-request timeout.

A fourth middleware, the __going-away__ header, is active only during a graceful
drain: while the 'ServerConfig''s 'DrainSignal' is raised it stamps @Connection:
close@ on every response so an HTTP\/1.1 keep-alive pool (a client's, or a service
mesh's connection pool) does not reuse a socket on an instance that is shutting down
— the cause of the 503-on-rollover this guards against (see
@docs\/architecture\/hosting.md@ → "Graceful rollover").

Two @wai-extra@ middlewares are deliberately __not__ used. @Autohead@ answers a
HEAD by running the GET handler and discarding the body, which on a tarball route
would open the upstream and stream a whole artifact to nowhere — HEAD on
artifacts is handled explicitly instead. @Gzip@ would re-compress already
compressed artifacts and fight the streaming backpressure the serve path relies
on.
-}
serverMiddleware :: ServerConfig -> Middleware
serverMiddleware cfg =
    sizeLimitMiddleware (scSizeLimit cfg)
        . realIp
        . timeout timeoutSeconds
        . goingAwayMiddleware (scDrain cfg)

{- While the instance is draining, stamp @Connection: close@ on every response so a
keep-alive client (or a mesh connection pool) does not reuse the socket on a closing
instance; while serving, pass responses through untouched. The flag is read
per-response — the same one-way 'DrainSignal' the readiness probe observes — so the
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
    -- keeps streaming — only its headers are rewritten.
    closeConnection :: Response -> Response
    closeConnection = mapResponseHeaders ((hConnection, "close") :)

-- The per-request timeout, in seconds. Generous enough for a large packument
-- fetch, bounded so a stuck upstream cannot pin a handler indefinitely.
timeoutSeconds :: Int
timeoutSeconds = 60

-- Cap the request body at the configured limit, rejecting an over-cap body
-- before it is buffered.
sizeLimitMiddleware :: RequestSizeLimit -> Middleware
sizeLimitMiddleware (RequestSizeLimit maxBytes) =
    requestSizeLimitMiddleware
        (setMaxLengthForRequest (\_req -> pure (Just maxBytes)) defaultRequestSizeLimitSettings)

-- ── running ──────────────────────────────────────────────────────────────────

{- | Serve the proxy's HTTP front door: start @warp@ on the 'ServerConfig''s port
with the 'application' built over it and the composition-root 'Env'. The
'ServerConfig' — in particular its mount bindings ('scMounts'), each a mount's
complete ecosystem wiring — is supplied by the composition root, which is where the
served ecosystems are mounted (see "Ecluse").

__Graceful shutdown.__ A fresh live 'DrainSignal' is allocated per launch and wired
into both the request path (the @application@ reads it through 'scDrain') and the
@warp@ shutdown handler. On @SIGTERM@ or @SIGINT@ the handler raises the drain — so
the readiness probe begins failing and responses gain @Connection: close@ — then
closes the listen socket, which puts @warp@ into graceful-shutdown mode: it stops
accepting new connections and waits for in-flight requests __and in-progress
artifact streams__ to finish before the process exits, bounded by 'scDrainTimeout'.
The handler is a 'CatchOnce', so a second signal during the drain hard-stops the
server rather than being swallowed.

__Local-dev quit key.__ The whole run is wrapped in 'withInteractiveHalt', which —
__only when attached to an interactive terminal__ — arms a watcher that forces an
immediate halt on Ctrl-D (end of standard input), bypassing the drain like a second
Ctrl-C. Outside a TTY (production) no watcher is installed and this changes nothing.
-}
runServer :: ServerConfig -> Env -> IO ()
runServer cfg0 env = do
    drain <- newDrainSignal
    let cfg = cfg0{scDrain = drain}
        ShutdownDrainTimeout timeoutSecs = scDrainTimeout cfg
        settings =
            Warp.setPort (scPort cfg)
                . Warp.setInstallShutdownHandler (installShutdownHandler drain)
                . Warp.setGracefulShutdownTimeout (Just timeoutSecs)
                $ Warp.defaultSettings
    withInteractiveHalt defaultInteractiveHalt (Warp.runSettings settings (application cfg env))

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
