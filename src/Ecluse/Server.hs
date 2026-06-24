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

    -- * Middleware
    serverMiddleware,

    -- * Request-body cap
    RequestSizeLimit (..),
    defaultRequestSizeLimit,
) where

import Network.HTTP.Types (Status, hContentType, status200, status404, status501)
import Network.Wai (Application, Middleware, Request, Response, ResponseReceived, pathInfo, responseLBS)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.RealIp (realIp)
import Network.Wai.Middleware.RequestSizeLimit (defaultRequestSizeLimitSettings, requestSizeLimitMiddleware, setMaxLengthForRequest)
import Network.Wai.Middleware.Timeout (timeout)

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
        ["readyz"] -> respond (readiness env)
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
-}
liveness :: Env -> Response
liveness _env = jsonResponse status200 "{\"status\":\"live\"}"

{- Readiness (@\/readyz@): config is loaded and the listener is serving. It is
deliberately __lenient about public-upstream reachability__ — the proxy still
serves private-upstream hits when public is down — so readiness must not flap on
an upstream blip and pull a healthy pod from rotation.
-}
readiness :: Env -> Response
readiness _env = jsonResponse status200 "{\"status\":\"ready\"}"

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
-}
runServer :: ServerConfig -> Env -> IO ()
runServer cfg env =
    Warp.run (scPort cfg) (application cfg env)
