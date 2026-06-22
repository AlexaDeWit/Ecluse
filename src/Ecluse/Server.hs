{- | The HTTP front door: the raw @wai@ 'Application', its dispatch, the
meta-routes, the middleware stack, and 'runServer'.

The proxy is a passthrough over a small, irregular URL surface, so the front
door is a raw 'Application' rather than a web framework — matching on @pathInfo@
keeps the encoded-slash handling and the streaming control the proxy depends on
(see @docs\/architecture\/web-layer.md@). Routing is two layers:

* __Mount dispatch__ — match a request's leading path segment to a configured
  mount, strip the prefix, and hand the remainder (an ecosystem-native path) to
  the __pure__ router 'classify' ("Ecluse.Server.Route"). A mount prefix is
  accepted with or without a trailing slash (see
  @docs\/architecture\/hosting.md@ → "Dispatch").

* __Control-plane meta-routes__ — orchestration health probes (@\/livez@,
  @\/readyz@) answered at the top level, above any mount.

The classified 'Route' renders through the error model
("Ecluse.Server.Response"): @\/-\/ping@ is answered locally with @200 {}@,
@\/-\/v1\/search@ is @501@ (search is not an install path), and anything
unrecognised is @404@ — deny by default at the routing layer. The
package\/artifact routes ('Packument', 'Tarball') resolve to the fetch → rules →
serve pipeline; until that pipeline is wired they return an explicit
@501 Not Implemented@, never a fabricated success.

Cross-cutting concerns are applied as middleware composed around the
'Application' (see @docs\/architecture\/web-layer.md@ → "Middleware"): a
defensive request-body size cap, correct client-IP recovery behind a load
balancer, and a request timeout. Handlers run in plain 'IO' taking 'Env', so the
hot path carries no transformer lifting.
-}
module Ecluse.Server (
    -- * The WAI application
    ServerConfig (..),
    defaultServerConfig,
    Mount (..),
    rootMount,
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
import Network.Wai (Application, Middleware, Response, pathInfo, responseLBS)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.RealIp (realIp)
import Network.Wai.Middleware.RequestSizeLimit (defaultRequestSizeLimitSettings, requestSizeLimitMiddleware, setMaxLengthForRequest)
import Network.Wai.Middleware.Timeout (timeout)

import Ecluse.Env (Env)
import Ecluse.Server.Response (denialBody)
import Ecluse.Server.Route (Route (..), classify)

-- ── server configuration ─────────────────────────────────────────────────────

{- | The server's own settings — the values the 'Application' and 'runServer'
need that the composition-root 'Env' does not (yet) carry: the listen port, the
served mounts, and the request-body cap. Backend selection and the resolved mount
map are a later composition-root concern; this is the minimal shape the web layer
needs to route.
-}
data ServerConfig = ServerConfig
    { scPort :: Int
    -- ^ The TCP port @warp@ listens on.
    , scMounts :: [Mount]
    -- ^ The mounts served, tried in order; the first whose prefix matches the
    -- request's leading segment wins. A single-mount deployment is the
    -- one-entry case at the root ('rootMount').
    , scSizeLimit :: RequestSizeLimit
    -- ^ The defensive cap on request-body size.
    }

{- | The default server settings: a single root mount on the conventional npm
proxy port (4873), with the default body cap. This is the env-only,
single-mount launch shape; a multi-mount or alternate-port deployment overrides
it at the composition root.
-}
defaultServerConfig :: ServerConfig
defaultServerConfig =
    ServerConfig
        { scPort = 4873
        , scMounts = [rootMount]
        , scSizeLimit = defaultRequestSizeLimit
        }

{- | A mount: a path prefix bound to a registry served beneath it. Dispatch
matches a request's leading path segment to 'mountPrefix', strips it, and routes
the remainder. The prefix is the path segments before the ecosystem-native path
(e.g. @["npm"]@ for a @\/npm@ mount); the empty list is the root mount, under
which the whole path is ecosystem-native.
-}
newtype Mount = Mount
    { mountPrefix :: [Text]
    -- ^ The leading path segments this mount is served under; @[]@ is the root.
    }
    deriving stock (Eq, Show)

{- | The root mount: no prefix, so the whole request path is the ecosystem-native
path handed to 'classify'. The degenerate single-mount case.
-}
rootMount :: Mount
rootMount = Mount{mountPrefix = []}

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

{- | Dispatch a request to its handler: top-level health probes first, then mount
dispatch into the pure router. Unrecognised paths (including an unmatched mount)
are a @404@ — deny by default.
-}
dispatch :: ServerConfig -> Env -> Application
dispatch cfg env request respond =
    case pathInfo request of
        ["livez"] -> respond (liveness env)
        ["readyz"] -> respond (readiness env)
        segments -> respond (route env (dispatchMount (scMounts cfg) segments))

{- | Strip the first matching mount prefix off the request path, returning the
remaining ecosystem-native segments to classify. A mount prefix is accepted with
or without a trailing slash, so @\/npm\/pkg@ and a bare @\/npm@ both match the
@\/npm@ mount. When no mount matches, the path is classified as-is, which denies
it by default (the router recognises no such path) — there is no separate
"unknown mount" status, by design.
-}
dispatchMount :: [Mount] -> [Text] -> Route
dispatchMount mounts segments =
    classify (fromMaybe segments (firstJust (strip segments) mounts))
  where
    strip :: [Text] -> Mount -> Maybe [Text]
    strip segs (Mount prefix) = stripPrefixSegments prefix segs

{- | Strip a mount's prefix segments off the front of a request path. The root
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

-- The first non-'Nothing' result of applying @f@ across the list, or 'Nothing'.
firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = foldr (\x acc -> f x <|> acc) Nothing

-- ── route rendering ──────────────────────────────────────────────────────────

{- | Render a classified 'Route' to a response. @\/-\/ping@ is answered locally;
@\/-\/v1\/search@ is @501@ with a pointer message; a package or artifact route
resolves to the (not-yet-wired) serve pipeline; anything unrecognised is @404@.
-}
route :: Env -> Route -> Response
route _env = \case
    Ping -> pong
    Search -> searchUnsupported
    Packument _ -> notYetServed
    Tarball _ _ -> notYetServed
    Unsupported -> notFound

-- ── meta-route responses ─────────────────────────────────────────────────────

{- | @\/-\/ping@: answered locally with @200 {}@, since the client is only
checking that the proxy endpoint it talks to is up. No upstream round-trip.
-}
pong :: Response
pong = jsonResponse status200 "{}"

{- | @\/-\/v1\/search@: @501 Not Implemented@. Search is a discovery convenience,
not an install path, so it is deliberately unsupported rather than scope-creeping
a filtered or pass-through search; the body points users to the public registry's
website.
-}
searchUnsupported :: Response
searchUnsupported =
    jsonResponse
        status501
        (denialBody Nothing "search is not supported by this proxy; use the public registry's website to discover packages")

{- | A package or artifact route whose serve pipeline is not yet wired:
@501 Not Implemented@ with an explicit message, rather than a fabricated @200@.
The fetch → rules → serve body is filled in by the request-pipeline slices.
-}
notYetServed :: Response
notYetServed =
    jsonResponse
        status501
        (denialBody Nothing "this route is recognised but not yet served by this proxy")

-- | An unrecognised path: @404@. Deny by default at the routing layer.
notFound :: Response
notFound = jsonResponse status404 (denialBody Nothing "not found")

-- ── health probes ────────────────────────────────────────────────────────────

{- | Liveness (@\/livez@): @200@ while the process is responsive. The architecture
folds the mirror worker's consume-loop heartbeat into single-process liveness so a
stalled worker fails it (see @docs\/architecture\/cloud-backends.md@ → "Process model").
-}
liveness :: Env -> Response
liveness _env = jsonResponse status200 "{\"status\":\"live\"}"

{- | Readiness (@\/readyz@): config is loaded and the listener is serving. It is
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

{- | Serve the proxy's HTTP front door: start @warp@ on the configured port with
the 'application' built over the composition-root 'Env'. Request handlers read
the 'Env' in plain 'IO'. This is the server entry function of the single-process
program (run concurrently with the mirror worker; see "Ecluse").
-}
runServer :: Env -> IO ()
runServer env =
    Warp.run (scPort cfg) (application cfg env)
  where
    cfg :: ServerConfig
    cfg = defaultServerConfig
