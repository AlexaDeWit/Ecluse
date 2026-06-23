-- TupleSections: pairing a matched mount with its classified remainder in
-- 'dispatchMount' ((mount,) . classifier); see STYLE.md §2.
{-# LANGUAGE TupleSections #-}

{- | The HTTP front door: the raw @wai@ 'Application', its dispatch, the
meta-routes, the middleware stack, and 'runServer'.

The proxy is a passthrough over a small, irregular URL surface, so the front
door is a raw 'Application' rather than a web framework — matching on @pathInfo@
keeps the encoded-slash handling and the streaming control the proxy depends on
(see @docs\/architecture\/web-layer.md@). Routing is two layers:

* __Mount dispatch__ — match a request's leading path segment to a configured
  mount, strip the prefix, and hand the remainder (an ecosystem-native path) to
  the mount's injected 'Ecluse.Server.Route.Classifier'. The web layer is closed
  over the shared 'Route' set ("Ecluse.Server.Route") and routes through whatever
  classifier a composition root wires in ('scClassify'), so no ecosystem's path
  grammar is baked in here. A mount prefix is accepted with or without a trailing
  slash (see @docs\/architecture\/hosting.md@ → "Dispatch").

* __Control-plane meta-routes__ — orchestration health probes (@\/livez@,
  @\/readyz@) answered at the top level, above any mount.

The classified 'Route' renders through the error model
("Ecluse.Server.Response"): @\/-\/ping@ is answered locally with @200 {}@,
@\/-\/v1\/search@ is @501@ (search is not an install path), and anything
unrecognised is @404@ — deny by default at the routing layer. The
package\/artifact routes ('Packument', 'Tarball') are recognised but not served
here: they return an explicit @501 Not Implemented@ rather than a fabricated
success, since their fetch → rules → serve pipeline lives outside this module.

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
    noPackumentDeps,
    noClassifier,
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
import Ecluse.Server.Pipeline (PackumentDeps, servePackument)
import Ecluse.Server.Response (denialBody)
import Ecluse.Server.Route (Classifier, Route (..), denyAll)

-- ── server configuration ─────────────────────────────────────────────────────

{- | The server's own settings — the values the 'Application' and 'runServer'
need that the composition-root 'Env' does not (yet) carry: the listen port, the
served mounts, and the request-body cap. Backend selection and the resolved mount
map are a composition-root concern; this is the minimal shape the web layer needs
to route.
-}
data ServerConfig = ServerConfig
    { scPort :: Int
    -- ^ The TCP port @warp@ listens on.
    , scMounts :: [Mount]
    {- ^ The mounts served, tried in order; the first whose prefix matches the
    request's leading segment wins. A single-mount deployment is the
    one-entry case at the root ('rootMount').
    -}
    , scSizeLimit :: RequestSizeLimit
    -- ^ The defensive cap on request-body size.
    , scClassify :: Mount -> Classifier
    {- ^ The route classifier for a matched mount — the ecosystem path grammar
    that maps its native path to a shared 'Route'. The default 'noClassifier'
    denies every path ('Ecluse.Server.Route.denyAll'), so the web layer carries no
    ecosystem's grammar and a composition root injects each mount's adapter
    classifier instead. The function form keys per matched mount, mirroring
    'scPackumentDeps', so a multi-ecosystem deployment routes each mount through
    its own grammar.
    -}
    , scPackumentDeps :: Mount -> Maybe PackumentDeps
    {- ^ The packument-serve dependencies for a matched mount — its upstream
    endpoints, rule policy, and edge token. 'Nothing' leaves the packument route
    recognised-but-unserved (the @501@ stub), so the resolved mount map is wired in
    at the composition root rather than baked into the web layer. The function form
    keys per matched mount without forcing 'PackumentDeps' (which carries a clock
    action) into the 'Eq'\/'Show' 'Mount'.
    -}
    }

{- | The default server settings: a single root mount on the conventional npm
proxy port (4873), with the default body cap, no route classifier (every path is
denied) and no packument-serve dependencies (the route stays the
recognised-but-unserved @501@ until a composition root supplies them). This is the
env-only, single-mount launch shape; a multi-mount or alternate-port deployment
overrides it at the composition root.

The default deliberately carries __no ecosystem grammar__: with 'noClassifier'
every request is 'Unsupported', so a composition root must wire an adapter's
classifier in for the server to recognise anything.
-}
defaultServerConfig :: ServerConfig
defaultServerConfig =
    ServerConfig
        { scPort = 4873
        , scMounts = [rootMount]
        , scSizeLimit = defaultRequestSizeLimit
        , scClassify = noClassifier
        , scPackumentDeps = noPackumentDeps
        }

{- | The "no route classifier wired" resolver: every mount maps to
'Ecluse.Server.Route.denyAll', so every path is 'Unsupported' (a @404@). The
default until a composition root supplies a mount's ecosystem grammar.
-}
noClassifier :: Mount -> Classifier
noClassifier _ = denyAll

{- | The "no packument dependencies wired" resolver: every mount maps to 'Nothing',
so the packument route renders the recognised-but-unserved @501@ stub. The default
until a composition root supplies a mount's upstreams and policy.
-}
noPackumentDeps :: Mount -> Maybe PackumentDeps
noPackumentDeps _ = Nothing

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
path handed to the mount's classifier. The degenerate single-mount case.
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

{- Dispatch a request to its handler: top-level health probes first, then mount
dispatch through the injected classifier. Unrecognised paths (including an
unmatched mount) are a @404@ — deny by default.
-}
dispatch :: ServerConfig -> Env -> Application
dispatch cfg env request respond =
    case pathInfo request of
        ["livez"] -> respond (liveness env)
        ["readyz"] -> respond (readiness env)
        segments ->
            let (mount, classified) = dispatchMount (scClassify cfg) (scMounts cfg) segments
             in serve cfg env mount classified request respond

{- Serve a classified route. The package\/artifact routes are effectful (they
fetch upstream), so they are handled in 'IO' over the 'respond' continuation; every
other route renders to a pure 'Response'. A 'Packument' on a mount with wired
dependencies is served by the pipeline; without them it falls back to the
recognised-but-unserved @501@.
-}
serve :: ServerConfig -> Env -> Mount -> Route -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
serve cfg env mount classified request respond =
    case classified of
        Packument name
            | Just deps <- scPackumentDeps cfg mount ->
                servePackument deps env name request respond
        _ -> respond (route env classified)

{- Strip the first matching mount prefix off the request path, classify the
remainder through that mount's injected classifier, and return the matched mount
with its 'Route'. A mount prefix is accepted with or without a trailing slash, so
@\/npm\/pkg@ and a bare @\/npm@ both match the @\/npm@ mount. When no mount
matches, the path is classified as-is under the first mount, which denies it by
default (the classifier recognises no such path) — there is no separate "unknown
mount" status, by design.
-}
dispatchMount :: (Mount -> Classifier) -> [Mount] -> [Text] -> (Mount, Route)
dispatchMount classifierFor mounts segments =
    fromMaybe (firstMount, classifierFor firstMount segments) (firstJust matched mounts)
  where
    -- The mount whose prefix the path begins with, paired with the remainder
    -- classified by that mount's classifier. 'Nothing' for a non-matching prefix.
    matched :: Mount -> Maybe (Mount, Route)
    matched mount@(Mount prefix) =
        (mount,) . classifierFor mount <$> stripPrefixSegments prefix segments

    -- When no mount matched, classification still runs (denying by default); it is
    -- reported under the first configured mount, whose deps are irrelevant to an
    -- 'Unsupported' route. A non-empty mount list is the launch invariant.
    firstMount :: Mount
    firstMount = case mounts of
        mount : _ -> mount
        [] -> rootMount

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

-- The first non-'Nothing' result of applying @f@ across the list, or 'Nothing'.
firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = foldr (\x acc -> f x <|> acc) Nothing

-- ── route rendering ──────────────────────────────────────────────────────────

{- Render a classified 'Route' to a pure response. @\/-\/ping@ is answered
locally; @\/-\/v1\/search@ is @501@ with a pointer message; anything unrecognised
is @404@. The package\/artifact routes are effectful and served by 'serve' before
reaching here; they fall through to the recognised-but-unserved @501@ stub only on
a mount with no serve dependencies wired (the tarball path is not yet served at
all).
-}
route :: Env -> Route -> Response
route _env = \case
    Ping -> pong
    Search -> searchUnsupported
    Packument _ -> notYetServed
    Tarball _ _ -> notYetServed
    Unsupported -> notFound

-- ── meta-route responses ─────────────────────────────────────────────────────

{- @\/-\/ping@: answered locally with @200 {}@, since the client is only
checking that the proxy endpoint it talks to is up. No upstream round-trip.
-}
pong :: Response
pong = jsonResponse status200 "{}"

{- @\/-\/v1\/search@: @501 Not Implemented@. Search is a discovery convenience,
not an install path, so it is deliberately unsupported rather than scope-creeping
a filtered or pass-through search; the body points users to the public registry's
website.
-}
searchUnsupported :: Response
searchUnsupported =
    jsonResponse
        status501
        (denialBody Nothing "search is not supported by this proxy; use the public registry's website to discover packages")

{- A recognised package or artifact route that is not served here: @501 Not
Implemented@ with an explicit message, rather than a fabricated @200@. The
fetch → rules → serve pipeline lives outside this module.
-}
notYetServed :: Response
notYetServed =
    jsonResponse
        status501
        (denialBody Nothing "this route is recognised but not yet served by this proxy")

-- An unrecognised path: @404@. Deny by default at the routing layer.
notFound :: Response
notFound = jsonResponse status404 (denialBody Nothing "not found")

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
with the 'application' built over it and the composition-root 'Env'. Request
handlers read the 'Env' in plain 'IO'. The 'ServerConfig' — in particular its
injected route classifier ('scClassify') — is supplied by the composition root,
which is where the served ecosystem's path grammar is wired in (see "Ecluse").
-}
runServer :: ServerConfig -> Env -> IO ()
runServer cfg env =
    Warp.run (scPort cfg) (application cfg env)
