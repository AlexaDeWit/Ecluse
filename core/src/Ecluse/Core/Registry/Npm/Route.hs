-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
-- TupleSections: local convenience for pairing a parsed name with its trailing
-- segments in 'takeScoped' ((,rest) / (,more)); see STYLE.md §2.
{-# LANGUAGE TupleSections #-}

{- | npm's route table: the list of routes an npm mount serves.

Each entry is one 'Ecluse.Core.Server.Route.Route' record, carrying its method condition,
its path template, what to /do/ when it matches, and its documentation. 'npmRouter' folds
the list into the mount's router (first match wins; no match is the deny-by-default @404@)
and 'npmRouteSpecs' projects the same list for the capability manifest, so the routed
surface and the documented one are two readings of one declaration.

There is no npm route /sum/ and no second dispatch: a route's record already says what
serving it amounts to, as the agnostic 'Ecluse.Core.Server.Context.RouteAction' the web
layer understands. The package, artifact, and publish routes name the shared data-plane
handlers ("Ecluse.Core.Server.Pipeline", which reach npm's client and projection as
injected capabilities, never as imports); the meta-routes are answered locally.

A @PUT \/{pkg}@ is the npm __publish__ request, so the method is part of the match: a
@PUT@ over a bare-package path publishes, while a __read__ (@GET@, or its bodiless @HEAD@)
over the same path fetches the packument. Those three methods are the only ones the front
door answers; any other (@POST@, @DELETE@, \u2026) matches no route and denies, so a @DELETE@
over a package path is a @404@ rather than being served a packument.

The model is __deny by default__. Three npm-specific facts shape the matching, all from
the protocol research (see @docs\/research\/reverse-engineering\/npm.md@ §2 and §7):

* __Reserved meta-routes (@\/-\/\u2026@) are matched first.__ A real package name can never
  begin with @\'-\'@, so a leading @"-"@ segment is unambiguously a meta-route; an
  /unknown/ one denies rather than being read as a package.

* __Scoped names arrive in two encodings.__ The path is percent-decoded before it reaches
  us, so a scoped name arrives either as one decoded segment (@\@scope\/pkg@) or as two
  (@\@scope@, @pkg@). Both are normalised to the same 'PackageName' here, so nothing
  downstream re-checks the encoding.

* __A tarball path is @\/{pkg}\/-\/{file}.tgz@.__ The interior @"-"@ segment and the @.tgz@
  suffix distinguish it from a packument request (@\/{pkg}@); for a scoped package the
  basename drops the scope (@\@babel\/code-frame@ \u2192 @code-frame-7.0.0.tgz@). 'tarballRoute'
  is the npm-side parse of the artifact coordinate: it checks the @file@\'s basename is
  exactly @{unscoped-name}-{rest}@ for the requested package and reads @rest@ as the
  version (@mkVersion@, total), preserving the file name __verbatim__. A basename that does
  not match the package is a __path-confusion__ attempt and denies, never a fabricated
  coordinate.

Mount dispatch, prefix-stripping, and the liveness\/readiness routes are handled in the
agnostic web layer (see @docs\/architecture\/web-layer.md@); this table only ever sees the
npm-native request.
-}
module Ecluse.Core.Registry.Npm.Route (
    -- * The mount's router
    npmRouter,

    -- * The table, as data
    npmRoutes,
    npmRouteSpecs,

    -- * The leaf parsers (exported for their specs)
    takePackage,
    tarballRoute,
) where

import Data.Text qualified as T
import Network.HTTP.Types (Method, methodHead, status200, status501)
import Network.HTTP.Types.Method (StdMethod (GET))
import Network.Wai (Response)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName, mkScope, unscopedName)
import Ecluse.Core.Server.Context (MountRouter, RouteAction (AnswerLocally, RunPipeline))
import Ecluse.Core.Server.Path (Filename (Filename), isSafeComponent)
import Ecluse.Core.Server.Pipeline (headPackument, headTarball, servePackument, servePublish, serveTarball)
import Ecluse.Core.Server.Pipeline.Shared (jsonResponse, renderedResponse)
import Ecluse.Core.Server.Response (MountRenderer, renderError)
import Ecluse.Core.Server.Route (
    Capture (Capture),
    MethodMatch (MethodPut, MethodRead),
    PatternSeg (SegCap, SegLit),
    Route (Route),
    RouteName (RouteName),
    routerOf,
 )
import Ecluse.Core.Server.RouteDoc (
    BodyDoc (ArtifactBody, EmptyObjectBody, ErrorEnvelopeBody, NoBody, PackumentBody, PublishDocumentBody),
    RequestDoc (RequestDoc),
    ResponseDoc (ResponseDoc),
    RouteDoc (RouteDoc),
 )
import Ecluse.Core.Server.RouteSpec (ParamSpec (ParamSpec), PathSeg (Param), RouteSpec (RouteSpec), specOf)
import Ecluse.Core.Version (Version, mkVersion)

{- | npm's mount router: the route table folded into the whole routing decision. The
first route that claims the request decides what is done with it; a request no route
claims is the deny-by-default @404@ in npm's own error surface.
-}
npmRouter :: MountRouter
npmRouter = routerOf npmRoutes

{- | npm's routes, in matching order.

The __structure__ (the literal segments, the capture arity, the ordering, the
packument-vs-tarball split) is here; the security-critical __leaf__ parsing stays in the
named functions the captures and builders reference ('takePackage' for a package unit,
'tarballRoute' for the artifact coordinate), which are tested directly.

Ordering follows npm's conventions (see the module header): the reserved meta-routes
(@\/-\/ping@, @\/-\/v1\/search@) are literal and tried first, and the package capture refuses
a bare leading @"-"@ (the meta prefix is never a package name, on any method), so an
unrecognised @\/-\/\u2026@ path denies rather than being read as a package.
-}
npmRoutes :: [Route NpmCap]
npmRoutes =
    [ Route (RouteName "ping") MethodRead [SegLit "-", SegLit "ping"] (answering (const npmPong)) pingDoc
    , Route (RouteName "search") MethodRead [SegLit "-", SegLit "v1", SegLit "search"] (answering npmSearchUnsupported) searchDoc
    , Route (RouteName "tarball") MethodRead [SegCap capPackage, SegLit "-", SegCap capFilename] buildTarball tarballDoc
    , Route (RouteName "packument") MethodRead [SegCap capPackage] buildPackument packumentDoc
    , Route (RouteName "publish") MethodPut [SegCap capPackage] buildPublish publishDoc
    ]

-- A route answered locally, whatever the method and captures (the literal meta-routes).
answering :: (MountRenderer -> Response) -> Method -> [NpmCap] -> Maybe RouteAction
answering render _method _captures = Just (AnswerLocally render)

{- @GET \/{package}@: a bare package unit is a packument read. A @HEAD@ takes the
head-mode handler, which runs the identical gating and merge but withholds the body. -}
buildPackument :: Method -> [NpmCap] -> Maybe RouteAction
buildPackument method = \case
    [NpmPackage name]
        | isHead method -> Just (RunPipeline (headPackument name))
        | otherwise -> Just (RunPipeline (servePackument name))
    _ -> Nothing

{- @PUT \/{package}@: a bare package unit under the write method is a publish. -}
buildPublish :: Method -> [NpmCap] -> Maybe RouteAction
buildPublish _method = \case
    [NpmPackage name] -> Just (RunPipeline (servePublish name))
    _ -> Nothing

{- @GET \/{package}\/-\/{filename}@: an artifact read. 'tarballRoute' applies the
__cross-capture__ path-confusion check (the file's basename must parse for /this/ package)
and reads the version; a mismatched name yields 'Nothing', so the route does not claim the
request and it falls through to the @404@ rather than being fabricated into a coordinate.

A @HEAD@ takes the head-mode handler, which gates the artifact identically but probes the
upstream bodiless, so a @HEAD@ can never open and pump a whole artifact. -}
buildTarball :: Method -> [NpmCap] -> Maybe RouteAction
buildTarball method = \case
    [NpmPackage name, NpmFilename file] -> do
        (version, filename) <- tarballRoute name file
        pure $
            if isHead method
                then RunPipeline (headTarball name version filename)
                else RunPipeline (serveTarball name version filename)
    _ -> Nothing

isHead :: Method -> Bool
isHead = (== methodHead)

{- @\/-\/ping@: answered locally with @200 {}@, since @npm ping@ is only checking that the
endpoint it talks to is up. No upstream round-trip, and no error surface to shape: the
empty JSON object is exactly what an npm client expects. -}
npmPong :: Response
npmPong = jsonResponse status200 [] "{}"

{- @\/-\/v1\/search@: a @501@ pointer, in npm's error surface. Search is a discovery
convenience, not an install path, so the proxy does not proxy it; the message sends the
client to the public registry's website rather than leaving it to guess. -}
npmSearchUnsupported :: MountRenderer -> Response
npmSearchUnsupported renderer =
    renderedResponse status501 [] (renderError renderer Nothing "search is not supported by this proxy; use the public registry's website to discover packages")

{- | The captured values npm's routes produce: a parsed package unit, or a raw,
safety-checked artifact file name. Each pattern's builder consumes these positionally.
-}
data NpmCap
    = NpmPackage PackageName
    | NpmFilename Text

{- | The package capture: one npm package unit, both scoped wire encodings handled by
'takePackage' (which may consume one or two segments).

A bare leading @"-"@ is refused __on every method__: @\/-\/…@ is the reserved
meta-route prefix, and a lone @"-"@ is never a package name. Every other
component-safety rejection is 'takePackage''s.
-}
capPackage :: Capture NpmCap
capPackage =
    Capture
        "package"
        "The package name, URL-encoded; a scoped name is `@scope%2Fname`."
        ( \case
            "-" : _ -> Nothing
            segs -> fmap (first NpmPackage) (takePackage segs)
        )

{- | The artifact-file capture: one segment, accepted only when it is a safe component
('isSafeComponent'); the coordinate parse (the @.tgz@ basename and the version) is
'tarballRoute''s, applied in 'buildTarball'.
-}
capFilename :: Capture NpmCap
capFilename =
    Capture
        "filename"
        "The artifact's on-the-wire file name, e.g. `lodash-4.17.21.tgz`."
        ( \case
            seg : rest | isSafeComponent seg -> Just (NpmFilename seg, rest)
            _ -> Nothing
        )

{- Peel the leading package unit off a path, returning its 'PackageName' and
the remaining segments. A leading segment beginning with @\'\@\'@ is a scoped
name, peeled by 'takeScoped' (which handles both wire encodings).

Returns 'Nothing' (so the caller denies it) for anything without a usable
package: an empty path, or a name with an __unsafe component__ -- a scope or base
name that 'isSafeComponent' rejects (empty, @"."@\/@".."@, or carrying a
@\'\/\'@, @\'\\\\\'@, or control character). This covers the degenerate scoped
names (@\@\/pkg@, @\@scope\/@ reachable from @\/\@scope%2F@, @\@scope\/a\/b@) and
the hostile unscoped names (@\/foo%2Fbar@ → @"foo\/bar"@, @".."@, @"."@) alike.
'mkScope'\/'mkPackageName' do no validation, so this boundary is where such names
are rejected rather than passed downstream into an interpolated upstream URL.
-}
takePackage :: [Text] -> Maybe (PackageName, [Text])
takePackage [] = Nothing
takePackage (seg : rest)
    | "@" <- T.take 1 seg = takeScoped seg rest
    | isSafeComponent seg = Just (mkPackageName Npm Nothing seg, rest)
    | otherwise = Nothing

{- Peel a scoped package unit -- the leading @\@…@ segment -- handling both wire
encodings of a scoped name:

\* one decoded segment, @\@scope\/pkg@ -- split on the first @\'\/\'@;
\* two segments, @\@scope@ then @pkg@ -- consume both.
-}
takeScoped :: Text -> [Text] -> Maybe (PackageName, [Text])
takeScoped seg rest =
    case T.breakOn "/" (T.drop 1 seg) of
        -- One decoded segment "@scope/pkg": scope before the '/', base after.
        -- 'scopedName' may reject it, propagating 'Nothing' through (,rest).
        (scope, base)
            | not (T.null base) ->
                (,rest) <$> scopedName scope (T.drop 1 base)
        -- Bare scope "@scope": the package name is the next segment.
        _ -> case rest of
            (base : more) -> (,more) <$> scopedName (T.drop 1 seg) base
            _ -> Nothing

-- A scoped name is usable only when both halves are safe components. The
-- leading '@' is already stripped from both arguments, so a degenerate or
-- hostile name ('@/pkg', '@scope/', '@scope/a/b', '@../pkg') is rejected here
-- rather than passed to the no-op 'mkScope'/'mkPackageName'.
scopedName :: Text -> Text -> Maybe PackageName
scopedName scope base
    | isSafeComponent scope && isSafeComponent base =
        Just (mkPackageName Npm (Just (mkScope scope)) base)
    | otherwise = Nothing

{- | Parse an npm tarball-slot @file@ into the artifact coordinate it names for @name@:
the 'Version' and the verbatim 'Filename'. 'Nothing' denies it.

The npm convention is @{unscoped-name}-{version}.tgz@, so the file must:

\* end in @.tgz@ over a non-empty name (a bare @.tgz@ is not an artifact), and
\* have a basename of exactly @{unscoped-name}-{version}@ -- the unscoped name (the
  scope dropped, as npm names the file), a @\'-\'@, then a non-empty @version@.

A basename that does not begin with @{unscoped-name}-@ is addressing some other
package's artifact under this package's path -- a path-confusion attempt -- so it denies
rather than fabricating a coordinate. On a match the @version@ run is read by the total
'mkVersion' (an unparseable version still yields a coordinate, so a parser gap never drops
a real artifact), and the @file@ is preserved verbatim in the 'Filename'. The caller has
already passed @file@ through 'isSafeComponent'.

Exported so the coordinate parse -- the security-critical half of the artifact route -- is
asserted directly, rather than only through the router.
-}
tarballRoute :: PackageName -> Text -> Maybe (Version, Filename)
tarballRoute name file =
    case T.stripSuffix ".tgz" file >>= T.stripPrefix (unscopedName name <> "-") of
        Just version
            | not (T.null version) -> Just (mkVersion Npm version, Filename file)
        _ -> Nothing

{- | npm's routes as data for the __capability manifest__: the 'specOf' projection of the
same 'npmRoutes' the router runs, plus the synthetic deny-by-default catch-all. Projecting
the specs from the records is what keeps the documented paths, methods, and parameters from
drifting from what the server matches: there is one table, read two ways.
-}
npmRouteSpecs :: NonEmpty RouteSpec
npmRouteSpecs = unsupportedSpec :| map specOf npmRoutes

{- | The synthetic spec for the deny-by-default catch-all. It is not a route (it is the
/absence/ of a match), so it has no record in 'npmRoutes'; the manifest documents it
explicitly as the boundary, so a reader learns the limit from the manifest rather than from
an error reply.
-}
unsupportedSpec :: RouteSpec
unsupportedSpec = RouteSpec (RouteName "unsupported") GET [Param unsupportedParam] unsupportedDoc

unsupportedParam :: ParamSpec
unsupportedParam = ParamSpec "unsupportedPath" "Any path under this mount matched by none of the routes above."

{- The documentation of each npm route, carried on its pattern above. It lives beside
the grammar it describes, so a route cannot be added without saying what it does and what
it answers with; the capability manifest renders these and holds no per-route knowledge
of its own. The body /shapes/ are named from the agnostic 'BodyDoc' vocabulary, which the
manifest maps to schemas, so nothing here knows what OpenAPI is. -}
packumentDoc :: RouteDoc
packumentDoc =
    RouteDoc
        "Fetch a package's metadata (packument)"
        "Returns Écluse's merged-and-filtered packument: versions merged across upstreams and \
        \gated, each `dist.tarball` rewritten to resolve back through this proxy. With no surviving \
        \version the status follows the most recoverable cause."
        Nothing
        [ ResponseDoc 200 "The synthesized packument." PackumentBody
        , ResponseDoc 403 "Every version was withheld by policy or admission, and none survived the merge." ErrorEnvelopeBody
        , ResponseDoc 404 "No such package upstream (a forwarded miss)." ErrorEnvelopeBody
        , ResponseDoc 500 "A permanent or internal inability to decide." ErrorEnvelopeBody
        , ResponseDoc 502 "A responding upstream returned a packument for a different package." ErrorEnvelopeBody
        , ResponseDoc 503 "A transient upstream or advisory condition; retry (see `Retry-After`)." ErrorEnvelopeBody
        ]

tarballDoc :: RouteDoc
tarballDoc =
    RouteDoc
        "Stream a package artifact (tarball)"
        "The artifact bytes are streamed verbatim with bounded memory; the manifest documents the \
        \media type and links out rather than re-specifying the upstream artifact protocol. The \
        \client verifies the bytes against the packument's preserved integrity digest."
        Nothing
        [ ResponseDoc 200 "The artifact bytes." ArtifactBody
        , ResponseDoc 403 "Refused by policy, or by admission (a missing or below-floor integrity digest)." ErrorEnvelopeBody
        , ResponseDoc 404 "The upstream did not have the artifact (a forwarded miss)." ErrorEnvelopeBody
        , ResponseDoc 500 "A permanent or internal inability to serve." ErrorEnvelopeBody
        , ResponseDoc 503 "A transient upstream condition; retry (see `Retry-After`)." ErrorEnvelopeBody
        ]

publishDoc :: RouteDoc
publishDoc =
    RouteDoc
        "Publish a first-party package"
        "Relays the publish document to the configured publication target after the anti-shadowing \
        \scope guard. Écluse keys the write on the route's package name, never the document's \
        \self-reported name."
        ( Just
            ( RequestDoc
                "The npm publish document (the version manifest plus the base64-encoded tarball in `_attachments`)."
                True
                PublishDocumentBody
            )
        )
        [ ResponseDoc 201 "The publication target accepted the package (its response is relayed)." NoBody
        , ResponseDoc 403 "The package name is outside the configured publish scopes (anti-shadowing), or refused by policy." ErrorEnvelopeBody
        , ResponseDoc 405 "Publishing is not configured (no publication target)." ErrorEnvelopeBody
        ]

pingDoc :: RouteDoc
pingDoc =
    RouteDoc
        "Liveness probe"
        "Answered locally with `200` and an empty object; `npm ping` checks the endpoint it talks \
        \to is up, so there is no reason to round-trip upstream."
        Nothing
        [ResponseDoc 200 "An empty object." EmptyObjectBody]

searchDoc :: RouteDoc
searchDoc =
    RouteDoc
        "Package search (not supported)"
        "Search is a first-class documented boundary: a discovery convenience, not an install path, \
        \so Écluse returns `501` and points to the public registry's website rather than scope-creeping \
        \a filtered or pass-through search."
        Nothing
        [ResponseDoc 501 "Not implemented: search is not supported." ErrorEnvelopeBody]

unsupportedDoc :: RouteDoc
unsupportedDoc =
    RouteDoc
        "Deny by default (unsupported path)"
        "Any request under this mount matched by none of the routes above is denied with `404` -- \
        \deny by default at the routing layer."
        Nothing
        [ResponseDoc 404 "Unrecognised path; deny by default." ErrorEnvelopeBody]
