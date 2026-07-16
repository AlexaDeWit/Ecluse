-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
-- TupleSections: local convenience for pairing a parsed name with its trailing
-- segments in 'takeScoped' ((,rest) / (,more)); see STYLE.md §2.
{-# LANGUAGE TupleSections #-}

{- | npm's route table: the list of routes an npm mount serves.

Each entry is one 'Ecluse.Core.Server.Route.Route' record, carrying its method condition,
its path template, what to /do/ when it matches, its prose, and the closed set of
'Ecluse.Core.Server.Contract.Outcome's it can emit. 'npmRouter' folds the list into the
mount's router (first match wins; no match is the deny-by-default @404@) and 'npmRouteSpecs'
projects the same list for the capability manifest, so the routed surface, the emitted
responses, and the documented ones are all readings of one declaration.

Each response body is a codec ('Ecluse.Core.Registry.Npm.Serve.npmErrorCodec' for a
denial) or a named hand-authored schema (the merged packument, the publish document), so
the wire body and the documented schema are one source. The package, artifact, and publish
routes name the shared data-plane handlers ("Ecluse.Core.Server.Pipeline"); the meta-routes
answer locally through their declared outcome.

A @PUT \/{pkg}@ is the npm __publish__ request, so the method is part of the match: a
@PUT@ over a bare-package path publishes, while a __read__ (@GET@, or its bodiless @HEAD@)
over the same path fetches the packument. Those three methods are the only ones the front
door answers; any other (@POST@, @DELETE@, …) matches no route and denies.

The model is __deny by default__. Three npm-specific facts shape the matching, all from
the protocol research (see @docs\/research\/reverse-engineering\/npm.md@ §2 and §7):

* __Reserved meta-routes (@\/-\/…@) are matched first.__ A real package name can never
  begin with @\'-\'@, so a leading @"-"@ segment is unambiguously a meta-route.

* __Scoped names arrive in two encodings.__ The path is percent-decoded before it reaches
  us, so a scoped name arrives either as one decoded segment (@\@scope\/pkg@) or as two
  (@\@scope@, @pkg@). Both are normalised to the same 'PackageName' here.

* __A tarball path is @\/{pkg}\/-\/{file}.tgz@.__ 'tarballRoute' is the npm-side parse of
  the artifact coordinate; a basename that does not match the package is a
  __path-confusion__ attempt and denies.

Mount dispatch, prefix-stripping, and the liveness\/readiness routes are handled in the
agnostic web layer (see @docs\/architecture\/web-layer.md@); this table only ever sees the
npm-native request.
-}
module Ecluse.Core.Registry.Npm.Route (
    -- * The mount's router and error surface
    npmRouter,
    npmNotFound,
    npmMountError,

    -- * The table, as data
    npmRoutes,
    npmRouteSpecs,

    -- * The leaf parsers (exported for their specs)
    takePackage,
    tarballRoute,
) where

import Autodocodec (JSONCodec, object, pureCodec)
import Data.Text qualified as T
import Network.HTTP.Types (
    Method,
    Status,
    methodHead,
    status200,
    status201,
    status403,
    status404,
    status405,
    status500,
    status501,
    status502,
    status503,
 )
import Network.HTTP.Types.Method (StdMethod (GET))

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName, mkScope, unscopedName)
import Ecluse.Core.Registry.Npm.Serve (NpmError (NpmError), npmError, npmErrorCodec)
import Ecluse.Core.Server.Context (MountError (MountError), MountRouter, RouteAction (AnswerLocally, RunPipeline))
import Ecluse.Core.Server.Contract (
    Answer,
    BodySchema (SchemaDocumented),
    Outcome (Outcome),
    OutcomeBody (DocumentedOutcome, EmptyOutcome, JsonOutcome, OpaqueOutcome),
    RequestSpec (RequestSpec),
    SomeOutcome (SomeOutcome),
    answerWith,
    encodeBody,
 )
import Ecluse.Core.Server.Path (Filename (Filename), isSafeComponent)
import Ecluse.Core.Server.Pipeline (headPackument, headTarball, servePackument, servePublish, serveTarball)
import Ecluse.Core.Server.Pipeline.Shared (jsonResponse)
import Ecluse.Core.Server.Route (
    Capture (Capture),
    MethodMatch (MethodPut, MethodRead),
    PatternSeg (SegCap, SegLit),
    Route (Route),
    RouteName (RouteName),
    routerOf,
 )
import Ecluse.Core.Server.RouteSpec (ParamSpec (ParamSpec), PathSeg (Param), RouteSpec (RouteSpec), specOf)
import Ecluse.Core.Version (Version, mkVersion)

{- | npm's mount router: the route table folded into the whole routing decision. The
first route that claims the request decides what is done with it; a request no route
claims is the deny-by-default @404@ ('npmNotFound') in npm's own error surface.
-}
npmRouter :: MountRouter
npmRouter = routerOf npmNotFound npmRoutes

{- | The deny-by-default @404@ 'Answer' for a path no route claims: npm's
@{"error": "not found"}@, answered through the declared @unsupported@ outcome so the
emitted body is the documented one.
-}
npmNotFound :: Answer
npmNotFound = answerWith [] unsupported404 (NpmError "not found")

{- | npm's renderer for the infrastructure error responses no declared outcome shapes: the
request perimeter's neutral @500@ on an escaped fault. It emits the same
'Ecluse.Core.Registry.Npm.Serve.NpmError' body as the routes' outcomes, so even these
share the one shape.
-}
npmMountError :: MountError
npmMountError = MountError (\status extra message -> jsonResponse status extra (encodeBody npmErrorCodec (NpmError message)))

{- | npm's routes, in matching order. The __structure__ is here; the security-critical
__leaf__ parsing stays in the named functions the captures and builders reference
('takePackage', 'tarballRoute'). Ordering follows npm's conventions: the reserved
meta-routes are literal and tried first.
-}
npmRoutes :: [Route NpmCap]
npmRoutes =
    [ Route
        (RouteName "ping")
        MethodRead
        [SegLit "-", SegLit "ping"]
        (answering pingAnswer)
        "Liveness probe"
        "Answered locally with `200` and an empty object; `npm ping` checks the endpoint it talks \
        \to is up, so there is no reason to round-trip upstream."
        Nothing
        [emptyObjectOutcome status200 "An empty object."]
    , Route
        (RouteName "search")
        MethodRead
        [SegLit "-", SegLit "v1", SegLit "search"]
        (answering searchAnswer)
        "Package search (not supported)"
        "Search is a first-class documented boundary: a discovery convenience, not an install path, \
        \so Écluse returns `501` and points to the public registry's website."
        Nothing
        [errorOutcome status501 "Not implemented: search is not supported."]
    , Route
        (RouteName "tarball")
        MethodRead
        [SegCap capPackage, SegLit "-", SegCap capFilename]
        buildTarball
        "Stream a package artifact (tarball)"
        "The artifact bytes are streamed verbatim with bounded memory; the client verifies the bytes \
        \against the packument's preserved integrity digest."
        Nothing
        [ opaqueOutcome status200 "The artifact bytes." octetStream
        , errorOutcome status403 "Refused by policy, or by admission (a missing or below-floor integrity digest)."
        , errorOutcome status404 "The upstream did not have the artifact (a forwarded miss)."
        , errorOutcome status500 "A permanent or internal inability to serve."
        , errorOutcome status503 "A transient upstream condition; retry (see `Retry-After`)."
        ]
    , Route
        (RouteName "packument")
        MethodRead
        [SegCap capPackage]
        buildPackument
        "Fetch a package's metadata (packument)"
        "Returns Écluse's merged-and-filtered packument: versions merged across upstreams and gated, \
        \each `dist.tarball` rewritten to resolve back through this proxy. With no surviving version \
        \the status follows the most recoverable cause."
        Nothing
        [ documentedOutcome status200 "The synthesized packument." synthesizedPackumentSchema
        , errorOutcome status403 "Every version was withheld by policy or admission, and none survived the merge."
        , errorOutcome status404 "No such package upstream (a forwarded miss)."
        , errorOutcome status500 "A permanent or internal inability to decide."
        , errorOutcome status502 "A responding upstream returned a packument for a different package."
        , errorOutcome status503 "A transient upstream or advisory condition; retry (see `Retry-After`)."
        ]
    , Route
        (RouteName "publish")
        MethodPut
        [SegCap capPackage]
        buildPublish
        "Publish a first-party package"
        "Relays the publish document to the configured publication target after the anti-shadowing \
        \scope guard. Écluse keys the write on the route's package name, never the document's \
        \self-reported name."
        (Just publishRequest)
        [ noBodyOutcome status201 "The publication target accepted the package (its response is relayed)."
        , errorOutcome status403 "The package name is outside the configured publish scopes (anti-shadowing), or refused by policy."
        , errorOutcome status405 "Publishing is not configured (no publication target)."
        ]
    ]

-- The octet-stream media type an artifact is documented and served under.
octetStream :: ByteString
octetStream = "application/octet-stream"

-- The named hand-authored schemas the manifest holds for the documents Écluse builds
-- imperatively rather than round-tripping through a codec.
synthesizedPackumentSchema :: Text
synthesizedPackumentSchema = "SynthesizedPackument"

publishDocumentSchema :: Text
publishDocumentSchema = "PublishDocument"

-- The publish document a @PUT@ accepts, documented by its hand-authored schema.
publishRequest :: RequestSpec
publishRequest =
    RequestSpec
        "The npm publish document (the version manifest plus the base64-encoded tarball in `_attachments`)."
        True
        (SchemaDocumented publishDocumentSchema)

-- An error outcome carrying npm's @{"error": …}@ body, documented from its codec.
errorOutcome :: Status -> Text -> SomeOutcome
errorOutcome status doc = SomeOutcome (Outcome status doc (JsonOutcome npmErrorCodec))

-- A JSON outcome whose body Écluse builds imperatively, documented by a named schema.
documentedOutcome :: Status -> Text -> Text -> SomeOutcome
documentedOutcome status doc schema = SomeOutcome (Outcome status doc (DocumentedOutcome schema))

-- An opaque (streamed) outcome of the given media type.
opaqueOutcome :: Status -> Text -> ByteString -> SomeOutcome
opaqueOutcome status doc media = SomeOutcome (Outcome status doc (OpaqueOutcome media))

-- A no-body outcome (a relayed publish acceptance).
noBodyOutcome :: Status -> Text -> SomeOutcome
noBodyOutcome status doc = SomeOutcome (Outcome status doc (EmptyOutcome :: OutcomeBody ()))

-- A @200 {}@ liveness outcome: an empty JSON object, from a codec that renders @{}@.
emptyObjectOutcome :: Status -> Text -> SomeOutcome
emptyObjectOutcome status doc = SomeOutcome (Outcome status doc (JsonOutcome emptyObjectCodec))

-- The empty-object codec: encodes @()@ to @{}@ and documents an empty object schema.
emptyObjectCodec :: JSONCodec ()
emptyObjectCodec = object "EmptyObject" (pureCodec ())

-- The @unsupported@ outcome the deny-by-default @404@ answers through.
unsupported404 :: Outcome NpmError
unsupported404 = Outcome status404 "Unrecognised path; deny by default." (JsonOutcome npmErrorCodec)

-- A route answered locally, whatever the method and captures (the literal meta-routes).
answering :: Answer -> Method -> [NpmCap] -> Maybe RouteAction
answering answer _method _captures = Just (AnswerLocally answer)

-- @\/-\/ping@: answered locally with @200 {}@.
pingAnswer :: Answer
pingAnswer = answerWith [] (Outcome status200 "" (JsonOutcome emptyObjectCodec)) ()

-- @\/-\/v1\/search@: a @501@ pointer, in npm's error surface.
searchAnswer :: Answer
searchAnswer =
    answerWith
        []
        (Outcome status501 "" (JsonOutcome npmErrorCodec))
        (npmError Nothing "search is not supported by this proxy; use the public registry's website to discover packages")

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
__cross-capture__ path-confusion check and reads the version; a mismatched name yields
'Nothing', so the route falls through to the @404@ rather than being fabricated into a
coordinate. A @HEAD@ takes the head-mode handler, which probes the upstream bodiless. -}
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
@\'\/\'@, @\'\\\\\'@, or control character). 'mkScope'\/'mkPackageName' do no
validation, so this boundary is where such names are rejected rather than passed
downstream into an interpolated upstream URL.
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
        (scope, base)
            | not (T.null base) ->
                (,rest) <$> scopedName scope (T.drop 1 base)
        _ -> case rest of
            (base : more) -> (,more) <$> scopedName (T.drop 1 seg) base
            _ -> Nothing

-- A scoped name is usable only when both halves are safe components. The
-- leading '@' is already stripped from both arguments, so a degenerate or
-- hostile name ('@/pkg', '@scope/', '@scope/a/b', '@../pkg') is rejected here.
scopedName :: Text -> Text -> Maybe PackageName
scopedName scope base
    | isSafeComponent scope && isSafeComponent base =
        Just (mkPackageName Npm (Just (mkScope scope)) base)
    | otherwise = Nothing

{- | Parse an npm tarball-slot @file@ into the artifact coordinate it names for @name@:
the 'Version' and the verbatim 'Filename'. 'Nothing' denies it.

The npm convention is @{unscoped-name}-{version}.tgz@, so the file must end in @.tgz@ over a
non-empty name and have a basename of exactly @{unscoped-name}-{version}@. A basename that
does not begin with @{unscoped-name}-@ is a path-confusion attempt and denies. On a match
the @version@ run is read by the total 'mkVersion', and the @file@ is preserved verbatim.

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
same 'npmRoutes' the router runs, plus the synthetic deny-by-default catch-all.
-}
npmRouteSpecs :: NonEmpty RouteSpec
npmRouteSpecs = unsupportedSpec :| map specOf npmRoutes

{- | The synthetic spec for the deny-by-default catch-all. It is not a route (it is the
/absence/ of a match), so it has no record in 'npmRoutes'; the manifest documents it
explicitly as the boundary.
-}
unsupportedSpec :: RouteSpec
unsupportedSpec =
    RouteSpec
        (RouteName "unsupported")
        GET
        [Param unsupportedParam]
        "Deny by default (unsupported path)"
        "Any request under this mount matched by none of the routes above is denied with `404` -- \
        \deny by default at the routing layer."
        Nothing
        [SomeOutcome unsupported404]

unsupportedParam :: ParamSpec
unsupportedParam = ParamSpec "unsupportedPath" "Any path under this mount matched by none of the routes above."
