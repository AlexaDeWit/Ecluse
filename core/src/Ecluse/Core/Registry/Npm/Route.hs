-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
-- TupleSections: local convenience for pairing a parsed name with its trailing
-- segments in 'takeScoped' ((,rest) / (,more)); see STYLE.md §2.
{-# LANGUAGE TupleSections #-}

{- | npm's route table: the list of routes an npm mount serves.

Each entry is one 'Ecluse.Core.Server.Route.Route' record, carrying its method condition,
its path template, what to /do/ when it matches, its prose, and the
'Ecluse.Core.Server.Contract.ResponseContract' that admits every response it can emit.
'npmRouter' folds the list into the
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

* __A tarball path is @\/{pkg}\/-\/{file}.tgz@.__ 'tarballCoordinate' is the npm-side parse of
  the artifact coordinate; a basename that does not match the package is a
  __path-confusion__ attempt and denies.

Mount dispatch, prefix-stripping, and the liveness\/readiness routes are handled in the
agnostic web layer (see @docs\/architecture\/web-layer.md@); this table only ever sees the
npm-native request.
-}
module Ecluse.Core.Registry.Npm.Route (
    -- * The mount's router and fallback action
    npmRouter,
    npmNotFound,

    -- * Route-scoped pipeline contracts (exported for direct pipeline specs)
    npmPackumentContract,
    npmPackumentReplies,
    npmTarballContract,
    npmTarballReplies,
    npmPublishContract,
    npmPublishReplies,

    -- * The table, as data
    npmRoutes,
    npmRouteSpecs,

    -- * The leaf parsers (exported for their specs)
    takePackage,
    tarballCoordinate,
) where

import Autodocodec (JSONCodec, object, pureCodec)
import Data.Text qualified as T
import Network.HTTP.Types (
    Method,
    hContentType,
    methodHead,
    status200,
    status304,
    status401,
    status403,
    status404,
    status500,
    status501,
    status502,
    status503,
 )
import Network.HTTP.Types.Method (StdMethod (GET, HEAD))

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName, mkScope, unscopedName)
import Ecluse.Core.Registry.Npm.Serve (NpmError (NpmError), npmError, npmErrorCodec)
import Ecluse.Core.Server.Context (
    MountRouter,
    ResponseAction (AnswerLocally, RunPipeline),
    RouteAction (RouteAction),
 )
import Ecluse.Core.Server.Contract (
    BodySchema (SchemaDocumented),
    PassthroughBody (PassthroughBytes, PassthroughEmpty, PassthroughStream),
    PassthroughResponse,
    RequestSpec (RequestSpec),
    ResponseChoice (FirstResponse, SecondResponse),
    ResponseContract,
    ResponseValue,
    VariableResponse,
    bodilessContract,
    chooseContract,
    documentedJsonContract,
    emptyContract,
    encodeBody,
    jsonContract,
    passthroughContract,
    passthroughResponse,
    responseDocs,
    responseValue,
    variableOpaqueContract,
    variableResponse,
 )
import Ecluse.Core.Server.Path (Filename (Filename), isSafeComponent)
import Ecluse.Core.Server.Pipeline.Packument (PackumentReplies (..), headPackument, servePackument)
import Ecluse.Core.Server.Pipeline.Publish (PublishReplies (..), servePublish)
import Ecluse.Core.Server.Pipeline.Tarball (TarballReplies (..), headTarball, serveTarball)
import Ecluse.Core.Server.Route (
    Capture (Capture),
    MethodMatch (MethodPut, MethodRead),
    PatternSeg (SegCap, SegLit),
    Route (Route),
    RouteName (RouteName),
    routerOf,
 )
import Ecluse.Core.Server.RouteSpec (ParamSpec (ParamSpec), PathSeg (Param), RouteSpec (RouteSpec), specsOf)
import Ecluse.Core.Version (Version, mkVersion)

{- | npm's mount router: the route table folded into the whole routing decision. The
first route that claims the request decides what is done with it; a request no route
claims is the deny-by-default @404@ ('npmNotFound') in npm's own error surface.
-}
npmRouter :: MountRouter
npmRouter = routerOf npmNotFound npmRoutes

{- | The deny-by-default @404@ action for a path no route claims. Its local value and
manifest entry are two interpretations of 'unsupportedContract'.
-}
npmNotFound :: RouteAction
npmNotFound =
    RouteAction
        unsupportedContract
        (AnswerLocally (responseValue [] (NpmError "not found")))

{- | npm's routes, in matching order: one named value each, aggregated here. The
__structure__ of each is in its own definition; the security-critical __leaf__ parsing
stays in the named functions the captures and builders reference ('takePackage',
'tarballCoordinate'). Ordering follows npm's conventions: the reserved meta-routes are
literal and tried first.
-}
npmRoutes :: [Route NpmCap]
npmRoutes = [pingRoute, searchRoute, tarballRoute, packumentRoute, publishRoute]

-- @GET \/-\/ping@: a liveness probe, answered locally with @200 {}@.
pingRoute :: Route NpmCap
pingRoute =
    Route
        (RouteName "ping")
        MethodRead
        [SegLit "-", SegLit "ping"]
        (answering pingAnswer)
        "Liveness probe"
        "Answered locally with `200` and an empty object; `npm ping` checks the endpoint it talks \
        \to is up, so there is no reason to round-trip upstream."
        Nothing
        pingContract

-- @GET \/-\/v1\/search@: a documented @501@ boundary; search is not proxied.
searchRoute :: Route NpmCap
searchRoute =
    Route
        (RouteName "search")
        MethodRead
        [SegLit "-", SegLit "v1", SegLit "search"]
        (answering searchAnswer)
        "Package search (not supported)"
        "Search is a first-class documented boundary: a discovery convenience, not an install path, \
        \so Écluse returns `501` and points to the public registry's website."
        Nothing
        searchContract

-- @GET \/{package}\/-\/{filename}@: a package artifact, streamed.
tarballRoute :: Route NpmCap
tarballRoute =
    Route
        (RouteName "tarball")
        MethodRead
        [SegCap capPackage, SegLit "-", SegCap capFilename]
        buildTarball
        "Stream a package artifact (tarball)"
        "The artifact bytes are streamed verbatim with bounded memory; the client verifies the bytes \
        \against the packument's preserved integrity digest. Upstream statuses, headers, and media \
        \types are relayed transparently; locally generated refusals use npm's JSON error shape."
        Nothing
        npmTarballContract

-- @GET \/{package}@: the merged, gated packument.
packumentRoute :: Route NpmCap
packumentRoute =
    Route
        (RouteName "packument")
        MethodRead
        [SegCap capPackage]
        buildPackument
        "Fetch a package's metadata (packument)"
        "Returns Écluse's merged-and-filtered packument: versions merged across upstreams and gated, \
        \each `dist.tarball` rewritten to resolve back through this proxy. With no surviving version \
        \the status follows the most recoverable cause."
        Nothing
        npmPackumentContract

-- @PUT \/{package}@: a first-party publish, relayed after the anti-shadowing guard.
publishRoute :: Route NpmCap
publishRoute =
    Route
        (RouteName "publish")
        MethodPut
        [SegCap capPackage]
        buildPublish
        "Publish a first-party package"
        "Relays the publish document to the configured publication target after the anti-shadowing \
        \scope guard. Écluse keys the write on the route's package name, never the document's \
        \self-reported name. The target's status and JSON-labelled bytes are relayed transparently."
        (Just publishRequest)
        npmPublishContract

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

-- The empty-object codec: encodes @()@ to @{}@ and documents an empty object schema.
emptyObjectCodec :: JSONCodec ()
emptyObjectCodec = object "EmptyObject" (pureCodec ())

pingContract :: ResponseContract (ResponseValue ())
pingContract = jsonContract status200 "An empty object." emptyObjectCodec

searchContract :: ResponseContract (ResponseValue NpmError)
searchContract = jsonContract status501 "Not implemented: search is not supported." npmErrorCodec

unsupportedContract :: ResponseContract (ResponseValue NpmError)
unsupportedContract = jsonContract status404 "Unrecognised path; deny by default." npmErrorCodec

{- | The closed packument response sum. Every constructor is introduced by the matching
leaf in 'npmPackumentContract'; 'npmPackumentReplies' is the only interface the pipeline
receives for selecting one.
-}
type NpmPackumentResponse =
    ResponseChoice
        (ResponseValue LByteString)
        ( ResponseChoice
            (ResponseValue ())
            ( ResponseChoice
                (ResponseValue NpmError)
                ( ResponseChoice
                    (ResponseValue NpmError)
                    ( ResponseChoice
                        (ResponseValue NpmError)
                        (ResponseChoice (ResponseValue NpmError) (ResponseValue NpmError))
                    )
                )
            )
        )

npmPackumentContract :: ResponseContract NpmPackumentResponse
npmPackumentContract =
    chooseContract
        (documentedJsonContract status200 "The synthesized packument." synthesizedPackumentSchema)
        ( chooseContract
            (emptyContract status304 "The client's validator matched the synthesized packument.")
            ( chooseContract
                (jsonContract status401 "Edge authentication failed." npmErrorCodec)
                ( chooseContract
                    (jsonContract status403 "Every version was withheld by policy or admission, and none survived the merge." npmErrorCodec)
                    ( chooseContract
                        (jsonContract status500 "A permanent or internal inability to decide." npmErrorCodec)
                        ( chooseContract
                            (jsonContract status502 "A responding upstream returned a packument for a different package." npmErrorCodec)
                            (jsonContract status503 "A transient upstream or advisory condition; retry (see `Retry-After`)." npmErrorCodec)
                        )
                    )
                )
            )
        )

npmPackumentReplies :: PackumentReplies NpmPackumentResponse
npmPackumentReplies =
    PackumentReplies
        { packumentOk = \headers body -> FirstResponse (responseValue headers body)
        , packumentNotModified = \headers -> SecondResponse (FirstResponse (responseValue headers ()))
        , packumentUnauthorised = \headers message -> SecondResponse (SecondResponse (FirstResponse (responseValue headers (NpmError message))))
        , packumentForbidden = \headers message -> SecondResponse (SecondResponse (SecondResponse (FirstResponse (responseValue headers (NpmError message)))))
        , packumentInternal = \headers message -> SecondResponse (SecondResponse (SecondResponse (SecondResponse (FirstResponse (responseValue headers (NpmError message))))))
        , packumentBadGateway = \headers message -> SecondResponse (SecondResponse (SecondResponse (SecondResponse (SecondResponse (FirstResponse (responseValue headers (NpmError message)))))))
        , packumentUnavailable = \headers message -> SecondResponse (SecondResponse (SecondResponse (SecondResponse (SecondResponse (SecondResponse (responseValue headers (NpmError message)))))))
        }

{- | The tarball is deliberately an open relay: any upstream status, headers, media type,
and bytes can be forwarded. The one @default@ document is therefore more accurate than a
closed list that the upstream can escape.
-}
npmTarballContract :: ResponseContract PassthroughResponse
npmTarballContract =
    passthroughContract
        "An upstream-controlled artifact response is relayed transparently. Local authentication, policy, availability, and internal failures use npm's JSON error body under their corresponding status."

npmTarballReplies :: TarballReplies PassthroughResponse
npmTarballReplies =
    TarballReplies
        { tarballError = \status headers message ->
            passthroughResponse
                status
                ((hContentType, "application/json") : headers)
                (PassthroughBytes (encodeBody npmErrorCodec (NpmError message)))
        , tarballStream = \status headers body -> passthroughResponse status headers (PassthroughStream body)
        , tarballEmpty = \status headers -> passthroughResponse status headers PassthroughEmpty
        }

type NpmPublishResponse = VariableResponse LByteString

npmPublishContract :: ResponseContract NpmPublishResponse
npmPublishContract =
    variableOpaqueContract
        "application/json"
        "The publication target's status and JSON-labelled response bytes are relayed. Local authentication, scope, configuration, transport, and internal failures use npm's JSON error body."

npmPublishReplies :: PublishReplies NpmPublishResponse
npmPublishReplies =
    PublishReplies
        { publishRelayed = variableResponse
        , publishError = \status headers message ->
            variableResponse status headers (encodeBody npmErrorCodec (NpmError message))
        }

-- A route answered locally, whatever the method and captures (the literal meta-routes).
answering :: response -> Method -> [NpmCap] -> Maybe (ResponseAction response)
answering answer _method _captures = Just (AnswerLocally answer)

-- @\/-\/ping@: answered locally with @200 {}@.
pingAnswer :: ResponseValue ()
pingAnswer = responseValue [] ()

-- @\/-\/v1\/search@: a @501@ pointer, in npm's error surface.
searchAnswer :: ResponseValue NpmError
searchAnswer =
    responseValue [] (npmError Nothing "search is not supported by this proxy; use the public registry's website to discover packages")

{- @GET \/{package}@: a bare package unit is a packument read. A @HEAD@ takes the
head-mode handler, which runs the identical gating and merge but withholds the body. -}
buildPackument :: Method -> [NpmCap] -> Maybe (ResponseAction NpmPackumentResponse)
buildPackument method = \case
    [NpmPackage name]
        | isHead method -> Just (RunPipeline perimeterFallback (headPackument npmPackumentReplies name))
        | otherwise -> Just (RunPipeline perimeterFallback (servePackument npmPackumentReplies name))
    _ -> Nothing
  where
    perimeterFallback = packumentInternal npmPackumentReplies [] "internal server error"

{- @PUT \/{package}@: a bare package unit under the write method is a publish. -}
buildPublish :: Method -> [NpmCap] -> Maybe (ResponseAction NpmPublishResponse)
buildPublish _method = \case
    [NpmPackage name] ->
        Just
            ( RunPipeline
                (publishError npmPublishReplies status500 [] "internal server error")
                (servePublish npmPublishReplies name)
            )
    _ -> Nothing

{- @GET \/{package}\/-\/{filename}@: an artifact read. 'tarballCoordinate' applies the
__cross-capture__ path-confusion check and reads the version; a mismatched name yields
'Nothing', so the route falls through to the @404@ rather than being fabricated into a
coordinate. A @HEAD@ takes the head-mode handler, which probes the upstream bodiless. -}
buildTarball :: Method -> [NpmCap] -> Maybe (ResponseAction PassthroughResponse)
buildTarball method = \case
    [NpmPackage name, NpmFilename file] -> do
        (version, filename) <- tarballCoordinate name file
        pure $
            if isHead method
                then RunPipeline perimeterFallback (headTarball npmTarballReplies name version filename)
                else RunPipeline perimeterFallback (serveTarball npmTarballReplies name version filename)
    _ -> Nothing
  where
    perimeterFallback = tarballError npmTarballReplies status500 [] "internal server error"

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
'tarballCoordinate''s, applied in 'buildTarball'.
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
tarballCoordinate :: PackageName -> Text -> Maybe (Version, Filename)
tarballCoordinate name file =
    case T.stripSuffix ".tgz" file >>= T.stripPrefix (unscopedName name <> "-") of
        Just version
            | not (T.null version) -> Just (mkVersion Npm version, Filename file)
        _ -> Nothing

{- | npm's routes as data for the __capability manifest__: the 'specsOf' projection of the
same 'npmRoutes' the router runs, plus the synthetic deny-by-default catch-all.
-}
npmRouteSpecs :: NonEmpty RouteSpec
npmRouteSpecs = unsupportedGetSpec :| (unsupportedHeadSpec : concatMap specsOf npmRoutes)

{- | The synthetic spec for the deny-by-default catch-all. It is not a route (it is the
/absence/ of a match), so it has no record in 'npmRoutes'; the manifest documents it
explicitly as the boundary.
-}
unsupportedGetSpec :: RouteSpec
unsupportedGetSpec =
    RouteSpec
        (RouteName "unsupported")
        GET
        [Param unsupportedParam]
        "Deny by default (unsupported path)"
        "Any request under this mount matched by none of the routes above is denied with `404` -- \
        \deny by default at the routing layer."
        Nothing
        (responseDocs unsupportedContract)

unsupportedHeadSpec :: RouteSpec
unsupportedHeadSpec =
    RouteSpec
        (RouteName "unsupported.head")
        HEAD
        [Param unsupportedParam]
        "Deny by default (unsupported path)"
        "Any HEAD request under this mount matched by none of the routes above is denied with `404` \
        \and no response body."
        Nothing
        (responseDocs (bodilessContract unsupportedContract))

unsupportedParam :: ParamSpec
unsupportedParam = ParamSpec "unsupportedPath" "Any path under this mount matched by none of the routes above."
