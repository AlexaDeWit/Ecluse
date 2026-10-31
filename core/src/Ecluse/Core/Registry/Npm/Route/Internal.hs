-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TupleSections #-}

{- | npm routes, response contracts and capture parsers.
Importing this internal module opts out of the public API's stability promise.
-}
module Ecluse.Core.Registry.Npm.Route.Internal (
    -- * The mount's router and fallback action
    npmRouter,
    npmNotFound,

    -- * Route-scoped pipeline contracts
    npmPackumentContract,
    npmPackumentReplies,
    npmTarballContract,
    npmTarballReplies,

    -- * The table, as data
    npmRoutes,
    npmRouteSpecs,

    -- * The served artifact URL (rendered from the route that claims it)
    tarballPath,

    -- * The capture values and leaf parsers
    NpmCap (..),
    takePackage,
    tarballCoordinate,
) where

import Autodocodec (JSONCodec, object, pureCodec)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Network.HTTP.Types (
    Method,
    hContentType,
    status200,
    status304,
    status401,
    status403,
    status404,
    status405,
    status406,
    status500,
    status501,
    status502,
    status503,
 )

import Ecluse.Core.Package (PackageName, pkgNamespace, renderPackageName, renderScope, unscopedName)
import Ecluse.Core.Registry.Npm.Project (projectName, tarballCoordinate)
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
    ResponseContract,
    ResponseValue,
    VariableResponse,
    chooseContract,
    documentedJsonContract,
    emptyContract,
    encodeBody,
    jsonContract,
    passthroughContract,
    passthroughResponse,
    responseValue,
    variableOpaqueContract,
    variableResponse,
 )
import Ecluse.Core.Server.Pipeline.Packument (PackumentReplies, packumentAction)
import Ecluse.Core.Server.Pipeline.Packument.Response (MetadataResponse, metadataReplies)
import Ecluse.Core.Server.Pipeline.Publish (PublishReplies (..), servePublish)
import Ecluse.Core.Server.Pipeline.Tarball (tarballAction)
import Ecluse.Core.Server.Pipeline.Tarball.Types (TarballReplies (..))
import Ecluse.Core.Server.Response (renderRefusal)
import Ecluse.Core.Server.Response.Summary (summarisedRejections)
import Ecluse.Core.Server.Route (
    Capture (Capture),
    MediaNegotiation (AcceptsAnything),
    MethodMatch (MethodDelete, MethodPut, MethodRead),
    PatternSeg (SegCap, SegLit),
    Route (Route),
    RouteName (RouteName),
    answering,
    renderRoute,
    routerOf,
    safeSegment,
 )
import Ecluse.Core.Server.RouteDescription (RouteSpec, catchAllSpecs, specsOf, unsupportedPathParam)

-- | Match the first applicable route, otherwise answer 'npmNotFound'.
npmRouter :: MountRouter
npmRouter = routerOf npmNotFound npmRoutes

-- | The deny-by-default @404@ action for a path no route claims.
npmNotFound :: RouteAction
npmNotFound =
    RouteAction
        unsupportedContract
        (AnswerLocally (responseValue [] (NpmError "not found")))

-- | Try reserved meta-routes before package captures.
npmRoutes :: [Route NpmCap]
npmRoutes =
    [ pingRoute
    , searchRoute
    , distTagListRoute
    , distTagSetRoute
    , distTagRemoveRoute
    , tarballRoute
    , packumentRoute
    , publishRoute
    ]

-- @GET \/-\/ping@: a liveness probe, answered locally with @200 {}@.
pingRoute :: Route NpmCap
pingRoute =
    Route
        (RouteName "ping")
        MethodRead
        AcceptsAnything
        [SegLit "-", SegLit "ping"]
        (answering pingAnswer)
        "Liveness probe"
        "Answered locally with `200` and an empty object; `npm ping` checks the endpoint it talks \
        \to is up, so there is no reason to round-trip upstream."
        Nothing
        pingContract

-- @GET \/-\/v1\/search@: a documented @501@ boundary. Search is not proxied.
searchRoute :: Route NpmCap
searchRoute =
    Route
        (RouteName "search")
        MethodRead
        AcceptsAnything
        [SegLit "-", SegLit "v1", SegLit "search"]
        (answering searchAnswer)
        "Package search (not supported)"
        "Search is a first-class documented boundary: a discovery convenience, not an install path, \
        \so Écluse returns `501` and points to the public registry's website."
        Nothing
        searchContract

-- @GET \/-\/package\/{package}\/dist-tags@: a documented @501@ boundary.
distTagListRoute :: Route NpmCap
distTagListRoute =
    Route
        (RouteName "distTagList")
        MethodRead
        AcceptsAnything
        [SegLit "-", SegLit "package", SegCap capPackage, SegLit "dist-tags"]
        (answering distTagAnswer)
        "List a package's dist-tags (not supported)"
        "A dist-tag is a mutable named pointer, which Écluse does not implement, so it returns \
        \`501` rather than the `404` an unrouted path takes. The merged packument already carries \
        \the reconciled `dist-tags` map, so a client reads a package's tags from its metadata."
        Nothing
        distTagContract

-- One path template for the tagged dist-tag routes, so the write and the removal cannot drift.
distTagTagSegs :: [PatternSeg NpmCap]
distTagTagSegs = [SegLit "-", SegLit "package", SegCap capPackage, SegLit "dist-tags", SegCap capTag]

-- @PUT \/-\/package\/{package}\/dist-tags\/{tag}@: a documented @501@ boundary.
distTagSetRoute :: Route NpmCap
distTagSetRoute =
    Route
        (RouteName "distTagSet")
        MethodPut
        AcceptsAnything
        distTagTagSegs
        (answering distTagAnswer)
        "Set a package's dist-tag (not supported)"
        "Écluse writes no mutable named pointer, so it returns `501` rather than the `404` an \
        \unrouted path takes. The publication target owns a package's tags, and a publisher sets \
        \them there."
        Nothing
        distTagContract

-- @DELETE \/-\/package\/{package}\/dist-tags\/{tag}@: a documented @501@ boundary.
distTagRemoveRoute :: Route NpmCap
distTagRemoveRoute =
    Route
        (RouteName "distTagRemove")
        MethodDelete
        AcceptsAnything
        distTagTagSegs
        (answering distTagAnswer)
        "Remove a package's dist-tag (not supported)"
        "Écluse holds no mutable named pointer to remove, so it returns `501` rather than the `404` \
        \an unrouted path takes. The publication target owns a package's tags, and a publisher \
        \removes them there."
        Nothing
        distTagContract

-- @GET \/{package}\/-\/{filename}@: a package artifact, streamed.
tarballRoute :: Route NpmCap
tarballRoute =
    Route
        (RouteName "tarball")
        MethodRead
        AcceptsAnything
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
        AcceptsAnything
        [SegCap capPackage]
        buildPackument
        "Fetch a package's metadata (packument)"
        "Returns Écluse's merged-and-filtered packument: versions merged across upstreams and gated, \
        \each `dist.tarball` rewritten to resolve back through this proxy. With no surviving version \
        \the status follows the most recoverable cause. The packument is gzipped for a client whose \
        \`Accept-Encoding` prefers gzip, unless `server.metadataGzip` is off."
        Nothing
        npmPackumentContract

-- @PUT \/{package}@: a first-party publish, relayed after the anti-shadowing guard.
publishRoute :: Route NpmCap
publishRoute =
    Route
        (RouteName "publish")
        MethodPut
        AcceptsAnything
        [SegCap capPackage]
        buildPublish
        "Publish a first-party package"
        "Relays the publish document to the configured publication target after the anti-shadowing \
        \scope guard. Écluse keys the write on the route's package name, never the document's \
        \self-reported name. Only a document with a non-empty `_attachments` object, exactly one \
        \version, and every `dist-tags` value on that version is relayed: any other body, such as the \
        \one `npm deprecate` sends, answers `501`. A body that repeats a key, is not a JSON object, or \
        \files its version under a key other than its `version` field answers `400`. So does a version \
        \whose `dependencies`, `optionalDependencies` or `peerDependencies` name a git source or a \
        \remote URL, which a client would fetch from outside Écluse. The target's status and \
        \JSON-labelled bytes are relayed transparently."
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
        (SchemaDocumented "application/json" publishDocumentSchema)

-- The empty-object codec: encodes @()@ to @{}@ and documents an empty object schema.
emptyObjectCodec :: JSONCodec ()
emptyObjectCodec = object "EmptyObject" (pureCodec ())

pingContract :: ResponseContract (ResponseValue ())
pingContract = jsonContract status200 "An empty object." emptyObjectCodec

searchContract :: ResponseContract (ResponseValue NpmError)
searchContract = jsonContract status501 "Not implemented: search is not supported." npmErrorCodec

-- No dist-tag operation is implemented, so the read, the write, and the removal share one contract.
distTagContract :: ResponseContract (ResponseValue NpmError)
distTagContract = jsonContract status501 "Not implemented: dist-tags are not supported." npmErrorCodec

unsupportedContract :: ResponseContract (ResponseValue NpmError)
unsupportedContract = jsonContract status404 "Unrecognised path; deny by default." npmErrorCodec

type NpmPackumentResponse = MetadataResponse NpmError

npmPackumentContract :: ResponseContract NpmPackumentResponse
npmPackumentContract =
    chooseContract
        (documentedJsonContract status200 "The synthesized packument." synthesizedPackumentSchema)
        ( chooseContract
            (emptyContract status304 "The client's validator matched the synthesized packument.")
            ( chooseContract
                (jsonContract status401 "Edge authentication failed." npmErrorCodec)
                ( chooseContract
                    (jsonContract status403 "Private access was refused, or no version survived policy and admission." npmErrorCodec)
                    ( chooseContract
                        (jsonContract status404 "A name neither the private nor the public upstream has, or a first-party name the private upstream does not have, which is never fetched from the public upstream." npmErrorCodec)
                        ( chooseContract
                            (jsonContract status406 "The request's `Accept-Encoding` excludes both gzip and identity coding." npmErrorCodec)
                            ( chooseContract
                                (jsonContract status500 "A permanent or internal inability to decide." npmErrorCodec)
                                ( chooseContract
                                    (jsonContract status502 "An upstream reply was invalid or unusable, such as a packument for a different package." npmErrorCodec)
                                    (jsonContract status503 "A transient upstream or advisory condition; retry (see `Retry-After`)." npmErrorCodec)
                                )
                            )
                        )
                    )
                )
            )
        )

npmPackumentReplies :: PackumentReplies NpmPackumentResponse
npmPackumentReplies = metadataReplies summarisedRejections (NpmError . renderRefusal)

npmTarballContract :: ResponseContract PassthroughResponse
npmTarballContract =
    passthroughContract
        "An upstream-controlled artifact response is relayed transparently. Local authentication, policy, availability, and internal failures use npm's JSON error body under their corresponding status. A private upstream reply that was invalid or unusable answers a local `502`."

npmTarballReplies :: TarballReplies PassthroughResponse
npmTarballReplies =
    TarballReplies
        { tarballError = \status headers message ->
            passthroughResponse
                status
                ((hContentType, "application/json") : headers)
                (PassthroughBytes (encodeBody npmErrorCodec (NpmError (renderRefusal message))))
        , tarballPrivateRefused = \_ -> tarballError npmTarballReplies status403
        , tarballStream = \status headers body -> passthroughResponse status headers (PassthroughStream body)
        , tarballEmpty = \status headers -> passthroughResponse status headers PassthroughEmpty
        }

type NpmPublishResponse = VariableResponse LByteString

npmPublishContract :: ResponseContract NpmPublishResponse
npmPublishContract =
    variableOpaqueContract
        "application/json"
        "The publication target's status and JSON-labelled response bytes are relayed. Local authentication, scope, unsupported-write, configuration, transport, and internal failures use npm's JSON error body."

npmPublishReplies :: PublishReplies NpmPublishResponse
npmPublishReplies =
    PublishReplies
        { publishAllowedMethods = "GET, HEAD"
        , publishDisabledReply = \headers _ -> publishError npmPublishReplies status405 headers "publishing is not enabled on this proxy (no publication target is configured); publish directly to the registry you intend to publish to"
        , publishRelayed = variableResponse
        , publishError = \status headers message ->
            variableResponse status headers (encodeBody npmErrorCodec (NpmError message))
        }

-- @\/-\/ping@: answered locally with @200 {}@.
pingAnswer :: ResponseValue ()
pingAnswer = responseValue [] ()

-- @\/-\/v1\/search@: a @501@ pointer, in npm's error surface.
searchAnswer :: ResponseValue NpmError
searchAnswer =
    responseValue [] (npmError Nothing "search is not supported by this proxy; use the public registry's website to discover packages")

-- The dist-tag routes' @501@ pointer, in npm's error surface.
distTagAnswer :: ResponseValue NpmError
distTagAnswer =
    responseValue [] (npmError Nothing "dist-tags are not supported by this proxy; a package's tags are in the metadata document it serves, and a publisher sets them at the publication target")

-- @GET \/{package}@: a bare package unit is a packument read.
buildPackument :: Method -> [NpmCap] -> Maybe (ResponseAction NpmPackumentResponse)
buildPackument method = \case
    [NpmPackage name] -> Just (packumentAction npmPackumentReplies method name)
    _ -> Nothing

-- @PUT \/{package}@: a bare package unit under the write method is a publish.
buildPublish :: Method -> [NpmCap] -> Maybe (ResponseAction NpmPublishResponse)
buildPublish _method = \case
    [NpmPackage name] ->
        Just
            ( RunPipeline
                (publishError npmPublishReplies status500 [] "internal server error")
                (servePublish npmPublishReplies (Just name))
            )
    _ -> Nothing

buildTarball :: Method -> [NpmCap] -> Maybe (ResponseAction PassthroughResponse)
buildTarball method = \case
    [NpmPackage name, NpmFilename file] -> do
        (version, filename) <- tarballCoordinate name file
        pure (tarballAction npmTarballReplies method name version filename)
    _ -> Nothing

-- | Positional captures distinguish parsed package identities from checked path segments.
data NpmCap
    = NpmPackage PackageName
    | NpmFilename Text
    | NpmTag Text

capPackage :: Capture NpmCap
capPackage =
    Capture
        "package"
        "The package name, URL-encoded; a scoped name is `@scope%2Fname`."
        ( \case
            "-" : _ -> Nothing
            segs -> fmap (first NpmPackage) (takePackage segs)
        )
        renderPackage

capFilename :: Capture NpmCap
capFilename =
    Capture
        "filename"
        "The artifact's on-the-wire file name, e.g. `lodash-4.17.21.tgz`."
        (safeSegment NpmFilename)
        renderSegment

capTag :: Capture NpmCap
capTag =
    Capture
        "tag"
        "The dist-tag name, e.g. `latest`."
        (safeSegment NpmTag)
        renderSegment

{- The segments a package capture claims, written back out. A scoped name takes the two-segment
encoding, which 'takeScoped' reads back into the one wire name 'projectName' owns. -}
renderPackage :: NpmCap -> [Text]
renderPackage = \case
    NpmPackage name -> case pkgNamespace name of
        Just scope -> [renderScope scope, unscopedName name]
        Nothing -> [renderPackageName name]
    other -> renderSegment other

-- The one segment a raw safety-checked capture claims, written back out.
renderSegment :: NpmCap -> [Text]
renderSegment = \case
    NpmPackage name -> [renderPackageName name]
    NpmFilename file -> [file]
    NpmTag tag -> [tag]

{- Peel the leading package unit off a path, returning its 'PackageName' and the remaining
segments. The route parses no name of its own: 'projectName' owns the npm name grammar. -}
takePackage :: [Text] -> Maybe (PackageName, [Text])
takePackage [] = Nothing
takePackage (seg : rest)
    | T.isPrefixOf "@" seg = takeScoped seg rest
    | otherwise = (,rest) <$> rightToMaybe (projectName seg)

{- Peel a scoped package unit off the leading @\@@ segment. Both wire encodings, one decoded
segment or two, join into the one wire name 'projectName' reads. -}
takeScoped :: Text -> [Text] -> Maybe (PackageName, [Text])
takeScoped seg rest
    | T.isInfixOf "/" seg = (,rest) <$> rightToMaybe (projectName seg)
    | otherwise = case rest of
        (base : more) -> (,more) <$> rightToMaybe (projectName (seg <> "/" <> base))
        [] -> Nothing

-- | Render through the artifact route so generated URLs obey its capture rules.
tarballPath :: PackageName -> Text -> Maybe Text
tarballPath name file = T.intercalate "/" <$> renderRoute tarballRoute [NpmPackage name, NpmFilename file]

-- | Describe the live router and its deny-by-default catch-all for OpenAPI.
npmRouteSpecs :: NonEmpty RouteSpec
npmRouteSpecs =
    catchAllSpecs unsupportedContract unsupportedPathParam
        `NE.appendList` concatMap specsOf npmRoutes
