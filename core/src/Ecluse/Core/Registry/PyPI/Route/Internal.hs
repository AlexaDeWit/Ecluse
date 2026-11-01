-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TupleSections #-}

{- | PyPI routes, response contracts and capture parsers.
Importing this internal module opts out of the public API's stability promise.
-}
module Ecluse.Core.Registry.PyPI.Route.Internal (
    -- * The mount's router
    pypiRouter,

    -- * The table, as data
    pypiRoutes,
    pypiRouteSpecs,

    -- * The served file URL (rendered from the route that claims it)
    distributionPath,

    -- * The capture values and leaf parsers
    PyPICap (..),
    takeProject,
    artifactCoordinate,
) where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Network.HTTP.Types (
    Method,
    ResponseHeaders,
    Status,
    status200,
    status304,
    status401,
    status403,
    status404,
    status405,
    status406,
    status500,
    status502,
    status503,
 )

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.PyPI.Project (FileCoordinate (fcVersion), canonicalName, fileCoordinate, isCanonicalName, projectName)
import Ecluse.Core.Registry.PyPI.Wire (simpleIndexMediaType)
import Ecluse.Core.Server.Context (
    MountRouter,
    ResponseAction (AnswerRefusal, RunPipeline),
    RouteAction (RouteAction),
 )
import Ecluse.Core.Server.Contract (
    BodySchema (SchemaDocumented, SchemaText),
    PassthroughBody (PassthroughBytes, PassthroughEmpty, PassthroughStream),
    PassthroughResponse,
    ResponseContract,
    ResponseValue,
    chooseContract,
    emptyContract,
    mediaContract,
    optionalBodyContract,
    passthroughContract,
    passthroughResponse,
    responseValue,
 )
import Ecluse.Core.Server.Path (Filename, mkFilename)
import Ecluse.Core.Server.Pipeline.Packument (PackumentReplies (packumentNotAcceptable), packumentAction)
import Ecluse.Core.Server.Pipeline.Packument.Response (MetadataResponse, metadataReplies)
import Ecluse.Core.Server.Pipeline.Publish (PublishReplies (..), servePublish)
import Ecluse.Core.Server.Pipeline.Sidecar (sidecarAction)
import Ecluse.Core.Server.Pipeline.Tarball (tarballAction)
import Ecluse.Core.Server.Pipeline.Tarball.Types (TarballReplies (..))
import Ecluse.Core.Server.Response (HelpMessage, Refusal, mkRefusal, refusalHelpBytes)
import Ecluse.Core.Server.Response.Summary (detailedRejections)
import Ecluse.Core.Server.Route (
    Capture (Capture),
    MediaNegotiation (AcceptsAnything, AcceptsOnly),
    MethodMatch (MethodPost, MethodRead),
    PatternSeg (SegCap, SegCapSuffix, SegLit),
    Route (Route),
    RouteName (RouteName),
    renderRoute,
    routerOf,
    safeSegment,
 )
import Ecluse.Core.Server.RouteDescription (RouteSpec, catchAllSpecs, specsOf, unsupportedPathParam)
import Ecluse.Core.Version (Version)

-- | Match the first applicable route, otherwise answer with the 404 fallback.
pypiRouter :: MountRouter
pypiRouter = routerOf pypiNotFound pypiRoutes

-- Refuse unmatched paths with 404.
pypiNotFound :: RouteAction
pypiNotFound = RouteAction unsupportedContract (AnswerRefusal (declaredRefusal "no route claims this path" []))

-- | PyPI's routes, in matching order.
pypiRoutes :: [Route PyPICap]
pypiRoutes =
    [ artifactRoute
    , simpleIndexRoute
    , sidecarRoute
    , uploadRoute
    ]

-- The companion route strips one suffix before applying the distribution coordinate parser.
sidecarRoute :: Route PyPICap
sidecarRoute =
    Route
        (RouteName "coreMetadata")
        MethodRead
        AcceptsAnything
        [SegLit "simple", SegCap capProject, SegCapSuffix capFile ".metadata"]
        buildSidecar
        "Stream a distribution's Core Metadata companion"
        "Relays an advertised companion after checking the parent file's origin and integrity. The resolver verifies the advertised companion digest."
        Nothing
        pypiArtifactContract

-- @GET \/simple\/{project}\/{file}@: a distribution file, streamed.
artifactRoute :: Route PyPICap
artifactRoute =
    Route
        (RouteName "distribution")
        MethodRead
        AcceptsAnything
        [SegLit "simple", SegCap capProject, SegCap capFile]
        buildArtifact
        "Stream a distribution file (wheel or sdist)"
        "The file's bytes are streamed verbatim with bounded memory, so the client verifies them \
        \against the `sha256` the served index advertised. `/simple/{project}/{file}` is the one \
        \spelling every rewritten file URL agrees with. Upstream statuses, headers, and media \
        \types are relayed transparently; a locally generated refusal is a bare status."
        Nothing
        pypiArtifactContract

-- @GET \/simple\/{project}@: the filtered PEP 691 JSON index.
simpleIndexRoute :: Route PyPICap
simpleIndexRoute =
    Route
        (RouteName "simpleIndex")
        MethodRead
        (AcceptsOnly (simpleIndexMediaType :| []) (notAcceptable []))
        [SegLit "simple", SegCap capProject]
        buildIndex
        "Fetch a project's Simple index"
        "Returns Écluse's merged-and-filtered PEP 691 JSON index: files merged across upstreams \
        \and gated, each file URL rewritten to resolve back through this proxy. \
        \`/simple/{project}/` and `/simple/{project}` both match, because the router strips a \
        \trailing empty segment. A non-canonical (non-PEP-503) project name matches no route and \
        \takes the `404`, with no redirect. A client that requires HTML and admits no JSON gets \
        \`406`: Écluse serves the JSON form alone. The index is gzipped for a client whose \
        \`Accept-Encoding` prefers gzip, unless `server.metadataGzip` is off."
        Nothing
        pypiIndexContract

-- The native upload URL has no project capture.
uploadRoute :: Route PyPICap
uploadRoute =
    Route
        (RouteName "upload")
        MethodPost
        AcceptsAnything
        [SegLit "legacy"]
        buildUpload
        "Upload a first-party distribution"
        "Relays a bounded multipart upload after authentication and first-party name validation. \
        \Only `:action=file_upload` is relayed: another action answers `501`, and a missing or repeated one answers `400`. \
        \The `content` part's file name must name the declared project at the declared `version`, or the upload answers `400`."
        Nothing
        pypiUploadContract

-- The named hand-authored schema the manifest holds for the index Écluse assembles.
simpleIndexSchema :: Text
simpleIndexSchema = "PyPISimpleIndex"

type PyPIIndexResponse = MetadataResponse (Maybe LByteString)

pypiIndexContract :: ResponseContract PyPIIndexResponse
pypiIndexContract =
    chooseContract
        (mediaContract status200 "The filtered Simple index." (SchemaDocumented simpleIndexMediaType simpleIndexSchema))
        ( chooseContract
            (emptyContract status304 "The client's validator matched the assembled index.")
            ( chooseContract
                (refusalContract status401 "Edge authentication failed.")
                ( chooseContract
                    (refusalContract status403 "Private access was refused.")
                    ( chooseContract
                        (refusalContract status404 "A project no configured upstream has, a first-party project the private upstream does not have, which is never fetched from the public upstream, a project whose upstreams list no release, or a project with no release that passed policy and admission.")
                        ( chooseContract
                            (refusalContract status406 "The request admits no representation this index serves: only the PEP 691 JSON form exists, in gzip or identity coding.")
                            ( chooseContract
                                (refusalContract status500 "A permanent or internal inability to decide.")
                                ( chooseContract
                                    (refusalContract status502 "An upstream reply was invalid or unusable, such as an index for a different project or one whose every file entry is malformed.")
                                    (refusalContract status503 "A transient upstream or advisory condition; retry (see `Retry-After`).")
                                )
                            )
                        )
                    )
                )
            )
        )

-- A refusal has no body unless operator help text is configured.
refusalContract :: Status -> Text -> ResponseContract (ResponseValue (Maybe LByteString))
refusalContract status description =
    optionalBodyContract status (description <> " The body is empty unless `server.helpMessage` is configured.") (SchemaText "text/plain")

pypiIndexReplies :: PackumentReplies PyPIIndexResponse
pypiIndexReplies = metadataReplies detailedRejections refusalHelpBytes

-- The router's @406@, for an @Accept@ that admits no JSON form, before any handler runs.
notAcceptable :: ResponseHeaders -> Maybe HelpMessage -> PyPIIndexResponse
notAcceptable headers help =
    packumentNotAcceptable pypiIndexReplies headers (mkRefusal help "no representation this index serves is acceptable")

-- Upstream-controlled artifact responses and local refusals share one open response contract.
pypiArtifactContract :: ResponseContract PassthroughResponse
pypiArtifactContract =
    passthroughContract
        "An upstream-controlled distribution response is relayed transparently. Local authentication, policy, availability, and internal failures answer a bare status under their own code. A private upstream reply that was invalid or unusable answers a local `502`."

pypiArtifactReplies :: TarballReplies PassthroughResponse
pypiArtifactReplies =
    TarballReplies
        { tarballError = \status headers refusal -> passthroughResponse status headers (artifactRefusalBody refusal)
        , tarballPrivateRefused = \_ -> tarballError pypiArtifactReplies status403
        , tarballStream = \status headers body -> passthroughResponse status headers (PassthroughStream body)
        , tarballEmpty = \status headers -> passthroughResponse status headers PassthroughEmpty
        }

pypiUploadContract :: ResponseContract PassthroughResponse
pypiUploadContract = passthroughContract "The publication target's status and raw bytes, or a local publication refusal, including the `501` for an action other than `file_upload`."

pypiPublishReplies :: PublishReplies PassthroughResponse
pypiPublishReplies =
    PublishReplies
        { publishAllowedMethods = "POST"
        , publishDisabledReply = \headers help -> passthroughResponse status405 (("Content-Type", "text/plain") : headers) (artifactRefusalBody (mkRefusal help "publishing is not enabled on this mount"))
        , publishRelayed = \status headers body -> passthroughResponse status (("Content-Type", "text/plain") : headers) (PassthroughBytes body)
        , publishError = \status headers message -> passthroughResponse status (("Content-Type", "text/plain") : headers) (PassthroughBytes (fromStrict (encodeUtf8 message)))
        }

buildUpload :: Method -> [PyPICap] -> Maybe (ResponseAction PassthroughResponse)
buildUpload _ = \case
    [] -> Just (RunPipeline (publishError pypiPublishReplies status500 [] "internal server error") (servePublish pypiPublishReplies Nothing))
    _ -> Nothing

unsupportedContract :: ResponseContract (ResponseValue (Maybe LByteString))
unsupportedContract = refusalContract status404 "Unrecognised path; deny by default."

-- Écluse's own wording stays off the wire: PyPI's refusal is a bare status, so the body is the
-- operator help message alone and nothing at all when none is configured.
helpBody :: ResponseHeaders -> Refusal -> ResponseValue (Maybe LByteString)
helpBody headers refusal = responseValue headers (refusalHelpBytes refusal)

declaredRefusal :: Text -> ResponseHeaders -> Maybe HelpMessage -> ResponseValue (Maybe LByteString)
declaredRefusal reason headers help = helpBody headers (mkRefusal help reason)

-- The artifact relay's refusal, which is the same body under the transparent-relay contract.
artifactRefusalBody :: Refusal -> PassthroughBody
artifactRefusalBody refusal = maybe PassthroughEmpty PassthroughBytes (refusalHelpBytes refusal)

-- @GET \/simple\/{project}@: a project unit is an index read.
buildIndex :: Method -> [PyPICap] -> Maybe (ResponseAction PyPIIndexResponse)
buildIndex method = \case
    [PyPIProject name] -> Just (packumentAction pypiIndexReplies method name)
    _ -> Nothing

-- 'artifactCoordinate' applies the cross-capture path-confusion check, so a file naming another
-- project falls through to the @404@ rather than having a coordinate fabricated for it.
buildArtifact :: Method -> [PyPICap] -> Maybe (ResponseAction PassthroughResponse)
buildArtifact method = \case
    [PyPIProject name, PyPIFile file] -> do
        (version, filename) <- artifactCoordinate name file
        pure (tarballAction pypiArtifactReplies method name version filename)
    _ -> Nothing

buildSidecar :: Method -> [PyPICap] -> Maybe (ResponseAction PassthroughResponse)
buildSidecar method = \case
    [PyPIProject name, PyPIFile file] -> do
        (version, filename) <- artifactCoordinate name file
        pure (sidecarAction pypiArtifactReplies method name version filename)
    _ -> Nothing

-- | Positional captures distinguish parsed projects from checked distribution filenames.
data PyPICap
    = PyPIProject PackageName
    | PyPIFile Text

-- Render project captures canonically so the parser can read them back.
renderCapture :: PyPICap -> [Text]
renderCapture = \case
    PyPIProject name -> [canonicalName name]
    PyPIFile file -> [file]

-- Accept canonical projects only. Non-canonical spellings receive 404 rather than redirects.
capProject :: Capture PyPICap
capProject =
    Capture
        "project"
        "The project name in PEP 503 canonical form, e.g. `zope-interface`."
        (fmap (first PyPIProject) . takeProject)
        renderCapture

-- The segment is checked here; 'artifactCoordinate' reads the release and archive form from it.
capFile :: Capture PyPICap
capFile =
    Capture
        "file"
        "The distribution file's on-the-wire name, e.g. `requests-2.34.2-py3-none-any.whl`."
        (safeSegment PyPIFile)
        renderCapture

-- | Reject project spellings that an index would redirect outside the matched route.
takeProject :: [Text] -> Maybe (PackageName, [Text])
takeProject = \case
    seg : rest | isCanonicalName seg -> (,rest) <$> rightToMaybe (projectName seg)
    _ -> Nothing

-- | Require the file's project identity to match the requested package.
artifactCoordinate :: PackageName -> Text -> Maybe (Version, Filename)
artifactCoordinate name file = do
    coordinate <- fileCoordinate name file
    (fcVersion coordinate,) <$> mkFilename file

-- | Render through the distribution route so generated URLs obey its capture rules.
distributionPath :: PackageName -> Text -> Maybe Text
distributionPath name file = T.intercalate "/" <$> renderRoute artifactRoute [PyPIProject name, PyPIFile file]

-- | Describe the live router and its deny-by-default catch-all for OpenAPI.
pypiRouteSpecs :: NonEmpty RouteSpec
pypiRouteSpecs =
    catchAllSpecs unsupportedContract unsupportedPathParam
        `NE.appendList` concatMap specsOf pypiRoutes
