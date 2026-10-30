-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native Cargo routes for the one sparse registry that replaces crates.io.
module Ecluse.Core.Registry.Cargo.Route.Internal (
    cargoRouter,
    cargoRoutes,
    cargoRouteSpecs,
    takeIndexPath,
    artifactCoordinate,
    indexContract,
    indexReplies,
    artifactContract,
    artifactReplies,
) where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Network.HTTP.Types (ResponseHeaders, Status, status200, status304, status401, status403, status404, status405, status406, status500, status502, status503)
import Network.Wai (Request, ResponseReceived)

import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Package (PackageName, pkgEcosystem, renderPackageName)
import Ecluse.Core.Registry.Cargo.Discovery (renderCargoDiscovery)
import Ecluse.Core.Registry.Cargo.Project (cargoArchiveFilename, projectName)
import Ecluse.Core.Registry.Cargo.Request (cargoIndexPath)
import Ecluse.Core.Server.Context (
    Handler,
    MountBinding (bindingPackumentDeps, bindingPublishDeps),
    MountRouter,
    PackumentDeps (..),
    RequestCtx (..),
    ResponseAction (..),
    RouteAction (RouteAction),
    pdPrivateBase,
 )
import Ecluse.Core.Server.Contract (
    BodySchema (SchemaOpaque, SchemaText),
    PassthroughBody (..),
    PassthroughResponse,
    RequestSpec (..),
    ResponseChoice (FirstResponse, SecondResponse),
    ResponseContract,
    ResponseValue,
    chooseContract,
    emptyContract,
    mediaContract,
    optionalBodyContract,
    passthroughContract,
    passthroughResponse,
    plainTextRefusal,
    responseValue,
 )
import Ecluse.Core.Server.Path (Filename, mkFilename)
import Ecluse.Core.Server.Pipeline.Packument (PackumentReplies (..), packumentAction)
import Ecluse.Core.Server.Pipeline.Packument.Response (MetadataResponse, keepPrivateUnauthorised, metadataReplies)
import Ecluse.Core.Server.Pipeline.Publish (PublishReplies (..), servePublish)
import Ecluse.Core.Server.Pipeline.Shared (edgeTokenMatches, forwardedCredential)
import Ecluse.Core.Server.Pipeline.Tarball (tarballAction)
import Ecluse.Core.Server.Pipeline.Tarball.Types (TarballReplies (..))
import Ecluse.Core.Server.Response (Refusal, refusalHelpBytes, renderRefusal)
import Ecluse.Core.Server.Response.Summary (detailedRejections)
import Ecluse.Core.Server.Route (Capture (..), MediaNegotiation (AcceptsAnything), MethodMatch (MethodPut, MethodRead), PatternSeg (SegCap, SegLit), Route (Route), RouteName (RouteName), routerOf, safeSegment)
import Ecluse.Core.Server.RouteDescription (RouteSpec, catchAllSpecs, specsOf, unsupportedPathParam)
import Ecluse.Core.Version (Version, mkVersion, renderVersion, versionKey, versionKeyIn)

-- | Route a request to the registry's discovery, index, archive and publication handlers.
cargoRouter :: MountRouter
cargoRouter = routerOf notFound cargoRoutes

data CargoCap = CargoProject PackageName | CargoVersion Version | CargoFile Text

-- | Discovery, sparse records, archives and publication, with shared capture validation.
cargoRoutes :: [Route CargoCap]
cargoRoutes =
    [ Route
        (RouteName "discovery")
        MethodRead
        AcceptsAnything
        [SegLit "config.json"]
        (\_ captures -> discoveryAction <$ guard (null captures))
        "Discover the local sparse registry"
        "Answered locally after edge authentication. Returns local archive locations, the publication API when publication is configured, and whether clients must authenticate."
        Nothing
        discoveryContract
    , Route
        (RouteName "archive")
        MethodRead
        AcceptsAnything
        [SegLit "crates", SegCap projectCapture, SegCap versionCapture, SegCap fileCapture]
        ( \method -> \case
            [CargoProject name, CargoVersion version, CargoFile file] -> tarballAction artifactReplies method name version <$> artifactCoordinate name version file
            _ -> Nothing
        )
        "Fetch a crate archive"
        "The explicit crate, version and logical filename must agree. A first-party crate never falls back to a public source."
        Nothing
        artifactContract
    , Route
        (RouteName "publish")
        MethodPut
        AcceptsAnything
        (map SegLit ["api", "v1", "crates", "new"])
        (\_ captures -> RunPipeline (publishError publishReplies status500 [] "internal server error") (servePublish publishReplies Nothing) <$ guard (null captures))
        "Publish a first-party crate"
        "Relays the validated native upload under the configured publication authority. Other registry management operations have no route."
        (Just (RequestSpec "The native metadata and crate archive, with uint32 length framing." True (SchemaOpaque "application/octet-stream")))
        publishContract
    , Route
        (RouteName "index")
        MethodRead
        AcceptsAnything
        [SegCap indexCapture]
        (\method -> \case [CargoProject name] -> Just (packumentAction indexReplies method name); _ -> Nothing)
        "Read a crate's sparse index"
        "Returns caller-authorised private records merged with admitted public records. A first-party crate reads only the private origin."
        Nothing
        indexContract
    ]

-- | Require the native logical filename without inferring a version from hyphenated text.
artifactCoordinate :: PackageName -> Version -> Text -> Maybe Filename
artifactCoordinate name version file = do
    guard (pkgEcosystem name == Cargo)
    _ <- rightToMaybe (projectName (renderPackageName name))
    _ <- versionKeyIn Cargo version
    guard (file == cargoArchiveFilename (renderPackageName name) version)
    mkFilename file

-- | Accept only the exact lower-case native sparse lookup path.
takeIndexPath :: [Text] -> Maybe (PackageName, [Text])
takeIndexPath segments = do
    raw <- listToMaybe (reverse segments)
    name <- rightToMaybe (projectName raw)
    guard (segments == T.splitOn "/" (cargoIndexPath name))
    pure (name, [])

indexCapture :: Capture CargoCap
indexCapture = Capture "indexPath" "The lower-case native sparse path, including its name prefix." (fmap (first CargoProject) . takeIndexPath) (\case CargoProject name -> T.splitOn "/" (cargoIndexPath name); _ -> [])

projectCapture :: Capture CargoCap
projectCapture = Capture "crate" "The crate's exact published spelling." consume (\case CargoProject name -> [renderPackageName name]; _ -> [])
  where
    consume = \case
        raw : rest -> (\name -> (CargoProject name, rest)) <$> rightToMaybe (projectName raw)
        _ -> Nothing

versionCapture :: Capture CargoCap
versionCapture = Capture "version" "The raw native Cargo version." consume (\case CargoVersion version -> [renderVersion version]; _ -> [])
  where
    consume = \case
        raw : rest -> let version = mkVersion Cargo raw in (CargoVersion version, rest) <$ versionKey version
        _ -> Nothing

fileCapture :: Capture CargoCap
fileCapture = Capture "file" "The exact crate-version.crate logical filename." (safeSegment CargoFile) (\case CargoFile file -> [file]; _ -> [])

type IndexResponse = MetadataResponse (Maybe LByteString)

-- | Responses for admitted sparse metadata and local refusals.
indexContract :: ResponseContract IndexResponse
indexContract =
    chooseContract (mediaContract status200 "Admitted native sparse records." (SchemaOpaque "text/plain")) $
        chooseContract (emptyContract status304 "The admitted records match the supplied validator.") $
            chooseContract (refusalContract status401 "Authentication is required.") $
                chooseContract (refusalContract status403 "Access or admission was refused.") $
                    chooseContract (refusalContract status404 "No package is available in this registry.") $
                        chooseContract (refusalContract status406 "The requested encoding is unavailable.") $
                            chooseContract (refusalContract status500 "The registry could not decide.") $
                                chooseContract
                                    (refusalContract status502 "An origin reply was invalid or unusable, such as a different identity.")
                                    (refusalContract status503 "The registry is temporarily unavailable.")

refusalContract :: Status -> Text -> ResponseContract (ResponseValue (Maybe LByteString))
refusalContract status description = optionalBodyContract status description (SchemaText "text/plain")

-- | Render public policy and evaluation diagnostics, keeping access refusals and misses quiet.
indexReplies :: PackumentReplies IndexResponse
indexReplies =
    (metadataReplies detailedRejections renderedRefusal)
        { packumentUnauthorised = packumentUnauthorised quiet
        , packumentPrivateRefused = keepPrivateUnauthorised quiet
        , packumentNotFound = packumentNotFound quiet
        }
  where
    plain = metadataReplies detailedRejections refusalHelpBytes
    quiet = plain{packumentUnauthorised = \headers -> packumentUnauthorised plain (challenge <> headers)}

renderedRefusal :: Refusal -> Maybe LByteString
renderedRefusal = Just . fromStrict . encodeUtf8 . renderRefusal

-- | Native archives share streamed responses and local refusal status codes.
artifactContract :: ResponseContract PassthroughResponse
artifactContract = passthroughContract "Relays the selected archive. Authentication, admission, integrity and availability failures return a local refusal."

-- | Render public archive diagnostics while access refusals and missing archives keep quiet bodies.
artifactReplies :: TarballReplies PassthroughResponse
artifactReplies =
    TarballReplies
        { tarballError = \status -> archiveRefusal (status == status401 || status == status404) status
        , tarballPrivateRefused = \code -> archiveRefusal True (if code == 401 then status401 else status403)
        , tarballStream = \status headers body -> passthroughResponse status headers (PassthroughStream body)
        , tarballEmpty = \status headers -> passthroughResponse status headers PassthroughEmpty
        }

-- A quiet refusal carries only configured help, and every 401 carries the native challenge.
archiveRefusal :: Bool -> Status -> ResponseHeaders -> Refusal -> PassthroughResponse
archiveRefusal quiet status headers refusal =
    passthroughResponse
        status
        (if status == status401 then challenge <> headers else headers)
        (maybe PassthroughEmpty PassthroughBytes ((if quiet then refusalHelpBytes else renderedRefusal) refusal))

type DiscoveryResponse = ResponseChoice (ResponseValue LByteString) (ResponseChoice (ResponseValue (Maybe LByteString)) (ResponseValue (Maybe LByteString)))

discoveryContract :: ResponseContract DiscoveryResponse
discoveryContract =
    chooseContract (mediaContract status200 "Local sparse registry discovery." (SchemaOpaque "application/json")) $
        chooseContract
            (refusalContract status401 "The client must retry with a Cargo credential provider.")
            (refusalContract status503 "Discovery could not be answered.")

discoveryAction :: ResponseAction DiscoveryResponse
discoveryAction = RunPipeline (discoveryError status503 [] "registry discovery is unavailable") serveDiscovery

{- Discovery reads no upstream. Authentication is required whenever a private upstream or the
edge token is configured, so a private outage surfaces at the first index read instead. -}
serveDiscovery :: Request -> (DiscoveryResponse -> IO ResponseReceived) -> Handler ResponseReceived
serveDiscovery request respond = do
    mount <- asks ctxMount
    let deps = bindingPackumentDeps mount
    liftIO . respond $
        if edgeTokenMatches (pdInboundToken deps) (forwardedCredential mount request)
            then
                FirstResponse . responseValue [("Cache-Control", "no-store")] . fromStrict $
                    renderCargoDiscovery
                        (pdMountBaseUrl deps <> "/crates/{crate}/{version}/{crate}-{version}.crate")
                        (pdMountBaseUrl deps <$ guard (isJust (bindingPublishDeps mount)))
                        (isJust (pdInboundToken deps) || isJust (pdPrivateBase deps))
            else discoveryError status401 challenge "authentication required"

discoveryError :: Status -> ResponseHeaders -> Text -> DiscoveryResponse
discoveryError status headers message =
    let value = responseValue (("Cache-Control", "no-store") : headers) (Just (fromStrict (encodeUtf8 message)))
     in SecondResponse (if status == status401 then FirstResponse value else SecondResponse value)

challenge :: ResponseHeaders
challenge = [("WWW-Authenticate", "Cargo")]

publishContract :: ResponseContract PassthroughResponse
publishContract = passthroughContract "Relays the publication target's response after first-party authority and native upload validation. Disabled publication returns 405."

publishReplies :: PublishReplies PassthroughResponse
publishReplies =
    PublishReplies
        { publishAllowedMethods = "PUT"
        , publishDisabledReply = \headers _ -> plainTextRefusal status405 headers "publishing is not enabled on this view"
        , publishRelayed = \status headers body -> passthroughResponse status (if status == status401 then challenge <> headers else headers) (PassthroughBytes body)
        , publishError = \status headers -> plainTextRefusal status (if status == status401 then challenge <> headers else headers)
        }

notFound :: RouteAction
notFound = RouteAction notFoundContract (AnswerLocally (responseValue [] Nothing))

notFoundContract :: ResponseContract (ResponseValue (Maybe LByteString))
notFoundContract = refusalContract status404 "No native route claims this path."

-- | Describe the native routes from their live route table.
cargoRouteSpecs :: NonEmpty RouteSpec
cargoRouteSpecs = catchAllSpecs notFoundContract unsupportedPathParam `NE.appendList` concatMap specsOf cargoRoutes
