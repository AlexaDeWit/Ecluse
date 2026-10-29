-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native Cargo routes bind the private view before any pipeline action runs.
module Ecluse.Core.Registry.Cargo.Route.Internal (
    cargoRouter,
    cargoRoutes,
    cargoRouteSpecs,
    CargoView (..),
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
import UnliftIO (tryAny)

import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Package (PackageName, pkgEcosystem, renderPackageName)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataChargeFactors))
import Ecluse.Core.Registry.Cargo.Discovery (cargoDiscovery, discoveryAuthRequired, renderCargoDiscovery)
import Ecluse.Core.Registry.Cargo.Project (projectName)
import Ecluse.Core.Registry.Cargo.Request (cargoIndexPath)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataAuthorisationFailure))
import Ecluse.Core.Registry.Origin (OriginClient (ocChargeFullRead))
import Ecluse.Core.Server.Admission.Budget (scaleCharge)
import Ecluse.Core.Server.Admission.Meter (charge)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (cfFullReadPermille))
import Ecluse.Core.Server.Context (
    Handler,
    MountBinding (bindingPackumentDeps, bindingPublishDeps),
    MountRouter,
    PackumentDeps (..),
    RequestCtx (..),
    ResponseAction (..),
    RouteAction (RouteAction),
    ServeRuntime (..),
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
    responseValue,
 )
import Ecluse.Core.Server.Path (Filename, mkFilename)
import Ecluse.Core.Server.Pipeline.Origin (mountOrigin)
import Ecluse.Core.Server.Pipeline.Packument (PackumentReplies (..), packumentAction)
import Ecluse.Core.Server.Pipeline.Packument.Response (MetadataResponse, metadataReplies)
import Ecluse.Core.Server.Pipeline.Publish (PublishReplies (..), servePublish)
import Ecluse.Core.Server.Pipeline.Shared (edgeTokenMatches, forwardedCredential, shedMessage, withMetadataAdmission)
import Ecluse.Core.Server.Pipeline.Tarball (tarballAction)
import Ecluse.Core.Server.Pipeline.Tarball.Types (TarballReplies (..))
import Ecluse.Core.Server.Response (Refusal, refusalHelp, renderRefusal)
import Ecluse.Core.Server.Response.Summary (detailedRejections)
import Ecluse.Core.Server.Route (Capture (..), MediaNegotiation (AcceptsAnything), MethodMatch (MethodPut, MethodRead), PatternSeg (SegCap, SegLit), Route (Route), RouteName (RouteName), routerOf, safeSegment)
import Ecluse.Core.Server.RouteDescription (RouteSpec, catchAllSpecs, specsOf, unsupportedPathParam)
import Ecluse.Core.Version (Version, mkVersion, renderVersion, versionKey, versionKeyIn)

-- | Bind a private alternate view without adding another ecosystem mount or worker policy.
cargoRouter :: Maybe PackumentDeps -> MountRouter
cargoRouter privateView method headers segments = case segments of
    "private" : _ -> case privateView of
        Nothing -> routerOf notFound [] method headers segments
        Just deps -> bindView deps (routerOf notFound cargoRoutes method headers segments)
    _ -> routerOf notFound cargoRoutes method headers segments

bindView :: PackumentDeps -> RouteAction -> RouteAction
bindView deps (RouteAction contract action) = RouteAction contract $ case action of
    RunPipeline fallback run -> RunPipeline fallback (\request respond -> local bind (run request respond))
    other -> other
  where
    bind ctx = ctx{ctxMount = (ctxMount ctx){bindingPackumentDeps = deps}}

-- | The route's view selects whether local refusals include diagnostic reasons.
data CargoView = PublicReplacement | PrivateRegistry
    deriving stock (Eq)

data CargoCap = CargoProject PackageName | CargoVersion Version | CargoFile Text

-- | The primary and private native paths share capture validation and response contracts.
cargoRoutes :: [Route CargoCap]
cargoRoutes = concatMap viewRoutes [PublicReplacement, PrivateRegistry]

viewRoutes :: CargoView -> [Route CargoCap]
viewRoutes view =
    [ Route
        (routeName view "discovery")
        MethodRead
        AcceptsAnything
        (prefix view <> [SegLit "config.json"])
        (\_ captures -> discoveryAction view <$ guard (null captures))
        "Discover the local sparse registry"
        "Returns local archive locations. Trusted discovery runs under the caller's credential after edge authentication. Public discovery omits the publication API."
        Nothing
        discoveryContract
    , artifactRoute view
    ]
        <> [ Route
                (routeName view "publish")
                MethodPut
                AcceptsAnything
                (prefix view <> map SegLit ["api", "v1", "crates", "new"])
                (\_ captures -> RunPipeline (publishError publishReplies status500 [] "internal server error") (servePublish publishReplies Nothing) <$ guard (null captures))
                "Publish a first-party crate"
                "Relays the validated native upload under the configured publication authority. Other registry management operations have no route."
                (Just (RequestSpec "The native metadata and crate archive, with uint32 length framing." True (SchemaOpaque "application/octet-stream")))
                publishContract
           | view == PrivateRegistry
           ]
        <> [ Route
                (routeName view "index")
                MethodRead
                AcceptsAnything
                (prefix view <> [SegCap indexCapture])
                (\method -> \case [CargoProject name] -> Just (packumentAction (indexReplies view) method name); _ -> Nothing)
                "Read a crate's sparse index"
                "Returns admitted native records. The public replacement excludes first-party names and unmanaged target records. The private alternate reads only the caller-authorised private origin."
                Nothing
                indexContract
           ]

prefix :: CargoView -> [PatternSeg CargoCap]
prefix PublicReplacement = []
prefix PrivateRegistry = [SegLit "private"]

routeName :: CargoView -> Text -> RouteName
routeName view suffix = RouteName ((if view == PublicReplacement then "public." else "private.") <> suffix)

artifactRoute :: CargoView -> Route CargoCap
artifactRoute view =
    Route
        (routeName view "archive")
        MethodRead
        AcceptsAnything
        (prefix view <> [SegLit "crates", SegCap projectCapture, SegCap versionCapture, SegCap fileCapture])
        ( \method -> \case
            [CargoProject name, CargoVersion version, CargoFile file] -> tarballAction (artifactReplies view) method name version <$> artifactCoordinate name version file
            _ -> Nothing
        )
        "Fetch a crate archive"
        "The explicit crate, version and logical filename must agree. Private reads never fall back to a public source."
        Nothing
        artifactContract

-- | Require the native logical filename without inferring a version from hyphenated text.
artifactCoordinate :: PackageName -> Version -> Text -> Maybe Filename
artifactCoordinate name version file = do
    guard (pkgEcosystem name == Cargo)
    _ <- rightToMaybe (projectName (renderPackageName name))
    _ <- versionKeyIn Cargo version
    guard (file == renderPackageName name <> "-" <> renderVersion version <> ".crate")
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
                    chooseContract (refusalContract status404 "No package is available in this view.") $
                        chooseContract (refusalContract status406 "The requested encoding is unavailable.") $
                            chooseContract (refusalContract status500 "The registry could not decide.") $
                                chooseContract
                                    (refusalContract status502 "The origin returned a different identity.")
                                    (refusalContract status503 "The registry is temporarily unavailable.")

refusalContract :: Status -> Text -> ResponseContract (ResponseValue (Maybe LByteString))
refusalContract status description = optionalBodyContract status description (SchemaText "text/plain")

-- | Render public policy and evaluation diagnostics, keeping access refusals quiet.
indexReplies :: CargoView -> PackumentReplies IndexResponse
indexReplies view =
    base
        { packumentUnauthorised = \headers -> packumentUnauthorised quiet (challenge <> headers)
        , packumentPrivateRefused = \code ->
            if code == 401
                then packumentUnauthorised (indexReplies PrivateRegistry)
                else packumentForbidden quiet
        , packumentNotFound = packumentNotFound quiet
        }
  where
    base = metadataReplies detailedRejections (refusalBytes view)
    quiet = metadataReplies detailedRejections helpBytes

refusalBytes :: CargoView -> Refusal -> Maybe LByteString
refusalBytes PublicReplacement = Just . fromStrict . encodeUtf8 . renderRefusal
refusalBytes PrivateRegistry = helpBytes

helpBytes :: Refusal -> Maybe LByteString
helpBytes = fmap (fromStrict . encodeUtf8) . refusalHelp

-- | Native archives share streamed responses and local refusal status codes.
artifactContract :: ResponseContract PassthroughResponse
artifactContract = passthroughContract "Relays the selected archive. Authentication, admission, integrity and availability failures return a local refusal."

-- | Render public archive diagnostics while access refusals and missing archives retain quiet bodies.
artifactReplies :: CargoView -> TarballReplies PassthroughResponse
artifactReplies view =
    TarballReplies
        { tarballError = \status headers refusal ->
            let bodyView = if status == status401 || status == status404 then PrivateRegistry else view
             in passthroughResponse status (if status == status401 then challenge <> headers else headers) (maybe PassthroughEmpty PassthroughBytes (refusalBytes bodyView refusal))
        , tarballPrivateRefused = \code -> tarballError (artifactReplies PrivateRegistry) (if code == 401 then status401 else status403)
        , tarballStream = \status headers body -> passthroughResponse status headers (PassthroughStream body)
        , tarballEmpty = \status headers -> passthroughResponse status headers PassthroughEmpty
        }

type DiscoveryResponse = ResponseChoice (ResponseValue LByteString) (ResponseChoice (ResponseValue (Maybe LByteString)) (ResponseChoice (ResponseValue (Maybe LByteString)) (ResponseValue (Maybe LByteString))))

discoveryContract :: ResponseContract DiscoveryResponse
discoveryContract =
    chooseContract (mediaContract status200 "Local sparse registry discovery." (SchemaOpaque "application/json")) $
        chooseContract (refusalContract status401 "The client must retry with a Cargo credential provider.") $
            chooseContract
                (refusalContract status403 "The trusted registry refused access.")
                (refusalContract status503 "Trusted discovery is unavailable.")

discoveryAction :: CargoView -> ResponseAction DiscoveryResponse
discoveryAction view = RunPipeline (discoveryError status503 [] "registry discovery is unavailable") (serveDiscovery view)

serveDiscovery :: CargoView -> Request -> (DiscoveryResponse -> IO ResponseReceived) -> Handler ResponseReceived
serveDiscovery view request respond = do
    mount <- asks ctxMount
    rt <- asks ctxRuntime
    let deps = bindingPackumentDeps mount
        token = forwardedCredential mount request
        reply = liftIO . respond
        discoveryReply auth =
            FirstResponse . responseValue [("Cache-Control", "no-store")] . fromStrict $
                renderCargoDiscovery
                    (pdMountBaseUrl deps <> "/crates/{crate}/{version}/{crate}-{version}.crate")
                    (pdMountBaseUrl deps <$ guard (view == PrivateRegistry && isJust (bindingPublishDeps mount)))
                    (isJust (pdInboundToken deps) || auth)
    if not (edgeTokenMatches (pdInboundToken deps) token)
        then reply (discoveryError status401 challenge "authentication required")
        else case pdPrivateBase deps of
            Nothing
                | view == PublicReplacement -> reply (discoveryReply False)
                | otherwise -> reply (discoveryError status503 [] "private registry discovery is unavailable")
            Just base ->
                withMetadataAdmission
                    rt
                    (\headers -> reply (discoveryError status503 headers shedMessage))
                    ( \ticket -> do
                        let origin = (mountOrigin deps (srPrivateClient rt) base token){ocChargeFullRead = charge ticket . scaleCharge (cfFullReadPermille (metadataChargeFactors (pdMetadata deps)))}
                        tryAny (liftIO (cargoDiscovery origin)) <&> \case
                            Right (Right discovery) -> discoveryReply (discoveryAuthRequired discovery)
                            Right (Left (MetadataAuthorisationFailure 401)) -> discoveryError status401 challenge "authentication required"
                            Right (Left (MetadataAuthorisationFailure 403)) -> discoveryError status403 [] "private registry refused access"
                            _ -> discoveryError status503 [] "registry discovery is unavailable"
                    )
                    reply

discoveryError :: Status -> ResponseHeaders -> Text -> DiscoveryResponse
discoveryError status headers message =
    let value = responseValue (("Cache-Control", "no-store") : headers) (Just (fromStrict (encodeUtf8 message)))
     in SecondResponse $
            if status == status401
                then FirstResponse value
                else SecondResponse $ if status == status403 then FirstResponse value else SecondResponse value

localError :: Status -> ResponseHeaders -> Text -> PassthroughResponse
localError status headers message = passthroughResponse status (("Content-Type", "text/plain") : ("Cache-Control", "no-store") : headers) (PassthroughBytes (fromStrict (encodeUtf8 message)))

challenge :: ResponseHeaders
challenge = [("WWW-Authenticate", "Cargo")]

publishContract :: ResponseContract PassthroughResponse
publishContract = passthroughContract "Relays the publication target's response after first-party authority and native upload validation. Disabled publication returns 405."

publishReplies :: PublishReplies PassthroughResponse
publishReplies =
    PublishReplies
        { publishAllowedMethods = "PUT"
        , publishDisabledReply = \headers _ -> localError status405 headers "publishing is not enabled on this view"
        , publishRelayed = \status headers body -> passthroughResponse status (if status == status401 then challenge <> headers else headers) (PassthroughBytes body)
        , publishError = \status headers -> localError status (if status == status401 then challenge <> headers else headers)
        }

notFound :: RouteAction
notFound = RouteAction notFoundContract (AnswerLocally (responseValue [] Nothing))

notFoundContract :: ResponseContract (ResponseValue (Maybe LByteString))
notFoundContract = refusalContract status404 "No native route claims this path."

-- | Describe both native views from their live route table.
cargoRouteSpecs :: NonEmpty RouteSpec
cargoRouteSpecs = catchAllSpecs notFoundContract unsupportedPathParam `NE.appendList` concatMap specsOf cargoRoutes
