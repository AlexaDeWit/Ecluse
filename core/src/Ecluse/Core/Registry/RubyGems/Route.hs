-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | Modern Bundler uses named metadata, compact specifications and gated archives.
module Ecluse.Core.Registry.RubyGems.Route (
    rubyGemsRouter,
    rubyGemsRouteSpecs,
    rubyGemsRoutes,
) where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Network.HTTP.Types (ResponseHeaders, Status, mkStatus, status404, status405, status500)

import Ecluse.Core.Registry.RubyGems.Handlers (serveDependencies)
import Ecluse.Core.Registry.RubyGems.Handlers.Coordinate (serveCompactSpec, serveGemArtifact)
import Ecluse.Core.Registry.RubyGems.Route.Response (rubyContract, rubyReplies)
import Ecluse.Core.Server.Context (MountRouter, ResponseAction (..), RouteAction (RouteAction))
import Ecluse.Core.Server.Contract (BodySchema (SchemaOpaque), PassthroughBody (..), PassthroughResponse, RequestSpec (RequestSpec), ResponseChoice (..), ResponseContract, ResponseValue, chooseContract, emptyContract, passthroughContract, passthroughResponse, responseValue)
import Ecluse.Core.Server.Path (Filename, mkFilename, unFilename)
import Ecluse.Core.Server.Pipeline.Packument (PackumentReplies (..))
import Ecluse.Core.Server.Pipeline.Publish (PublishReplies (..), servePublish)
import Ecluse.Core.Server.Pipeline.Tarball.Types (TarballReplies (..))
import Ecluse.Core.Server.Response (mkRefusal, renderRefusal)
import Ecluse.Core.Server.Route (Capture (..), MediaNegotiation (AcceptsAnything), MethodMatch (MethodPost, MethodRead), PatternSeg (..), Route (Route), RouteName (RouteName), routerOf)
import Ecluse.Core.Server.RouteDescription (RouteSpec, catchAllSpecs, specsOf, unsupportedPathParam)

-- | The same native route table drives dispatch and API documentation.
rubyGemsRouter :: MountRouter
rubyGemsRouter = routerOf notFound rubyGemsRoutes

-- | No global catalogue or client-facing management route is exposed.
rubyGemsRoutes :: [Route Filename]
rubyGemsRoutes =
    [ Route
        (RouteName "versions")
        MethodRead
        AcceptsAnything
        [SegLit "versions"]
        (\_ captures -> AnswerLocally (responseValue [] ()) <$ guard (null captures))
        "Select named-gem metadata"
        "The Compact Index probe returns 404 so supported Bundler clients use the Dependencies API."
        Nothing
        notFoundContract
    , Route
        (RouteName "dependencies")
        MethodRead
        AcceptsAnything
        (map SegLit ["api", "v1", "dependencies"])
        (\_ captures -> RunPipeline metadataFallback serveDependencies <$ guard (null captures))
        "Read admitted dependency records"
        "Returns native Marshal records for at most 100 named gems. Query syntax, cumulative bytes, work and the whole request deadline are bounded."
        Nothing
        (rubyContract "application/octet-stream")
    , Route
        (RouteName "compactSpec")
        MethodRead
        AcceptsAnything
        (map SegLit ["quick", "Marshal.4.8"] <> [SegCapSuffix nativeCapture ".gemspec.rz"])
        (\_ -> \case [file] -> Just (RunPipeline metadataFallback (serveCompactSpec file)); _ -> Nothing)
        "Read an admitted compact specification"
        "Relays original validated native bytes for the exact admitted package, raw version and platform."
        Nothing
        (rubyContract "application/octet-stream")
    , Route
        (RouteName "artifact")
        MethodRead
        AcceptsAnything
        [SegLit "gems", SegCapSuffix nativeCapture ".gem"]
        (\method -> \case [file] -> Just (RunPipeline (SecondResponse (localError status500 [] "internal server error")) (\request respond -> serveGemArtifact artifactReplies method file request (respond . SecondResponse) (respond . FirstResponse))); _ -> Nothing)
        "Read an admitted gem archive"
        "Resolves the exact native coordinate before current artifact admission and caller-authorised relay."
        Nothing
        (chooseContract (rubyContract "application/octet-stream") artifactContract)
    , Route
        (RouteName "publish")
        MethodPost
        AcceptsAnything
        (map SegLit ["api", "v1", "gems"])
        (\_ captures -> RunPipeline (localError status500 [] "internal server error") (servePublish publishReplies Nothing) <$ guard (null captures))
        "Publish a first-party gem"
        "Validates the complete native archive and package identity before relaying unchanged bytes under publication authority. Disabled publication returns 405."
        (Just (RequestSpec "The complete built .gem archive." True (SchemaOpaque "application/octet-stream")))
        publishContract
    ]
  where
    metadataFallback = packumentInternal rubyReplies [] (mkRefusal Nothing "internal server error")

nativeCapture :: Capture Filename
nativeCapture = Capture "coordinate" "The native package, raw version and platform filename stem." consume (\file -> [T.dropEnd 4 (unFilename file)])
  where
    consume = \case
        stem : rest -> (,rest) <$> mkFilename (stem <> ".gem")
        _ -> Nothing

artifactContract :: ResponseContract PassthroughResponse
artifactContract = passthroughContract "Relays the selected gem archive and preserves upstream statuses. Local errors retain authentication, admission, integrity and availability failures."

artifactReplies :: TarballReplies PassthroughResponse
artifactReplies =
    TarballReplies
        { tarballError = \status headers -> localError status headers . renderRefusal
        , tarballPrivateRefused = \code headers -> localError (mkStatus code "") headers . renderRefusal
        , tarballStream = \status headers -> passthroughResponse status headers . PassthroughStream
        , tarballEmpty = \status headers -> passthroughResponse status headers PassthroughEmpty
        }

publishContract :: ResponseContract PassthroughResponse
publishContract = passthroughContract "Relays the publication target status after body validation and first-party authorisation."

publishReplies :: PublishReplies PassthroughResponse
publishReplies =
    PublishReplies
        { publishAllowedMethods = "POST"
        , publishDisabledReply = \headers _ -> localError status405 headers "publishing is not enabled on this mount"
        , publishRelayed = \status headers -> passthroughResponse status headers . PassthroughBytes
        , publishError = localError
        }

localError :: Status -> ResponseHeaders -> Text -> PassthroughResponse
localError status headers = passthroughResponse status (("Content-Type", "text/plain") : ("Cache-Control", "no-store") : headers) . PassthroughBytes . fromStrict . encodeUtf8

notFoundContract :: ResponseContract (ResponseValue ())
notFoundContract = emptyContract status404 "No native route claims this path."

notFound :: RouteAction
notFound = RouteAction notFoundContract (AnswerLocally (responseValue [] ()))

-- | Publish the routes and structural fallback from the same contracts used at runtime.
rubyGemsRouteSpecs :: NonEmpty RouteSpec
rubyGemsRouteSpecs = catchAllSpecs notFoundContract unsupportedPathParam `NE.appendList` concatMap specsOf rubyGemsRoutes
