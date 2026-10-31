-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Serve caller-authorised private artifacts and policy-admitted public artifacts.
Only a settled private absence reaches the public leg, and never for a first-party name.
GET and HEAD share admission.
-}
module Ecluse.Core.Server.Pipeline.Tarball (
    tarballAction,
    serveTarball,
    headTarball,
) where

import Network.HTTP.Types (Method, status401, status500)
import Network.Wai (Request, ResponseReceived, requestHeaders)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Server.Conditional (forwardValidators)
import Ecluse.Core.Server.Context (
    Handler,
    MountBinding (bindingPackumentDeps),
    PackumentDeps (..),
    ResponseAction (RunPipeline),
    ServeRuntime (..),
    ctxMount,
    ctxRuntime,
 )
import Ecluse.Core.Server.Path (Filename)
import Ecluse.Core.Server.Pipeline.Internal (recordDenials, serveDecisionClass)
import Ecluse.Core.Server.Pipeline.Shared
import Ecluse.Core.Server.Pipeline.Tarball.Private (
    PrivateLeg (PrivateAnswered, PrivateHandled, PrivateMissed),
    streamPrivateArtifact,
 )
import Ecluse.Core.Server.Pipeline.Tarball.Public (servePublicArtifact)
import Ecluse.Core.Server.Pipeline.Tarball.Refusal (artifactError, privateMissRefusal)
import Ecluse.Core.Server.Pipeline.Tarball.Relay (ArtifactServe (ServeFull, ServeHead))
import Ecluse.Core.Server.Pipeline.Tarball.Types (ArtifactRequest (..), TarballReplies (..))
import Ecluse.Core.Server.Response (ServeDecision, mkRefusal)
import Ecluse.Core.Server.Route (isHead)
import Ecluse.Core.Telemetry.Record (MetricsPort (..))
import Ecluse.Core.Version (Version)

{- | The action a read route names for an artifact coordinate. A @HEAD@ runs the same policy as
@GET@ without pumping upstream bytes.
-}
tarballAction ::
    TarballReplies response ->
    Method ->
    PackageName ->
    Version ->
    Filename ->
    ResponseAction response
tarballAction replies method name version filename
    | isHead method = RunPipeline perimeterFallback (headTarball replies name version filename)
    | otherwise = RunPipeline perimeterFallback (serveTarball replies name version filename)
  where
    perimeterFallback = tarballError replies status500 [] (mkRefusal Nothing "internal server error")

-- | Serve an artifact with the caller's credential confined to the private origin.
serveTarball ::
    TarballReplies response ->
    PackageName ->
    Version ->
    Filename ->
    Request ->
    (response -> IO ResponseReceived) ->
    Handler ResponseReceived
serveTarball = tarballWith ServeFull

-- | Probe an artifact with HEAD through the same policy as GET, without pumping upstream bytes.
headTarball ::
    TarballReplies response ->
    PackageName ->
    Version ->
    Filename ->
    Request ->
    (response -> IO ResponseReceived) ->
    Handler ResponseReceived
headTarball = tarballWith ServeHead

-- Read the mount's dependencies, check the edge token, and serve in the given mode. The mount's
-- ecosystem presentation supplies the credential the private leg may carry.
tarballWith ::
    ArtifactServe ->
    TarballReplies response ->
    PackageName ->
    Version ->
    Filename ->
    Request ->
    (response -> IO ResponseReceived) ->
    Handler ResponseReceived
tarballWith mode replies name version file request respond = do
    mount <- asks ctxMount
    rt <- asks ctxRuntime
    let deps = bindingPackumentDeps mount
        token = forwardedCredential mount request
        ctx =
            ArtifactRequest
                { arMode = mode
                , arReplies = replies
                , arRuntime = rt
                , arDeps = deps
                , -- Relayed onto both legs' upstream requests so upstream can answer a 304
                  -- for a pass-through body (the conditional-GET contract).
                  arValidators = forwardValidators (requestHeaders request)
                , arPackage = name
                , arVersion = version
                , arFile = file
                , arRespond = respond
                }
    if edgeTokenMatches (pdInboundToken deps) token
        then serveArtifactLegs ctx token
        else liftIO (respond (tarballError replies status401 [] (mkRefusal Nothing unauthorisedMessage)))

-- The private leg first, then the gated public leg on a miss.
serveArtifactLegs :: ArtifactRequest response -> Maybe ClientCredential -> Handler ResponseReceived
serveArtifactLegs ctx token =
    streamPrivateArtifact ctx token >>= \case
        PrivateHandled received -> pure received
        PrivateAnswered decision received -> do
            liftIO (mpServeDecision (srMetrics (arRuntime ctx)) decision)
            pure received
        PrivateMissed miss -> maybe (servePublicArtifact ctx) (refuseMiss ctx) (privateMissRefusal (arDeps ctx) (arPackage ctx) miss)

-- A private miss that ends the request here records its decision and denial before answering.
refuseMiss :: ArtifactRequest response -> ServeDecision -> Handler ResponseReceived
refuseMiss ctx decision = do
    let metrics = srMetrics (arRuntime ctx)
    liftIO (mpServeDecision metrics (serveDecisionClass decision))
    liftIO (recordDenials metrics [decision])
    liftIO (arRespond ctx (artifactError (arReplies ctx) (arDeps ctx) decision))
