-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Serve artifacts from the private origin or an admitted public fallback.

Private access refusals stop the fallback, and other private misses keep the first-party
restriction. @GET@ and @HEAD@ share policy. The private leg lives in
"Ecluse.Core.Server.Pipeline.Tarball.Private", the gated public leg in
"Ecluse.Core.Server.Pipeline.Tarball.Public", and this module and the public leg render refusals
through "Ecluse.Core.Server.Pipeline.Tarball.Refusal".
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
import Ecluse.Core.Server.Pipeline.Origin (OriginMiss)
import Ecluse.Core.Server.Pipeline.Shared
import Ecluse.Core.Server.Pipeline.Tarball.Private (
    PrivateLeg (PrivateAnswered, PrivateMissed),
    streamPrivateArtifact,
 )
import Ecluse.Core.Server.Pipeline.Tarball.Public (servePublicArtifact)
import Ecluse.Core.Server.Pipeline.Tarball.Refusal (artifactError, firstPartyMissRefusal)
import Ecluse.Core.Server.Pipeline.Tarball.Relay (ArtifactServe (ServeFull, ServeHead))
import Ecluse.Core.Server.Pipeline.Tarball.Types (ArtifactRequest (..), TarballReplies (..))
import Ecluse.Core.Server.Response (mkRefusal)
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
        PrivateAnswered decision received -> do
            liftIO (mpServeDecision (srMetrics (arRuntime ctx)) decision)
            pure received
        PrivateMissed miss
            | pdFirstParty (arDeps ctx) (arPackage ctx) -> refuseFirstParty ctx miss
            | otherwise -> servePublicArtifact ctx

-- A first-party name has one authority, so its private miss ends the request here rather than
-- falling through to the public leg.
refuseFirstParty :: ArtifactRequest response -> OriginMiss -> Handler ResponseReceived
refuseFirstParty ctx miss = do
    let metrics = srMetrics (arRuntime ctx)
        decision = firstPartyMissRefusal miss
    liftIO (mpServeDecision metrics (serveDecisionClass decision))
    liftIO (recordDenials metrics [decision])
    liftIO (arRespond ctx (artifactError (arReplies ctx) (arDeps ctx) decision))
