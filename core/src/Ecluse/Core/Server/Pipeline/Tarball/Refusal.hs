-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Artifact refusal decisions and their route-specific responses.
module Ecluse.Core.Server.Pipeline.Tarball.Refusal (
    -- * Rendering a refusal
    artifactOutcomeStatus,
    artifactError,
    internalArtifactError,
    crossHostRefused,

    -- * The refusals themselves
    upstreamUnavailable,
    upstreamCooling,
    versionAbsent,
    privateMissRefusal,
    privateReadRefusal,
) where

import Network.HTTP.Types (ResponseHeaders, status403, status500)

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Metadata (
    VersionEvaluation (VersionMetadataUnavailable, VersionMissing),
    versionTransience,
 )
import Ecluse.Core.Server.Context (PackumentDeps (pdFirstParty, pdHelp))
import Ecluse.Core.Server.Pipeline.Origin (OriginMiss (MissAbsent, MissUnresolved), ReadFailure (ReadInvalid, ReadUnavailable))
import Ecluse.Core.Server.Pipeline.Shared
import Ecluse.Core.Server.Pipeline.Tarball.Types (TarballReplies (tarballError))
import Ecluse.Core.Server.Response (
    ArtifactStatus (BadGateway, Forbidden, NotFound, Ok, ServerError, Unavailable'),
    RejectReason (ByPolicy, UpstreamInvalid),
    Rejection (Rejection, rejectionMessage),
    RetryAfter,
    ServeDecision (Admit, Reject),
    Transience (WillResolve, WontResolve),
    artifactHttpStatus,
    artifactStatus,
    mkRefusal,
    rejectUnavailable,
 )

-- | Missing versions and a first-party absence map to 404. Other outcomes use 'artifactStatus'.
artifactOutcomeStatus :: ServeDecision -> ArtifactStatus
artifactOutcomeStatus decision
    | decision `elem` [versionAbsent, firstPartyAbsent] = NotFound
    | otherwise = artifactStatus decision

-- | Render an artifact refusal, preserving any retry delay.
artifactError :: TarballReplies response -> PackumentDeps -> ServeDecision -> response
artifactError replies deps decision =
    tarballError replies (artifactHttpStatus status) retryHeaders (mkRefusal (pdHelp deps) message)
  where
    status :: ArtifactStatus
    status = artifactOutcomeStatus decision

    retryHeaders :: ResponseHeaders
    retryHeaders = case status of
        Unavailable' retry -> retryAfterHeaders retry
        Ok -> []
        Forbidden -> []
        ServerError -> []
        BadGateway -> []
        NotFound -> []

    message :: Text
    message = case decision of
        Admit -> "the artifact is available"
        Reject rej -> rejectionMessage rej

-- | The @500@ for an artifact URL that configuration and the package name could not form.
internalArtifactError :: TarballReplies response -> response
internalArtifactError replies =
    tarballError replies status500 [] (mkRefusal Nothing "could not form the upstream artifact URL")

-- | The @403@ for an artifact host the tarball-host policy does not permit.
crossHostRefused :: TarballReplies response -> response
crossHostRefused replies =
    tarballError replies status403 [] (mkRefusal Nothing "the upstream artifact host is not permitted by the tarball-host policy")

-- | A transient upstream outage, public or private (to @503@).
upstreamUnavailable :: ServeDecision
upstreamUnavailable =
    versionUnresolved VersionMetadataUnavailable upstreamUnavailableMessage

upstreamUnavailableMessage :: Text
upstreamUnavailableMessage = "the upstream registry was unavailable"

-- | A version absent from the public metadata. Its cause is terminal, and 'artifactOutcomeStatus' overrides it to a @404@ forwarded miss.
versionAbsent :: ServeDecision
versionAbsent =
    versionUnresolved VersionMissing "the requested version was not found upstream"

{- | The refusal a private miss renders, or 'Nothing' when the public leg may answer. Only a settled
absence of a name outside first-party reaches that leg, so no public namesake replaces a private package.
-}
privateMissRefusal :: PackumentDeps -> PackageName -> OriginMiss -> Maybe ServeDecision
privateMissRefusal deps name miss
    | pdFirstParty deps name = Just (firstPartyMissRefusal miss)
    | MissUnresolved failure <- miss = Just (privateReadRefusal failure)
    | otherwise = Nothing

-- | A failed private read: an unavailable registry @503@ with its delay, an invalid reply @502@.
privateReadRefusal :: ReadFailure -> ServeDecision
privateReadRefusal = \case
    ReadUnavailable retry -> rejectUnavailable (WillResolve retry) upstreamUnavailableMessage
    ReadInvalid -> privateReplyInvalid

-- The refusal a first-party private miss renders: a settled absence @404@, else its cause's status.
firstPartyMissRefusal :: OriginMiss -> ServeDecision
firstPartyMissRefusal = \case
    MissAbsent -> firstPartyAbsent
    MissUnresolved (ReadUnavailable retry) -> firstPartyUnresolved retry
    MissUnresolved ReadInvalid -> firstPartyInvalid

{- A first-party artifact the private upstream does not hold. No public artifact may stand in for
it, and 'artifactOutcomeStatus' renders it @404@. -}
firstPartyAbsent :: ServeDecision
firstPartyAbsent =
    Reject
        ( Rejection
            (ByPolicy firstPartyRule [])
            "the requested artifact is first-party to this deployment, so it is served from the private upstream only and is never fetched from the public registry"
        )

{- A first-party artifact whose private upstream could not be read. That upstream is the name's one
authority, so a retry may still resolve it. -}
firstPartyUnresolved :: Maybe RetryAfter -> ServeDecision
firstPartyUnresolved retry =
    rejectUnavailable
        (WillResolve retry)
        "the private upstream for this first-party artifact was unavailable"

-- A first-party artifact whose private upstream answered with an invalid or unusable reply.
firstPartyInvalid :: ServeDecision
firstPartyInvalid =
    Reject
        ( Rejection
            UpstreamInvalid
            "the private upstream for this first-party artifact returned an invalid or unusable reply"
        )

{- The refusal a version the single-version read could not resolve renders as. Its transience is
the shared projection's, the one the worker's retry-versus-drop reads. -}
versionUnresolved :: VersionEvaluation -> Text -> ServeDecision
versionUnresolved eval = rejectUnavailable (fromMaybe WontResolve (versionTransience eval))
