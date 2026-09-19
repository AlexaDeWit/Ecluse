-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The refusals the artifact path can answer with, and their rendering onto the route's replies.

Both legs render through here, so one artifact outcome has one status and one message wherever it
was decided.
-}
module Ecluse.Core.Server.Pipeline.Tarball.Refusal (
    -- * Rendering a refusal
    artifactOutcomeStatus,
    artifactError,
    internalArtifactError,
    crossHostRefused,

    -- * The refusals themselves
    upstreamUnavailable,
    versionAbsent,
    firstPartyMissRefusal,
) where

import Network.HTTP.Types (ResponseHeaders, status403, status500)

import Ecluse.Core.Registry.Metadata (
    VersionEvaluation (VersionMetadataUnavailable, VersionMissing),
    versionTransience,
 )
import Ecluse.Core.Server.Context (PackumentDeps, pdHelp)
import Ecluse.Core.Server.Pipeline.Origin (OriginMiss (MissAbsent, MissUnresolved))
import Ecluse.Core.Server.Pipeline.Shared
import Ecluse.Core.Server.Pipeline.Tarball.Types (TarballReplies (tarballError))
import Ecluse.Core.Server.Response (
    ArtifactStatus (Forbidden, NotFound, Ok, ServerError, Unavailable'),
    RejectReason (ByPolicy),
    Rejection (Rejection, rejectionMessage),
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

{- Render a non-admit artifact outcome as the serve error model. A transient status carries no
suggested delay, because the single-artifact path has none to offer. -}
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
        NotFound -> []

    message :: Text
    message = case decision of
        Admit -> "the artifact is available"
        Reject rej -> rejectionMessage rej

internalArtifactError :: TarballReplies response -> response
internalArtifactError replies =
    tarballError replies status500 [] (mkRefusal Nothing "could not form the upstream artifact URL")

crossHostRefused :: TarballReplies response -> response
crossHostRefused replies =
    tarballError replies status403 [] (mkRefusal Nothing "the upstream artifact host is not permitted by the tarball-host policy")

-- | A transient public-upstream outage (to @503@).
upstreamUnavailable :: ServeDecision
upstreamUnavailable =
    versionUnresolved VersionMetadataUnavailable "the upstream registry was unavailable"

{- | A version absent from the public metadata. Its cause is terminal, and 'artifactOutcomeStatus'
overrides that status to a @404@ forwarded miss.
-}
versionAbsent :: ServeDecision
versionAbsent =
    versionUnresolved VersionMissing "the requested version was not found upstream"

-- | The refusal a first-party private miss renders: a settled absence @404@, an outage @503@.
firstPartyMissRefusal :: OriginMiss -> ServeDecision
firstPartyMissRefusal = \case
    MissAbsent -> firstPartyAbsent
    MissUnresolved -> firstPartyUnresolved

{- A first-party artifact the private upstream does not hold. No public artifact may stand in for
it, and 'artifactOutcomeStatus' renders it @404@. -}
firstPartyAbsent :: ServeDecision
firstPartyAbsent =
    Reject
        ( Rejection
            (ByPolicy firstPartyRule)
            "the requested artifact is first-party to this deployment, so it is served from the private upstream only and is never fetched from the public registry"
        )

{- A first-party artifact whose private upstream could not be read. That upstream is the name's one
authority, so a retry may still resolve it. -}
firstPartyUnresolved :: ServeDecision
firstPartyUnresolved =
    rejectUnavailable
        (WillResolve Nothing)
        "the private upstream for this first-party artifact was unavailable"

{- The refusal a version the single-version read could not resolve renders as. Its transience is
the shared projection's, the one the worker's retry-versus-drop reads. -}
versionUnresolved :: VersionEvaluation -> Text -> ServeDecision
versionUnresolved eval = rejectUnavailable (fromMaybe WontResolve (versionTransience eval))
