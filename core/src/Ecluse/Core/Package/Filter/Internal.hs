-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The per-artifact location check behind "Ecluse.Core.Package.Filter": a safe filename, the https
normalisation, and an authority the document's upstream honours, in that order.

Importing this module opts out of the public surface's stability promises. It exists so a spec can
hold the check to a per-artifact reference.
-}
module Ecluse.Core.Package.Filter.Internal (
    ArtifactOrigin (..),
    artifactOrigin,
    ArtifactRefusal (..),
    resolveArtifact,
) where

import Ecluse.Core.Package (Artifact (artFilename, artUrl))
import Ecluse.Core.Security (AllowedHostPorts, HostPort, artifactAuthorityHonoured, hostAddress, hostPortAddress)
import Ecluse.Core.Security.Egress (registryUrlText, resolveTarballUrl)
import Ecluse.Core.Text (httpsPrefix, isPrefixOfLowered, urlFilename)

-- | The inputs every artifact of one document is checked against, derived once from its upstream.
data ArtifactOrigin = ArtifactOrigin
    { originHosts :: AllowedHostPorts
    -- ^ The ecosystem's declared artifact hosts.
    , originAuthority :: Maybe HostPort
    -- ^ The upstream's dialled authority, when one extracts.
    , originHttpsHost :: Maybe Text
    -- ^ The upstream's bare host, or 'Nothing' for a non-https (loopback) upstream.
    }

-- | Derive a document's 'ArtifactOrigin' from its ecosystem's artifact hosts and upstream base URL.
artifactOrigin :: AllowedHostPorts -> Text -> ArtifactOrigin
artifactOrigin ecosystemHosts upstreamBaseUrl =
    ArtifactOrigin
        { originHosts = ecosystemHosts
        , originAuthority = hostPortAddress upstreamBaseUrl
        , originHttpsHost = httpsUpstreamHost upstreamBaseUrl
        }

-- | Why one artifact was refused, for the drop record that reports it.
data ArtifactRefusal = ArtifactRefusal
    { refusedFile :: Text
    , refusedReason :: Text
    , refusedUrl :: Text
    -- ^ The URL the failing check read. A drop record reduces it to its authority.
    }
    deriving stock (Eq, Show)

{- | Check an artifact's filename, its https normalisation, then its authority. A non-https (loopback)
upstream skips normalisation, but not the authority check, which the download gate also applies.
-}
resolveArtifact :: ArtifactOrigin -> Artifact -> Either ArtifactRefusal Artifact
resolveArtifact origin art = do
    checkFilename url
    normalised <- normaliseScheme
    -- Text equal to the original has passed the filename check already.
    located <-
        if normalised == url
            then Right art
            else art{artUrl = normalised} <$ checkFilename normalised
    if artifactAuthorityHonoured (originHosts origin) (originAuthority origin) (hostPortAddress normalised)
        then Right located
        else Left (refusal "artifact authority is neither the serving upstream nor a declared artifact host" normalised)
  where
    url = artUrl art

    checkFilename candidate =
        when (isNothing (urlFilename candidate)) $
            Left (refusal "artifact URL has no safe filename" candidate)

    normaliseScheme = case originHttpsHost origin of
        Nothing -> Right url
        Just upstreamHost -> bimap (`refusal` url) registryUrlText (resolveTarballUrl upstreamHost url)

    refusal reason candidate = ArtifactRefusal{refusedFile = artFilename art, refusedReason = reason, refusedUrl = candidate}

-- The bare host of an @https@ upstream base URL, or 'Nothing' for a non-https (test/dev
-- loopback) upstream whose artifact URLs the scheme normalisation leaves untouched.
httpsUpstreamHost :: Text -> Maybe Text
httpsUpstreamHost baseUrl
    | isPrefixOfLowered httpsPrefix baseUrl = Just (hostAddress baseUrl)
    | otherwise = Nothing
