-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A per-artifact reference for the location check in "Ecluse.Core.Package.Filter.Internal". It
re-derives the upstream's authority and host for every artifact, lower-cases whole URLs, and checks
the filename before and after normalising, so a spec can hold the per-document check to it.
-}
module Ecluse.Package.Filter.Support (referenceResolveArtifact) where

import Data.Text qualified as T

import Ecluse.Core.Package (Artifact (artFilename, artUrl))
import Ecluse.Core.Package.Filter.Internal (ArtifactRefusal (..))
import Ecluse.Core.Security (AllowedHostPorts, artifactAuthorityHonoured, authorityLabel, hostAddress, hostPortAddress)
import Ecluse.Core.Text (urlFilename)

-- | The reference for 'Ecluse.Core.Package.Filter.Internal.resolveArtifact', given the upstream base URL.
referenceResolveArtifact :: AllowedHostPorts -> Text -> Artifact -> Either ArtifactRefusal Artifact
referenceResolveArtifact ecosystemHosts upstreamBaseUrl art = do
    checkFilename art
    normalised <- normaliseScheme
    checkFilename normalised
    if artifactAuthorityHonoured ecosystemHosts originAuthority (hostPortAddress (artUrl normalised))
        then Right normalised
        else Left (refusal "artifact authority is neither the serving upstream nor a declared artifact host" (artUrl normalised))
  where
    originAuthority = hostPortAddress upstreamBaseUrl

    checkFilename candidate =
        when (isNothing (urlFilename (artUrl candidate))) $
            Left (refusal "artifact URL has no safe filename" (artUrl candidate))

    normaliseScheme = case httpsUpstreamHost upstreamBaseUrl of
        Nothing -> Right art
        Just upstreamHost -> case referenceTarballUrl upstreamHost (artUrl art) of
            Right resolved -> Right art{artUrl = resolved}
            Left reason -> Left (refusal reason (artUrl art))

    refusal reason url = ArtifactRefusal{refusedFile = artFilename art, refusedReason = reason, refusedUrl = url}

httpsUpstreamHost :: Text -> Maybe Text
httpsUpstreamHost baseUrl
    | "https://" `T.isPrefixOf` T.toLower baseUrl = Just (hostAddress baseUrl)
    | otherwise = Nothing

-- 'Ecluse.Core.Security.Egress.resolveTarballUrl' with whole-URL lower-casing, returning the text.
referenceTarballUrl :: Text -> Text -> Either Text Text
referenceTarballUrl upstreamHost url
    | "https://" `T.isPrefixOf` lowered = referenceRegistryUrl url
    | "http://" `T.isPrefixOf` lowered =
        if hostAddress url == upstreamHost
            then referenceRegistryUrl ("https://" <> T.drop 7 url)
            else Left ("dist.tarball is http on a host other than the upstream registry: " <> authorityLabel url)
    | otherwise = Left ("dist.tarball is not an https URL: " <> authorityLabel url)
  where
    lowered = T.toLower url

-- 'Ecluse.Core.Security.Egress.mkRegistryUrl' with whole-URL lower-casing, returning the text.
referenceRegistryUrl :: Text -> Either Text Text
referenceRegistryUrl raw
    | T.null trimmed = Left "expected a non-empty https URL"
    | "https://" `T.isPrefixOf` T.toLower trimmed = Right trimmed
    | otherwise = Left ("registry URL must use https (got " <> trimmed <> ")")
  where
    trimmed = T.strip raw
