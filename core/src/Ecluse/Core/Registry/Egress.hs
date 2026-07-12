-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Enforce the https-only egress policy on the artifact URLs a projected package
carries.

Every registry adapter fetches metadata that names, for each version, the URL its
artifact bytes will be dialled from (npm's @dist.tarball@; a PyPI wheel or sdist URL;
a RubyGems @.gem@ URL). Those URLs arrive from upstream and are __untrusted__: left
alone, a packument could point a client's fetch at plaintext @http@, or at a foreign
host entirely.

This module is the one place that decision is made, for every ecosystem. It reasons
over the __domain model__ ('PackageInfo', 'PackageDetails', 'Artifact') rather than any
wire format, so it sits above the per-ecosystem adapters and each one applies it as a
projection post-step at the fetch boundary, where the upstream URL is known.

The policy, per artifact URL: an https URL is kept, a same-host @http@ URL is
__upgraded__ to https, and anything else (@http@ on a foreign host, or a non-http(s)
scheme) __drops__ the version from the served set. See "Ecluse.Core.Security.Egress"
for the URL-level decision this applies.
-}
module Ecluse.Core.Registry.Egress (
    enforceArtifactScheme,
    enforceArtifactSchemeDetails,
) where

import Data.Aeson (Value (String))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Ecluse.Core.Package (
    Artifact (..),
    InvalidEntry (..),
    InvalidEntryKind (InvalidVersionManifest),
    PackageDetails (..),
    PackageInfo (..),
 )
import Ecluse.Core.Security (hostAddress)
import Ecluse.Core.Security.Egress (registryUrlText, resolveTarballUrl)

{- | Normalise every served version's artifact URLs against the https-only egress
policy ('Ecluse.Core.Security.Egress.resolveTarballUrl'), given the @upstreamBaseUrl@
the metadata was served from. An https URL is kept, a same-host @http@ URL is
__upgraded__ to https, and a version whose artifact is @http@ on a foreign host (or any
non-http(s) URL) is __dropped__ from the served set and recorded as an
'Ecluse.Core.Package.InvalidVersionManifest' carrying the offending URL (the #486
drop-and-record contract), so the version is never dialled in plaintext and the drop is
observable.

The enforcement applies only when the upstream is __https__ (in production every
configured upstream is https by construction). A non-https upstream is the test\/dev
loopback opt-in, whose artifact URLs are left untouched.
-}
enforceArtifactScheme :: Text -> PackageInfo -> PackageInfo
enforceArtifactScheme upstreamBaseUrl info =
    case httpsUpstreamHost upstreamBaseUrl of
        Nothing -> info
        Just upstreamHost ->
            let (kept, drops) = Map.foldrWithKey (step upstreamHost) (Map.empty, []) (infoVersions info)
             in info{infoVersions = kept, infoInvalidEntries = infoInvalidEntries info <> drops}
  where
    step upstreamHost rawVersion details (keptAcc, dropAcc) =
        case resolveDetails upstreamHost details of
            Right ok -> (Map.insert rawVersion ok keptAcc, dropAcc)
            Left (reason, badUrl) ->
                (keptAcc, InvalidEntry InvalidVersionManifest rawVersion (String badUrl) reason : dropAcc)

{- | The single-version form of 'enforceArtifactScheme' for the selective decode path:
'Nothing' drops the version (its artifact URL is non-https and not upgradeable), a
'Just' carries the version with each artifact's URL normalised to https. A non-https
(test\/dev loopback) upstream leaves the version untouched.
-}
enforceArtifactSchemeDetails :: Text -> PackageDetails -> Maybe PackageDetails
enforceArtifactSchemeDetails upstreamBaseUrl details =
    case httpsUpstreamHost upstreamBaseUrl of
        Nothing -> Just details
        Just upstreamHost -> rightToMaybe (resolveDetails upstreamHost details)

-- The bare host of an @https@ upstream base URL, or 'Nothing' for a non-https (test/dev
-- loopback) upstream whose artifact URLs the scheme enforcement leaves untouched.
httpsUpstreamHost :: Text -> Maybe Text
httpsUpstreamHost baseUrl
    | "https://" `T.isPrefixOf` T.toLower baseUrl = Just (hostAddress baseUrl)
    | otherwise = Nothing

-- Resolve every artifact of a version against the egress policy: 'Right' the version
-- with each @artUrl@ normalised to https, or 'Left' the drop reason and the first
-- offending URL.
resolveDetails :: Text -> PackageDetails -> Either (Text, Text) PackageDetails
resolveDetails upstreamHost details =
    (\arts -> details{pkgArtifacts = arts}) <$> traverse (resolveArtifact upstreamHost) (pkgArtifacts details)

-- Normalise one artifact's URL: keep https, upgrade a same-host http, drop otherwise.
resolveArtifact :: Text -> Artifact -> Either (Text, Text) Artifact
resolveArtifact upstreamHost art =
    case resolveTarballUrl upstreamHost (artUrl art) of
        Right resolved -> Right art{artUrl = registryUrlText resolved}
        Left reason -> Left (reason, artUrl art)
