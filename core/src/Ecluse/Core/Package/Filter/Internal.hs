-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The location check behind "Ecluse.Core.Package.Filter", and the drop records of its refusals.
The check reads a URL's text: a safe filename, the https normalisation, and an authority the
document's upstream honours, in that order, so the first failing test decides the recorded reason.
Importing this module opts out of the public surface's stability promises. It exists so a spec can
hold the check to a per-artifact reference.
-}
module Ecluse.Core.Package.Filter.Internal (
    -- * A document's origin
    ArtifactOrigin (..),
    artifactOrigin,

    -- * Checking a URL
    ArtifactLocation (..),
    LocationRefusal (..),
    locateArtifact,

    -- * Checking an artifact
    ArtifactRefusal (..),
    resolveArtifact,

    -- * Recording refusals
    partitionArtifacts,
) where

import Data.Aeson (Value (String))
import Data.Text qualified as T

import Ecluse.Core.Package (
    Artifact (artFilename, artUrl),
    InvalidEntry,
    InvalidEntryKind (InvalidIndexFile, InvalidVersionManifest),
    mkInvalidEntry,
 )
import Ecluse.Core.Security (AllowedHostPorts, HostPort, artifactAuthorityHonoured, authorityLabel, hostAddress, hostPortAddress)
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

-- | A URL that passed the check, with what its text says about the file it names.
data ArtifactLocation = ArtifactLocation
    { locatedFilename :: Text
    -- ^ The filename of the URL as written, a slice of its text.
    , locatedNormalised :: Maybe Text
    -- ^ The normalised URL, when normalising changed the text.
    , locatedTrimmedNamesFile :: Bool
    -- ^ Whether the trimmed URL also names a safe file, which a served document needs to rebase it.
    }
    deriving stock (Eq, Show)

-- | Why a URL failed the check: the failing test's reason, and the text that test read.
data LocationRefusal = LocationRefusal
    { unlocatedReason :: Text
    , unlocatedUrl :: Text
    }
    deriving stock (Eq, Show)

{- | Check a URL's filename, its https normalisation, then its authority. A non-https (loopback)
upstream skips normalisation, but not the authority check, which the download gate also applies.
-}
locateArtifact :: ArtifactOrigin -> Text -> Either LocationRefusal ArtifactLocation
locateArtifact origin url = do
    filename <- namedFile url
    normalised <- normaliseScheme
    -- Text equal to the original has passed the filename check already.
    changed <-
        if normalised == url
            then Right Nothing
            else Just normalised <$ namedFile normalised
    if artifactAuthorityHonoured (originHosts origin) (originAuthority origin) (hostPortAddress normalised)
        then Right $! ArtifactLocation{locatedFilename = filename, locatedNormalised = changed, locatedTrimmedNamesFile = trimmedNamesFile}
        else Left (LocationRefusal "artifact authority is neither the serving upstream nor a declared artifact host" normalised)
  where
    namedFile candidate = maybeToRight (LocationRefusal "artifact URL has no safe filename" candidate) (urlFilename candidate)

    normaliseScheme = case originHttpsHost origin of
        Nothing -> Right url
        Just upstreamHost -> bimap (`LocationRefusal` url) registryUrlText (resolveTarballUrl upstreamHost url)

    -- Trimming that changes nothing leaves the name already checked.
    trimmedNamesFile = trimmed == url || isJust (urlFilename trimmed)
    trimmed = T.strip url

-- | Why one artifact was refused, for the drop record that reports it.
data ArtifactRefusal = ArtifactRefusal
    { refusedFile :: Text
    , refusedReason :: Text
    , refusedUrl :: Text
    -- ^ The URL the failing check read. A drop record reduces it to its authority.
    }
    deriving stock (Eq, Show)

-- | 'locateArtifact' for a typed artifact, which stays as written unless normalising changed its URL.
resolveArtifact :: ArtifactOrigin -> Artifact -> Either ArtifactRefusal Artifact
resolveArtifact origin art = case locateArtifact origin (artUrl art) of
    Left refusal -> Left ArtifactRefusal{refusedFile = artFilename art, refusedReason = unlocatedReason refusal, refusedUrl = unlocatedUrl refusal}
    Right location -> case locatedNormalised location of
        Nothing -> Right art
        Just normalised -> Right art{artUrl = normalised}

{- | Split one version's checked artifacts, in file order, into its survivors and its drop records. A
refused file records under its own name. A version left with none records its first refusal, under its key.
-}
partitionArtifacts :: Text -> [Either ArtifactRefusal artifact] -> (Maybe (NonEmpty artifact), [InvalidEntry])
partitionArtifacts rawVersion checked = case nonEmpty (rights checked) of
    Just survivors -> (Just survivors, [dropRecord InvalidIndexFile (refusedFile refusal) refusal | refusal <- refusals])
    Nothing -> (Nothing, [dropRecord InvalidVersionManifest rawVersion refusal | refusal <- take 1 refusals])
  where
    refusals = lefts checked

-- 'mkInvalidEntry' reduces only a scheme-bearing string, so the URL is reduced here, whatever its spelling.
dropRecord :: InvalidEntryKind -> Text -> ArtifactRefusal -> InvalidEntry
dropRecord kind key refusal = mkInvalidEntry kind key (String (authorityLabel (refusedUrl refusal))) (refusedReason refusal)

-- The bare host of an @https@ upstream base URL, or 'Nothing' for a non-https (test/dev
-- loopback) upstream whose artifact URLs the scheme normalisation leaves untouched.
httpsUpstreamHost :: Text -> Maybe Text
httpsUpstreamHost baseUrl
    | isPrefixOfLowered httpsPrefix baseUrl = Just (hostAddress baseUrl)
    | otherwise = Nothing
