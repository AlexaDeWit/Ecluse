{- | The RubyGems registry __wire__ JSON, decoded into a typed model.

A __placeholder boundary__ mirroring "Ecluse.Core.Registry.Pypi.Wire". Écluse serves
only npm, so this models just the one shape another part of the system reads from
RubyGems: the published version strings of the @\/api\/v1\/versions\/{gem}.json@
response, which is a JSON array with one entry per version. Only each entry's
@number@ (the version string) is modelled; the rest of the entry (platform, SHA,
timestamps) is ignored, leaving room for a full RubyGems adapter to grow the model
later.

Unlike the npm and PyPI listings, the document is a top-level array, so 'parseJSON'
decodes it as a list of 'VersionEntry'. An entry without a @number@ is a decode
failure rather than a silently-dropped element, since a version entry that names no
version is meaningless.
-}
module Ecluse.Core.Registry.Rubygems.Wire (
    VersionEntry (..),
    VersionListing (..),
    listingVersions,
) where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))

-- | One entry of the RubyGems versions array, modelled only by its version string.
newtype VersionEntry = VersionEntry
    { veNumber :: Text
    -- ^ The version string (@number@), exactly as RubyGems lists it.
    }
    deriving stock (Eq, Show)

instance FromJSON VersionEntry where
    parseJSON = withObject "RubyGems version entry" $ \o ->
        VersionEntry <$> o .: "number"

-- | The whole @\/api\/v1\/versions\/{gem}.json@ array, one 'VersionEntry' per version.
newtype VersionListing = VersionListing
    { vlEntries :: [VersionEntry]
    -- ^ The version entries, in the order RubyGems returns them (newest first).
    }
    deriving stock (Eq, Show)

instance FromJSON VersionListing where
    parseJSON = fmap VersionListing . parseJSON

{- | The published version strings of a gem: each entry's @number@, in the order
RubyGems returns them.
-}
listingVersions :: VersionListing -> [Text]
listingVersions = map veNumber . vlEntries
