-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The RubyGems registry __wire__ JSON, decoded into a typed model for the
version-capture oracles.

This is __oracle apparatus__ mirroring "Ecluse.Test.Registry.PyPI.Wire". It decodes a live
RubyGems response as far as the one shape the capture path reads. That shape is the
published version strings of the @\/api\/v1\/versions\/{gem}.json@ response, a JSON array
with one entry per version. "Ecluse.Test.RegistryCapture" dispatches a RubyGems capture
through 'listingVersions' to feed the version-ordering differential and to detect
protocol drift against the live registry. This decoder models only each entry's @number@
(the version string) and ignores the rest of the entry (platform, SHA, timestamps).

This decoder serves the test oracles only. It is not the production RubyGems wire module,
which belongs to the RubyGems adapter's own design.

Unlike the npm and PyPI listings, the document is a top-level array, so 'parseJSON'
decodes it as a list of 'VersionEntry'. An entry without a @number@ is a decode failure,
not a silently-dropped element: a version entry that names no version is meaningless.
-}
module Ecluse.Test.Registry.Rubygems.Wire (
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
