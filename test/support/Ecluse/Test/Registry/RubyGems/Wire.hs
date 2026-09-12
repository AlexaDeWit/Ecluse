-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | RubyGems versions-listing JSON for the test capture oracles.
The live endpoint returns a top-level array. Each entry must supply its @number@,
or decoding fails. "Ecluse.Test.RegistryCapture" uses this module for RubyGems
version ordering checks. It models no other response fields.
-}
module Ecluse.Test.Registry.RubyGems.Wire (
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
