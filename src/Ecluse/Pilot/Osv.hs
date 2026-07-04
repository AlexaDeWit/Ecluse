{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Pilot.Osv (
    OsvAdvisory (..),
    OsvAffected (..),
    OsvPackage (..),
    OsvRange (..),
    OsvEvent (..),
    OsvDatabaseSpecific (..),
    ExtractedOsv (..),
    extractFromAdvisory,
    osvExportUrl,
) where

import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import Data.Text qualified as T

{- | An ecosystem's advisory export under an OSV-layout base URL
(@\<base\>\/\<ecosystem\>\/all.zip@): a zip archive of every advisory currently
published for the ecosystem. The base comes from configuration
(@osvExportBaseUrl@), so a moved or mirrored upstream never needs a new
binary; a trailing slash on the base is tolerated.

>>> osvExportUrl "https://osv-vulnerabilities.storage.googleapis.com/" "npm"
"https://osv-vulnerabilities.storage.googleapis.com/npm/all.zip"
-}
osvExportUrl :: Text -> Text -> String
osvExportUrl baseUrl ecosystem =
    toString (T.dropWhileEnd (== '/') baseUrl) <> "/" <> toString ecosystem <> "/all.zip"

-- | Exact model of what osv.dev makes available
data OsvAdvisory = OsvAdvisory
    { osvId :: Text
    , osvAffected :: Maybe [OsvAffected]
    , osvDatabaseSpecific :: Maybe OsvDatabaseSpecific
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON OsvAdvisory where
    parseJSON = withObject "OsvAdvisory" $ \v ->
        OsvAdvisory
            <$> v .: "id"
            <*> v .:? "affected"
            <*> v .:? "database_specific"

-- | The subset of an advisory's @database_specific@ block the pipeline consumes.
newtype OsvDatabaseSpecific = OsvDatabaseSpecific
    { dbsSeverity :: Maybe Text
    {- ^ The source database's qualitative severity label (for GHSA-sourced npm
    advisories: @LOW@, @MODERATE@, @HIGH@, or @CRITICAL@).
    -}
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON OsvDatabaseSpecific where
    parseJSON = withObject "OsvDatabaseSpecific" $ \v ->
        OsvDatabaseSpecific
            <$> v .:? "severity"

data OsvAffected = OsvAffected
    { affectedPackage :: OsvPackage
    , affectedRanges :: Maybe [OsvRange]
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON OsvAffected where
    parseJSON = withObject "OsvAffected" $ \v ->
        OsvAffected
            <$> v .: "package"
            <*> v .:? "ranges"

data OsvPackage = OsvPackage
    { packageName :: Text
    , packageEcosystem :: Text
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON OsvPackage where
    parseJSON = withObject "OsvPackage" $ \v ->
        OsvPackage
            <$> v .: "name"
            <*> v .: "ecosystem"

data OsvRange = OsvRange
    { rangeType :: Text
    , rangeEvents :: [OsvEvent]
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON OsvRange where
    parseJSON = withObject "OsvRange" $ \v ->
        OsvRange
            <$> v .: "type"
            <*> v .: "events"

data OsvEvent = OsvEvent
    { eventIntroduced :: Maybe Text
    , eventFixed :: Maybe Text
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON OsvEvent where
    parseJSON = withObject "OsvEvent" $ \v ->
        OsvEvent
            <$> v .:? "introduced"
            <*> v .:? "fixed"

{- | The necessary subset of data requested by the goal:
package names, ecosystem identifiers, cve_id, and remediation boundaries.
-}
data ExtractedOsv = ExtractedOsv
    { extPackage :: Text
    , extEcosystem :: Text
    , extCveId :: Text
    , extIntroduced :: Maybe Text
    , extFixed :: Maybe Text
    , extSeverity :: Maybe Text
    {- ^ The advisory-level severity label, carried onto each of the advisory's
    ranges; 'Nothing' when the source database supplies none.
    -}
    }
    deriving stock (Show, Eq)

extractFromAdvisory :: OsvAdvisory -> [ExtractedOsv]
extractFromAdvisory adv = do
    aff <- fromMaybe [] (osvAffected adv)
    let pkg = affectedPackage aff
    rng <- fromMaybe [] (affectedRanges aff)
    (intro, fixed) <- extractBounds (rangeEvents rng)
    pure $
        ExtractedOsv
            { extPackage = packageName pkg
            , extEcosystem = packageEcosystem pkg
            , extCveId = osvId adv
            , extIntroduced = intro
            , extFixed = fixed
            , extSeverity = dbsSeverity =<< osvDatabaseSpecific adv
            }

extractBounds :: [OsvEvent] -> [(Maybe Text, Maybe Text)]
extractBounds = go Nothing
  where
    go (Just i) [] = [(Just i, Nothing)]
    go Nothing [] = []
    go current (e : es)
        | Just i <- eventIntroduced e = go (Just i) es
        | Just f <- eventFixed e = (current, Just f) : go Nothing es
        | otherwise = go current es
