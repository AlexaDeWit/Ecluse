{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Osv (
    OsvAdvisory (..),
    OsvAffected (..),
    OsvPackage (..),
    OsvRange (..),
    OsvEvent (..),
    ExtractedOsv (..),
    extractFromAdvisory,
) where

import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))

-- | Exact model of what osv.dev makes available
data OsvAdvisory = OsvAdvisory
    { osvId :: Text
    , osvAffected :: Maybe [OsvAffected]
    }
    deriving stock (Show, Eq, Generic)

instance FromJSON OsvAdvisory where
    parseJSON = withObject "OsvAdvisory" $ \v ->
        OsvAdvisory
            <$> v .: "id"
            <*> v .:? "affected"

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
package names, ecosystem identifiers, and fixedVersion remediation boundaries.
-}
data ExtractedOsv = ExtractedOsv
    { extPackage :: Text
    , extEcosystem :: Text
    , extFixedVersions :: [Text]
    }
    deriving stock (Show, Eq)

extractFromAdvisory :: OsvAdvisory -> [ExtractedOsv]
extractFromAdvisory adv = do
    affs <- maybeToList (osvAffected adv)
    aff <- affs
    let pkg = affectedPackage aff
        rngs = fromMaybe [] (affectedRanges aff)
        fixed = [f | r <- rngs, e <- rangeEvents r, f <- maybeToList (eventFixed e)]
    pure $ ExtractedOsv (packageName pkg) (packageEcosystem pkg) fixed
