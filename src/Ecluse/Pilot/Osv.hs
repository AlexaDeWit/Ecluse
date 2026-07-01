{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Pilot.Osv (
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
package names, ecosystem identifiers, cve_id, and remediation boundaries.
-}
data ExtractedOsv = ExtractedOsv
    { extPackage :: Text
    , extEcosystem :: Text
    , extCveId :: Text
    , extIntroduced :: Maybe Text
    , extFixed :: Maybe Text
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
