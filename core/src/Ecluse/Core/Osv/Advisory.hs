-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Core.Osv.Advisory (
    OsvAdvisory (..),
    OsvAffected (..),
    OsvPackage (..),
    OsvRange (..),
    OsvEvent (..),
    OsvDatabaseSpecific (..),
    OsvSeverityEntry (..),
    ExtractedOsv (..),
    advisorySeverity,
    extractFromAdvisory,
    osvExportUrl,
) where

import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import Data.Text qualified as T
import Security.CVSS (cvssScore, parseCVSS)

{- | An ecosystem's advisory export under an OSV-layout base URL
(@\<base\>\/\<ecosystem\>\/all.zip@). The base comes from configuration
(@osvExportBaseUrl@), so a moved or mirrored upstream never needs a new binary.

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
    , osvSeverity :: Maybe [OsvSeverityEntry]
    , osvDatabaseSpecific :: Maybe OsvDatabaseSpecific
    }
    deriving stock (Show, Eq)

instance FromJSON OsvAdvisory where
    parseJSON = withObject "OsvAdvisory" $ \v ->
        OsvAdvisory
            <$> v .: "id"
            <*> v .:? "affected"
            <*> v .:? "severity"
            <*> v .:? "database_specific"

{- | One entry of an advisory's @severity@ array: a scoring-system tag (@CVSS_V3@) and
its value. For a CVSS system that value is the /vector string/, not a number.
-}
data OsvSeverityEntry = OsvSeverityEntry
    { sevType :: Text
    , sevScore :: Text
    }
    deriving stock (Show, Eq)

instance FromJSON OsvSeverityEntry where
    parseJSON = withObject "OsvSeverityEntry" $ \v ->
        OsvSeverityEntry
            <$> v .: "type"
            <*> v .: "score"

-- | The subset of an advisory's @database_specific@ block the pipeline consumes.
newtype OsvDatabaseSpecific = OsvDatabaseSpecific
    { dbsSeverity :: Maybe Text
    {- ^ The source database's qualitative severity label (for GHSA-sourced npm
    advisories: @LOW@, @MODERATE@, @HIGH@, or @CRITICAL@).
    -}
    }
    deriving stock (Show, Eq)

instance FromJSON OsvDatabaseSpecific where
    parseJSON = withObject "OsvDatabaseSpecific" $ \v ->
        OsvDatabaseSpecific
            <$> v .:? "severity"

data OsvAffected = OsvAffected
    { affectedPackage :: OsvPackage
    , affectedRanges :: Maybe [OsvRange]
    , affectedVersions :: Maybe [Text]
    {- ^ Exact affected versions enumerated outside any range, each an affected point.
    Much of the npm malware feed names the single bad version here with no @ranges@.
    -}
    }
    deriving stock (Show, Eq)

instance FromJSON OsvAffected where
    parseJSON = withObject "OsvAffected" $ \v ->
        OsvAffected
            <$> v .: "package"
            <*> v .:? "ranges"
            <*> v .:? "versions"

data OsvPackage = OsvPackage
    { packageName :: Text
    , packageEcosystem :: Text
    }
    deriving stock (Show, Eq)

instance FromJSON OsvPackage where
    parseJSON = withObject "OsvPackage" $ \v ->
        OsvPackage
            <$> v .: "name"
            <*> v .: "ecosystem"

data OsvRange = OsvRange
    { rangeType :: Text
    , rangeEvents :: [OsvEvent]
    }
    deriving stock (Show, Eq)

instance FromJSON OsvRange where
    parseJSON = withObject "OsvRange" $ \v ->
        OsvRange
            <$> v .: "type"
            <*> v .: "events"

{- | One event in a range's ordered event list, carrying exactly one bound. @introduced@ opens
the affected interval inclusively, @fixed@ closes it exclusively, and @last_affected@ inclusively.
-}
data OsvEvent = OsvEvent
    { eventIntroduced :: Maybe Text
    , eventFixed :: Maybe Text
    , eventLastAffected :: Maybe Text
    }
    deriving stock (Show, Eq)

instance FromJSON OsvEvent where
    parseJSON = withObject "OsvEvent" $ \v ->
        OsvEvent
            <$> v .:? "introduced"
            <*> v .:? "fixed"
            <*> v .:? "last_affected"

{- | One affected segment of one package, one row of the artifact's ranges table.
'extIntroduced' is the inclusive lower bound and 'Nothing' means from the beginning. The
upper bound is 'extFixed' (exclusive), 'extLastAffected' (inclusive), or neither (open-ended).
-}
data ExtractedOsv = ExtractedOsv
    { extPackage :: Text
    , extEcosystem :: Text
    , extCveId :: Text
    , extIntroduced :: Maybe Text
    , extFixed :: Maybe Text
    , extLastAffected :: Maybe Text
    , extSeverity :: Maybe Double
    {- ^ The advisory's CVSS base score (0 to 10), carried onto each of its segments.
    'Nothing' when the advisory is unscored, as much of the npm malware feed is.
    -}
    }
    deriving stock (Show, Eq)

{- | The advisory's CVSS base score, normalised at ingest so the stored artifact holds one
comparable form. The highest parsing vector wins, then @ghsaSeverityCeiling@ on the
qualitative label, and 'Nothing' when the advisory offers neither.
-}
advisorySeverity :: OsvAdvisory -> Maybe Double
advisorySeverity adv = vectorScore <|> labelScore
  where
    vectorScore = case mapMaybe (parseVectorScore . sevScore) (fromMaybe [] (osvSeverity adv)) of
        [] -> Nothing
        (s : ss) -> Just (foldl' max s ss)
    labelScore = ghsaSeverityCeiling =<< (dbsSeverity =<< osvDatabaseSpecific adv)

-- The CVSS base score of a vector string via the library, or 'Nothing' if it does
-- not parse (a CVSS version this build's parser rejects).
parseVectorScore :: Text -> Maybe Double
parseVectorScore = either (const Nothing) (Just . oneDecimal . snd . cvssScore) . parseCVSS

-- The CVSS specification defines base scores to one decimal place. Rounding in 'Double'
-- space keeps the stored value exact to compare, not a Float-to-Double widening artefact.
oneDecimal :: Float -> Double
oneDecimal f = fromIntegral (round (realToFrac f * 10 :: Double) :: Integer) / 10

-- GitHub's qualitative severity label mapped to the ceiling of its CVSS v3 band. The
-- ceiling is the highest score the label could denote, so a coarse label is never
-- under-counted past a downstream deny threshold.
ghsaSeverityCeiling :: Text -> Maybe Double
ghsaSeverityCeiling label = case T.toUpper (T.strip label) of
    "NONE" -> Just 0.0
    "LOW" -> Just 3.9
    "MODERATE" -> Just 6.9
    "MEDIUM" -> Just 6.9
    "HIGH" -> Just 8.9
    "CRITICAL" -> Just 10.0
    _ -> Nothing

{- | Flatten an advisory into one 'ExtractedOsv' per affected segment: every
range segment of every affected package, plus each exactly-enumerated version as
a point. An advisory with neither ranges nor versions yields nothing.
-}
extractFromAdvisory :: OsvAdvisory -> [ExtractedOsv]
extractFromAdvisory adv = do
    aff <- fromMaybe [] (osvAffected adv)
    let pkg = affectedPackage aff
    Segment intro fixed lastAffected <- affectedSegments aff
    pure $
        ExtractedOsv
            { extPackage = packageName pkg
            , extEcosystem = packageEcosystem pkg
            , extCveId = osvId adv
            , extIntroduced = intro
            , extFixed = fixed
            , extLastAffected = lastAffected
            , extSeverity = severity
            }
  where
    -- Shared across every segment the advisory yields: the score is a property of
    -- the advisory, not of a segment.
    severity = advisorySeverity adv

-- | One affected interval: an inclusive lower bound and at most one upper bound.
data Segment = Segment (Maybe Text) (Maybe Text) (Maybe Text)

affectedSegments :: OsvAffected -> [Segment]
affectedSegments aff =
    maybe [] (concatMap (extractRange . rangeEvents) . filter versionTyped) (affectedRanges aff)
        <> maybe [] (map exactVersion) (affectedVersions aff)
  where
    exactVersion v = Segment (Just v) Nothing (Just v)

    -- Only @SEMVER@ and @ECOSYSTEM@ ranges carry version bounds. A @GIT@ range's events are
    -- commit identifiers, and 'insideAffectedRange' fails an unparseable bound closed to
    -- affected, so carving a @GIT@ range would quarantine every version of a healthy package.
    versionTyped :: OsvRange -> Bool
    versionTyped r = T.toUpper (T.strip (rangeType r)) `elem` ["SEMVER", "ECOSYSTEM"]

{- | Carve a range's ordered events into affected segments. An @introduced@ arriving while a
segment is open closes that one as unbounded above, and a segment still open at the end of
the list is unbounded above too.
-}
extractRange :: [OsvEvent] -> [Segment]
extractRange = go Nothing
  where
    go Nothing [] = []
    go (Just i) [] = [Segment (Just i) Nothing Nothing]
    go current (e : es)
        | Just i <- eventIntroduced e =
            case current of
                Just prev -> Segment (Just prev) Nothing Nothing : go (Just i) es
                Nothing -> go (Just i) es
        | Just f <- eventFixed e = Segment current (Just f) Nothing : go Nothing es
        | Just la <- eventLastAffected e = Segment current Nothing (Just la) : go Nothing es
        | otherwise = go current es
