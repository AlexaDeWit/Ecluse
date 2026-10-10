-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Decode advisory evidence for the compiled artifact.
Package keys use the same ecosystem identity as policy queries.
-}
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
    orderableBounds,
    unorderableBounds,
    osvExportUrl,
) where

import Prelude hiding (universe)

import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import Data.Foldable1 qualified as Foldable1
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Universe.Class (Universe (universe))
import Security.CVSS (cvssScore, parseCVSS)

import Ecluse.Core.Cve (AdvisoryRange (..), affecting, packageAdvisories)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Osv.Ecosystem (osvEcosystemFor, osvExportDirectory)
import Ecluse.Core.Osv.Epss (EpssScores, epssForIds)
import Ecluse.Core.Osv.Types (UpperBound (..))
import Ecluse.Core.Package (canonicalise)
import Ecluse.Core.Text (joinUrlPath)
import Ecluse.Core.Version (mkVersion, parseVersionKey, versionKey)

-- | The OSV fields used to select and score active advisory evidence.
data OsvAdvisory = OsvAdvisory
    { osvId :: Text
    , osvAliases :: Maybe [Text]
    -- ^ EPSS joins a GHSA-keyed advisory through its CVE aliases.
    , osvAffected :: Maybe [OsvAffected]
    , osvSeverity :: Maybe [OsvSeverityEntry]
    , osvDatabaseSpecific :: Maybe OsvDatabaseSpecific
    , osvWithdrawn :: Maybe UTCTime
    -- ^ A withdrawn record supplies no active evidence, even when it retains affected ranges.
    , osvModified :: Maybe Text
    -- ^ The source modification date as written. An unreadable date does not discard the record.
    }
    deriving stock (Show, Eq)

instance FromJSON OsvAdvisory where
    parseJSON = withObject "OsvAdvisory" $ \v ->
        OsvAdvisory
            <$> v .: "id"
            <*> v .:? "aliases"
            <*> v .:? "affected"
            <*> v .:? "severity"
            <*> v .:? "database_specific"
            <*> v .:? "withdrawn"
            <*> v .:? "modified"

-- | A severity system and its value. CVSS values contain a vector string.
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
    -- ^ The source's qualitative label, such as @HIGH@ or @CRITICAL@.
    }
    deriving stock (Show, Eq)

instance FromJSON OsvDatabaseSpecific where
    parseJSON = withObject "OsvDatabaseSpecific" $ \v ->
        OsvDatabaseSpecific
            <$> v .:? "severity"

-- | One package's range evidence and enumerated affected versions.
data OsvAffected = OsvAffected
    { affectedPackage :: OsvPackage
    , affectedRanges :: Maybe [OsvRange]
    , affectedVersions :: Maybe [Text]
    -- ^ Exact affected points. Malware records can supply these without ranges.
    }
    deriving stock (Show, Eq)

instance FromJSON OsvAffected where
    parseJSON = withObject "OsvAffected" $ \v ->
        OsvAffected
            <$> v .: "package"
            <*> v .:? "ranges"
            <*> v .:? "versions"

-- | The package identity as written by the OSV source.
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

-- | Bound events with a type tag. Only version-based types supply affected intervals.
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

-- | One bound event: @introduced@ and @last_affected@ are inclusive, @fixed@ is exclusive.
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

-- | A canonical package segment. No introduced bound means affected from the beginning.
data ExtractedOsv = ExtractedOsv
    { extPackage :: Text
    , extEcosystem :: Text
    , extCveId :: Text
    , extIntroduced :: Maybe Text
    , extUpperBound :: UpperBound
    , extSeverity :: Maybe Double
    -- ^ CVSS base score (0 to 10), absent for unscored advisories.
    , extEpss :: Maybe Double
    -- ^ EPSS probability (0 to 1), absent when the feed scores none of the identifiers.
    }
    deriving stock (Show, Eq)

-- | Prefer the highest parsing CVSS vector, then the qualitative label's ceiling, or no score.
advisorySeverity :: OsvAdvisory -> Maybe Double
advisorySeverity adv = vectorScore <|> labelScore
  where
    vectorScore = viaNonEmpty Foldable1.maximum (mapMaybe (parseVectorScore . sevScore) (fromMaybe [] (osvSeverity adv)))
    labelScore = ghsaSeverityCeiling =<< (dbsSeverity =<< osvDatabaseSpecific adv)

parseVectorScore :: Text -> Maybe Double
parseVectorScore = either (const Nothing) (Just . oneDecimal . snd . cvssScore) . parseCVSS

-- The CVSS specification defines base scores to one decimal place. Rounding in 'Double'
-- space keeps the stored value exact to compare, not a Float-to-Double widening artefact.
oneDecimal :: Float -> Double
oneDecimal f = fromIntegral (round (realToFrac f * 10 :: Double) :: Integer) / 10

-- Use the band's ceiling so a qualitative score cannot fall below its possible deny threshold.
ghsaSeverityCeiling :: Text -> Maybe Double
ghsaSeverityCeiling label = case T.toUpper (T.strip label) of
    "NONE" -> Just 0.0
    "LOW" -> Just 3.9
    "MODERATE" -> Just 6.9
    "MEDIUM" -> Just 6.9
    "HIGH" -> Just 8.9
    "CRITICAL" -> Just 10.0
    _ -> Nothing

-- | Emit active rows, dropping orderable points covered by the same advisory. Unknown ecosystems retain every point.
extractFromAdvisory :: EpssScores -> OsvAdvisory -> [ExtractedOsv]
extractFromAdvisory scores adv = do
    guard (isNothing (osvWithdrawn adv))
    aff <- fromMaybe [] (osvAffected adv)
    let pkg = affectedPackage aff
        eco = find ((== packageEcosystem pkg) . osvExportDirectory . osvEcosystemFor) universe
        name = maybe id canonicalise eco (packageName pkg)
    let samePackage other =
            let otherPkg = affectedPackage other
             in packageEcosystem otherPkg == packageEcosystem pkg
                    && maybe id canonicalise eco (packageName otherPkg) == name
        ranges = concatMap rangeSegments (filter samePackage (fromMaybe [] (osvAffected adv)))
    Segment intro upper <- affectedSegments eco ranges aff
    pure $
        ExtractedOsv
            { extPackage = name
            , extEcosystem = packageEcosystem pkg
            , extCveId = osvId adv
            , extIntroduced = intro
            , extUpperBound = upper
            , extSeverity = severity
            , extEpss = epss
            }
  where
    severity = advisorySeverity adv
    epss = epssForIds scores (osvId adv : fromMaybe [] (osvAliases adv))

-- | Whether every bound parses. Unorderable bounds cannot justify dropping an exact version.
orderableBounds :: Ecosystem -> ExtractedOsv -> Bool
orderableBounds eco = null . unorderableBounds eco

-- | The bounds this segment carries that the ecosystem's version grammar cannot parse.
unorderableBounds :: Ecosystem -> ExtractedOsv -> [Text]
unorderableBounds eco osv = unorderableSegmentBounds eco (Segment (extIntroduced osv) (extUpperBound osv))

unorderableSegmentBounds :: Ecosystem -> Segment -> [Text]
unorderableSegmentBounds eco (Segment introduced upper) = filter (not . parses) (catMaybes [introduced, upperBound upper])
  where
    parses = isRight . parseVersionKey eco

    upperBound = \case
        FixedBefore f -> Just f
        LastAffected la -> Just la
        Unbounded -> Nothing

-- One affected interval: an inclusive lower bound and where it closes.
data Segment = Segment (Maybe Text) UpperBound

-- OSV's introduced "0" means no lower bound. An enumerated "0" remains a version.
rangeSegment :: Maybe Text -> UpperBound -> Segment
rangeSegment introduced = Segment (introduced >>= beyondTheBeginning)
  where
    beyondTheBeginning i = if i == "0" then Nothing else Just i

affectedSegments :: Maybe Ecosystem -> [Segment] -> OsvAffected -> [Segment]
affectedSegments eco ranges aff =
    rangeSegments aff <> [Segment (Just v) (LastAffected v) | v <- fromMaybe [] (affectedVersions aff), not (covered v)]
  where
    covered = case eco of
        Nothing -> const False
        Just ecosystem ->
            let orderable = filter (null . unorderableSegmentBounds ecosystem) ranges
                rows = [AdvisoryRange "" Nothing introduced upper Nothing | Segment introduced upper <- orderable]
                advisories = packageAdvisories ecosystem rows
             in \v ->
                    let version = mkVersion ecosystem v
                     in isJust (versionKey version) && not (null (affecting advisories version))

rangeSegments :: OsvAffected -> [Segment]
rangeSegments = maybe [] (concatMap (extractRange . rangeEvents) . filter versionTyped) . affectedRanges
  where
    -- Git commits are not version bounds and cannot cover an enumerated release.
    versionTyped r = T.toUpper (T.strip (rangeType r)) `elem` ["SEMVER", "ECOSYSTEM"]

-- An introduced event closes an already-open interval as unbounded.
extractRange :: [OsvEvent] -> [Segment]
extractRange = go Nothing
  where
    go Nothing [] = []
    go (Just i) [] = [rangeSegment (Just i) Unbounded]
    go current (e : es)
        | Just i <- eventIntroduced e =
            case current of
                Just prev -> rangeSegment (Just prev) Unbounded : go (Just i) es
                Nothing -> go (Just i) es
        | Just f <- eventFixed e = rangeSegment current (FixedBefore f) : go Nothing es
        | Just la <- eventLastAffected e = rangeSegment current (LastAffected la) : go Nothing es
        | otherwise = go current es

-- | Build the ecosystem archive URL under a configured OSV export base.
osvExportUrl :: Text -> Text -> String
osvExportUrl baseUrl ecosystem = toString (joinUrlPath baseUrl (ecosystem <> "/all.zip"))
