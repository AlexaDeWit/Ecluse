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

{- | One entry of an advisory's @severity@ array: a scoring-system tag (for
example @CVSS_V3@) and its value. For the CVSS systems the value is the
/vector string/, not a number; the numeric base score is computed from it
('advisorySeverity').
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
    {- ^ Exact affected versions enumerated outside any range. A real and common
    OSV shape (much of the npm malware feed names the single bad version here
    with no @ranges@ at all); each is an affected point in its own right.
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

{- | One event in a range's ordered event list. An event carries exactly one
bound: @introduced@ opens the affected interval (inclusive), @fixed@ closes it
below the fix (exclusive), and @last_affected@ closes it at an inclusive upper
bound. The two upper bounds are genuinely different -- @fixed 2.0@ excludes
@2.0@, @last_affected 2.0@ includes it -- so they are decoded and carried
separately.
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

{- | One affected segment of one package, flattened for storage: the advisory
identity and severity carried alongside the interval bounds. Each 'ExtractedOsv'
becomes a row of the artifact's ranges table.

The bounds mirror OSV's own model: 'extIntroduced' is the inclusive lower bound
('Nothing' == from the beginning); the upper bound is @'extFixed'@ (exclusive) or
@'extLastAffected'@ (inclusive) or neither (open-ended). An exact enumerated
version becomes a point segment (@introduced == last_affected == v@).
-}
data ExtractedOsv = ExtractedOsv
    { extPackage :: Text
    , extEcosystem :: Text
    , extCveId :: Text
    , extIntroduced :: Maybe Text
    , extFixed :: Maybe Text
    , extLastAffected :: Maybe Text
    , extSeverity :: Maybe Double
    {- ^ The advisory's CVSS base score (0 to 10), carried onto each of its
    segments; 'Nothing' when the advisory is unscored (much of the npm malware
    feed). See 'advisorySeverity'.
    -}
    }
    deriving stock (Show, Eq)

{- | The advisory's CVSS base score, normalised to a number at ingest so the
stored artifact holds a single comparable form and the reader needs no parsing.

OSV carries severity as a CVSS /vector string/, not a number, so the score is
computed from it with the "Security.CVSS" library (the highest, when several
vectors parse). When no vector parses, the source database's qualitative label
('dbsSeverity') is mapped to its band ceiling (@ghsaSeverityCeiling@). 'Nothing'
when the advisory offers neither.
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

-- CVSS base scores are defined to one decimal place; round the library's 'Float' to
-- that precision in 'Double' space, so the stored value is the canonical one-decimal
-- 'Double' (exact to compare) rather than a Float-to-Double widening artefact.
oneDecimal :: Float -> Double
oneDecimal f = fromIntegral (round (realToFrac f * 10 :: Double) :: Integer) / 10

-- GitHub's qualitative severity label mapped to the ceiling of its CVSS v3 band --
-- the highest score it could denote, so a coarse label is never under-counted past
-- a downstream deny threshold. GHSA labels are GitHub's own taxonomy (note
-- @MODERATE@, where CVSS says @Medium@), which the CVSS library does not parse, so
-- this small bridge is the irreducible remainder. 'Nothing' for an unknown label.
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

-- The affected segments of one package entry: the segments carved from each
-- __version-typed__ range's event list, plus one point segment per
-- exactly-enumerated version.
affectedSegments :: OsvAffected -> [Segment]
affectedSegments aff =
    maybe [] (concatMap (extractRange . rangeEvents) . filter versionTyped) (affectedRanges aff)
        <> maybe [] (map exactVersion) (affectedVersions aff)
  where
    exactVersion v = Segment (Just v) Nothing (Just v)

    -- OSV defines three range types (@SEMVER@, @ECOSYSTEM@, @GIT@); only the first
    -- two carry version-string bounds this model can order with
    -- 'Ecluse.Core.Version.compareVersions'. A @GIT@ range's events are __commit
    -- identifiers__, not versions, so carving them into segments would store a
    -- commit hash as a version bound; the downstream matcher ('insideAffectedRange')
    -- then fails that unparseable bound __closed to affected__, so a single @GIT@
    -- range (@introduced: "0"@, @fixed: \<sha\>@) would flag /every/ version of a
    -- healthy package as CVE-affected and quarantine it wholesale. Such a range
    -- expresses no npm-version constraint at all, so it must contribute nothing; a
    -- genuinely version-affecting npm advisory always carries an @ECOSYSTEM@ (or
    -- @SEMVER@) range. The type match is case-folded so a mixed-case producer's
    -- version range is still honoured rather than silently dropped.
    versionTyped :: OsvRange -> Bool
    versionTyped r = T.toUpper (T.strip (rangeType r)) `elem` ["SEMVER", "ECOSYSTEM"]

{- | Carve a range's ordered events into affected segments. An @introduced@ opens
a segment; a @fixed@ or @last_affected@ closes it; an @introduced@ that arrives
with one already open closes the open one as unbounded first. A segment still open
at the end is unbounded above.
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
