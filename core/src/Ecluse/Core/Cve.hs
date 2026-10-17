-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Read one synced advisory artifact through a pinned lookup capability.
Package keys are canonical per ecosystem.
-}
module Ecluse.Core.Cve (
    -- * The opened artifact
    CveDb (..),
    CveDbRejected (..),

    -- * The consumer view
    CveLookup (..),
    AdvisoryRange (..),
    CveQueryFault (..),

    -- * Pure range matching
    PackageAdvisories,
    packageAdvisories,
    keepAdvisories,
    hasAdvisories,
    affecting,
    hasFixes,
    fixedAt,
    advisoryIdsOf,
    MissingScorePolicy (..),
    scoreAtLeast,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Ecluse.Core.Cve.Internal (AdvisoryRange (..), CveDb (..), CveDbRejected (..), CveLookup (..), CveQueryFault (..))
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Osv.Types (UpperBound (..))
import Ecluse.Core.Version (Version, VersionKey, parseVersionKey, renderVersion, renderVersionKey, versionKeyIn)

-- | One package's advisory segments, each with its bounds parsed once under the package's ecosystem.
data PackageAdvisories = PackageAdvisories
    { paEcosystem :: Ecosystem
    , paSegments :: [Segment]
    , -- Lazy: only the remediation rule reads it, so a deny rule's filtered copy never builds it.
      paFixes :: ~(Map Text [AdvisoryRange])
    , -- Missing parsed fixes disable rejection because a parsed probe can render to their raw text.
      paFixCandidates :: ~(Maybe (Set VersionKey))
    }

-- One advisory row and the bounds matching reads from it.
data Segment = Segment
    { segRange :: AdvisoryRange
    , -- Lazy: parsed when a version is first matched, so a request that matches none parses nothing.
      segBounds :: ~SegmentBounds
    }

-- A bound the grammar cannot parse is no bound, so no version can be shown to be outside it.
data SegmentBounds
    = -- A point whose one string the grammar rejects, matched as text.
      OnlyText Text
    | -- The inclusive lower bound and the upper bound.
      Ordered (Maybe VersionKey) UpperKey

data UpperKey = Below VersionKey | AtMost VersionKey | NoUpper

-- | Parse every segment's bounds at most once, under the package's ecosystem.
packageAdvisories :: Ecosystem -> [AdvisoryRange] -> PackageAdvisories
packageAdvisories eco = fromSegments eco . map (\ar -> Segment ar (segmentBounds eco ar))

-- | Keep only the segments whose row passes the test.
keepAdvisories :: (AdvisoryRange -> Bool) -> PackageAdvisories -> PackageAdvisories
keepAdvisories keep advisories = fromSegments (paEcosystem advisories) (filter (keep . segRange) (paSegments advisories))

-- | Whether any segment remains. Without one, 'affecting' holds no row for any version.
hasAdvisories :: PackageAdvisories -> Bool
hasAdvisories = not . null . paSegments

-- | Whether any row names a fix. Without one, 'fixedAt' holds no row for any version.
hasFixes :: PackageAdvisories -> Bool
hasFixes = not . Map.null . paFixes

-- The fixes index keeps row order within each fixed version.
fromSegments :: Ecosystem -> [Segment] -> PackageAdvisories
fromSegments eco segments =
    PackageAdvisories
        { paEcosystem = eco
        , paSegments = segments
        , paFixes = Map.fromListWith (flip (<>)) [(fixedIdentity s fixed, [segRange s]) | (s, fixed) <- fixedSegments]
        , paFixCandidates = Set.fromList <$> traverse (fixedKey . fst) fixedSegments
        }
  where
    fixedSegments = [(s, fixed) | s <- segments, FixedBefore fixed <- [arUpperBound (segRange s)]]

-- Reuse the parsed upper bound. An unorderable fix retains its literal identity.
fixedIdentity :: Segment -> Text -> Text
fixedIdentity segment raw = maybe raw renderVersionKey (fixedKey segment)

fixedKey :: Segment -> Maybe VersionKey
fixedKey segment = case segBounds segment of
    Ordered _ (Below key) -> Just key
    _ -> Nothing

-- | Rows with this ecosystem's canonical exact fix identity, retaining order and duplicates.
fixedAt :: PackageAdvisories -> Version -> [AdvisoryRange]
fixedAt advisories version
    | Map.null fixes = []
    | Just key <- probeKey
    , Just candidates <- paFixCandidates advisories
    , Set.notMember key candidates =
        []
    | otherwise = Map.findWithDefault [] probeIdentity fixes
  where
    fixes = paFixes advisories
    probeKey = versionKeyIn (paEcosystem advisories) version
    probeIdentity = maybe (renderVersion version) renderVersionKey probeKey

-- | The rows whose interval holds the version, in row order. __Fail-closed:__ an unprovable comparison counts as inside, bar an unorderable point.
affecting :: PackageAdvisories -> Version -> [AdvisoryRange]
affecting advisories version = [segRange s | s <- paSegments advisories, holds (segBounds s)]
  where
    key = versionKeyIn (paEcosystem advisories) version
    holds = \case
        OnlyText only -> renderVersion version == only
        Ordered lower upper -> maybe True (\k -> atOrAbove k lower && withinUpper k upper) key

atOrAbove :: VersionKey -> Maybe VersionKey -> Bool
atOrAbove k = maybe True (k >=)

-- A fix is an exclusive upper bound and last_affected an inclusive one.
withinUpper :: VersionKey -> UpperKey -> Bool
withinUpper k = \case
    Below fixed -> k < fixed
    AtMost lastAffected -> k <= lastAffected
    NoUpper -> True

-- | The distinct advisory identifiers of these rows, in the order they first appear.
advisoryIdsOf :: [AdvisoryRange] -> [Text]
advisoryIdsOf = ordNub . map arCveId

{- OSV writes an enumerated version as introduced == last_affected. When the grammar rejects that
string, the segment names it literally, since nothing can order it against anything. -}
segmentBounds :: Ecosystem -> AdvisoryRange -> SegmentBounds
segmentBounds eco ar = case (arIntroduced ar, arUpperBound ar) of
    (Just introduced, LastAffected lastAffected)
        | introduced == lastAffected -> maybe (OnlyText introduced) (\k -> Ordered (Just k) (AtMost k)) introducedKey
    (_, upper) -> Ordered introducedKey (upperKey upper)
  where
    keyOf = rightToMaybe . parseVersionKey eco
    introducedKey = keyOf =<< arIntroduced ar
    upperKey = \case
        FixedBefore fixed -> maybe NoUpper Below (keyOf fixed)
        LastAffected lastAffected -> maybe NoUpper AtMost (keyOf lastAffected)
        Unbounded -> NoUpper

-- | Whether an individual absent score supplies threshold evidence.
data MissingScorePolicy
    = -- | CVSS keeps its denial for unscored advisories, including malware.
      DenyMissingScore
    | -- | EPSS requires a known score to supply a denial.
      AbstainMissingScore

-- | Compare a score with the deny threshold using the metric's missing-score policy.
scoreAtLeast :: MissingScorePolicy -> Double -> Maybe Double -> Bool
scoreAtLeast missing threshold = maybe absent (>= threshold)
  where
    absent = case missing of
        DenyMissingScore -> True
        AbstainMissingScore -> False
