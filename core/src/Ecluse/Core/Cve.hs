-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Read one synced advisory artifact through a pinned lookup capability.
Package keys are canonical per ecosystem.
-}
module Ecluse.Core.Cve (
    -- * The opened artifact
    CveDb (..),
    openCveDb,
    CveDbRejected (..),

    -- * The consumer view
    CveLookup (..),
    AdvisoryRange (..),
    CveQueryFault (..),

    -- * Pure range matching
    PackageAdvisories,
    packageAdvisories,
    keepAdvisories,
    affecting,
    fixedAt,
    MissingScorePolicy (..),
    scoreAtLeast,
) where

import Data.Map.Strict qualified as Map
import UnliftIO.Exception (catch, catchAny, onException, throwIO)

import Ecluse.Core.Cve.Internal (AdvisoryRange (..), CveDbRejected (..), advisoriesQuery, coveredNamesQuery, openHardenedConnection, provenanceQuery)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Osv.Provenance (AdvisoryProvenance, decodeProvenance)
import Ecluse.Core.Osv.Schema (EpssRequirement)
import Ecluse.Core.Osv.Types (UpperBound (..))
import Ecluse.Core.Version (Version, VersionKey, parseVersionKey, renderVersion, versionKeyIn)

import Database.SQLite.Simple (Connection, SQLError, close)

{- | Query canonical package keys, with npm scopes inline, and raw version strings.
Display names must not be used as query keys.
-}
data CveLookup = CveLookup
    { cveAdvisoriesFor :: Text -> IO [AdvisoryRange]
    {- ^ Every advisory range recorded against a package name, for a rule predicate to read.
    Throws the confined 'CveQueryFault' on a query fault, as every field here does.
    -}
    , cveCoveredNames :: IO [Text]
    -- ^ Every package name this generation records an advisory against, for a store sweep.
    }

-- | A database query fault for the rule's resilience policy to classify.
data CveQueryFault = CveQueryFault
    { cqfQuery :: Text
    -- ^ Which handle field was asked (@advisories-for@ or @covered-names@).
    , cqfDetail :: Text
    -- ^ The rendered 'SQLError', for the operator's outage report. Never parsed.
    }
    deriving stock (Eq, Show)

instance Exception CveQueryFault

{- | One opened artifact: the consumer view plus the owner's close. Whoever holds
this owns the connection's lifetime. Hand a consumer 'cveDbLookup' only.
-}
data CveDb = CveDb
    { cveDbLookup :: CveLookup
    -- ^ The view consumers query through.
    , cveDbClose :: IO ()
    {- ^ Release the connection. Owner-only, and __never throws__, since the connection is
    going away either way.
    -}
    , cveDbMeta :: [(Text, Text)]
    -- ^ The artifact's @meta@ provenance rows, snapshotted at open and key-sorted for the audit trail.
    , cveDbProvenance :: AdvisoryProvenance
    {- ^ What the artifact records about the sources it was compiled from. An artifact written
    before those keys decodes as absence.
    -}
    }

-- | Reject incompatible artifacts as values. Opening faults leave no connection behind.
openCveDb :: Ecosystem -> EpssRequirement -> FilePath -> IO (Either CveDbRejected CveDb)
openCveDb eco epssRequirement dbFile =
    openHardenedConnection eco epssRequirement dbFile >>= \case
        Left rejection -> pure (Left rejection)
        Right conn -> do
            -- No-leak backstop for a fault below the artifact contract. Acceptance already made the
            -- provenance decode itself total.
            meta <- provenanceQuery conn `onException` close conn
            pure (Right (mkCveDb conn meta))

mkCveDb :: Connection -> [(Text, Text)] -> CveDb
mkCveDb conn meta =
    CveDb
        { cveDbLookup =
            CveLookup
                { cveAdvisoriesFor = taggedQuery "advisories-for" . advisoriesQuery conn
                , cveCoveredNames = taggedQuery "covered-names" (coveredNamesQuery conn)
                }
        , cveDbClose = close conn `catchAny` const pass
        , cveDbMeta = meta
        , cveDbProvenance = decodeProvenance meta
        }

-- The SQLite edge: the driver's 'SQLError' never escapes the handle, only this module's
-- confined 'CveQueryFault'.
taggedQuery :: Text -> IO a -> IO a
taggedQuery tag act = act `catch` \(err :: SQLError) -> throwIO (CveQueryFault tag (show err))

{- | One package's advisory segments, each with its bounds parsed once under the package's
ecosystem, so every version of the package tests against ordering keys.
-}
data PackageAdvisories = PackageAdvisories
    { paEcosystem :: Ecosystem
    , paSegments :: [Segment]
    , -- Lazy: only the remediation rule reads it, so a deny rule's filtered copy never builds it.
      paFixes :: ~(Map Text [AdvisoryRange])
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

-- The fixes index keeps row order within each fixed version.
fromSegments :: Ecosystem -> [Segment] -> PackageAdvisories
fromSegments eco segments =
    PackageAdvisories
        { paEcosystem = eco
        , paSegments = segments
        , paFixes = Map.fromListWith (flip (<>)) [(fixed, [segRange s]) | s <- segments, FixedBefore fixed <- [arUpperBound (segRange s)]]
        }

{- | The rows whose fixed bound is this version's exact text, in row order. A row with a fixed bound
always decodes to 'FixedBefore', so this is an exact match on the artifact's @fixed_version@.
-}
fixedAt :: PackageAdvisories -> Version -> [AdvisoryRange]
fixedAt advisories version = Map.findWithDefault [] (renderVersion version) (paFixes advisories)

{- | The rows whose affected interval holds a version of the package, in row order. __Fail-closed:__
an unprovable comparison counts as __inside__, bar a point the grammar cannot order.
-}
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
