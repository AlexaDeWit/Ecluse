-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Merge admitted source snapshots with trusted-source precedence.
The plan carries exact artifact coordinates for raw assembly and reports integrity divergence.
-}
module Ecluse.Core.Package.Merge (
    -- * Provenance
    Provenance (..),

    -- * Merging
    SourceId,
    MergePlan (..),
    Divergence (..),
    IntegrityFingerprint,
    integrityHashes,
    integrityDivergences,
    mergePackuments,

    -- * The merge accumulator
    -- $accumulator
    Merge,
    contribute,
    planFrom,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (UTCTime)

import Ecluse.Core.Package (
    Artifact (..),
    Hash,
    HashAlg,
    PackageDetails (..),
    PackageInfo (..),
    PackageName,
    hashValue,
 )
import Ecluse.Core.Package.Entry (AdmittedEntry (..))
import Ecluse.Core.Package.Hash (canonicalHashValue)
import Ecluse.Core.Package.Integrity (assertedAlg)
import Ecluse.Core.Snapshot (ContentDigest, Snapshot (..))
import Ecluse.Core.Version (Version, renderVersion, selectLatest)

{- | Which upstream a document came from. The caller decides this and applies it before merging.
'Ord' is the trust order: 'TrustedSource' sorts before 'GatedSource', so the smallest wins.
-}
data Provenance
    = {- | A private-upstream document. Its versions are already vetted, so they
      enter the union unfiltered and win any collision.
      -}
      TrustedSource
    | {- | A public-upstream document. Its versions are the set that already
      survived the rules engine. The merge unions them but never re-filters.
      -}
      GatedSource
    deriving stock (Eq, Ord, Show)

{- | The 0-based index of an input to one 'mergePackuments' call. The caller pairs each 'SourceId'
back to the raw @Value@ it passed at that position, which 'Provenance' alone cannot name.
-}
type SourceId = Int

-- | Conflicting digests for one version. The private copy wins and both fingerprints remain for alarms.
data Divergence = Divergence
    { divVersion :: Text
    {- ^ The raw version-string key the conflict was found at (the
    'Ecluse.Core.Package.infoVersions' key).
    -}
    , divWinning :: IntegrityFingerprint
    -- ^ Integrity of the copy that won the merge (the higher-precedence source).
    , divLosing :: IntegrityFingerprint
    -- ^ Integrity of the copy that lost, kept so the conflict is auditable.
    }
    deriving stock (Eq, Ord, Show)

{- | The decisions a merge reached over several upstream packuments. The serve layer replays the
plan onto the raw upstream @Value@s. It is never a finished, re-serialisable document.
-}
data MergePlan = MergePlan
    { mpName :: PackageName
    {- ^ The package identity, carried from the contributions. A check upstream of the merge drops
    any contribution whose name disagrees, so this is never a substituted or manufactured value.
    -}
    , mpSurvivors :: Map Text SourceId
    {- ^ Each surviving version key mapped to the 'SourceId' of the input that won it. Trusted wins
    a collision. The serve layer takes that version's object from that source's raw @Value@.
    -}
    , mpDistTags :: Map Text Version
    {- ^ @dist-tags@ reconciled over the survivors. @latest@ comes from the public tag or the
    ordering, never the private one. Other tags carry by precedence, and an absent target drops.
    -}
    , mpArtifacts :: Map Text (NonEmpty AdmittedEntry)
    -- ^ Exact admitted entries from each version's winning source snapshot.
    , mpTime :: Map Text UTCTime
    -- ^ Publish times from winning candidates. A winner with no known time contributes no entry.
    , mpDivergences :: Set Divergence
    {- ^ Every distinct same-version integrity conflict: the winner's fingerprint against each
    fingerprint that contradicts it on a shared algorithm. Differing algorithm sets do not count.
    -}
    }
    deriving stock (Eq, Show)

-- | Distinct sorted file, asserted algorithm, and canonical digest triples. Only shared file/algorithm keys can contradict.
newtype IntegrityFingerprint = IntegrityFingerprint [(Text, Maybe HashAlg, Text)]
    deriving stock (Eq, Ord, Show)

-- | Distinct sorted filename, algorithm, and lowercase hex triples. Invalid record updates retain their raw text.
integrityHashes :: IntegrityFingerprint -> [(Text, Maybe HashAlg, Text)]
integrityHashes (IntegrityFingerprint hs) = hs

rank :: Provenance -> SourceId -> (Provenance, SourceId)
rank prov sid = (prov, sid)

data Candidate = Candidate
    { candProvenance :: Provenance
    , candSourceId :: SourceId
    , candFingerprint :: ~IntegrityFingerprint
    , -- Fingerprints are forced only for colliding versions because ranks are unique.
      candDetails :: PackageDetails
    , candSnapshot :: ContentDigest
    }
    deriving stock (Show)

-- Eq and Ord both go through this key. 'candDetails' is deliberately excluded: a 'SourceId' is
-- unique per call, so two contributions agreeing on rank and integrity are the same candidate.
candKey :: Candidate -> ((Provenance, SourceId), IntegrityFingerprint)
candKey c = (rank (candProvenance c) (candSourceId c), candFingerprint c)

instance Eq Candidate where
    a == b = candKey a == candKey b

instance Ord Candidate where
    compare a b = compare (candKey a) (candKey b)

-- Ordering ignores the value so precedence resolves collisions before content.
data Ranked a = Ranked
    { rankedRank :: (Provenance, SourceId)
    , rankedValue :: a
    }
    deriving stock (Eq, Show)

instance (Eq a) => Ord (Ranked a) where
    compare a b = compare (rankedRank a) (rankedRank b)

-- Keeps the higher-precedence (smaller-rank) value. Associative and commutative, so a
-- 'Map.unionWith' over it resolves a key's collision independent of input order.
keepBetter :: Ranked a -> Ranked a -> Ranked a
keepBetter x y = if rankedRank x <= rankedRank y then x else y

-- 'keepBetter' where either side may be absent, so a source that offered nothing never
-- displaces one that did.
keepBetterOf :: Maybe (Ranked a) -> Maybe (Ranked a) -> Maybe (Ranked a)
keepBetterOf (Just x) (Just y) = Just (keepBetter x y)
keepBetterOf x y = x <|> y

{- $accumulator
The merge folds each input's 'contribute' into the lawful 'Merge' 'Monoid', which 'planFrom' then
projects to a 'MergePlan'. 'Merge' is opaque, so a 'SourceId' always names a real input position.
-}

{- | The monoidal accumulator the merge folds into. It leaves every version key's candidates
unresolved, because a pairwise winner decision during the fold is not associative for 3+ copies.
-}
data Merge = Merge
    { mergeCount :: Int
    -- ^ How many inputs this accumulator represents (the next free 'SourceId').
    , mergeVersions :: Map Text (Set Candidate)
    -- ^ Every candidate offered for each version key, unresolved.
    , mergeDistTags :: Map Text (Ranked Version)
    -- ^ The precedence-winning @dist-tags@ target offered for each tag.
    , mergePublicLatest :: Maybe (Ranked Version)
    {- ^ The @latest@ a 'GatedSource' offered, held apart because the 'mergeDistTags' union
    resolves every tag to the trusted source.
    -}
    , mergeName :: Maybe PackageName
    {- ^ The package identity. Every contribution carries the same name, because a check upstream
    of the merge validates each one against the requested name. 'Nothing' only for 'mempty'.
    -}
    }
    deriving stock (Eq, Show)

-- Source IDs follow input positions, so regrouping preserves the result but permutation changes labels.
instance Semigroup Merge where
    a <> b =
        Merge
            { mergeCount = mergeCount a + mergeCount b
            , mergeVersions =
                Map.unionWith Set.union (mergeVersions a) (shiftVersions (mergeVersions b))
            , mergeDistTags =
                Map.unionWith keepBetter (mergeDistTags a) (shiftRanked <$> mergeDistTags b)
            , mergePublicLatest =
                keepBetterOf (mergePublicLatest a) (shiftRanked <$> mergePublicLatest b)
            , mergeName = mergeName a <|> mergeName b
            }
      where
        -- Re-index the right operand's SourceIds past the left operand's inputs, so a fold of
        -- single-input contributions lands each at its list index.
        offset = mergeCount a
        shiftVersions = fmap (Set.map shiftCandidate)
        shiftCandidate c = c{candSourceId = candSourceId c + offset}
        shiftRanked (Ranked (prov, sid) v) = Ranked (prov, sid + offset) v

instance Monoid Merge where
    mempty =
        Merge
            { mergeCount = 0
            , mergeVersions = Map.empty
            , mergeDistTags = Map.empty
            , mergePublicLatest = Nothing
            , mergeName = Nothing
            }

{- | One input's contribution to the accumulator, at local 'SourceId' @0@. The 'Semigroup' offset
re-indexes it to the input's position when 'mergePackuments' folds over the inputs.
-}
contribute :: Provenance -> Snapshot PackageInfo -> Merge
contribute prov (Snapshot digest info) =
    Merge
        { mergeCount = 1
        , mergeVersions = Map.map candidateFor (infoVersions info)
        , mergeDistTags = Map.map (Ranked here) (infoDistTags info)
        , mergePublicLatest = Ranked here <$> publicLatest
        , mergeName = Just (infoName info)
        }
  where
    -- Local SourceId 0. The Semigroup offset re-indexes it to the input position.
    here = (prov, 0)
    publicLatest = case prov of
        GatedSource -> Map.lookup "latest" (infoDistTags info)
        TrustedSource -> Nothing
    candidateFor details =
        Set.singleton
            Candidate
                { candProvenance = prov
                , candSourceId = 0
                , candFingerprint = fingerprint details
                , candDetails = details
                , candSnapshot = digest
                }

{- | Merge admitted snapshots with trusted-source precedence and divergence reporting.
Empty input yields 'Nothing'.
-}
mergePackuments :: [(Provenance, Snapshot PackageInfo)] -> Maybe MergePlan
mergePackuments [] = Nothing
mergePackuments inputs = planFrom (foldMap (uncurry contribute) inputs)

{- | Project the resolved 'MergePlan' from a folded 'Merge'. It resolves each version key to its
precedence winner. 'Nothing' only for 'mempty', the empty merge, which has nothing to serve.
-}
planFrom :: Merge -> Maybe MergePlan
planFrom acc = do
    name <- mergeName acc
    pure
        MergePlan
            { mpName = name
            , mpSurvivors = Map.map (candSourceId . winnerOf) (mergeVersions acc)
            , mpArtifacts = Map.map (admittedEntries . winnerOf) (mergeVersions acc)
            , mpDistTags = reconciledTags
            , mpTime = reconciledTimes
            , mpDivergences = divergences
            }
  where
    -- The precedence winner among a key's candidates: the minimum by rank. A key always has at
    -- least one candidate, so 'Set.findMin' is total here.
    winnerOf :: Set Candidate -> Candidate
    winnerOf = Set.findMin

    admittedEntries candidate =
        fmap
            (\artifact -> AdmittedEntry (candSnapshot candidate) (artEntryKey artifact) (artFilename artifact))
            (pkgArtifacts (candDetails candidate))

    survives :: Text -> Bool
    survives key = Map.member key (mergeVersions acc)

    -- The surviving version objects (the details that won each key).
    survivingDetails :: [PackageDetails]
    survivingDetails =
        [candDetails (winnerOf cs) | cs <- Map.elems (mergeVersions acc)]

    -- Divergence is a property of the /set/ of distinct fingerprints offered for a key, never
    -- of a pairwise fold step. That keeps it order-independent and associative for 3+ sources.
    divergences :: Set Divergence
    divergences =
        Set.fromList
            [ Divergence{divVersion = key, divWinning = win, divLosing = lose}
            | (key, cs) <- Map.toList (mergeVersions acc)
            , Set.size cs > 1
            , let win = candFingerprint (winnerOf cs)
            , let distinct = Set.fromList [candFingerprint c | c <- Set.toList cs]
            , lose <- Set.toList distinct
            , contradicts win lose
            ]

    -- The accumulator has already resolved same-tag collisions by provenance, so the carried
    -- tags never depend on the order the caller passed the inputs.
    reconciledTags :: Map Text Version
    reconciledTags =
        let carried = Map.filter (survives . renderVersion) (Map.map rankedValue (mergeDistTags acc))
         in case resolvedLatest of
                Nothing -> Map.delete "latest" carried
                Just v -> Map.insert "latest" v carried

    -- 'selectLatest' owns the keep-or-repoint precedence.
    resolvedLatest :: Maybe Version
    resolvedLatest =
        selectLatest chosenLatest (map pkgVersion survivingDetails)

    -- The public document's @latest@ and nothing else: a private document, mirror store or not,
    -- is never authoritative here, so 'selectLatest' projects over the survivors without one.
    chosenLatest :: Maybe Version
    chosenLatest = rankedValue <$> mergePublicLatest acc

    -- Publish times retain the same source authority as the served manifest.
    reconciledTimes :: Map Text UTCTime
    reconciledTimes =
        Map.mapMaybe (pkgPublishedAt . candDetails . winnerOf) (mergeVersions acc)

{- | Compare integrity-admitted versions independently of rule eligibility, with the trusted map winning.
Inputs must share a validated package identity. Only shared version keys are compared.
-}
integrityDivergences :: Map Text PackageDetails -> Map Text PackageDetails -> Set Divergence
integrityDivergences trusted public =
    Set.fromList
        [ Divergence key win lose
        | (key, (privateDetails, publicDetails)) <- Map.toList (Map.intersectionWith (,) trusted public)
        , let win = fingerprint privateDetails
        , let lose = fingerprint publicDetails
        , contradicts win lose
        ]

-- Sorted triples make the comparison order-independent across artifacts and hashes. Keying by
-- 'assertedAlg', not the raw wrapper tag, compares what each digest claims about each file.
fingerprint :: PackageDetails -> IntegrityFingerprint
fingerprint =
    IntegrityFingerprint
        . Set.toAscList
        . Set.fromList
        . concatMap artHashPairs
        . toList
        . pkgArtifacts
  where
    artHashPairs art = [(artFilename art, assertedAlg h, comparableBody h) | h <- artHashes art]

-- Record updates can bypass 'mkHash'. Keep that text as diagnostic evidence when decoding fails.
comparableBody :: Hash -> Text
comparableBody h = fromMaybe (hashValue h) (canonicalHashValue h)

-- An omitted file or algorithm makes no conflicting claim.
contradicts :: IntegrityFingerprint -> IntegrityFingerprint -> Bool
contradicts a b =
    or (Map.intersectionWith (/=) (digestsByKey a) (digestsByKey b))
  where
    digestsByKey :: IntegrityFingerprint -> Map (Text, Maybe HashAlg) (Set Text)
    digestsByKey (IntegrityFingerprint triples) =
        Map.fromListWith Set.union [((file, alg), Set.singleton digest) | (file, alg, digest) <- triples]
