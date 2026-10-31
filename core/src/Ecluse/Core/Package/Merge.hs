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
    withoutTrustedReleases,

    -- * The merge accumulator
    -- $accumulator
    Merge,
    contribute,
    planFrom,
) where

import Data.Functor.Classes (liftEq)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (UTCTime)

import Ecluse.Core.Package (
    Artifact (..),
    Hash,
    HashAlg (SRI),
    PackageDetails (..),
    PackageInfo (..),
    PackageName,
    hashAlg,
    hashValue,
    sriBody,
 )
import Ecluse.Core.Package.Entry (AdmittedEntry (..))
import Ecluse.Core.Package.Hash (canonicalHashValue)
import Ecluse.Core.Package.Integrity (assertedAlg)
import Ecluse.Core.Snapshot (ContentDigest, Snapshot (..))
import Ecluse.Core.Version (Version, releaseIdentity, renderVersion, selectLatest)

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
    , -- Lazy, as are 'mpArtifacts' and 'mpTime': only the render reads them, so a 304 or an assembled hit builds none.
      mpDistTags :: ~(Map Text Version)
    {- ^ @dist-tags@ reconciled over the survivors. @latest@ comes from the public tag or the
    ordering, never the private one. Other tags carry by precedence, and an absent target drops.
    -}
    , mpArtifacts :: ~(Map Text (NonEmpty AdmittedEntry))
    -- ^ Exact admitted entries from each version's winning source snapshot.
    , mpTime :: ~(Map Text UTCTime)
    -- ^ Publish times from winning candidates. A winner with no known time contributes no entry.
    , mpDivergences :: Set Divergence
    {- ^ Every distinct same-version integrity conflict: the winner's fingerprint against each
    fingerprint that contradicts it on a shared algorithm. Differing algorithm sets do not count.
    -}
    }
    deriving stock (Eq, Show)

-- | Sorted file, asserted algorithm, and diagnostic digest triples. Only shared file/algorithm keys can contradict.
newtype IntegrityFingerprint = IntegrityFingerprint [(Text, Maybe HashAlg, Text)]
    deriving stock (Eq, Ord, Show)

-- | Sorted filename, algorithm, and original digest-body triples, including duplicates.
integrityHashes :: IntegrityFingerprint -> [(Text, Maybe HashAlg, Text)]
integrityHashes (IntegrityFingerprint hs) = hs

data Candidate = Candidate
    { candProvenance :: Provenance
    , candSourceId :: SourceId
    , -- Lazy: forced only for a colliding version whose copies spell their digests differently.
      candFingerprint :: ~IntegrityFingerprint
    , candDetails :: PackageDetails
    , candSnapshot :: ContentDigest
    }
    deriving stock (Show)

-- Eq and Ord both go through this key. 'candDetails' is left out: a 'SourceId' is unique per call,
-- so two contributions agreeing on rank and integrity are the same candidate.
candKey :: Candidate -> ((Provenance, SourceId), IntegrityFingerprint)
candKey c = ((candProvenance c, candSourceId c), candFingerprint c)

instance Eq Candidate where
    a == b = candKey a == candKey b

instance Ord Candidate where
    compare a b = compare (candKey a) (candKey b)

data Ranked a = Ranked
    { rankedRank :: (Provenance, SourceId)
    , rankedValue :: a
    }
    deriving stock (Eq, Show)

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
'mergePackuments' equals 'planFrom' over a fold of each input's 'contribute' into the lawful 'Merge'
'Monoid'. 'Merge' is opaque, so a 'SourceId' always names a real input position.
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
        unionMerge
            a
            b
                { mergeVersions = fmap (Set.map shiftCandidate) (mergeVersions b)
                , mergeDistTags = shiftRanked <$> mergeDistTags b
                , mergePublicLatest = shiftRanked <$> mergePublicLatest b
                }
      where
        offset = mergeCount a
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

-- The operands must already hold disjoint 'SourceId's.
unionMerge :: Merge -> Merge -> Merge
unionMerge a b =
    Merge
        { mergeCount = mergeCount a + mergeCount b
        , mergeVersions = Map.unionWith Set.union (mergeVersions a) (mergeVersions b)
        , mergeDistTags = Map.unionWith keepBetter (mergeDistTags a) (mergeDistTags b)
        , mergePublicLatest = keepBetterOf (mergePublicLatest a) (mergePublicLatest b)
        , mergeName = mergeName a <|> mergeName b
        }

-- | One input's contribution at local 'SourceId' @0@. The 'Semigroup' shifts it to its input position.
contribute :: Provenance -> Snapshot PackageInfo -> Merge
contribute = contributeAt 0

contributeAt :: SourceId -> Provenance -> Snapshot PackageInfo -> Merge
contributeAt sourceId prov (Snapshot digest info) =
    Merge
        { mergeCount = 1
        , mergeVersions = Map.map candidateFor (infoVersions info)
        , mergeDistTags = Map.map (Ranked here) (infoDistTags info)
        , mergePublicLatest = Ranked here <$> publicLatest
        , mergeName = Just (infoName info)
        }
  where
    here = (prov, sourceId)
    publicLatest = case prov of
        GatedSource -> Map.lookup "latest" (infoDistTags info)
        TrustedSource -> Nothing
    candidateFor details =
        Set.singleton
            Candidate
                { candProvenance = prov
                , candSourceId = sourceId
                , candFingerprint = fingerprint details
                , candDetails = details
                , candSnapshot = digest
                }

{- | Merge admitted snapshots with trusted-source precedence and divergence reporting.
Empty input yields 'Nothing'.
-}
mergePackuments :: [(Provenance, Snapshot PackageInfo)] -> Maybe MergePlan
mergePackuments [] = Nothing
mergePackuments (input : inputs) = planFrom (foldl' appendInput (uncurry contribute input) inputs)

-- Contributes the next input at its final 'SourceId', so the union shifts no candidate.
appendInput :: Merge -> (Provenance, Snapshot PackageInfo) -> Merge
appendInput acc (prov, snapshot) = unionMerge acc (contributeAt (mergeCount acc) prov snapshot)

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
            , mpDistTags = reconciledTags acc
            , mpTime = Map.mapMaybe (pkgPublishedAt . candDetails . winnerOf) (mergeVersions acc)
            , mpDivergences = divergencesOf (mergeVersions acc)
            }

-- The minimum by rank. A key always has at least one candidate, so 'Set.findMin' is total here.
winnerOf :: Set Candidate -> Candidate
winnerOf = Set.findMin

admittedEntries :: Candidate -> NonEmpty AdmittedEntry
admittedEntries candidate =
    fmap
        (\artifact -> AdmittedEntry (candSnapshot candidate) (artEntryKey artifact) (artFilename artifact))
        (pkgArtifacts (candDetails candidate))

-- Divergence is a property of the /set/ of distinct fingerprints offered for a key, never of a
-- pairwise fold step. That keeps it order-independent and associative for 3+ sources.
divergencesOf :: Map Text (Set Candidate) -> Set Divergence
divergencesOf versions =
    Set.fromList
        [ Divergence{divVersion = key, divWinning = candFingerprint winner, divLosing = candFingerprint loser}
        | (key, cs) <- Map.toList versions
        , Set.size cs > 1 -- A version with one copy stops here, with nothing built: the common serve.
        , Just (winner, losers) <- [Set.minView cs]
        , let winningDigests = digestsByKey (candDetails winner)
        , loser <- Set.toList losers
        , not (digestsSpelledAlike (candDetails winner) (candDetails loser))
        , candFingerprint loser /= candFingerprint winner
        , contradicts winningDigests (digestsByKey (candDetails loser))
        ]

-- The accumulator has already resolved same-tag collisions by provenance, so a carried tag never
-- depends on the order the caller passed the inputs.
reconciledTags :: Merge -> Map Text Version
reconciledTags acc = case selectLatest chosenLatest (map pkgVersion survivingDetails) of
    Nothing -> Map.delete "latest" carried
    Just v -> Map.insert "latest" v carried
  where
    carried = Map.filter (survives . renderVersion) (Map.map rankedValue (mergeDistTags acc))
    survives key = Map.member key (mergeVersions acc)
    survivingDetails = [candDetails (winnerOf cs) | cs <- Map.elems (mergeVersions acc)]

    -- A private document, mirror store or not, is never authoritative for @latest@, so
    -- 'selectLatest' projects over the survivors from the public tag alone.
    chosenLatest = rankedValue <$> mergePublicLatest acc

{- | Drop gated versions that a trusted version claims under another spelling of the same release.
An exact key collision stays, so precedence and divergence reporting still see it.
-}
withoutTrustedReleases :: Map Text PackageDetails -> PackageInfo -> PackageInfo
withoutTrustedReleases trusted info = info{infoVersions = Map.filterWithKey kept (infoVersions info)}
  where
    claimed = Set.fromList (map (releaseIdentity . pkgVersion) (Map.elems trusted))
    kept key details = Map.member key trusted || Set.notMember (releaseIdentity (pkgVersion details)) claimed

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
        , contradicts (digestsByKey privateDetails) (digestsByKey publicDetails)
        ]

-- Each file's name and hashes as written and in listed order, so a reordered copy falls through to
-- both tests. Alike copies have equal fingerprints, which the first test would reject at more cost.
digestsSpelledAlike :: PackageDetails -> PackageDetails -> Bool
digestsSpelledAlike a b = liftEq sameFile (pkgArtifacts a) (pkgArtifacts b)
  where
    sameFile x y = artFilename x == artFilename y && artHashes x == artHashes y

-- Diagnostics retain the existing digest spelling, sorted order, and duplicate entries.
fingerprint :: PackageDetails -> IntegrityFingerprint
fingerprint =
    IntegrityFingerprint
        . sort
        . concatMap artHashPairs
        . toList
        . pkgArtifacts
  where
    artHashPairs art = [(artFilename art, assertedAlg h, diagnosticBody h) | h <- artHashes art]

diagnosticBody :: Hash -> Text
diagnosticBody h = case hashAlg h of
    SRI -> sriBody (hashValue h)
    _ -> hashValue h

-- Record updates can bypass 'mkHash'. Invalid text remains distinct comparison evidence.
comparableBody :: Hash -> Text
comparableBody h = fromMaybe (hashValue h) (canonicalHashValue h)

type DigestSets = Map (Text, Maybe HashAlg) (Set Text)

digestsByKey :: PackageDetails -> DigestSets
digestsByKey details =
    Map.fromListWith
        Set.union
        [ ((artFilename art, assertedAlg h), Set.singleton (comparableBody h))
        | art <- toList (pkgArtifacts details)
        , h <- artHashes art
        ]

-- An omitted file or algorithm makes no conflicting claim. Shared keys require complete set equality.
contradicts :: DigestSets -> DigestSets -> Bool
contradicts a b = or (Map.intersectionWith (/=) a b)
