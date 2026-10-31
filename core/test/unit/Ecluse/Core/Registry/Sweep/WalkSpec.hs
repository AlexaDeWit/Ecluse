-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Registry.Sweep.WalkSpec (spec) where

import Data.Conduit (ConduitT, fuseUpstream, yield)
import Data.Conduit.List qualified as CL
import Data.Text qualified as T
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Maintenance (
    DeleteCeiling (AtMost),
    StoreFault,
    chunksOfCeiling,
    protocolFault,
 )
import Ecluse.Core.Registry.Maintenance.NameSpace (
    NameAlphabet,
    NamePrefix,
    extendBucket,
    mkNameAlphabet,
    noNameAlphabet,
    parseNamePrefix,
    renderNamePrefix,
 )
import Ecluse.Core.Registry.PyPI.Project (pypiNameLeadChars)
import Ecluse.Core.Registry.RubyGems.Project (rubyGemsNameChars)
import Ecluse.Core.Registry.Sweep.Walk (
    BucketNames (BucketFaulted, BucketOverflowed, BucketRead, BucketUnsplittable),
    bucketDepthLimit,
    bucketNameBudget,
    collectBucketWith,
    resumeAfter,
    walkBuckets,
 )
import Ecluse.Test.Maintenance (withBucket)
import Ecluse.Test.Package (unscopedNpm)

spec :: Spec
spec = do
    sequenceSpec
    resumeSpec
    collectSpec

sequenceSpec :: Spec
sequenceSpec = describe "walkBuckets" $ do
    it "covers the alphabet's buckets in the alphabet's own order" $
        map renderNamePrefix (walkBuckets (mkNameAlphabet "abc")) `shouldBe` ["a", "b", "c"]

    it "covers a store its listing cannot partition as the one bucket that holds everything" $
        map renderNamePrefix (walkBuckets noNameAlphabet) `shouldBe` [""]

{- A restart re-does at most the bucket that was in flight, so resumption drops every bucket the
walk covers at or before the one the last run recorded. -}
resumeSpec :: Spec
resumeSpec = describe "resumeAfter" $ do
    it "starts from the first bucket when no walk is under way" $
        map renderNamePrefix (resumeAfter Nothing buckets) `shouldBe` ["a", "b", "c"]

    it "drops the recorded bucket and everything before it" $
        withBucket "b" $ \b ->
            map renderNamePrefix (resumeAfter (Just b) buckets) `shouldBe` ["c"]

    it "has nothing left after the last bucket, which is what ends a walk" $
        withBucket "c" $
            \c -> resumeAfter (Just c) buckets `shouldBe` []

    it "keeps the bucket a record falls inside, so a split walk does not skip its remainder" $
        -- A record of "ab" means the walk split "a" and got as far as "ab" inside it. Dropping
        -- "a" outright would skip "ac" onward and then clear the marker as though complete.
        withBucket "ab" $ \ab ->
            map renderNamePrefix (resumeAfter (Just ab) buckets) `shouldBe` ["a", "b", "c"]

    it "drops the completed narrower buckets when that kept bucket is split again" $
        -- The same record is applied to the narrower sequence, which is what makes keeping the
        -- parent safe: "aa" and "ab" are done, and the walk resumes at "ac".
        withBucket "ab" $ \ab ->
            map renderNamePrefix (resumeAfter (Just ab) (spelledBuckets ["aa", "ab", "ac"]))
                `shouldBe` ["ac"]

    -- Both alphabets walk digits after letters, although digits sort first.
    it "resumes a PyPI walk interrupted after z at the digit buckets" $
        withBucket "z" $ \z ->
            map renderNamePrefix (resumeAfter (Just z) (walkBuckets (mkNameAlphabet pypiNameLeadChars)))
                `shouldBe` map T.singleton ['0' .. '9']

    it "resumes a RubyGems walk interrupted after z at the digit and punctuation buckets" $
        withBucket "z" $ \z ->
            map renderNamePrefix (resumeAfter (Just z) (walkBuckets (mkNameAlphabet rubyGemsNameChars)))
                `shouldBe` map T.singleton (['0' .. '9'] <> "._-")

    it "resumes every interrupted walk at the bucket after the one it recorded, however the walk split" $
        hedgehog $ do
            chars <- forAll (Gen.shuffle "ab0._")
            oversized <- forAll (Gen.subsequence (map T.singleton chars <> [T.pack [outer, inner] | outer <- chars, inner <- chars]))
            let walked = completedBuckets chars oversized Nothing
            for_ (zip [1 ..] walked) $ \(done, record) ->
                completedBuckets chars oversized (Just record) === drop done walked
  where
    buckets = walkBuckets (mkNameAlphabet "abc")

-- The buckets a walk completes from a record, as the sweep splits each oversized bucket.
completedBuckets :: [Char] -> [Text] -> Maybe NamePrefix -> [NamePrefix]
completedBuckets chars oversized record = go (resumeAfter record (walkBuckets alphabet))
  where
    alphabet = mkNameAlphabet chars
    go = \case
        [] -> []
        bucket : rest
            | renderNamePrefix bucket `elem` oversized -> go (resumeAfter record (extendBucket alphabet bucket) <> rest)
            | otherwise -> bucket : go rest

collectSpec :: Spec
collectSpec = describe "collectBucketWith" $ do
    it "reads a bucket whole and sorts it, so the walk's order does not follow the store's" $ do
        outcome <- withBucket "a" $ \a -> collectBucket alphabet a (pagesOf [["apricot", "almond"], ["apple"]])
        namesOf outcome `shouldBe` Just (map unscopedNpm ["almond", "apple", "apricot"])

    it "reads an empty bucket as no names rather than as a fault" $ do
        outcome <- withBucket "z" $ \z -> collectBucket alphabet z (pagesOf [])
        namesOf outcome `shouldBe` Just []

    it "reports the fault a listing stopped on, and nothing it read before it" $ do
        outcome <- withBucket "a" $ \a -> collectBucket alphabet a (faultingAfter [["apple"]])
        case outcome of
            BucketFaulted _ -> pass
            _ -> expectationFailure "expected the bucket to report the listing's fault"

    it "abandons a bucket that outgrew the budget and gives the narrower buckets covering it" $ do
        -- The stream stops as soon as the budget is crossed, so an oversized bucket costs a
        -- partial listing rather than the whole of it.
        outcome <- withBucket "a" $ \a -> collectBucket (mkNameAlphabet "ab") a (pagesOf [oversized])
        case outcome of
            BucketOverflowed narrower -> map renderNamePrefix (toList narrower) `shouldBe` ["aa", "ab"]
            _ -> expectationFailure "expected the oversized bucket to be split"

    it "reads a bucket exactly at the budget rather than splitting it" $ do
        outcome <- withBucket "a" $ \a -> collectBucket alphabet a (pagesOf [atBudget])
        fmap length (namesOf outcome) `shouldBe` Just bucketNameBudget

    it "reads at the budget and splits past it, for pages of 1 to 1,000 names" $
        for_ [1, 7, 100, 999, 1000] $ \size -> do
            atLimit <- withBucket "a" $ \a -> collectBucket alphabet a (pagesOf (chunksOfCeiling (AtMost size) atBudget))
            fmap length (namesOf atLimit) `shouldBe` Just bucketNameBudget
            pastLimit <- withBucket "a" $ \a -> collectBucket alphabet a (pagesOf (chunksOfCeiling (AtMost size) oversized))
            case pastLimit of
                BucketOverflowed _ -> pass
                _ -> expectationFailure ("expected the oversized bucket to be split at a page size of " <> show size)

    it "reports an oversized store whose listing cannot partition" $ do
        outcome <- withBucket "" $ \everything -> collectBucket noNameAlphabet everything (pagesOf [oversized])
        case outcome of
            BucketUnsplittable -> pass
            _ -> expectationFailure "expected an incomplete unsplittable inventory"

    it "reports a bucket that outgrew the budget at the depth narrowing stops at" $ do
        -- Past the depth bound a further character has stopped dividing the names, so the walk
        -- says so rather than descending without end or skipping the bucket in silence.
        outcome <- withBucket deepest $ \deep -> collectBucket alphabet deep (pagesOf [oversized])
        case outcome of
            BucketUnsplittable -> pass
            _ -> expectationFailure "expected the deepest bucket to report that nothing narrows it"
  where
    alphabet = mkNameAlphabet "abcz"
    deepest = T.replicate bucketDepthLimit "a"
    oversized = [show n | n <- [1 .. bucketNameBudget + 1 :: Int]]
    atBudget = [show n | n <- [1 .. bucketNameBudget :: Int]]

    namesOf = \case
        BucketRead names -> Just names
        _ -> Nothing

{- One listing read under the bucket budget, with no location evidence to retain. The grouped
walk joins two inventories through the same reader, so the arms below are its own. -}
collectBucket ::
    NameAlphabet ->
    NamePrefix ->
    ConduitT () [PackageName] IO (Maybe StoreFault) ->
    IO (BucketNames StoreFault PackageName)
collectBucket alphabet prefix source =
    fmap fst <$> collectBucketWith alphabet prefix const (fuseUpstream source (CL.map (map (,()))))

-- A listing that yields the given pages and ends cleanly.
pagesOf :: [[Text]] -> ConduitT () [PackageName] IO (Maybe StoreFault)
pagesOf pages = Nothing <$ traverse_ (yield . map unscopedNpm) pages

-- A listing that yields the given pages and then stops on a fault.
faultingAfter :: [[Text]] -> ConduitT () [PackageName] IO (Maybe StoreFault)
faultingAfter pages = do
    traverse_ (yield . map unscopedNpm) pages
    pure (Just (protocolFault "the store stopped answering the listing"))

-- The buckets those spellings name, through the parser a cursor read uses.
spelledBuckets :: [Text] -> [NamePrefix]
spelledBuckets = mapMaybe (\raw -> parseNamePrefix (mkNameAlphabet (toString raw)) raw)
