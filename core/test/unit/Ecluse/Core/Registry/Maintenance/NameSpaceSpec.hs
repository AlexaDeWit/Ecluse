-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Registry.Maintenance.NameSpaceSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName, mkScope)
import Ecluse.Core.Registry.Maintenance.NameSpace (
    extendBucket,
    inBucket,
    initialBuckets,
    mkNameAlphabet,
    noNameAlphabet,
    parseNamePrefix,
    renderNamePrefix,
    wholeNameSpace,
 )
import Ecluse.Test.Maintenance (withBucket)

-- | Tests the name-space buckets: how an alphabet divides a store, and which bucket holds a name.
spec :: Spec
spec = bucketSpec

bucketSpec :: Spec
bucketSpec = do
    describe "initialBuckets" $ do
        it "gives one bucket per character of the alphabet, in the order it was built with" $
            map renderNamePrefix (toList (initialBuckets (mkNameAlphabet "abc")))
                `shouldBe` ["a", "b", "c"]

        it "drops a repeated character, so no name lands in two buckets" $
            length (initialBuckets (mkNameAlphabet "aab")) `shouldBe` 2

        it "gives the one bucket that covers everything when the alphabet has no characters" $
            initialBuckets noNameAlphabet `shouldBe` wholeNameSpace :| []

        it "puts every name the alphabet leads into exactly one of its buckets" $
            map bucketsHolding names `shouldBe` replicate (length names) 1

    describe "extendBucket" $ do
        it "narrows a bucket by one character of the alphabet at a time" $
            withBucket "a" $ \a ->
                map renderNamePrefix (extendBucket (mkNameAlphabet "ab") a) `shouldBe` ["aa", "ab"]

        it "narrows nothing under an alphabet with no characters" $
            withBucket "a" $
                \a -> extendBucket noNameAlphabet a `shouldBe` []

        it "keeps the narrower buckets inside the one they came from" $
            withBucket "a" $ \a ->
                all (\narrower -> renderNamePrefix a `T.isPrefixOf` renderNamePrefix narrower) (extendBucket alphabet a)
                    `shouldBe` True

    describe "parseNamePrefix" $ do
        it "reads back a prefix the alphabet spells" $
            fmap renderNamePrefix (parseNamePrefix (mkNameAlphabet "abc") "ab") `shouldBe` Just "ab"

        it "reads no prefix from a spelling the alphabet does not carry" $
            parseNamePrefix (mkNameAlphabet "abc") "az" `shouldBe` Nothing

        it "still reads a prefix after a repeated character was dropped" $
            fmap renderNamePrefix (parseNamePrefix (mkNameAlphabet "aab") "ab") `shouldBe` Just "ab"

        it "reads the empty prefix under any alphabet, because it filters nothing" $ do
            fmap renderNamePrefix (parseNamePrefix (mkNameAlphabet "abc") "") `shouldBe` Just ""
            fmap renderNamePrefix (parseNamePrefix noNameAlphabet "") `shouldBe` Just ""

    describe "inBucket" $ do
        it "reads a name by its base component, so a namespace never decides the bucket" $
            withBucket "c" $ \prefix -> do
                inBucket prefix scopedName `shouldBe` True
                inBucket prefix (unscoped "banana") `shouldBe` False

        it "puts a name under exactly one of two buckets that do not overlap" $
            withBucket "a" $ \a -> withBucket "b" $ \b ->
                map (\name -> (inBucket a name, inBucket b name)) names
                    `shouldBe` [(True, False), (False, True), (False, False)]

        it "holds every name in the bucket that covers a whole store" $
            withBucket "" $ \everything ->
                map (inBucket everything) names `shouldBe` replicate (length names) True
  where
    names = [unscoped "apple", unscoped "banana", scopedName]

    -- The alphabet leads every one of those names, so the buckets it gives partition them.
    alphabet = mkNameAlphabet "abc"

    bucketsHolding name = length [() | b <- toList (initialBuckets alphabet), inBucket b name]

unscoped :: Text -> PackageName
unscoped = mkPackageName Npm Nothing

scopedName :: PackageName
scopedName = mkPackageName Npm (Just (mkScope "babel")) "core"
