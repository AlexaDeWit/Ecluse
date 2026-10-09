-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The frame format, and the proof that no two component tuples frame to the same bytes.
module Ecluse.Core.Server.FramingSpec (spec) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.ByteString.Char8 qualified as BS8
import Hedgehog (Gen, cover, forAll, (/==), (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Server.Framing (frameBytes, frameComponents)

spec :: Spec
spec = do
    describe "frameBytes" $ do
        it "writes the decimal byte length, a colon, then the bytes" $ do
            built (frameBytes "") `shouldBe` "0:"
            built (frameBytes "abc") `shouldBe` "3:abc"
            built (frameBytes "0123456789ab") `shouldBe` "12:0123456789ab"

        it "counts bytes, not characters" $
            built (frameBytes (encodeUtf8 ("\233" :: Text))) `shouldBe` "2:\195\169"

    describe "frameComponents" $ do
        it "frames present components in order and an absent one as a lone hyphen" $
            frameComponents [Just "ab", Nothing, Just ""] `shouldBe` "2:ab-0:"

        it "keeps an absent component apart from an empty one and from the shorter tuple" $ do
            frameComponents [Just "a", Nothing] `shouldNotBe` frameComponents [Just "a"]
            frameComponents [Just "a", Nothing] `shouldNotBe` frameComponents [Just "a", Just ""]
            frameComponents [Nothing] `shouldNotBe` frameComponents []

        it "keeps a component that holds frame syntax inside its own frame" $ do
            frameComponents [Just "1:a1:b"] `shouldNotBe` frameComponents [Just "a", Just "b"]
            frameComponents [Just "-"] `shouldNotBe` frameComponents [Nothing]
            frameComponents [Just "1.0", Just "0.2.0"] `shouldNotBe` frameComponents [Just "1.0.0", Just "2.0"]

        it "frames a component above the builder's copy limit as its plain frames concatenate" $ do
            let tuple = [Just "a", Just (BS.replicate 8161 120), Nothing, Just "b"]
            frameComponents tuple `shouldBe` built (foldMap (maybe "-" frameBytes) tuple)

        describe "properties" $ do
            it "reads back every tuple from its frames" $
                hedgehog $ do
                    tuple <- forAll genTuple
                    cover 5 "a two-digit length" (any (maybe False ((>= 10) . BS.length)) tuple)
                    unframe (frameComponents tuple) === Just tuple

            it "frames two tuples alike only when they are the same tuple" $
                hedgehog $ do
                    tuple <- forAll genTuple
                    neighbour <- forAll (genNeighbour tuple)
                    (frameComponents tuple == frameComponents neighbour) === (tuple == neighbour)

            it "frames a tuple apart from the same tuple with one more absent component" $
                hedgehog $ do
                    tuple <- forAll genTuple
                    at <- forAll (Gen.int (Range.linear 0 (length tuple)))
                    frameComponents (insertAbsent at tuple) /== frameComponents tuple

built :: Builder -> ByteString
built = toStrict . toLazyByteString

-- An independent reader of the frame format. Reading every tuple back shows the framing is injective.
unframe :: ByteString -> Maybe [Maybe ByteString]
unframe bytes
    | BS.null bytes = Just []
    | Just rest <- BS.stripPrefix "-" bytes = (Nothing :) <$> unframe rest
    | otherwise = do
        (size, afterSize) <- BS8.readInt bytes
        body <- BS.stripPrefix ":" afterSize
        guard (size >= 0 && size <= BS.length body)
        let (component, rest) = BS.splitAt size body
        (Just component :) <$> unframe rest

genTuple :: Gen [Maybe ByteString]
genTuple = Gen.list (Range.linear 0 5) (Gen.frequency [(1, pure Nothing), (4, Just <$> genBytes)])

-- Strings over the bytes framing itself uses, a unit separator and plain letters, long enough for a two-digit length.
genBytes :: Gen ByteString
genBytes = BS8.pack <$> Gen.list (Range.linear 0 24) (Gen.element ("0123:-\US ab" :: String))

-- The same tuple, an unrelated one, or one edit away: the cases a weak framing confuses.
genNeighbour :: [Maybe ByteString] -> Gen [Maybe ByteString]
genNeighbour tuple = do
    at <- Gen.int (Range.linear 0 (length tuple))
    Gen.choice
        [ pure tuple
        , genTuple
        , pure (insertAbsent at tuple)
        , pure (editAt at (drop 1) tuple)
        , pure (editAt at emptyForAbsent tuple)
        , pure (editAt at joinPair tuple)
        , pure (editAt at splitHead tuple)
        , pure (editAt at shiftByte tuple)
        ]

insertAbsent :: Int -> [Maybe ByteString] -> [Maybe ByteString]
insertAbsent at = editAt at (Nothing :)

editAt :: Int -> ([a] -> [a]) -> [a] -> [a]
editAt at edit items = leading <> edit trailing
  where
    (leading, trailing) = splitAt at items

emptyForAbsent :: [Maybe ByteString] -> [Maybe ByteString]
emptyForAbsent = \case
    Nothing : rest -> Just "" : rest
    Just "" : rest -> Nothing : rest
    rest -> rest

joinPair :: [Maybe ByteString] -> [Maybe ByteString]
joinPair = \case
    Just a : Just b : rest -> Just (a <> b) : rest
    rest -> rest

splitHead :: [Maybe ByteString] -> [Maybe ByteString]
splitHead = \case
    Just a : rest -> Just (BS.take 1 a) : Just (BS.drop 1 a) : rest
    rest -> rest

shiftByte :: [Maybe ByteString] -> [Maybe ByteString]
shiftByte = \case
    Just a : Just b : rest | Just (kept, moved) <- BS.unsnoc a -> Just kept : Just (BS.cons moved b) : rest
    rest -> rest
