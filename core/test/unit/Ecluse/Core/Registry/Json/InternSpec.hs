-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | One shared copy per name and document, whatever the key, and SipHash's reference values.
module Ecluse.Core.Registry.Json.InternSpec (spec) where

import Data.ByteArray.Hash (SipHash (SipHash), sipHashWith)
import Data.ByteString qualified as BS
import Hedgehog (Gen, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Mem.StableName (makeStableName)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)
import UnliftIO.Exception (evaluate)

import Ecluse.Core.Registry.Json.Intern
import Ecluse.Core.Text (textStorageBytes)

-- | The table's sharing and keeping contracts, and its hash.
spec :: Spec
spec = do
    describe "internName" $ do
        it "holds a name read plain and read with escapes as one copy" $ do
            let Interned earlier held = internName (Plain "dep") (newInternTable key [])
                Interned later _ = internName (decodedName "dep") held
            sameObject (entryText earlier) (entryText later) `shouldReturn` True

        it "keeps the values of the named members as read, and shares the names themselves" $ do
            let Interned kept _ = internName (Plain "url") (newInternTable key ["url"])
                Interned shared _ = internName (Plain "dep") (newInternTable key ["url"])
            (entryKeeps kept, entryKeeps shared) `shouldBe` (True, False)

        it "stores an owned copy of a name cut from a larger input" $ do
            let Interned entry _ = internName (Plain (BS.take 5 (BS.drop 1 "xvaluex"))) (newInternTable key [])
            textStorageBytes (entryText entry) `shouldBe` 5

        it "seeds a repeated member name once, so each index names one entry" $ do
            let Interned entry held = internName (Plain "dep") (newInternTable key ["url", "url", "tarball"])
            (entryIndex entry, toList (tableTexts held)) `shouldBe` (2, ["url", "tarball", "dep"])

        describe "properties" $ do
            it "lays out every name once, at its entry's index, in first-read order" $
                hedgehog $ do
                    seeds <- forAll (Gen.list (Range.linear 0 4) (Gen.element pool))
                    names <- forAll (Gen.list (Range.linear 0 60) (Gen.element pool))
                    let step table name = let Interned entry held = internName (plain name) table in (held, (entryIndex entry, entryText entry))
                        (final, entries) = mapAccumL step (newInternTable otherKey (map decodeUtf8 seeds)) names
                        texts = toList (tableTexts final)
                    texts === ordNub (map decodeUtf8 (seeds <> names))
                    [(index, text) | (index, text) <- entries, Just text /= (texts !!? index)] === []

            it "gives each name its own text, and keeps only the named members, under any key" $
                hedgehog $ do
                    (k0, k1) <- forAll ((,) <$> Gen.word64 Range.linearBounded <*> Gen.word64 Range.linearBounded)
                    names <- forAll (Gen.list (Range.linear 0 200) (Gen.element pool))
                    let step table name =
                            let Interned entry held = internName (plain name) table
                             in (held, (entryText entry, entryKeeps entry))
                        (_, entries) = mapAccumL step (newInternTable (SipKey k0 k1) ["url", "tarball"]) names
                    entries === [(decodeUtf8 name, name `elem` ["url", "tarball"]) | name <- names]

    describe "prepared names" $ do
        it "inserts only on use, after names already read, and shares the existing entry" $ do
            let initial = newInternTable key ["url"]
            prepared <- evaluate (prepareName initial "dep")
            let Interned dynamic held = internName (Plain "dynamic") initial
                Interned preparedEntry shared = internPreparedName prepared held
                Interned again final = internName (decodedName "dep") shared
            toList (tableTexts final) `shouldBe` ["url", "dynamic", "dep"]
            entryIndex dynamic `shouldBe` firstUnseededIndex
            entryIndex preparedEntry `shouldBe` entryIndex again
            sameObject (entryText preparedEntry) (entryText again) `shouldReturn` True

        it "uses the receiving table's entry and keep flag under another key" $ do
            let prepared = prepareName (newInternTable key []) "url"
                target = newInternTable otherKey ["before", "url"]
                Interned preparedEntry held = internPreparedName prepared target
                Interned again final = internName (Plain "url") held
            entryKeeps preparedEntry `shouldBe` True
            entryIndex preparedEntry `shouldBe` entryIndex again
            toList (tableTexts final) `shouldBe` ["before", "url"]
            sameObject (entryText preparedEntry) (entryText again) `shouldReturn` True

        it "preserves indices and keep flags across mixed representations and keys" $
            hedgehog $ do
                prepareKeyWords <- forAll genKeyWords
                receivingKeyWords <- forAll (Gen.choice [pure prepareKeyWords, genKeyWords])
                seeds <- forAll (Gen.subsequence pool)
                names <- forAll (Gen.list (Range.linear 0 preparedNameCount) ((,,) <$> Gen.bool <*> Gen.bool <*> Gen.element pool))
                let preparation = newInternTable (uncurry SipKey prepareKeyWords) []
                    seedTexts = map decodeUtf8 seeds
                    initial = newInternTable (uncurry SipKey receivingKeyWords) seedTexts
                    step table (prepared, decoded, bytes) =
                        let name = if decoded then decodedName (decodeUtf8 bytes) else plain bytes
                            Interned entry held =
                                if prepared
                                    then internPreparedName (prepareName preparation (decodeUtf8 bytes)) table
                                    else internName name table
                         in (held, (entryIndex entry, entryText entry, entryKeeps entry))
                    (final, entries) = mapAccumL step initial names
                    texts = toList (tableTexts final)
                    expected = map (\(_, _, bytes) -> decodeUtf8 bytes) names
                texts === ordNub (seedTexts <> expected)
                [(text, keeps) | (_, text, keeps) <- entries] === [(text, text `elem` seedTexts) | text <- expected]
                [(index, text) | (index, text, _) <- entries, Just text /= (texts !!? index)] === []

    describe "sipHash" $ do
        it "matches SipHash-1-3's reference value for the empty message" $
            sipHash 1 3 key BS.empty `shouldBe` 0xabac0158050fc4dc

        it "matches SipHash-2-4's reference value" $
            sipHash 2 4 key (BS.pack [0 .. 14]) `shouldBe` 0xa129ca6149be45e5

        describe "properties" $
            it "agrees with ram's SipHash-1-3 and SipHash-2-4 for any message" $
                hedgehog $ do
                    message <- forAll (Gen.bytes (Range.linear 0 80))
                    let SipHash expected13 = sipHashWith 1 3 key message
                        SipHash expected24 = sipHashWith 2 4 key message
                    (sipHash 1 3 key message, sipHash 2 4 key message) === (expected13, expected24)

-- The reference key 00 01 .. 0f, as little-endian words.
key :: SipKey
key = SipKey 0x0706050403020100 0x0f0e0d0c0b0a0908

otherKey :: SipKey
otherKey = SipKey 1 2

genKeyWords :: Gen (Word64, Word64)
genKeyWords = (,) <$> Gen.word64 Range.linearBounded <*> Gen.word64 Range.linearBounded

firstUnseededIndex, preparedNameCount :: Int
firstUnseededIndex = 1
preparedNameCount = 200

-- Names that collide in short prefixes and lengths, with a multi-byte one.
pool :: [ByteString]
pool = ["dep", "url", "^2", "\xc3\xa9", "", "key1", "key2", "tarball", "a", "aa", "aaa"]

plain :: ByteString -> Name
plain bytes = if BS.all (< 0x80) bytes then Plain bytes else decodedName (decodeUtf8 bytes)

sameObject :: Text -> Text -> IO Bool
sameObject left right = (==) <$> (makeStableName =<< evaluate left) <*> (makeStableName =<< evaluate right)
