-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | One shared copy per name and document, whatever the key, and SipHash's reference values.
module Ecluse.Core.Registry.Json.InternSpec (spec) where

import Data.ByteArray.Hash (SipHash (SipHash), sipHashWith)
import Data.ByteString qualified as BS
import Hedgehog (forAll, (===))
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

        describe "properties" $
            it "gives each name its own text, and keeps only the named members, under any key" $
                hedgehog $ do
                    (k0, k1) <- forAll ((,) <$> Gen.word64 Range.linearBounded <*> Gen.word64 Range.linearBounded)
                    names <- forAll (Gen.list (Range.linear 0 200) (Gen.element pool))
                    let step table name =
                            let Interned entry held = internName (plain name) table
                             in (held, (entryText entry, entryKeeps entry))
                        (_, entries) = mapAccumL step (newInternTable (SipKey k0 k1) ["url", "tarball"]) names
                    entries === [(decodeUtf8 name, name `elem` ["url", "tarball"]) | name <- names]

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

-- Names that collide in short prefixes and lengths, with a multi-byte one.
pool :: [ByteString]
pool = ["dep", "url", "^2", "\xc3\xa9", "", "key1", "key2", "tarball", "a", "aa", "aaa"]

plain :: ByteString -> Name
plain bytes = if BS.all (< 0x80) bytes then Plain bytes else decodedName (decodeUtf8 bytes)

sameObject :: Text -> Text -> IO Bool
sameObject left right = (==) <$> (makeStableName =<< evaluate left) <*> (makeStableName =<< evaluate right)
