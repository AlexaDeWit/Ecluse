-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | One shared copy per name and document, the same entries under every table hash, and SipHash's reference values.
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

-- | The table's sharing, keeping and hashing contracts.
spec :: Spec
spec = describe "internName" $ do
    it "holds every occurrence of a name as one copy" $ do
        let table = newInternTable (SipHash13 key) []
            Interned earlier held = internName (Plain "dep") table
            Interned later _ = internName (Decoded "dep") held
        sameObject (entryText earlier) (entryText later) `shouldReturn` True

    it "keeps the values of the named members as read, and shares the names themselves" $ do
        let Interned kept _ = internName (Plain "url") (newInternTable FixedSeed ["url"])
            Interned shared _ = internName (Plain "dep") (newInternTable FixedSeed ["url"])
        (entryKeeps kept, entryKeeps shared) `shouldBe` (True, False)

    it "stores an owned copy of a name cut from a larger input" $ do
        let Interned entry _ = internName (Plain (BS.take 5 (BS.drop 1 "xvaluex"))) (newInternTable ByteOrder [])
        textStorageBytes (entryText entry) `shouldBe` 5

    it "finds the same entries under every table hash" $
        hedgehog $ do
            names <- forAll (Gen.list (Range.linear 0 60) (Gen.element ["dep", "url", "^2", "é𝄞", "", "key1", "key2", "tarball"]))
            let entries kind = snd (mapAccumL (\table name -> let Interned entry held = internName (Decoded name) table in (held, (entryText entry, entryKeeps entry))) (newInternTable kind ["url", "tarball"]) names)
            entries (SipHash13 key) === entries FixedSeed
            entries (SipHash24 key) === entries FixedSeed
            entries ByteOrder === entries FixedSeed

    it "matches SipHash-2-4's reference value" $
        sipHash24 (SipKey 0x0706050403020100 0x0f0e0d0c0b0a0908) (BS.pack [0 .. 14]) `shouldBe` 0xa129ca6149be45e5

    it "agrees with ram's SipHash-1-3 and SipHash-2-4 for any message" $
        hedgehog $ do
            message <- forAll (Gen.bytes (Range.linear 0 80))
            let SipHash expected13 = sipHashWith 1 3 key message
                SipHash expected24 = sipHashWith 2 4 key message
            (sipHash13 key message, sipHash24 key message) === (expected13, expected24)

key :: SipKey
key = SipKey 0x0123456789abcdef 0xfedcba9876543210

sameObject :: Text -> Text -> IO Bool
sameObject left right = (==) <$> (makeStableName =<< evaluate left) <*> (makeStableName =<< evaluate right)
