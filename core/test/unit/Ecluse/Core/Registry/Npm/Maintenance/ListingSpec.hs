-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native package listings preserve the independent parser's acceptance and byte accounting.
module Ecluse.Core.Registry.Npm.Maintenance.ListingSpec (spec) where

import Data.ByteString qualified as BS
import Hedgehog (forAll, (===))
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (ParseError (ParseError))
import Ecluse.Core.Registry.Npm.Maintenance.Listing (packageListingWalk)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge))
import Ecluse.Test.Package (scopedNpm, unscopedNpm)
import Ecluse.Test.Registry.JsonBytes (damaged, genChunks, genJsonBytes)
import Ecluse.Test.Registry.JsonStream (parseJsonChunks, readOutcome, walkJsonChunks)
import Ecluse.Test.Registry.Npm.Maintenance (parsePackageListing, referencePackageListingParser)

-- | Compare hostile inputs and explicit boundaries without imposing new enumeration limits.
spec :: Spec
spec = describe "packageListingWalk" $ do
    modifyMaxSuccess (const 2000) $
        it "matches reference names, bytes and failures across generated chunks" $
            hedgehog $ do
                body <- forAll (genJsonBytes ["leftpad", "rightpad", "_updated", "@foo", "@acme/tool"] >>= damaged)
                chunks <- forAll (genChunks body)
                let (reference, walked) = bothReads chunks
                walked === reference

    forM_ successCases $ \(label, body, names) ->
        it label $ do
            parsePackageListing body `shouldBe` Right (map unscopedNpm names)
            atEverySplit body

    forM_ ["null ", "false ", "1 ", "\"text\"", "[]", "[{}]"] $ \body ->
        it ("retains the non-object diagnostic for " <> show body) $ do
            parsePackageListing body `shouldBe` Left (ParseError "the store's package listing is not a JSON object")
            atEverySplit body

    forM_ ["", "null", "false", "1", "{", "{\"leftpad\":", "{\"leftpad\":[]", "{\"leftpad\":\"unfinished", "{\"\\x\":0}"] $ \body ->
        it ("refuses incomplete input or malformed keys: " <> show body) $ do
            parsePackageListing body `shouldSatisfy` isLeft
            atEverySplit body

    it "orders scoped names by their raw keys" $ do
        let body = "{\"leftpad\":0,\"@acme/tool\":null,\"Leftpad\":false}"
        parsePackageListing body `shouldBe` Right [scopedNpm "acme" "tool", unscopedNpm "Leftpad", unscopedNpm "leftpad"]
        atEverySplit body

    it "refuses invalid UTF-8 in keys but skips it in values" $
        forM_ [BS.pack [0xff], BS.pack [0xc0, 0xaf]] $ \invalid -> do
            let keyBody = "{\"" <> invalid <> "\":0}"
                valueBody = "{\"leftpad\":\"" <> invalid <> "\"}"
            parsePackageListing keyBody `shouldSatisfy` isLeft
            parsePackageListing valueBody `shouldBe` Right [unscopedNpm "leftpad"]
            atEverySplit keyBody
            atEverySplit valueBody

    it "skips structures deeper than metadata's retained-depth ceiling" $ do
        let body = "{\"leftpad\":" <> BS.replicate 256 0x5b <> "0" <> BS.replicate 256 0x5d <> "}"
        parsePackageListing body `shouldBe` Right [unscopedNpm "leftpad"]
        checkChunks (chunksOf 17 body)

    it "enumerates a large set without a package-count setting" $ do
        let names = ["package" <> show index | index <- [1000 .. 2500 :: Int]]
            body = "{" <> BS.intercalate "," ["\"" <> encodeUtf8 name <> "\":0" | name <- names] <> "}"
        parsePackageListing body `shouldBe` Right (map unscopedNpm names)
        checkChunks (chunksOf 1024 body)

    it "preserves long-key dropping across lexer chunks" $
        forM_ [65535, 65536, 65537, 100000] $ \size -> do
            let body = "{\"" <> BS.replicate size 0x61 <> "\":{},\"leftpad\":0}"
            parsePackageListing body `shouldBe` Right [unscopedNpm "leftpad"]
            forM_ [1024, 32768] $ \width -> checkChunks (chunksOf width body)

    it "preserves the digit ceiling in discarded values" $ do
        let body = "{\"leftpad\":" <> BS.replicate 300000 0x31 <> "}"
            chunks = chunksOf 32768 body
        checkChunks chunks
        snd (bothReads chunks) `shouldSatisfy` either (const False) (isLeft . snd)

    it "counts trailing bytes and enforces the bound after extraction" $ do
        let body = "{\"leftpad\":0}" <> BS.replicate 80 0x20
            chunks = chunksOf 7 body
            bound = MetadataBodyLimit (BS.length body - 1)
        void (walkJsonChunks bound packageListingWalk chunks) `shouldBe` Left (BodyTooLarge bound)
        void (parseJsonChunks bound referencePackageListingParser (\_ names -> Right names) [] chunks)
            `shouldBe` Left (BodyTooLarge bound)
        snd (bothReads chunks) `shouldBe` Right (BS.length body, Right [unscopedNpm "leftpad"])

successCases :: [(String, ByteString, [Text])]
successCases =
    [ ("reads an empty object", "{}", [])
    , ("sorts raw keys and collapses escaped duplicates", "{\"rightpad\":0,\"leftpad\":1,\"\\u006ceftpad\":false}", ["leftpad", "rightpad"])
    , ("omits bookkeeping and invalid names", "{\"_updated\":1,\"@foo\":{},\"leftpad\":null}", ["leftpad"])
    , ("skips malformed value strings", "{\"leftpad\":\"\\x\",\"rightpad\":{\"\\x\":\"\\x\"}}", ["leftpad", "rightpad"])
    , ("accepts mismatched closing tokens inside skipped values", "{\"leftpad\":[}}", ["leftpad"])
    , ("ignores trailing bytes after the root", "{\"leftpad\":0}garbage", ["leftpad"])
    , ("retains lexer separator leniency", "{\"leftpad\" 0 \"rightpad\" false}", ["leftpad", "rightpad"])
    ]

type Outcome = Either LimitError (Int, Either Bool [PackageName])

bothReads :: [ByteString] -> (Outcome, Outcome)
bothReads chunks =
    ( readOutcome (parseJsonChunks bound referencePackageListingParser (\_ names -> Right names) [] chunks)
    , readOutcome (walkJsonChunks bound packageListingWalk chunks)
    )
  where
    bound = MetadataBodyLimit (sum (map BS.length chunks))

atEverySplit :: ByteString -> Expectation
atEverySplit body = forM_ [0 .. BS.length body] $ \cut ->
    checkChunks (filter (not . BS.null) [BS.take cut body, BS.drop cut body])

checkChunks :: [ByteString] -> Expectation
checkChunks chunks = do
    let (reference, walked) = bothReads chunks
    walked `shouldBe` reference

chunksOf :: Int -> ByteString -> [ByteString]
chunksOf width body
    | BS.null body = []
    | otherwise = let (chunk, rest) = BS.splitAt width body in chunk : chunksOf width rest
