-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Json.SelectiveSpec (spec) where

import Data.Aeson (Value (Number), object, (.=))
import Data.Aeson.Decoding.ByteString (bsToTokens)
import Data.Aeson.Decoding.Tokens (Tokens (TkArrayOpen, TkRecordOpen))
import Test.Hspec

import Ecluse.Core.Json.Selective (
    SelectiveError (SelectiveTooDeeplyNested, SelectiveUndecodable),
    collectFromArray,
    findInRecord,
    materialiseWithinBudget,
    skipValue,
    trailingWhitespace,
    withArray,
    withRecord,
 )

{- | Direct tests for the generic bounded token-walk engine, weighted to the unhappy and edge
paths a consumer inherits. The npm consumer already covers the engine's happy paths.
-}
spec :: Spec
spec = do
    findInRecordSpec
    collectFromArraySpec
    materialiseWithinBudgetSpec
    skipValueSpec
    withRecordSpec
    withArraySpec
    trailingWhitespaceSpec

findInRecordSpec :: Spec
findInRecordSpec = describe "findInRecord" $ do
    it "materialises the first occurrence of a duplicate key, not a later one" $
        findTop 5 "a" "{\"a\":1,\"a\":2}" `shouldBe` Right (Just (Number 1), 2)

    it "refuses a malformed later duplicate of the target, despite a valid first occurrence" $
        findTop 5 "a" "{\"a\":1,\"a\":x}" `shouldBe` Left SelectiveUndecodable

    it "refuses an over-deep later duplicate of the target, despite a valid first occurrence" $
        findTop 1 "a" "{\"a\":1,\"a\":[[1]]}" `shouldBe` Left SelectiveTooDeeplyNested

    it "counts every entry scanned, duplicates included (the raw count)" $
        findTop 5 "a" "{\"a\":1,\"b\":2,\"a\":3}" `shouldBe` Right (Just (Number 1), 3)

    it "reports an absent key as Nothing while still counting the entries" $
        findTop 5 "z" "{\"a\":1,\"b\":2}" `shouldBe` Right (Nothing, 2)

    it "reads an empty record as an absent key with a zero count" $
        findTop 5 "a" "{}" `shouldBe` Right (Nothing, 0)

    it "runs the scan to the record's end, so a malformed non-selected sibling still refuses" $
        findTop 5 "a" "{\"a\":1,\"b\":}" `shouldBe` Left SelectiveUndecodable

    it "refuses a malformed record structure (a missing comma)" $
        findTop 5 "a" "{\"a\":1 \"b\":2}" `shouldBe` Left SelectiveUndecodable

    it "depth-bounds a selected value at the child budget" $
        findTop 1 "a" "{\"a\":[[1]]}" `shouldBe` Left SelectiveTooDeeplyNested

    it "depth-bounds a non-selected sibling too (the skip path)" $
        findTop 1 "z" "{\"a\":[[1]]}" `shouldBe` Left SelectiveTooDeeplyNested

{- | The array half of the engine, driven over a PEP 691 @files@ list rather than an npm
document, so nothing here rests on the keyed-packument shape.
-}
collectFromArraySpec :: Spec
collectFromArraySpec = describe "collectFromArray" $ do
    it "materialises only the picked item and counts every item scanned" $
        collectTop 5 (== 1) simpleFiles
            `shouldBe` Right ([object ["filename" .= ("acme-1.0-py3-none-any.whl" :: Text)]], 3)

    it "picks nothing when the predicate rejects every position" $
        collectTop 5 (const False) simpleFiles `shouldBe` Right ([], 3)

    it "picks every item when the predicate accepts every position" $
        (length . fst <$> collectTop 5 (const True) simpleFiles) `shouldBe` Right 3

    it "keeps the picked items in array order" $
        collectTop 5 (< 2) "[1,2,3]" `shouldBe` Right ([Number 1, Number 2], 3)

    it "reads an empty array as no picks with a zero count" $
        collectTop 5 (const True) "[]" `shouldBe` Right ([], 0)

    it "runs the scan to the array's end, so a malformed unpicked item still refuses" $
        collectTop 5 (== 0) "[1,]" `shouldBe` Left SelectiveUndecodable

    it "depth-bounds a picked item at the item budget" $
        collectTop 1 (== 0) "[[1]]" `shouldBe` Left SelectiveTooDeeplyNested

    it "depth-bounds an unpicked item too (the skip path)" $
        collectTop 1 (const False) "[[1]]" `shouldBe` Left SelectiveTooDeeplyNested

materialiseWithinBudgetSpec :: Spec
materialiseWithinBudgetSpec = describe "materialiseWithinBudget" $ do
    it "materialises a value within budget" $
        materialiseValue 5 "42" `shouldBe` Right (Number 42)

    it "accepts a value nested at exactly the budget" $
        void (materialiseValue 2 "[1]") `shouldBe` Right ()

    it "refuses a value nested one level past the budget" $
        materialiseValue 1 "[1]" `shouldBe` Left SelectiveTooDeeplyNested

    it "refuses a value nested well past the budget" $
        materialiseValue 1 "[[1]]" `shouldBe` Left SelectiveTooDeeplyNested

    it "reports malformed tokens as undecodable" $
        materialiseValue 5 "{" `shouldBe` Left SelectiveUndecodable

skipValueSpec :: Spec
skipValueSpec = describe "skipValue / skipArray / skipRecord" $ do
    it "consumes a well-formed array within budget" $
        skipTop 5 "[1,2,3]" `shouldBe` Right ()

    it "consumes a well-formed object within budget" $
        skipTop 5 "{\"a\":1,\"b\":2}" `shouldBe` Right ()

    it "consumes an empty array (a leaf, no descent)" $
        skipTop 5 "[]" `shouldBe` Right ()

    it "consumes an empty object (a leaf, no descent)" $
        skipTop 5 "{}" `shouldBe` Right ()

    it "accepts a value nested at exactly the budget" $
        skipTop 2 "[1]" `shouldBe` Right ()

    it "refuses a value nested one level past the budget" $
        skipTop 1 "[1]" `shouldBe` Left SelectiveTooDeeplyNested

    it "refuses a value nested well past the budget" $
        skipTop 1 "[[1]]" `shouldBe` Left SelectiveTooDeeplyNested

    it "reports a malformed array as undecodable (skipArray)" $
        skipTop 5 "[1," `shouldBe` Left SelectiveUndecodable

    it "reports a malformed object as undecodable (skipRecord)" $
        skipTop 5 "{\"a\":1 \"b\":2}" `shouldBe` Left SelectiveUndecodable

    it "reports a malformed leading token as undecodable" $
        skipTop 5 "nope" `shouldBe` Left SelectiveUndecodable

withRecordSpec :: Spec
withRecordSpec = describe "withRecord" $ do
    it "runs the continuation on a record" $
        withRecordTop 1 "{}" `shouldBe` Right ()

    it "refuses a non-record scalar" $
        withRecordTop 5 "5" `shouldBe` Left SelectiveUndecodable

    it "refuses a non-record array" $
        withRecordTop 5 "[1]" `shouldBe` Left SelectiveUndecodable

    it "refuses a malformed leading token" $
        withRecordTop 5 "nope" `shouldBe` Left SelectiveUndecodable

    it "refuses the container when the depth budget is spent" $
        withRecordTop 0 "{}" `shouldBe` Left SelectiveTooDeeplyNested

withArraySpec :: Spec
withArraySpec = describe "withArray" $ do
    it "runs the continuation on an array" $
        withArrayTop 1 "[]" `shouldBe` Right ()

    it "refuses a non-array scalar" $
        withArrayTop 5 "5" `shouldBe` Left SelectiveUndecodable

    it "refuses a non-array record" $
        withArrayTop 5 "{\"a\":1}" `shouldBe` Left SelectiveUndecodable

    it "refuses a malformed leading token" $
        withArrayTop 5 "nope" `shouldBe` Left SelectiveUndecodable

    it "refuses the container when the depth budget is spent" $
        withArrayTop 0 "[]" `shouldBe` Left SelectiveTooDeeplyNested

trailingWhitespaceSpec :: Spec
trailingWhitespaceSpec = describe "trailingWhitespace" $ do
    it "accepts the four JSON whitespace bytes" $
        trailingWhitespace " \n\r\t" `shouldBe` True

    it "accepts an empty remainder" $
        trailingWhitespace "" `shouldBe` True

    it "rejects trailing non-whitespace" $
        trailingWhitespace " x" `shouldBe` False

    it "rejects non-whitespace among whitespace" $
        trailingWhitespace "\n\t x \r" `shouldBe` False

-- Drive 'findInRecord' over a document's top-level object, dropping the continuation the tests
-- do not read. A non-object body is 'SelectiveUndecodable', the same refusal the walk raises.
findTop :: Int -> Text -> ByteString -> Either SelectiveError (Maybe Value, Int)
findTop budget target body = case bsToTokens body of
    TkRecordOpen rec -> (\(found, count, _) -> (found, count)) <$> findInRecord budget target rec
    _ -> Left SelectiveUndecodable

-- Drive 'collectFromArray' over a document's top-level array, dropping the continuation the
-- tests do not read. A non-array body is 'SelectiveUndecodable'.
collectTop :: Int -> (Int -> Bool) -> ByteString -> Either SelectiveError ([Value], Int)
collectTop budget pick body = case bsToTokens body of
    TkArrayOpen arr -> (\(picked, count, _) -> (picked, count)) <$> collectFromArray budget pick arr
    _ -> Left SelectiveUndecodable

{- | A PEP 691 @files@ array: three distribution files under one project, the shape an
array-bearing index serves. The npm packument has no counterpart.
-}
simpleFiles :: ByteString
simpleFiles =
    "[{\"filename\":\"acme-1.0.tar.gz\"},\
    \{\"filename\":\"acme-1.0-py3-none-any.whl\"},\
    \{\"filename\":\"acme-1.1-py3-none-any.whl\"}]"

-- Materialise a whole body as one value, dropping the continuation.
materialiseValue :: Int -> ByteString -> Either SelectiveError Value
materialiseValue budget body = fst <$> materialiseWithinBudget budget (bsToTokens body)

-- Skip a whole body, discarding the continuation so only the refusal or success shows.
skipTop :: Int -> ByteString -> Either SelectiveError ()
skipTop budget body = void (skipValue budget (bsToTokens body))

-- Run 'withRecord' over a whole body with a trivial continuation, so only the guard shows.
withRecordTop :: Int -> ByteString -> Either SelectiveError ()
withRecordTop budget body = withRecord budget (bsToTokens body) (const (Right ()))

-- Run 'withArray' over a whole body with a trivial continuation, so only the guard shows.
withArrayTop :: Int -> ByteString -> Either SelectiveError ()
withArrayTop budget body = withArray budget (bsToTokens body) (const (Right ()))
