-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Json.SelectiveSpec (spec) where

import Data.Aeson (Value (Number))
import Data.Aeson.Decoding.ByteString (bsToTokens)
import Data.Aeson.Decoding.Tokens (Tokens (TkRecordOpen))
import Test.Hspec

import Ecluse.Core.Json.Selective (
    SelectiveError (SelectiveTooDeeplyNested, SelectiveUndecodable),
    findInRecord,
    materialiseWithinBudget,
    skipValue,
    trailingWhitespace,
    withRecord,
 )

{- | Direct tests for the generic bounded token-walk engine, weighted to the unhappy and edge
paths a consumer inherits: malformed-token refusal on each primitive, the depth-budget boundary on
both the materialise and the skip path, non-record and empty-container handling, and error
propagation from a duplicate key whose later occurrence breaches. One happy anchor per primitive
fixes the baseline; the engine's happy paths are already exercised through the npm consumer.
-}
spec :: Spec
spec = do
    findInRecordSpec
    materialiseWithinBudgetSpec
    skipValueSpec
    withRecordSpec
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

-- Materialise a whole body as one value, dropping the continuation.
materialiseValue :: Int -> ByteString -> Either SelectiveError Value
materialiseValue budget body = fst <$> materialiseWithinBudget budget (bsToTokens body)

-- Skip a whole body, discarding the continuation so only the refusal or success shows.
skipTop :: Int -> ByteString -> Either SelectiveError ()
skipTop budget body = void (skipValue budget (bsToTokens body))

-- Run 'withRecord' over a whole body with a trivial continuation, so only the guard shows.
withRecordTop :: Int -> ByteString -> Either SelectiveError ()
withRecordTop budget body = withRecord budget (bsToTokens body) (const (Right ()))
