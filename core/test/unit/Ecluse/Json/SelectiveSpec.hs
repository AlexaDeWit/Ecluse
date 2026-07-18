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

{- | Direct tests for the generic bounded token-walk engine. They pin the security-relevant
semantics the engine guarantees to every JSON ecosystem that layers a selective decode on it:
first-occurrence-wins duplicate resolution, the raw entry count, depth bounding on both the
materialised and the skipped path, and the bounded read that runs the scan to the stream's end.
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

    it "runs the scan to the record's end, so a malformed non-selected sibling still refuses" $
        findTop 5 "a" "{\"a\":1,\"b\":}" `shouldBe` Left SelectiveUndecodable

    it "depth-bounds a selected value at the child budget" $
        findTop 1 "a" "{\"a\":[[1]]}" `shouldBe` Left SelectiveTooDeeplyNested

    it "depth-bounds a non-selected sibling too (the skip path)" $
        findTop 1 "z" "{\"a\":[[1]]}" `shouldBe` Left SelectiveTooDeeplyNested

materialiseWithinBudgetSpec :: Spec
materialiseWithinBudgetSpec = describe "materialiseWithinBudget" $ do
    it "materialises a value within budget" $
        materialiseValue 5 "42" `shouldBe` Right (Number 42)

    it "refuses a value nested past the budget" $
        materialiseValue 1 "[[1]]" `shouldBe` Left SelectiveTooDeeplyNested

    it "reports malformed tokens as undecodable" $
        materialiseValue 5 "{" `shouldBe` Left SelectiveUndecodable

skipValueSpec :: Spec
skipValueSpec = describe "skipValue" $ do
    it "consumes a well-formed value within budget" $
        skipTop 5 "[1,2,3]" `shouldBe` Right ()

    it "refuses a value nested past the budget" $
        skipTop 1 "[[1]]" `shouldBe` Left SelectiveTooDeeplyNested

    it "reports malformed tokens as undecodable" $
        skipTop 5 "[1," `shouldBe` Left SelectiveUndecodable

withRecordSpec :: Spec
withRecordSpec = describe "withRecord" $ do
    it "runs the continuation on a record" $
        withRecordTop 1 "{}" `shouldBe` Right ()

    it "refuses a non-record value" $
        withRecordTop 5 "5" `shouldBe` Left SelectiveUndecodable

    it "refuses the container when the depth budget is spent" $
        withRecordTop 0 "{}" `shouldBe` Left SelectiveTooDeeplyNested

trailingWhitespaceSpec :: Spec
trailingWhitespaceSpec = describe "trailingWhitespace" $ do
    it "accepts only JSON whitespace after the value" $
        trailingWhitespace " \n\r\t" `shouldBe` True

    it "accepts an empty remainder" $
        trailingWhitespace "" `shouldBe` True

    it "rejects trailing non-whitespace" $
        trailingWhitespace " x" `shouldBe` False

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
