-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The public parser retains its chunk and leftover-input contracts.
module Data.JsonStream.ParserSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.JsonStream.Parser qualified as J
import Data.Text qualified as T
import Test.Hspec

-- | Check compatibility through public parser results without private token types.
spec :: Spec
spec = describe "public parser compatibility" $ do
    it "reports input after a completed array, object or string" $
        for_ ["[]", "{}", "\"value\""] $ \body ->
            case J.runParser' (J.value :: J.Parser Aeson.Value) (body <> trailingBytes) of
                J.ParseYield _ (J.ParseDone remaining) -> remaining `shouldBe` trailingBytes
                result -> expectationFailure ("expected completed value with leftovers, got " <> show result)

    it "decodes an escape split between input pieces" $
        case J.runParser' J.string "\"prefix\\u0" of
            J.ParseNeedData resume -> case resume "0e9\" rest" of
                J.ParseYield value (J.ParseDone remaining) -> do
                    value `shouldBe` "prefixé"
                    remaining `shouldBe` " rest"
                result -> expectationFailure ("expected the resumed string, got " <> show result)
            result -> expectationFailure ("expected an input request, got " <> show result)

    it "accepts large initial pieces without changing lenient number chunk semantics" $ do
        let body = BS.replicate numericPrefixBytes spaceByte <> "[-]"
        case J.runParser' (J.value :: J.Parser Aeson.Value) body of
            J.ParseYield value (J.ParseDone remaining) -> do
                value `shouldBe` Aeson.Array (fromList [Aeson.Number 0])
                remaining `shouldBe` BS.empty
            result -> expectationFailure ("expected the original single-piece number result, got " <> show result)
        case J.runParser' (J.value :: J.Parser Aeson.Value) "[-" of
            J.ParseNeedData resume -> case resume "]" of
                J.ParseFailed _ -> pure ()
                result -> expectationFailure ("expected the original split-number failure, got " <> show result)
            result -> expectationFailure ("expected an input request, got " <> show result)

    it "accepts a retained string larger than the bounded registry driver's pieces" $ do
        let value = T.replicate largeStringBytes "a"
            body = toStrict (Aeson.encode value)
        J.eitherDecodeStrict body `shouldBe` Right value

trailingBytes :: ByteString
trailingBytes = " untouched"

numericPrefixBytes, largeStringBytes :: Int
numericPrefixBytes = 32766
largeStringBytes = 65537

spaceByte :: Word8
spaceByte = 32
