-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The public parser retains its chunk and leftover-input contracts.
module Data.JsonStream.ParserSpec (spec) where

import Control.Exception qualified as Exception
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.JsonStream.Parser qualified as J
import Data.Text qualified as T
import Test.Hspec

-- | Check compatibility through public parser results without private token types.
spec :: Spec
spec = do
    objectSpec
    parserSpec

parserSpec :: Spec
parserSpec = describe "public parser compatibility" $ do
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

    it "preserves ErrorCall for missing input through both throwing entry points" $ do
        Exception.evaluate (J.parseByteString J.string BS.empty) `shouldThrow` hasErrorMessage "Not enough data."
        Exception.evaluate (J.parseLazyByteString J.string LBS.empty) `shouldThrow` hasErrorMessage "Not enough data."

    it "preserves the reported parser failure in throwing entry points" $ do
        let body = "\"\\x\""
        case J.runParser' J.string body of
            J.ParseFailed message -> do
                Exception.evaluate (J.parseByteString J.string body) `shouldThrow` hasErrorMessage message
                Exception.evaluate (J.parseLazyByteString J.string (LBS.fromStrict body)) `shouldThrow` hasErrorMessage message
            other -> expectationFailure ("expected a reported failure, got " <> show other)

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

objectSpec :: Spec
objectSpec = describe "typed one-pass object fields" $ do
    it "keeps heterogeneous fields and repeated JSON keys in Cartesian order at every cut" $ do
        let parser = J.objectOf $ (,) <$> "name" J..: J.string <*> "age" J..: (J.integer :: J.Parser Int)
            expected = [("first", 1), ("first", 2), ("second", 1), ("second", 2)]
        expectEveryCut parser "{\"age\":1,\"name\":\"first\",\"age\":2,\"name\":\"second\"}" (Right expected)

    it "retains every yield from a selected field parser" $
        expectEveryCut
            (J.objectOf ("items" J..: J.arrayOf (J.integer :: J.Parser Int)))
            "{\"items\":[1,2],\"items\":[3]}"
            (Right [1, 2, 3])

    it "preserves missing, mismatched and defaulted field results" $ do
        let parser = J.objectOf $ (,) <$> "missing" J..:? J.string <*> (("count" J..: (J.integer :: J.Parser Int)) J..| missingCount)
        expectEveryCut parser "{\"count\":null}" (Right [(Nothing, missingCount)])
        expectEveryCut (J.objectOf ("required" J..: J.string)) "{}" (Right [])

    it "keeps Object alternatives left-biased and concatenation grouped by declaration" $ do
        let first = "first" J..: (J.integer :: J.Parser Int)
            second = "second" J..: (J.integer :: J.Parser Int)
            body = "{\"second\":2,\"first\":1,\"first\":3}"
        expectEveryCut (J.objectOf (first <|> second)) body (Right [1, 3])
        expectEveryCut (J.objectOf (first <> second)) body (Right [1, 3, 2])
        expectEveryCut (J.objectOf (first <|> second)) "{\"second\":2}" (Right [2])

    it "parses selected right-hand fields even when the left alternative produces a value" $ do
        let parser = J.objectOf (("first" J..: J.string) <|> ("second" J..: J.string))
            body = "{\"first\":\"ok\",\"second\":\"\\uD800\"}"
        for_ (cuts body) $ \pieces -> collectParser parser pieces `shouldSatisfy` isLeft

    it "does not decode malformed UTF-8 in unknown fields" $ do
        let body = "{\"known\":\"ok\",\"unknown\":\"" <> BS.singleton invalidUtf8Byte <> "\"}"
        expectEveryCut (J.objectOf ("known" J..: J.string)) body (Right ["ok"])

    it "rejects duplicate declarations before mixing their distinct field types" $
        Exception.evaluate (J.objectOf ((,) <$> "same" J..: J.string <*> "same" J..: (J.integer :: J.Parser Int)))
            `shouldThrow` hasErrorMessage "JStream Object - duplicate field access: same"

    it "preserves lazy field values until the caller demands them" $ do
        let deferred = fmap (const (Exception.throw DeferredField)) J.string :: J.Parser Text
            parser = J.objectOf ("field" J..: deferred)
        case J.runParser' parser "{\"field\":\"ok\"}" of
            J.ParseYield value _ -> Exception.evaluate value `shouldThrow` (== DeferredField)
            other -> expectationFailure ("expected a lazy field result, got " <> show other)

expectEveryCut :: (Eq a, Show a) => J.Parser a -> ByteString -> Either String [a] -> Expectation
expectEveryCut parser body expected =
    for_ (cuts body) $ \pieces -> collectParser parser pieces `shouldBe` expected

cuts :: ByteString -> [[ByteString]]
cuts body = [let (prefix, suffix) = BS.splitAt cut body in [prefix, suffix] | cut <- [0 .. BS.length body]]

collectParser :: J.Parser a -> [ByteString] -> Either String [a]
collectParser parser = collect (J.runParser parser)
  where
    collect (J.ParseYield value next) pieces = (value :) <$> collect next pieces
    collect (J.ParseNeedData resume) (piece : pieces) = collect (resume piece) pieces
    collect (J.ParseNeedData _) [] = Left "Incomplete input."
    collect (J.ParseFailed message) _ = Left message
    collect (J.ParseDone _) _ = Right []

hasErrorMessage :: String -> Exception.ErrorCall -> Bool
hasErrorMessage expected (Exception.ErrorCall message) = message == expected

data DeferredField = DeferredField
    deriving stock (Eq, Show)

instance Exception.Exception DeferredField

missingCount :: Int
missingCount = -1

invalidUtf8Byte :: Word8
invalidUtf8Byte = 255
