-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Recorded C lexer outputs, independent of the replacement scanner, and the skip against a loop over its tokens.
module Data.JsonStream.LexerSpec (spec) where

import Control.Monad (foldM)
import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.JsonStream.Lexer (Cursor, Element (..), Scanned (..))
import Data.JsonStream.Lexer qualified as Lexer
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldReturn, shouldSatisfy)

import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Registry.JsonStream (sliceChunks)

data Reference = Reference Text [ByteString] [Aeson.Value]

instance Aeson.FromJSON Reference where
    parseJSON = Aeson.withObject "C lexer reference" $ \object ->
        Reference <$> object .: "name" <*> (map BS.pack <$> object .: "chunks") <*> object .: "events"

-- | Check token values, chunk waits, failures, ASCII flags and leftover input.
spec :: Spec
spec = do
    guardSpec
    it "matches the recorded C scanner for every directed input and chunk cut" $ do
        references <- loadReferences
        length references `shouldBe` referenceCount
        for_ references $ \(Reference name chunks expected) ->
            (name, traceTokens chunks) `shouldBe` (name, expected)
    skipSpec

loadReferences :: IO [Reference]
loadReferences = do
    bytes <- readFileBS "core/test/unit/fixtures/json-stream/c-lexer-events.json"
    either (\failure -> [] <$ expectationFailure failure) pure (Aeson.eitherDecodeStrict bytes)

traceTokens :: [ByteString] -> [Aeson.Value]
traceTokens = traceFrom (Lexer.start BS.empty)

traceFrom :: Cursor -> [ByteString] -> [Aeson.Value]
traceFrom tokens chunks = case Lexer.next tokens of
    Token element rest -> tokenEvent element rest : traceFrom rest chunks
    Failed -> [Aeson.String "Failure"]
    More waiting ->
        Aeson.String "Wait" : case chunks of
            [] -> []
            piece : rest -> traceFrom (Lexer.feed waiting piece) rest

tokenEvent :: Element -> Cursor -> Aeson.Value
tokenEvent element after = case element of
    ArrayBegin -> tagged "ArrayBegin" []
    ObjectBegin -> tagged "ObjectBegin" []
    ArrayEnd -> tagged "ArrayEnd" ["context" .= payload rest]
    ObjectEnd -> tagged "ObjectEnd" ["context" .= payload rest]
    StringEnd -> tagged "StringEnd" ["context" .= payload rest]
    StringPlain plain -> raw (Lexer.plainBytes plain) True
    StringEncoded bytes -> raw bytes False
    StringContent bytes -> tagged "StringContent" ["payload" .= payload bytes]
    JInteger number -> tagged "JInteger" ["integer" .= (show number :: Text)]
    JValue value -> tagged "JValue" ["value" .= (show value :: Text)]
  where
    rest = Lexer.remaining after
    raw bytes ascii = tagged "StringRaw" ["payload" .= payload bytes, "ascii" .= ascii, "context" .= payload rest]
    tagged name fields = Aeson.object ("token" .= (name :: Text) : fields)

payload :: ByteString -> Aeson.Value
payload bytes = Aeson.object ["length" .= BS.length bytes, "sha256" .= hexSha256Of bytes]

referenceCount :: Int
referenceCount = 2914

guardSpec :: Spec
guardSpec = describe "immutable lexer" $ do
    it "rejects every byte that requires escaping or UTF-8 decoding as a plain string" $
        for_ allBytes $ \byte -> do
            let bytes = BS.singleton byte
                expected = byte `elem` plainByteSet
            isJust (Lexer.mkPlainString bytes) `shouldBe` expected
            case Lexer.next (Lexer.start (quoteBytes <> bytes <> quoteBytes)) of
                Token (StringPlain plain) _ -> do
                    Lexer.plainBytes plain `shouldSatisfy` BS.all (`elem` plainByteSet)
                    Lexer.decodeString (Lexer.plainBytes plain) `shouldBe` Right (Lexer.plainText plain)
                _ -> pure ()

    it "accepts empty plain strings and preserves offset slices" $ do
        fmap Lexer.plainBytes (Lexer.mkPlainString BS.empty) `shouldBe` Just BS.empty
        let backing = "invalid-prefix[\"visible\",42]invalid-suffix"
            body = "[\"visible\",42]"
            selected = BS.take (BS.length body) (BS.drop prefixBytes backing)
        before <- BS.useAsCStringLen backing BS.packCStringLen
        traceTokens [selected] `shouldBe` traceTokens [body]
        BS.useAsCStringLen backing BS.packCStringLen `shouldReturn` before

    it "keeps cursor snapshots independent when either is read again" $ do
        let original = Lexer.start "[1,2]"
        case Lexer.next original of
            Token ArrayBegin after -> do
                remainingTokens original `shouldBe` [ArrayBegin, JInteger 1, JInteger 2, ArrayEnd]
                remainingTokens after `shouldBe` [JInteger 1, JInteger 2, ArrayEnd]
                remainingTokens original `shouldBe` [ArrayBegin, JInteger 1, JInteger 2, ArrayEnd]
            _ -> expectationFailure "expected an opening array token"

    it "keeps lexical and premature-feed failures terminal" $
        for_ [Lexer.start "@", Lexer.feed (Lexer.start "[1]") "[2]"] $ \cursor -> do
            isFailed (Lexer.next cursor) `shouldBe` True
            isFailed (Lexer.next (Lexer.feed cursor "[]")) `shouldBe` True
            isFailed (Lexer.next cursor) `shouldBe` True

    it "lets independent continuations resume the same partial string" $
        case Lexer.next (Lexer.start "\"prefix") of
            Token (StringContent "prefix") after -> case Lexer.next after of
                More waiting -> do
                    remainingTokens (Lexer.feed waiting "left\"") `shouldBe` [StringContent "left", StringEnd]
                    remainingTokens (Lexer.feed waiting "right\"") `shouldBe` [StringContent "right", StringEnd]
                _ -> expectationFailure "expected a chunk boundary"
            _ -> expectationFailure "expected partial string content"

    it "checks the retained number limit before appending each incomplete piece" $
        case Lexer.next (Lexer.start (BS.replicate 200000 48)) of
            More waiting -> case Lexer.next (Lexer.feed waiting "0") of
                More overLimit -> do
                    isFailed (Lexer.next (Lexer.feed overLimit "0")) `shouldBe` True
                    remainingTokens (Lexer.feed overLimit "]") `shouldBe` [JValue (Aeson.Number 0), ArrayEnd]
                    isFailed (Lexer.next (Lexer.feed waiting "00")) `shouldBe` False
                _ -> expectationFailure "expected the piece crossing the retained limit to wait"
            _ -> expectationFailure "expected the initial number piece to wait"

    it "does not apply the retained limit to an initial piece or a completed number" $
        case Lexer.next (Lexer.start (BS.replicate 200001 48)) of
            More waiting -> do
                isFailed (Lexer.next (Lexer.feed waiting "0")) `shouldBe` True
                remainingTokens (Lexer.feed waiting "]") `shouldBe` [JValue (Aeson.Number 0), ArrayEnd]
            _ -> expectationFailure "expected the oversized initial number piece to wait"

    it "keeps number continuations independent after many tiny pieces" $ do
        let consume cursor = case Lexer.next (Lexer.feed cursor "0") of
                More waiting -> pure waiting
                _ -> expectationFailure "expected another incomplete number piece" >> pure cursor
        case Lexer.next (Lexer.start "0") of
            More initial -> do
                waiting <- foldM (\cursor _ -> consume cursor) initial [1 .. 4096 :: Int]
                remainingTokens (Lexer.feed waiting "]") `shouldBe` [JValue (Aeson.Number 0), ArrayEnd]
                remainingTokens (Lexer.feed waiting "1]") `shouldBe` [JValue (Aeson.Number 1), ArrayEnd]
                remainingTokens (Lexer.feed initial "2]") `shouldBe` [JValue (Aeson.Number 2), ArrayEnd]
            _ -> expectationFailure "expected a partial number"

    it "reads dense tokens in pieces larger than the bounded body's chunk size" $ do
        let body = "[" <> BS.intercalate "," (replicate denseCount "0") <> "]"
        remainingTokens (Lexer.start body) `shouldBe` ArrayBegin : replicate denseCount (JInteger 0) <> [ArrayEnd]

-- Where a skip ends: the trace after its closer, its open levels with what probes read next, or its failure.
data Skip = SkipClosed [Aeson.Value] | SkipOpen Int [[Aeson.Value]] | SkipStopped Text
    deriving stock (Eq, Show)

skipSpec :: Spec
skipSpec = describe "skipLevels" $
    it "stops where a loop over next stops, from every token, level and chunk cut" $ do
        references <- loadReferences
        take 1 (skipMismatches ([chunks | Reference _ chunks _ <- references] <> nestedChunks)) `shouldBe` []

skipMismatches :: [[ByteString]] -> [([ByteString], Int, Skip, Skip)]
skipMismatches inputs =
    [ (chunks, level, actual, expected)
    | input <- inputs
    , chunks <- [input, input <> closers]
    , (cursor, pieces) <- entries (Lexer.start BS.empty) chunks
    , level <- [0, 1, 2]
    , let actual = skipped level cursor pieces
          expected = referenceSkip level cursor pieces
    , actual /= expected
    ]

-- Each cursor a loop over next passes through, with the chunks not yet fed.
entries :: Cursor -> [ByteString] -> [(Cursor, [ByteString])]
entries cursor chunks =
    (cursor, chunks) : case Lexer.next cursor of
        Token _ rest -> entries rest chunks
        More waiting -> case chunks of
            piece : pieces -> entries (Lexer.feed waiting piece) pieces
            [] -> []
        Failed -> []

skipped :: Int -> Cursor -> [ByteString] -> Skip
skipped level cursor chunks = case Lexer.skipLevels level cursor of
    Lexer.Closed rest -> SkipClosed (traceFrom rest chunks)
    Lexer.Unclosed open waiting -> resume open waiting chunks skipped
    Lexer.SkipFailed -> SkipStopped lexerFailed
    Lexer.StrayStringEnd -> SkipStopped strayStringEnd

-- The walk's skip before skipLevels: drop each token of next, count levels, and pass split strings.
referenceSkip :: Int -> Cursor -> [ByteString] -> Skip
referenceSkip level cursor chunks = case Lexer.next cursor of
    Token element rest -> case element of
        ArrayEnd -> closed rest
        ObjectEnd -> closed rest
        ArrayBegin -> referenceSkip (level + 1) rest chunks
        ObjectBegin -> referenceSkip (level + 1) rest chunks
        StringContent _ -> referenceString level rest chunks
        StringEnd -> SkipStopped strayStringEnd
        _ -> referenceSkip level rest chunks
    More waiting -> resume level waiting chunks referenceSkip
    Failed -> SkipStopped lexerFailed
  where
    closed rest
        | level <= 1 = SkipClosed (traceFrom rest chunks)
        | otherwise = referenceSkip (level - 1) rest chunks

referenceString :: Int -> Cursor -> [ByteString] -> Skip
referenceString level cursor chunks = case Lexer.next cursor of
    Token (StringContent _) rest -> referenceString level rest chunks
    Token StringEnd rest -> referenceSkip level rest chunks
    Token _ _ -> SkipStopped "unexpected token in a string"
    More waiting -> resume level waiting chunks referenceString
    Failed -> SkipStopped lexerFailed

resume :: Int -> Cursor -> [ByteString] -> (Int -> Cursor -> [ByteString] -> Skip) -> Skip
resume level waiting chunks continue = case chunks of
    piece : pieces -> continue level (Lexer.feed waiting piece) pieces
    [] -> SkipOpen level [traceFrom (Lexer.feed waiting probe) [] | probe <- probes]

-- Nested containers, which the recorded inputs lack, in pieces of every size.
nestedChunks :: [[ByteString]]
nestedChunks = [sliceChunks size body | body <- nestedBodies, size <- [1 .. BS.length body]]

nestedBodies :: [ByteString]
nestedBodies =
    [ "{\"a\":[1,{\"b\":\"c\\\"d\"}],\"e\":true}"
    , "[[\"x\\\\\"],{\"y\":-1.5e3,\"z\":[]}]"
    , "[{\"a\":1]}"
    , "[\"\\u00e9\",nul]"
    , "[1,@]"
    ]

-- Inputs that tell the lexer's resumable states apart: base, string start, escape, number and literal.
probes :: [ByteString]
probes = ["]", "0]", "e]", "x\"]", "\"]\"]"]

closers :: [ByteString]
closers = ["]", "}", " 7"]

lexerFailed, strayStringEnd :: Text
lexerFailed = "the JSON lexer failed"
strayStringEnd = "unexpected end of string"

remainingTokens :: Cursor -> [Element]
remainingTokens cursor = case Lexer.next cursor of
    Token element rest -> element : remainingTokens rest
    _ -> []

isFailed :: Scanned -> Bool
isFailed Failed = True
isFailed _ = False

allBytes :: [Word8]
allBytes = [minBound .. maxBound]

printableFirst, printableLast :: Word8
printableFirst = 32
printableLast = 126

plainByteSet :: [Word8]
plainByteSet = filter (`notElem` delimiters) [printableFirst .. printableLast]

delimiters :: [Word8]
delimiters = [34, 92]

quoteBytes :: ByteString
quoteBytes = "\""

prefixBytes, denseCount :: Int
prefixBytes = 14
denseCount = 20000
