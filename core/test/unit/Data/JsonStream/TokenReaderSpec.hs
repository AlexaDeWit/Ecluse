-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The token cursor against json-stream's lazy token list: the same tokens, waits and failures.
module Data.JsonStream.TokenReaderSpec (spec) where

import Control.Monad.ST (ST, runST, stToIO)
import Data.ByteString qualified as BS
import Data.JsonStream.CLexer (tokenParser)
import Data.JsonStream.TokenParser qualified as List
import Data.JsonStream.TokenReader (Element (..), Next (..), Tokens, maxChunkBytes, newTokenReader, nextToken, supplyTokens)
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Test.Corpus (corpusPackages, cpPath)
import Ecluse.Test.Registry.JsonBytes (damaged, genChunks, genJsonBytes, genPackumentBytes, genSimpleIndexBytes)

-- | The owned reader preserves the lazy list's tokens, waits, and failures.
spec :: Spec
spec = describe "nextToken" $ do
    modifyMaxSuccess (const 5000) $
        it "yields the lazy list's tokens, waits and failure for generated bodies and chunks" $
            hedgehog $ do
                body <- forAll (Gen.choice [genJsonBytes ["a", "b"], genPackumentBytes, genSimpleIndexBytes] >>= damaged)
                chunks <- forAll (genChunks body)
                cursorEvents chunks === listEvents chunks

    it "yields the lazy list's events for every cut of a document into three pieces" $
        for_ documents $ \document ->
            for_ (cuts document) $ \chunks ->
                (chunks, cursorEvents chunks) `shouldBe` (chunks, listEvents chunks)

    it "yields the lazy list's events for each capture in the pieces a read feeds" $
        for_ corpusPackages $ \package -> do
            body <- readFileBS (cpPath package)
            for_ [8192, 32768, 4099] $ \size -> do
                let chunks = pieces size body
                -- Compared by equality alone: a failure would print every token of the capture.
                (cpPath package, size, cursorEvents chunks == listEvents chunks) `shouldBe` (cpPath package, size, True)

    it "keeps returned payloads valid after many buffer refills and the reader ends" $ do
        let chunks = ["[\"first\",12345]", "[\"replacement\",67890]"] <> replicate 100 "[true,false,null]"
            result = cursorEvents chunks
        result `shouldBe` listEvents chunks

    it "reads dense tokens within one input piece" $ do
        let chunks = ["[" <> BS.intercalate "," (replicate 12000 "0") <> "]"]
        cursorEvents chunks `shouldBe` listEvents chunks

    it "reads alternating small and large pieces" $ do
        let array count = "[" <> BS.intercalate "," (replicate count "0") <> "]"
            chunks = map array [1, 1, 20, 1, 40, 1, 400, 100, 10000, 5, 16000]
        cursorEvents chunks `shouldBe` listEvents chunks

    it "reads only the selected slice of a larger backing value" $ do
        let prefix = "invalid-prefix"
            body = "[\"visible\",42]"
            suffix = "invalid-suffix"
            backing = prefix <> body <> suffix
            selected = BS.take (BS.length body) (BS.drop (BS.length prefix) backing)
        original <- BS.useAsCStringLen backing BS.packCStringLen
        tokens <- stToIO newTokenReader
        observed <- stToIO (supplyTokens tokens selected >> readEvents tokens [])
        unchanged <- BS.useAsCStringLen backing BS.packCStringLen
        observed `shouldBe` dropInitialWait (listEvents [body])
        unchanged `shouldBe` original

    it "keeps a lexical failure terminal across later input" $ do
        failuresAfter (`supplyTokens` "@") `shouldBe` terminalFailures

    it "keeps interleaved readers independent" $ do
        let left = ["[\"left", " side\",12", "3]"]
            right = ["[\"right", " side\",45", "6]"]
            result = runST $ do
                a <- newTokenReader
                b <- newTokenReader
                pairs <- forM (zip left right) $ \(x, y) -> do
                    supplyTokens a x
                    supplyTokens b y
                    leftEvents <- readEvents a []
                    rightEvents <- readEvents b []
                    pure (leftEvents, rightEvents)
                pure (concatMap fst pairs, concatMap snd pairs)
        result `shouldBe` (dropInitialWait (listEvents left), dropInitialWait (listEvents right))

    it "advances aliases without exposing an earlier buffer position" $ do
        let result = runST $ do
                tokens <- newTokenReader
                supplyTokens tokens "[1]"
                opening <- nextToken tokens
                let alias = tokens
                number <- nextToken alias
                closing <- nextToken tokens
                pure [opening, number, closing]
        result `shouldBe` [PartialResult ArrayBegin, PartialResult (JInteger 1), PartialResult ArrayEnd]

    it "refuses a piece above the reader's buffer bound" $ do
        failuresAfter (\tokens -> supplyTokens tokens (BS.replicate (maxChunkBytes + 1) 32)) `shouldBe` terminalFailures

    it "refuses input supplied over unread results" $ do
        failuresAfter (\tokens -> supplyTokens tokens "[1,2,3]" >> supplyTokens tokens "[4,5,6]") `shouldBe` terminalFailures

failuresAfter :: (forall st. Tokens st -> ST st ()) -> [Next]
failuresAfter prepare = runST $ do
    tokens <- newTokenReader
    prepare tokens
    firstResult <- nextToken tokens
    supplyTokens tokens "[]"
    resupplied <- nextToken tokens
    repeated <- nextToken tokens
    pure [firstResult, resupplied, repeated]

terminalFailures :: [Next]
terminalFailures = [TokFailed, TokFailed, TokFailed]

dropInitialWait :: [Event] -> [Event]
dropInitialWait = \case
    Wait : rest -> rest
    events -> events

-- What a consumer sees between the start of a body and its last chunk.
data Event = Token Element | Wait | Failure
    deriving stock (Eq, Show)

listEvents :: [ByteString] -> [Event]
listEvents = go (tokenParser BS.empty)
  where
    go tokens chunks = case tokens of
        List.PartialResult element rest -> Token (cursorElement element) : go rest chunks
        List.TokFailed -> [Failure]
        List.TokMoreData more ->
            Wait : case chunks of
                [] -> []
                chunk : later -> go (more chunk) later

cursorEvents :: [ByteString] -> [Event]
cursorEvents chunks = runST (newTokenReader >>= \tokens -> readEvents tokens chunks)

readEvents :: Tokens st -> [ByteString] -> ST st [Event]
readEvents tokens = go []
  where
    go events chunks =
        nextToken tokens >>= \case
            PartialResult element -> go (Token element : events) chunks
            TokFailed -> pure (reverse (Failure : events))
            TokMoreData -> case chunks of
                [] -> pure (reverse (Wait : events))
                chunk : later -> supplyTokens tokens chunk >> go (Wait : events) later

-- The list's element without the rest of the input it carries for json-stream's own parser.
cursorElement :: List.Element -> Element
cursorElement = \case
    List.ArrayBegin -> ArrayBegin
    List.ArrayEnd _ -> ArrayEnd
    List.ObjectBegin -> ObjectBegin
    List.ObjectEnd _ -> ObjectEnd
    List.StringContent part -> StringContent part
    List.StringRaw bytes ascii _ -> StringRaw bytes ascii
    List.StringEnd _ -> StringEnd
    List.JValue value -> JValue value
    List.JInteger number -> JInteger number

pieces :: Int -> ByteString -> [ByteString]
pieces size body
    | BS.null body = []
    | otherwise = BS.take size body : pieces size (BS.drop size body)

-- Every cut of a body into at most three pieces, empty pieces included.
cuts :: ByteString -> [[ByteString]]
cuts body = [[BS.take near body, BS.take (far - near) (BS.drop near body), BS.drop far body] | near <- [0 .. size], far <- [near .. size]]
  where
    size = BS.length body

-- Well-formed values, the lexer's leniencies, split numbers and strings, and input the lexer fails.
documents :: [ByteString]
documents =
    [ "{\"name\":\"thing\",\"versions\":{\"1.0.0\":{\"dist\":{\"tarball\":\"https://r/x.tgz\",\"n\":12.5}},\"2.0.0\":null},\"k\":[true,false,null,-1,1e5,0.25]}"
    , "[1,22,4444.5,-5e-3,1E+2,12345678901234567890,1234567890123456789012345678901234567890.5,\"a\\u00e9b\",\"\xc3\xa9\",\"\",{},[],[[[]]]]"
    , "{\"a\" \"b\",,\"c\"::1 ,, [1 2 3] true:false}"
    , "[-,.,-.,+,1.2.3,--1,1e,+1,01,0.000,1e18446744073709551617]"
    , "[true,false,null]true false nulll truex"
    , "\"esc\\\"aped\\\\\" \"unterminated\\"
    , "{\"k\":\"v\"}garbage"
    , "}{][\"\\ud800\",\"\xff\xc0\x80\",\"\x01\x1f\"]"
    , "-"
    , "tru"
    , ""
    ]
