-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The token cursor against json-stream's lazy token list: the same tokens, waits and failures.
module Data.JsonStream.TokenReaderSpec (spec) where

import Data.ByteString qualified as BS
import Data.JsonStream.CLexer (tokenParser)
import Data.JsonStream.TokenParser qualified as List
import Data.JsonStream.TokenReader (Element (..), Next (..), Tokens, nextToken, reusingTokenReader, tokenReader)
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Test.Corpus (corpusPackages, cpPath)
import Ecluse.Test.Registry.JsonBytes (damaged, genChunks, genJsonBytes, genPackumentBytes, genSimpleIndexBytes)

-- | Both cursors read every body, cut anywhere, to the events the lazy list yields.
spec :: Spec
spec = describe "nextToken" $ do
    modifyMaxSuccess (const 5000) $
        it "yields the lazy list's tokens, waits and failure for generated bodies and chunks" $
            hedgehog $ do
                body <- forAll (Gen.choice [genJsonBytes ["a", "b"], genPackumentBytes, genSimpleIndexBytes] >>= damaged)
                chunks <- forAll (genChunks body)
                start <- forAll (Gen.element [Fresh, Reusing])
                cursorEvents (startOf start) chunks === listEvents chunks

    it "yields the lazy list's events for every cut of a document into three pieces" $
        for_ documents $ \document ->
            for_ (cuts document) $ \chunks ->
                for_ [Fresh, Reusing] $ \start ->
                    (start, chunks, cursorEvents (startOf start) chunks) `shouldBe` (start, chunks, listEvents chunks)

    it "yields the lazy list's events for each capture in the pieces a read feeds" $
        for_ corpusPackages $ \package -> do
            body <- readFileBS (cpPath package)
            for_ [8192, 32768, 4099] $ \size ->
                for_ [Fresh, Reusing] $ \start -> do
                    let chunks = pieces size body
                    -- Compared by equality alone: a failure would print every token of the capture.
                    (cpPath package, size, start, cursorEvents (startOf start) chunks == listEvents chunks) `shouldBe` (cpPath package, size, start, True)

data Start = Fresh | Reusing
    deriving stock (Eq, Show)

startOf :: Start -> Tokens
startOf = \case
    Fresh -> tokenReader
    Reusing -> reusingTokenReader

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

-- Each cursor is read once and in order, as the reusing reader requires.
cursorEvents :: Tokens -> [ByteString] -> [Event]
cursorEvents = go
  where
    go tokens chunks = case nextToken tokens of
        PartialResult element rest -> Token element : go rest chunks
        TokFailed -> [Failure]
        TokMoreData more ->
            Wait : case chunks of
                [] -> []
                chunk : later -> go (more chunk) later

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
