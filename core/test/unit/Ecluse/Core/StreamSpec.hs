-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The advisory limiters preserve what passes and stop before forwarding excess bytes.
module Ecluse.Core.StreamSpec (spec) where

import Conduit (runConduit, yieldMany, (.|))
import Data.Conduit.Combinators qualified as C
import Test.Hspec (Spec, describe, expectationFailure, it, shouldReturn)

import Ecluse.Core.Stream (boundBytes, boundLines)

-- | Exercise the shared caps without a network or decompressor.
spec :: Spec
spec = do
    boundBytesSpec
    boundLinesSpec

boundBytesSpec :: Spec
boundBytesSpec = describe "boundBytes" $ do
    it "preserves binary bytes below the cap" $
        collect 8 ["\NUL\255", "\128x"] `shouldReturn` ["\NUL\255", "\128x"]

    it "preserves bytes and chunk boundaries at the exact cap" $
        collect 4 ["a", "bcd"] `shouldReturn` ["a", "bcd"]

    it "preserves empty chunks before, between and after nonempty chunks" $
        collect 4 ["", "ab", "", "cd", ""] `shouldReturn` ["", "ab", "", "cd", ""]

    it "accepts an empty stream and empty chunks under a zero cap" $ do
        collect 0 [] `shouldReturn` []
        collect 0 ["", ""] `shouldReturn` ["", ""]

    it "stops before the excess chunk even when the breach action returns" $ do
        pulled <- newIORef ([] :: [ByteString])
        breaches <- newIORef ([] :: [Int])
        let record chunk = modifyIORef' pulled (<> [chunk]) $> chunk
        runConduit
            ( yieldMany ["ab", "cde", "unread"]
                .| C.mapM record
                .| boundBytes 4 (\seen -> modifyIORef' breaches (<> [seen]))
                .| C.sinkList
            )
            `shouldReturn` ["ab"]
        readIORef pulled `shouldReturn` ["ab", "cde"]
        readIORef breaches `shouldReturn` [5]

    it "refuses the first nonempty chunk under a zero cap" $
        runConduit (yieldMany ["x", "y"] .| boundBytes 0 (const pass) .| C.sinkList)
            `shouldReturn` []

boundLinesSpec :: Spec
boundLinesSpec = describe "boundLines" $ do
    it "splits lines across chunk boundaries and keeps an unterminated last line" $
        lineCollect 8 ["ab\ncd", "e\n", "", "\nfg"] `shouldReturn` ["ab", "cde", "", "fg"]

    it "passes a line at the exact cap" $
        lineCollect 4 ["abcd\n"] `shouldReturn` ["abcd"]

    it "stops on a finished line past the cap, forwarding only the lines before it" $ do
        breaches <- newIORef ([] :: [Int])
        runConduit (yieldMany ["ok\ntoo-long\nunread\n"] .| boundLines 4 (\seen -> modifyIORef' breaches (<> [seen])) .| C.sinkList)
            `shouldReturn` ["ok"]
        readIORef breaches `shouldReturn` [8]

    it "stops an unfinished line once it passes the cap, without reading the rest of the stream" $ do
        pulled <- newIORef (0 :: Int)
        breaches <- newIORef ([] :: [Int])
        let count chunk = modifyIORef' pulled (+ 1) $> chunk
        runConduit
            ( yieldMany (replicate 100_000 "x")
                .| C.mapM count
                .| boundLines 4 (\seen -> modifyIORef' breaches (<> [seen]))
                .| C.sinkList
            )
            `shouldReturn` []
        readIORef breaches `shouldReturn` [5]
        readIORef pulled `shouldReturn` 5

lineCollect :: Int -> [ByteString] -> IO [ByteString]
lineCollect cap chunks =
    runConduit (yieldMany chunks .| boundLines cap (const (expectationFailure "unexpected line cap breach")) .| C.sinkList)

collect :: Int -> [ByteString] -> IO [ByteString]
collect cap chunks =
    runConduit (yieldMany chunks .| boundBytes cap (const (expectationFailure "unexpected byte cap breach")) .| C.sinkList)
