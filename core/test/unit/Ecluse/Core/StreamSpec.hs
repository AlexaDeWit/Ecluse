-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The advisory limiter preserves chunks and stops before forwarding excess bytes.
module Ecluse.Core.StreamSpec (spec) where

import Conduit (runConduit, yieldMany, (.|))
import Data.Conduit.Combinators qualified as C
import Test.Hspec (Spec, describe, expectationFailure, it, shouldReturn)

import Ecluse.Core.Stream (boundBytes)

-- | Exercise the shared cap without a network or decompressor.
spec :: Spec
spec = describe "boundBytes" $ do
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

collect :: Int -> [ByteString] -> IO [ByteString]
collect cap chunks =
    runConduit (yieldMany chunks .| boundBytes cap (const (expectationFailure "unexpected byte cap breach")) .| C.sinkList)
