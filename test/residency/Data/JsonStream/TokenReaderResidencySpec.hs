-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Raw token slices own their input after refills and reader completion.
module Data.JsonStream.TokenReaderResidencySpec (spec) where

import Control.Monad.ST (stToIO)
import Data.ByteString qualified as BS
import Data.JsonStream.TokenReader (Element (..), Next (..), Tokens, maxChunkBytes, newTokenReader, nextToken, supplyTokens)
import Foreign.StablePtr (deRefStablePtr, freeStablePtr, newStablePtr)
import GHC.Exts (RealWorld)
import System.Mem (performMajorGC)
import Test.Hspec
import UnliftIO.Exception (bracket)

import Ecluse.Test.Registry.Source (assertSourceHeld, awaitSourceRelease, trackedSource)

-- | Keep the raw slice through collections, then observe release of its backing allocation.
spec :: Spec
spec = describe "raw token ownership" $
    it "retains its source through refills and frees it after the last slice is dropped" $ do
        released <- newEmptyMVar
        bracket (readTracked released >>= newStablePtr) freeStablePtr $ \root -> do
            assertSourceHeld released
            deRefStablePtr root >>= (`shouldBe` "held")
        awaitSourceRelease released

readTracked :: MVar () -> IO ByteString
readTracked released = do
    source <- trackedSource "\"held\" " released
    tokens <- stToIO newTokenReader
    stToIO (supplyTokens tokens (BS.take maxChunkBytes source))
    payload <-
        stToIO (nextToken tokens) >>= \case
            PartialResult (StringRaw bytes True) -> pure bytes
            result -> expectationFailure ("expected a raw token, got " <> show result) >> pure BS.empty
    drain tokens
    replicateM_ refillCount $ do
        stToIO (supplyTokens tokens "[true,false,null]")
        drain tokens
        performMajorGC
    pure payload

drain :: Tokens RealWorld -> IO ()
drain tokens =
    stToIO (nextToken tokens) >>= \case
        PartialResult _ -> drain tokens
        TokMoreData -> pure ()
        TokFailed -> expectationFailure "unexpected lexer failure while refilling"

refillCount :: Int
refillCount = 4
