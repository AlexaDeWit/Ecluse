-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Token slices retain their backing allocation, while decoded text owns its copy.
module Data.JsonStream.LexerResidencySpec (spec) where

import Control.Monad (foldM_)
import Data.ByteString qualified as BS
import Data.JsonStream.Lexer (Cursor, Element (..), Scanned (..))
import Data.JsonStream.Lexer qualified as Lexer
import Data.Text qualified as T
import Foreign.StablePtr (deRefStablePtr, freeStablePtr, newStablePtr)
import System.Mem (performMajorGC)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldReturn)
import UnliftIO.Exception (bracket, evaluate)

import Ecluse.Test.Registry.Source (assertSourceHeld, awaitSourceRelease, trackedSource)

-- | Check backing-allocation lifetime through the supported token interface.
spec :: Spec
spec = describe "lexer payload ownership" $ do
    it "retains its source through refills and releases it after the last slice is dropped" $ do
        released <- newEmptyMVar
        bracket (readTracked released Lexer.plainBytes >>= newStablePtr) freeStablePtr $ \root -> do
            assertSourceHeld released
            deRefStablePtr root >>= (`shouldBe` "held")
        awaitSourceRelease released

    for_ [("split", True), ("unsplit", False)] $ \(label, split) ->
        it ("releases completed " <> label <> " number input while a later cursor stays live") $ do
            released <- newEmptyMVar
            bracket (completedNumber split released >>= newStablePtr) freeStablePtr $ \root -> do
                awaitSourceRelease released
                cursor <- deRefStablePtr root
                Lexer.remaining cursor `shouldBe` BS.empty

    it "lets copied text outlive the released input allocation" $ do
        released <- newEmptyMVar
        bracket (readTracked released Lexer.plainText >>= newStablePtr) freeStablePtr $ \root -> do
            text <- deRefStablePtr root
            evaluate (T.length text) `shouldReturn` expectedTextBytes
            awaitSourceRelease released
            text `shouldBe` "held"

completedNumber :: Bool -> MVar () -> IO Cursor
completedNumber split released = do
    source <- trackedSource "1 " released
    waiting <- drain (Lexer.start (BS.take (if split then splitPrefixBytes else wholePrefixBytes) source))
    completed <- if split then drain (Lexer.feed waiting "2 ") else pure waiting
    drain (Lexer.feed completed "[]")

readTracked :: MVar () -> (Lexer.PlainString -> a) -> IO a
readTracked released project = do
    source <- trackedSource "\"held\" " released
    case Lexer.next (Lexer.start source) of
        Token (StringPlain plain) after -> do
            payload <- evaluate (project plain)
            drained <- drain after
            foldM_ refill drained [1 .. refillCount]
            pure payload
        _ -> fail "expected a plain string token"
  where
    refill cursor _ = do
        after <- drain (Lexer.feed cursor "[true,false,null]")
        performMajorGC
        pure after

drain :: Cursor -> IO Cursor
drain cursor = case Lexer.next cursor of
    Token _ after -> drain after
    More waiting -> pure waiting
    Failed -> expectationFailure "unexpected lexer failure while refilling" >> pure cursor

refillCount, expectedTextBytes :: Int
refillCount = 4
expectedTextBytes = 4

splitPrefixBytes, wholePrefixBytes :: Int
splitPrefixBytes = 1
wholePrefixBytes = 2
