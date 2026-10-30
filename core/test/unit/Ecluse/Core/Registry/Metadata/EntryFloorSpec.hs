-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | A full read pays the per-entry floor its per-byte charge leaves uncovered, after each piece.
module Ecluse.Core.Registry.Metadata.EntryFloorSpec (spec) where

import Control.Monad.ST (RealWorld, ST, stToIO)
import Data.ByteString qualified as BS
import Data.JsonStream.Lexer (Cursor)
import Data.Primitive.PrimVar (PrimVar, newPrimVar, readPrimVar, writePrimVar)
import Test.Hspec

import Ecluse.Core.Registry.JsonStream (Steps (..), StreamResult (..))
import Ecluse.Core.Registry.Metadata.EntryFloor (RetainedEntries (RetainedEntries), countingEntries, entryFloorBytes, entryFloorCharge, readFlooredWalk, releaseFloorBytes)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (TooManyVersions))
import Ecluse.Test.Registry.JsonStream (heldChunks)

spec :: Spec
spec = describe "the full-read entry floor" $ do
    it "charges each retained entry at least 704 bytes" $
        entryFloorBytes `shouldBe` 704

    it "charges each npm version and PyPI file at least 1152 bytes" $
        releaseFloorBytes `shouldBe` 1152

    it "owes the release floor for each version or file and the entry floor for every other entry" $ do
        entryFloorCharge (RetainedEntries 2 3) `shouldBe` 2 * 1152 + 3 * 704
        entryFloorCharge mempty `shouldBe` 0

    it "saturates the floors at the largest charge" $
        entryFloorCharge (RetainedEntries maxBound 1) `shouldBe` maxBound

    it "pays nothing while the per-byte charge covers every entry" $ do
        (result, payments) <- floored 1000 ["{" <> BS.replicate 1000 120 <> ",}"]
        result `shouldBe` Right (StreamResult (Right 1) 1003)
        payments `shouldBe` []

    it "raises the workspace after each piece to the floor less the per-byte charge so far" $ do
        (result, payments) <- floored 1000 ["{,,,,", ",,,}"]
        result `shouldBe` Right (StreamResult (Right 7) 9)
        payments `shouldBe` [4 * 704 - 5, 7 * 704 - 9]

    it "publishes the floors of an accepted state's entries and leaves a refused one unpublished" $ do
        counts <- stToIO $ do
            counter <- newPrimVar 0
            let entries others = RetainedEntries 0 (length (others :: [()]))
            accepted <- countingEntries counter entries (const (readPrimVar counter)) (Right [(), (), ()])
            refused <- countingEntries counter entries (const (readPrimVar counter)) (Left (TooManyVersions 2 1))
            pure (accepted, refused)
        counts `shouldBe` (3 * 704, 3 * 704)

-- The walk result and every workspace payment, in order, for a read at @permille@ per byte.
floored :: Int -> [ByteString] -> IO (Either LimitError (StreamResult Int), [Int])
floored permille chunks = do
    paid <- newIORef []
    readChunk <- heldChunks chunks
    result <- readFlooredWalk (\amount -> modifyIORef' paid (amount :)) permille (MetadataBodyLimit (sum (map BS.length chunks))) commaWalk readChunk
    (result,) . reverse <$> readIORef paid

-- Publish the floor of one retained entry for each comma, and finish at the closing brace.
commaWalk :: PrimVar RealWorld Int -> Cursor -> ST RealWorld (Steps (ST RealWorld) Int)
commaWalk owed _ = pure (go 0)
  where
    go seen = NeedData $ \piece -> do
        let counted = seen + BS.count 44 piece
        writePrimVar owed (entryFloorBytes * counted)
        pure (if BS.elem 125 piece then Finished counted else go counted)
