-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The writer against aeson's tree for the same read, on generated shapes, bodies and chunks.
module Ecluse.Core.Registry.Json.WriterSpec (spec) where

import Control.Monad.ST (ST)
import Data.Aeson (Value (Object, String), encode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.JsonStream.TokenParser (TokenResult)
import Hedgehog (Gen, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Registry.Json.Intern (Interned (..), decodedName, internName, tableTexts)
import Ecluse.Core.Registry.Json.Packed (docTable, packedBlob, packedBytes, packedValue, valueEnd)
import Ecluse.Core.Registry.Json.Shape (Mode (..), Shape (Generic), Trees (..), readShape)
import Ecluse.Core.Registry.Json.Walk (Steps (Finished), withElement)
import Ecluse.Core.Registry.Json.Writer (Pick (..), decodePicked, decodeWhole, newWriter, replacedMember, sealValue)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit))
import Ecluse.Test.Registry.JsonBytes (damaged, genChunks, genJsonBytes)
import Ecluse.Test.Registry.JsonStream (readOutcome, testTable, walkJsonChunks, walkWritingChunks)
import Ecluse.Test.Registry.Packed (renderAlone)
import Ecluse.Test.Registry.Shape (genShape, shapeNames, toShape)

-- | A read that writes fails where the tree read fails, and otherwise holds the tree it would build.
spec :: Spec
spec = describe "Writer" $
    modifyMaxSuccess (const 3000) $ do
        it "packs a value that decodes and renders as the tree aeson would hold, for generated shapes, bodies and chunks" $
            hedgehog $ do
                shape <- forAll (genShape 3)
                body <- forAll (genJsonBytes shapeNames >>= damaged)
                chunks <- forAll (genChunks body)
                share <- forAll Gen.bool
                let bound = MetadataBodyLimit (BS.length body)
                    mode = if share then Share else Keep
                    tree tokens = withElement tokens $ \element rest ->
                        readShape Trees (toShape shape) mode (testTable ["url"]) element rest $ \value _ _ ->
                            Finished (Just (value, value, toStrict (encode value), toStrict (encode value), True))
                    packed :: ST st (TokenResult -> ST st (Steps (ST st) (Maybe (Value, Value, ByteString, ByteString, Bool))))
                    packed =
                        newWriter Nothing <&> \writer tokens -> withElement tokens $ \element rest ->
                            readShape writer (toShape shape) mode (testTable ["url"]) element rest $ \() table _ -> do
                                form <- sealValue writer []
                                shared <- decodeWhole writer form
                                let held = docTable (tableTexts table)
                                    decoded = packedValue held form
                                pure (Finished (Just (shared, decoded, renderAlone held Nothing form, toStrict (encode decoded), valueEnd (packedBlob form) 0 == packedBytes form)))
                readOutcome (walkJsonChunks bound tree chunks) === readOutcome (walkWritingChunks bound packed chunks)

        it "decodes only the members a pick names, as the tree holds them" $
            hedgehog $ do
                body <- forAll (genJsonBytes shapeNames)
                pick <- forAll (genPick 2)
                let bound = MetadataBodyLimit (BS.length body)
                    tree tokens = withElement tokens $ \element rest ->
                        readShape Trees (Generic 8) Share (testTable ["url"]) element rest (\value _ _ -> Finished (restrict pick value))
                    packed :: ST st (TokenResult -> ST st (Steps (ST st) Value))
                    packed =
                        newWriter Nothing <&> \writer tokens -> withElement tokens $ \element rest ->
                            readShape writer (Generic 8) Share (testTable ["url"]) element rest $ \() _ _ ->
                                sealValue writer [] >>= decodePicked writer (asPick pick) <&> Finished
                readOutcome (walkJsonChunks bound tree [body]) === readOutcome (walkWritingChunks bound packed [body])

        it "puts the added member in place of an object's own, and keeps the one it replaced until the next write" $
            hedgehog $ do
                body <- forAll (genJsonBytes ("author" : shapeNames) >>= damaged)
                chunks <- forAll (genChunks body)
                let bound = MetadataBodyLimit (BS.length body)
                    Interned key withKey = internName (decodedName "author") (testTable ["url"])
                    Interned pointer table = internName (decodedName "See the source") withKey
                    tree tokens = withElement tokens $ \element rest ->
                        readShape Trees (Generic 8) Share table element rest $ \value _ _ -> Finished (withPointer value, ownAuthor value)
                    packed :: ST st (TokenResult -> ST st (Steps (ST st) (Value, Maybe Value)))
                    packed =
                        newWriter (Just (key, pointer)) <&> \writer tokens -> withElement tokens $ \element rest ->
                            readShape writer (Generic 8) Share table element rest $ \() _ _ -> do
                                whole <- sealValue writer [] >>= decodeWhole writer
                                original <- replacedMember writer
                                pure (Finished (whole, original <* ownAuthor whole))
                readOutcome (walkJsonChunks bound tree chunks) === readOutcome (walkWritingChunks bound packed chunks)
  where
    withPointer = \case
        Object fields -> Object (KeyMap.insert "author" (String "See the source") fields)
        other -> other
    ownAuthor = \case
        Object fields -> KeyMap.lookup "author" fields
        _ -> Nothing

-- A pick with a 'Show' instance, for generation.
data TestPick = TestWhole | TestOnly [(Text, TestPick)]
    deriving stock (Show)

genPick :: Int -> Gen TestPick
genPick depth =
    Gen.frequency
        ( [(1, pure TestWhole)]
            <> [(2, TestOnly <$> Gen.list (Range.linear 0 3) ((,) <$> Gen.element (map decodeUtf8 shapeNames) <*> genPick (depth - 1))) | depth > 0]
        )

asPick :: TestPick -> Pick
asPick = \case
    TestWhole -> Whole
    TestOnly picks -> Only [(name, asPick pick) | (name, pick) <- picks]

-- The parts of a tree a pick names: an object's named members, and any other value whole.
restrict :: TestPick -> Value -> Value
restrict pick value = case (pick, value) of
    (TestOnly picks, Object fields) -> Object (KeyMap.mapMaybeWithKey (\key member -> (`restrict` member) . snd <$> find ((== Key.toText key) . fst) picks) fields)
    _ -> value
