-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The packed form against aeson: its string encoding, its render with and without a rebased hole,
its decoding, and the length each value stores, for hostile values packed through the production
reader and writer.
-}
module Ecluse.Core.Registry.Json.PackedSpec (spec) where

import Data.Aeson (Value (..), encode)
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Scientific (Scientific, scientific)
import Data.Vector qualified as V
import Hedgehog (Gen, PropertyT, annotateShow, cover, diff, failure, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Registry.Json.Intern (tableTexts)
import Ecluse.Core.Registry.Json.Packed (DocTable, Packed, Piece (..), Pieces (..), RenderPlan (..), docTable, encodeString, encodedLength, packedEncodedLength, packedResident, packedValue, plain, planResident, planValue, renderPlan, tableResident, urlPrefix, withoutHole)
import Ecluse.Core.Registry.Json.Shape (Mode (Share), Shape (Generic), Trees (..), readShape)
import Ecluse.Core.Registry.Json.Walk (Steps (Finished), withElement)
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError)
import Ecluse.Core.Text (urlFilenameComponent)
import Ecluse.Test.Registry.JsonStream (readOutcome, testTable, walkJsonChunks)
import Ecluse.Test.Registry.Packed (packBytes, packTable, packValue, renderAlone, walkedLength)

spec :: Spec
spec = modifyMaxSuccess (const 2000) $ do
    describe "encodeString" $
        it "writes every string as aeson writes it, escapes, controls and astral characters included" $
            hedgehog $ do
                text <- forAll genHostileText
                encodeString text === toStrict (encodingToLazyByteString (Encoding.text text))
                encodedLength text === BS.length (encodeString text)

    describe "renderPlan, one value" $ do
        it "renders a hostile value as aeson encodes the tree its read builds, and decodes to that value" $
            hedgehog $ do
                value <- forAll (genHostile 5)
                (table, form) <- packedOf (packValue (Generic limit) [] value)
                packedValue table form === Just value
                renderAlone table Nothing form === Just (toStrict (encode (treeValue [toStrict (encode value) <> " "])))
                fmap (toStrict . encode) (packedValue table form) === renderAlone table Nothing form

        it "packs a value nested to the reader's limit, and fails a deeper one as the tree read does" $
            hedgehog $ do
                depth <- forAll (Gen.int (Range.linear (limit - 3) (limit + 1)))
                leaf <- forAll (Gen.element [Null, String "x", Number 1, Object mempty, Array mempty])
                keyed <- forAll (Gen.list (Range.singleton depth) Gen.bool)
                let nested = foldr (\isObject inner -> if isObject then Object (KeyMap.singleton "k" inner) else Array (V.singleton inner)) leaf keyed
                    chunks = [toStrict (encode nested) <> " "]
                fmap (second (fmap rendered)) (readOutcome (packBytes (Generic limit) [] chunks))
                    === fmap (second (fmap (Just . toStrict . encode))) (readOutcome (treeRead chunks))

        it "rebases the hole's URL onto a prefix, keeping the URL's file name" $
            hedgehog $ do
                url <- forAll genUrl
                prefix <- forAll genHostileText
                others <- forAll (Gen.list (Range.linear 0 4) ((,) <$> Gen.element ["name", "size", "shasum"] <*> genHostile 2))
                nested <- forAll Gen.bool
                let path = if nested then ["dist", "tarball"] else ["url"]
                    value = at path (String url) others
                    expected = toStrict (encode (at path (String (prefix <> urlFilenameComponent url)) others))
                (table, form) <- packedOf (packValue (Generic limit) path value)
                renderAlone table (Just (urlPrefix prefix)) form === Just expected
                renderAlone table Nothing form === Just (toStrict (encode value))
                renderAlone table (Just (urlPrefix prefix)) (withoutHole form) === Just (toStrict (encode value))

        it "writes a value whose path holds no string as read" $
            hedgehog $ do
                member <- forAll (Gen.element [Null, Number 7, Bool True, Object (KeyMap.singleton "url" "x"), Array (V.singleton "x")])
                let value = Object (KeyMap.fromList [("url", member), ("name", "y")])
                (table, form) <- packedOf (packValue (Generic limit) ["url"] value)
                renderAlone table (Just (urlPrefix "https://mirror/")) form === Just (toStrict (encode value))

        it "refuses a value that names a string its table lacks, rather than render or decode it short" $
            hedgehog $ do
                value <- forAll (genHostile 3)
                url <- forAll genUrl
                -- A hole under @url@ holds its own bytes, and one under @tarball@ names a table string.
                path <- forAll (Gen.element [["url"], ["dist", "tarball"]])
                (_, form) <- packedOf (packValue (Generic limit) path (at path (String url) [("name", value)]))
                let lacking prefix = RenderPlan{planMembers = mempty, planSlot = "k", planTables = fromList [docTable mempty], planPieces = ArrayPieces [Piece 0 form], planPrefix = prefix}
                renderAlone (docTable mempty) Nothing form === Nothing
                renderAlone (docTable mempty) (Just (urlPrefix "https://mirror/")) form === Nothing
                packedValue (docTable mempty) form === Nothing
                planValue (lacking Nothing) === Nothing
                planValue (lacking (Just (urlPrefix "https://mirror/"))) === Nothing

    describe "packedEncodedLength" $
        it "is the length a walk of the sealed value measures over its read's table, with and without a hole" $
            hedgehog $ do
                leaf <- forAll (Gen.frequency [(3, String <$> genUrl), (1, genHostile 2)])
                others <- forAll (Gen.list (Range.linear 0 4) ((,) <$> Gen.element ["name", "size", "shasum"] <*> Gen.frequency [(1, Gen.element [Object mempty, Array mempty]), (2, genHostile 3)]))
                path <- forAll (Gen.element [[], ["url"], ["dist", "tarball"]])
                prefix <- forAll genHostileText
                let value = at path leaf others
                    rebased = case leaf of
                        String url -> at path (String (prefix <> urlFilenameComponent url)) others
                        _ -> value
                cover 40 "with a hole" (isString leaf)
                cover 10 "without a hole" (not (isString leaf))
                cover 5 "an empty object or array" (any emptyContainer (within value))
                cover 20 "a string or key that needs an escape" (any escaped (within value))
                cover 20 "a container inside a container" (any nests (within value))
                (table, form) <- packedOf (packTable (Generic limit) path [toStrict (encode value) <> " "])
                let held = docTable (tableTexts table)
                packedEncodedLength form === walkedLength table form
                packedEncodedLength (withoutHole form) === packedEncodedLength form
                fmap BS.length (renderAlone held Nothing form) === Just (packedEncodedLength form)
                renderAlone held (Just (urlPrefix prefix)) form === Just (toStrict (encode rebased))

    describe "renderPlan" $ do
        it "renders a document whose pieces come from several tables as aeson encodes it, holes rebased" $
            hedgehog $ do
                members <- forAll (Gen.list (Range.linear 0 3) ((,) <$> genHostileText <*> genHostile 2))
                slot <- forAll genHostileText
                items <- forAll (Gen.list (Range.linear 0 5) ((,) <$> genUrl <*> Gen.list (Range.linear 0 3) ((,) <$> Gen.element ["name", "size"] <*> genHostile 2)))
                prefix <- forAll (Gen.maybe genHostileText)
                asObject <- forAll Gen.bool
                packedItems <- traverse (\(url, others) -> packedOf (packValue (Generic limit) ["url"] (at ["url"] (String url) others))) items
                let served (url, others) = at ["url"] (String (maybe url (<> urlFilenameComponent url) prefix)) others
                    pieces = [Piece index form | (index, (_, form)) <- zip [0 ..] packedItems]
                    keyed = [show index | index <- [0 .. length items - 1]]
                    plan =
                        RenderPlan
                            { planMembers = KeyMap.fromList [(Key.fromText key, value) | (key, value) <- members]
                            , planSlot = Key.fromText slot
                            , planTables = fromList (map fst packedItems)
                            , planPieces = if asObject then ObjectPieces (zip keyed pieces) else ArrayPieces pieces
                            , planPrefix = urlPrefix <$> prefix
                            }
                    slotValue
                        | asObject = Object (KeyMap.fromList (zip (map Key.fromText keyed) (map served items)))
                        | otherwise = Array (V.fromList (map served items))
                    expected = toStrict (encode (Object (KeyMap.insert (Key.fromText slot) slotValue (KeyMap.fromList [(Key.fromText key, value) | (key, value) <- members]))))
                renderPlan plan === Just expected
                fmap (toStrict . encode) (planValue plan) === Just expected
                diff (planResident plan) (>=) (sum (map (tableResident . fst) packedItems) + sum (map (packedResident . snd) packedItems))

        it "refuses a piece that names a table the plan lacks" $
            hedgehog $ do
                value <- forAll (genHostile 3)
                (table, form) <- packedOf (packValue (Generic limit) [] value)
                let plan = RenderPlan{planMembers = mempty, planSlot = "k", planTables = fromList [table], planPieces = ArrayPieces [Piece 0 form, Piece 1 form], planPrefix = Nothing}
                renderPlan plan === Nothing
                planValue plan === Nothing

    describe "resident bytes" $
        it "counts each array with its header, in whole words, and each record" $ do
            tableResident (docTable (fromList ["ab"])) `shouldBe` 24 + (16 + 8) + (16 + 16)
            case packValue (Generic limit) [] (String "abcdefghij") of
                Right (StreamResult (Right (_, form)) _) -> packedResident form `shouldBe` 24 + 16 + 8
                _ -> expectationFailure "did not pack a string"

    describe "decodeScalar" $
        it "reads each number back in the form aeson writes it, past the exponents aeson writes whole" $
            forM_ ["5e1025", "5.0e1025", "12e1030", "1e-400", "-0", "0.000", "1E+2", "-1.5e-7", "123456789012345678901234567890", "-9223372036854775809", "9223372036854775807", "1.5e308", "1e1024"] $ \number ->
                case packBytes (Generic limit) [] [number <> " "] of
                    Right (StreamResult (Right (table, form)) _) -> do
                        renderAlone table Nothing form `shouldBe` Just (toStrict (encode (treeValue [number <> " "])))
                        fmap (toStrict . encode) (packedValue table form) `shouldBe` renderAlone table Nothing form
                    _ -> expectationFailure ("did not pack " <> decodeUtf8 number)

-- The structural budget every property reads with.
limit :: Int
limit = 64

rendered :: (DocTable, Packed) -> Maybe ByteString
rendered (table, form) = renderAlone table Nothing form

-- The packed value of a read that succeeds, with its table, failing the property otherwise.
packedOf :: Either LimitError (StreamResult a) -> PropertyT IO a
packedOf = \case
    Right (StreamResult (Right result) _) -> pure result
    other -> annotateShow (readOutcome other $> ()) >> failure

-- The value at the path, in objects that also hold the other members.
at :: [Text] -> Value -> [(Text, Value)] -> Value
at path leaf others = case path of
    [] -> leaf
    name : rest -> Object (KeyMap.insert (Key.fromText name) (at rest leaf others) (KeyMap.fromList [(Key.fromText key, member) | (key, member) <- others]))

-- The value and every value it holds, at any depth.
within :: Value -> [Value]
within value =
    value : case value of
        Object fields -> concatMap within (KeyMap.elems fields)
        Array items -> concatMap within (toList items)
        _ -> []

isString :: Value -> Bool
isString = \case
    String _ -> True
    _ -> False

emptyContainer :: Value -> Bool
emptyContainer = \case
    Object fields -> KeyMap.null fields
    Array items -> V.null items
    _ -> False

-- Whether the value is a string, or holds a key, that aeson escapes.
escaped :: Value -> Bool
escaped = \case
    String text -> not (plain text)
    Object fields -> not (all (plain . Key.toText) (KeyMap.keys fields))
    _ -> False

-- Whether the value holds an object or an array as a member or an item.
nests :: Value -> Bool
nests = \case
    Object fields -> any container fields
    Array items -> any container items
    _ -> False
  where
    container = \case
        Object _ -> True
        Array _ -> True
        _ -> False

-- The tree the production reader builds for the chunks.
treeRead :: [ByteString] -> Either LimitError (StreamResult Value)
treeRead chunks = walkJsonChunks (MetadataBodyLimit (sum (map BS.length chunks))) walk chunks
  where
    walk tokens = withElement tokens $ \element rest -> readShape Trees (Generic limit) Share (testTable ["url"]) element rest (\value _ _ -> Finished value)

-- The tree of a read that succeeds, or null.
treeValue :: [ByteString] -> Value
treeValue chunks = case treeRead chunks of
    Right (StreamResult (Right value) _) -> value
    _ -> Null

-- Every escape and control character, astral and other multi-byte characters, and plain text.
genHostileText :: Gen Text
genHostileText = toText <$> Gen.list (Range.linear 0 16) (Gen.frequency [(3, Gen.unicode), (3, Gen.element hostile), (3, Gen.alphaNum)])
  where
    hostile = ['\0' .. '\x1f'] <> "\"\\/\x7f\x80\x9f\x2028\x2029\xfeff\x1F600\x10FFFF"

-- Scalars of every kind and containers to the given depth, empty ones included.
genHostile :: Int -> Gen Value
genHostile depth = Gen.frequency ([(4, scalar)] <> [(2, container) | depth > 0])
  where
    scalar = Gen.choice [pure Null, Bool <$> Gen.bool, String <$> genHostileText, Number <$> genHostileNumber]
    container =
        Gen.choice
            [ Array . V.fromList <$> Gen.list (Range.linear 0 4) (genHostile (depth - 1))
            , Object . KeyMap.fromList <$> Gen.list (Range.linear 0 4) ((,) . Key.fromText <$> genHostileText <*> genHostile (depth - 1))
            ]

-- Integers at and past 'Int''s bounds, and fractions and exponents on both sides of what aeson writes whole.
genHostileNumber :: Gen Scientific
genHostileNumber =
    Gen.choice
        [ fromIntegral <$> Gen.int Range.linearBounded
        , fromInteger <$> Gen.integral (Range.linearFrom 0 (-(10 ^ (40 :: Int))) (10 ^ (40 :: Int)))
        , scientific <$> Gen.integral (Range.linearFrom 0 (-1000000) 1000000) <*> Gen.int (Range.linearFrom 0 (-1100) 1100)
        , Gen.element [0, 5e1025, 1e1024, 1e-400, -1.5e-7]
        ]

-- URL-shaped strings with separators, queries, fragments, escapes and dot segments.
genUrl :: Gen Text
genUrl = mconcat <$> Gen.list (Range.linear 0 8) (Gen.frequency [(3, Gen.element pieces), (1, genHostileText)])
  where
    pieces = ["https://", "registry.example", "/", "//", "?", "#", "a=1", "%2F", ".", "..", "pkg-1.0.0.tgz", "\\", "\"", " "]
