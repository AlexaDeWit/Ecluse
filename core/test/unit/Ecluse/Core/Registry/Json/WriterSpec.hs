-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The writer against aeson's tree for the same read, on generated shapes, bodies and chunks.
module Ecluse.Core.Registry.Json.WriterSpec (spec) where

import Control.Monad.ST (ST)
import Data.Aeson (Value (Object, String), decodeStrict, encode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.JsonStream.Lexer (Cursor)
import Hedgehog (Gen, PropertyT, cover, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Registry.Json.Intern (Entry, InternTable, Interned (..), decodedName, internName, tableTexts)
import Ecluse.Core.Registry.Json.Packed (Packed, docTable, hasHole, packedBlob, packedBytes, packedEncodedLength, packedValue, urlPrefix, valueEnd)
import Ecluse.Core.Registry.Json.Shape (Mode (..), Shape (Generic, Scalar), Trees (..), prepareMembers, readShape)
import Ecluse.Core.Registry.Json.Walk (Steps (Finished), withElement)
import Ecluse.Core.Registry.Json.Writer (Pick (..), decodePicked, decodeWhole, newWriter, replacedMember, sealValue)
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError)
import Ecluse.Test.Json (genValue)
import Ecluse.Test.Registry.JsonBytes (damaged, genChunks, genJsonBytes)
import Ecluse.Test.Registry.JsonStream (readOutcome, testTable, walkJsonChunks, walkWritingChunks)
import Ecluse.Test.Registry.Packed (renderAlone, walkedLength)
import Ecluse.Test.Registry.Shape (genShape, shapeNames, toShape, toShapeWith)

-- | A read that writes fails where the tree read fails, and otherwise holds the tree it would build.
spec :: Spec
spec = describe "Writer" $ do
    modifyMaxSuccess (const 3000) $ do
        it "packs a value that decodes and renders as the tree aeson would hold, for generated shapes, bodies and chunks" $
            hedgehog $ do
                shape <- forAll (genShape 3)
                body <- forAll (genJsonBytes shapeNames >>= damaged)
                chunks <- forAll (genChunks body)
                share <- forAll Gen.bool
                prepared <- forAll Gen.bool
                let retained = if prepared then toShapeWith (prepareMembers (testTable ["url"])) shape else toShape shape
                sameAsTree retained (if share then Share else Keep) chunks

        it "decodes only the members a pick names, as the tree holds them" $
            hedgehog $ do
                body <- forAll (genJsonBytes shapeNames)
                pick <- forAll (genPick 2)
                let bound = MetadataBodyLimit (BS.length body)
                    tree tokens = withElement tokens $ \element rest ->
                        readShape Trees (Generic 8) Share (testTable ["url"]) element rest (\value _ _ -> Finished (restrict pick value))
                    packed :: ST st (Cursor -> ST st (Steps (ST st) Value))
                    packed =
                        newWriter Nothing <&> \writer tokens -> withElement tokens $ \element rest ->
                            readShape writer (Generic 8) Share (testTable ["url"]) element rest $ \() _ _ ->
                                sealValue writer [] >>= decodePicked writer (asPick pick) <&> Finished
                readOutcome (walkJsonChunks bound tree [body]) === readOutcome (walkWritingChunks bound packed [body])

        it "puts the added member in place of an object's own, and keeps the one it replaced" $
            hedgehog $ do
                body <- forAll (genJsonBytes ("author" : shapeNames) >>= damaged)
                chunks <- forAll (genChunks body)
                let bound = MetadataBodyLimit (BS.length body)
                    tree tokens = withElement tokens $ \element rest ->
                        readShape Trees (Generic 8) Share authorTable element rest $ \value _ _ -> Finished (withPointer value, ownAuthor value)
                    packed :: ST st (Cursor -> ST st (Steps (ST st) (Value, Maybe Value)))
                    packed =
                        newWriter (Just (authorKey, pointer)) <&> \writer tokens -> withElement tokens $ \element rest ->
                            readShape writer (Generic 8) Share authorTable element rest $ \() _ _ -> do
                                whole <- sealValue writer [] >>= decodeWhole writer
                                original <- replacedMember writer
                                pure (Finished (whole, original))
                readOutcome (walkJsonChunks bound tree chunks) === readOutcome (walkWritingChunks bound packed chunks)

    modifyMaxSuccess (const 500) $
        it "writes objects of 17 to 100 members with repeated and hostile keys as the tree holds them" $
            hedgehog $ do
                body <- forAll (genWideObject 2)
                chunks <- forAll (genChunks body)
                share <- forAll Gen.bool
                sameAsTree (Generic 16) (if share then Share else Keep) chunks

    it "writes a read with more distinct strings than its first arrays hold, the first member under each key kept" $ do
        let distinct = [0 .. 299 :: Int]
            quoted text = "\"" <> text <> "\""
            pairs = [quoted ("k" <> show n) <> ":" <> quoted ("v" <> show n) | n <- distinct] <> [quoted ("k" <> show n) <> ":\"repeat\"" | n <- [0, 150, 299 :: Int]]
            body = "{" <> BS.intercalate "," (pairs <> ["\"list\":[" <> BS.intercalate "," [quoted ("s" <> show n) | n <- distinct] <> "]"]) <> "}"
        uncurry shouldBe (sameOutcomes (Generic 8) Share [body])

    it "forgets the replaced member of an earlier object once it seals a value that is not one" $ do
        let body = "{\"author\":{\"name\":\"abcdefghij\"}} \"zzzzzzzzzzzzzzzzzzzz\" "
            bound = MetadataBodyLimit (BS.length body)
            packed :: ST st (Cursor -> ST st (Steps (ST st) (Maybe Value, Maybe Value)))
            packed =
                newWriter (Just (authorKey, pointer)) <&> \writer tokens -> withElement tokens $ \element rest ->
                    readShape writer (Generic 8) Share authorTable element rest $ \() table afterObject -> do
                        first' <- sealValue writer [] >> replacedMember writer
                        withElement afterObject $ \next afterString ->
                            readShape writer (Scalar 8) Keep table next afterString $ \() _ _ -> do
                                second' <- sealValue writer [] >> replacedMember writer
                                pure (Finished (first', second'))
        readOutcome (walkWritingChunks bound packed [body])
            `shouldBe` Right (BS.length body, Right (Just (Object (KeyMap.singleton "name" (String "abcdefghij"))), Nothing))

    for_ [False, True] $ \shared -> for_ sealCases $ \(label, path, body, expectedBody, rebasedBody) ->
        it (label <> if shared then " with shared keys" else " with owned keys") $ do
            let mode = if shared then Share else Keep
                bound = MetadataBodyLimit (BS.length body)
                setup :: ST st (Cursor -> ST st (Steps (ST st) (InternTable, Packed)))
                setup =
                    newWriter Nothing <&> \writer tokens -> withElement tokens $ \element rest ->
                        readShape writer (Generic 16) mode (testTable ["url"]) element rest $ \() table _ -> do
                            form <- sealValue writer path
                            pure (Finished (table, form))
            case walkWritingChunks bound setup [body] of
                Left fault -> expectationFailure (show fault)
                Right (StreamResult (Left fault) _) -> expectationFailure (show fault)
                Right (StreamResult (Right (table, form)) _) -> do
                    let held = docTable (tableTexts table)
                        expected = decodeStrict expectedBody :: Maybe Value
                        rebased = decodeStrict (fromMaybe expectedBody rebasedBody) :: Maybe Value
                    expected `shouldSatisfy` isJust
                    rebased `shouldSatisfy` isJust
                    packedValue held form `shouldBe` expected
                    hasHole form `shouldBe` isJust rebasedBody
                    Just (packedEncodedLength form) `shouldBe` (BS.length . toStrict . encode <$> expected)
                    packedEncodedLength form `shouldBe` walkedLength table form
                    valueEnd (packedBlob form) 0 `shouldBe` packedBytes form
                    renderAlone held Nothing form `shouldBe` (toStrict . encode <$> expected)
                    renderAlone held (Just (urlPrefix "https://mirror/")) form `shouldBe` (toStrict . encode <$> rebased)

    it "forces sealed lengths before the writer grows its shared-string arrays for another value" $ do
        let firstBody = "{\"dist\":{\"tarball\":\"https://files.example/a.tgz\"},\"z\":\"kept\"}"
            laterValue = Object (KeyMap.fromList [(Key.fromText ("k" <> show n), String ("v" <> show n)) | n <- [0 .. 299 :: Int]])
            body = firstBody <> " " <> toStrict (encode laterValue) <> " "
            bound = MetadataBodyLimit (BS.length body)
            setup :: ST st (Cursor -> ST st (Steps (ST st) (InternTable, Packed, Packed)))
            setup =
                newWriter Nothing <&> \writer tokens -> withElement tokens $ \element rest ->
                    readShape writer (Generic 16) Share (testTable ["url"]) element rest $ \() table afterFirst -> do
                        firstForm <- sealValue writer ["dist", "tarball"]
                        withElement afterFirst $ \element2 rest2 ->
                            readShape writer (Generic 16) Share table element2 rest2 $ \() finalTable _ -> do
                                laterForm <- sealValue writer ["absent"]
                                pure (Finished (finalTable, firstForm, laterForm))
        case walkWritingChunks bound setup [body] of
            Left fault -> expectationFailure (show fault)
            Right (StreamResult (Left fault) _) -> expectationFailure (show fault)
            Right (StreamResult (Right (table, firstForm, laterForm)) _) -> do
                let held = docTable (tableTexts table)
                    expected = decodeStrict firstBody :: Maybe Value
                    rebased = decodeStrict "{\"dist\":{\"tarball\":\"https://mirror/a.tgz\"},\"z\":\"kept\"}" :: Maybe Value
                expected `shouldSatisfy` isJust
                packedValue held firstForm `shouldBe` expected
                packedValue held laterForm `shouldBe` Just laterValue
                Just (packedEncodedLength firstForm) `shouldBe` (BS.length . toStrict . encode <$> expected)
                packedEncodedLength laterForm `shouldBe` BS.length (toStrict (encode laterValue))
                renderAlone held (Just (urlPrefix "https://mirror/")) firstForm `shouldBe` (toStrict . encode <$> rebased)
                hasHole firstForm `shouldBe` True
                hasHole laterForm `shouldBe` False
  where
    withPointer = \case
        Object fields -> Object (KeyMap.insert "author" (String "See the source") fields)
        other -> other
    ownAuthor = \case
        Object fields -> KeyMap.lookup "author" fields
        _ -> Nothing

sealCases :: [(String, [Text], ByteString, ByteString, Maybe ByteString)]
sealCases =
    [ ("keeps an empty-path string hole", [], "\"a.tgz\" ", "\"a.tgz\"", Just "\"https://mirror/a.tgz\"")
    , ("keeps an empty-path number without a hole", [], "12 ", "12", Nothing)
    , ("does not follow a member path through a root array", ["url"], "[{\"url\":\"a.tgz\"}]", "[{\"url\":\"a.tgz\"}]", Nothing)
    , ("does not follow a member path through a nested array", ["dist", "tarball"], "{\"dist\":[{\"tarball\":\"a.tgz\"}],\"z\":[]}", "{\"dist\":[{\"tarball\":\"a.tgz\"}],\"z\":[]}", Nothing)
    , ("measures all subtrees when the target is absent", ["missing"], "{\"a\":{\"k\":[1,2]},\"z\":\"tail\"}", "{\"a\":{\"k\":[1,2]},\"z\":\"tail\"}", Nothing)
    , ("does not repair a first non-string target with a duplicate", ["url"], "{\"a\":[1,2],\"url\":null,\"url\":\"a.tgz\",\"z\":\"tail\"}", "{\"a\":[1,2],\"url\":null,\"z\":\"tail\"}", Nothing)
    , ("does not repair a first non-object ancestor with a duplicate", ["dist", "tarball"], "{\"dist\":false,\"dist\":{\"tarball\":\"a.tgz\"},\"z\":{}}", "{\"dist\":false,\"z\":{}}", Nothing)
    , ("finds decoded escaped member names", ["dist", "tarball"], "{\"di\\u0073t\":{\"tar\\u0062all\":\"https://files.example/a.tgz\"},\"z\":[]}", "{\"dist\":{\"tarball\":\"https://files.example/a.tgz\"},\"z\":[]}", Just "{\"dist\":{\"tarball\":\"https://mirror/a.tgz\"},\"z\":[]}")
    , ("matches quoted and backslash member names", ["a\"", "b\\"], "{\"a\\\"\":{\"b\\\\\":\"https://files.example/a.tgz\"},\"z\":[]}", "{\"a\\\"\":{\"b\\\\\":\"https://files.example/a.tgz\"},\"z\":[]}", Just "{\"a\\\"\":{\"b\\\\\":\"https://mirror/a.tgz\"},\"z\":[]}")
    , ("matches an empty member name", ["", "url"], "{\"\":{\"url\":\"a.tgz\"},\"z\":0}", "{\"\":{\"url\":\"a.tgz\"},\"z\":0}", Just "{\"\":{\"url\":\"https://mirror/a.tgz\"},\"z\":0}")
    ]

-- A table that holds @author@ and a pointer string, as an npm full read's table does, and both entries.
authorTable :: InternTable
authorKey, pointer :: Entry
(authorTable, authorKey, pointer) = case internName (decodedName "author") (testTable ["url"]) of
    Interned key withKey -> case internName (decodedName "See the source") withKey of
        Interned string held -> (held, key, string)

-- The tree read and the writing read of the same chunks, as what each read holds or how it failed.
sameOutcomes :: Shape -> Mode -> [ByteString] -> (Either LimitError (Int, Either Bool (Maybe Held)), Either LimitError (Int, Either Bool (Maybe Held)))
sameOutcomes shape mode chunks = (readOutcome (walkJsonChunks bound tree chunks), readOutcome (walkWritingChunks bound packed chunks))
  where
    bound = MetadataBodyLimit (sum (map BS.length chunks))
    tree tokens = withElement tokens $ \element rest ->
        readShape Trees shape mode (testTable ["url"]) element rest $ \value _ _ ->
            Finished (Just (value, Just value, Just (toStrict (encode value)), toStrict (encode value), True, True))
    packed :: ST st (Cursor -> ST st (Steps (ST st) (Maybe Held)))
    packed =
        newWriter Nothing <&> \writer tokens -> withElement tokens $ \element rest ->
            readShape writer shape mode (testTable ["url"]) element rest $ \() table _ -> do
                form <- sealValue writer []
                shared <- decodeWhole writer form
                let held = docTable (tableTexts table)
                    decoded = packedValue held form
                pure (Finished (Just (shared, decoded, renderAlone held Nothing form, foldMap (toStrict . encode) decoded, valueEnd (packedBlob form) 0 == packedBytes form, packedEncodedLength form == walkedLength table form)))

type Held = (Value, Maybe Value, Maybe ByteString, ByteString, Bool, Bool)

sameAsTree :: Shape -> Mode -> [ByteString] -> PropertyT IO ()
sameAsTree shape mode chunks = do
    let (tree, packed) = sameOutcomes shape mode chunks
    cover 30 "read held a value" (either (const False) (either (const False) isJust . snd) tree)
    tree === packed

-- An object of 17 to 100 members under keys from a pool of 20, repeats and escapes included, whose
-- values are any JSON or such objects to the given depth.
genWideObject :: Int -> Gen ByteString
genWideObject depth = do
    count <- Gen.int (Range.linear 17 100)
    members <- replicateM count ((,) <$> Gen.element widePool <*> value)
    pure ("{" <> BS.intercalate "," [toStrict (encode (String key)) <> ":" <> member | (key, member) <- members] <> "}")
  where
    value = Gen.frequency ([(8, toStrict . encode <$> genValue widePool)] <> [(1, genWideObject (depth - 1)) | depth > 0])

widePool :: [Text]
widePool = ["a", "b", "B", "_", "aa", "ab", "url", "name", "version", "dist", "", " ", "\"", "\\", "\n", "\x01", "\x7f", "\xe9", "\x1F600", "a\x2028"]

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
