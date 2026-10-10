-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The Simple-index walk against the json-stream field parser it replaced, on generated bodies, and
the one place a selected read departs from that parser: the rest of a file its name rejects.
-}
module Ecluse.Core.Registry.PyPI.ReaderSpec (spec) where

import Data.Aeson (Value (String), decodeStrict)
import Data.ByteString qualified as BS
import Data.JsonStream.Parser qualified as J
import Hedgehog (Gen, annotateShow, assert, cover, forAll, success, (/==), (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.JsonStream (StreamResult)
import Ecluse.Core.Registry.PyPI.Project (fcVersionKey, fileCoordinate)
import Ecluse.Core.Registry.PyPI.Reader (fileUniqueFields, pypiWalk)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIField (FileField), PyPIRead (..))
import Ecluse.Core.Registry.PyPI.StreamingProjection (PyPIProjection, collectField, emptyProjection, keepsFile)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge, TooManyVersions), defaultLimits)
import Ecluse.Test.Package (unscopedPyPI)
import Ecluse.Test.Registry.JsonBytes (damaged, genChunks, genSimpleIndexBytes, releaseKeys)
import Ecluse.Test.Registry.JsonStream (parseJsonChunks, readOutcome, testTable, walkJsonChunks)
import Ecluse.Test.Registry.PyPI.Streaming (pypiFields)

spec :: Spec
spec = describe "pypiWalk" $ do
    paritySpec
    rejectedFileSpec
    repairSpec

{- | Every generated body reads to the same fields, refusal or failure class as json-stream's reader,
through the production projection with its keep predicate or with every file kept. A selected read
with a level for a hash value may differ in one way: it reads past a parse error of that reader.
-}
paritySpec :: Spec
paritySpec = modifyMaxSuccess (const 2000) $
    it "emits json-stream's fields and outcome for generated Simple indexes, or reads past its parse error" $
        hedgehog $ do
            body <- forAll (genSimpleIndexBytes >>= damaged)
            chunks <- forAll (genChunks body)
            depth <- forAll (Gen.frequency [(3, pure 64), (2, Gen.int (Range.constant 0 6))])
            selected <- forAll (Gen.maybe (Gen.element ("9.9" : map decodeUtf8 releaseKeys)))
            cap <- forAll (Gen.maybe (Gen.int (Range.linear 0 20)))
            production <- forAll Gen.bool
            let reading = Reading depth (maybe FullRead (SelectedRead thing) selected) cap production
                bound = MetadataBodyLimit (BS.length body)
                expected = reference reading bound chunks
                actual = walked reading bound chunks
            cover 0.5 "a selected read past the reference's parse error" (actual /= expected)
            if actual == expected
                then success
                else do
                    annotateShow (expected, actual)
                    assert (isJust selected && depth > 4)
                    fmap snd expected === Right (Left False)
                    fmap snd actual /== Right (Left True)
                    -- json-stream's own skip of the whole body stands for what the lexer accepts.
                    when (succeeded actual) $
                        fmap snd (readOutcome (parseJsonChunks bound (mempty :: J.Parser ()) (\acc _ -> Right acc) () chunks)) === Right (Right ())

-- | The rule by example, for a read of release 1.2.3 of @thing@.
rejectedFileSpec :: Spec
rejectedFileSpec = describe "a selected read of a file whose name rejects it" $ do
    for_ undecodable $ \(what, fault, _) -> do
        it ("reads past " <> what <> " after the name") $
            kept 64 (indexOf [fileOf [otherName, fault], fileOf [ownName]]) `shouldBe` Right (Right [1])
        it ("fails on " <> what <> " before the name") $
            kept 64 (indexOf [fileOf [fault, otherName], fileOf [ownName]]) `shouldBe` Right (Left False)
        it ("fails on " <> what <> " in a file of the release") $
            kept 64 (indexOf [fileOf [ownName, fault]]) `shouldBe` Right (Left False)
        it ("fails on " <> what <> " in a file with no name") $
            kept 64 (indexOf [fileOf [fault], fileOf [ownName]]) `shouldBe` Right (Left False)

    for_ ["null", "7", "[]", "{}", "\"not a release\"", "\"other-1.2.3.tar.gz\""] $ \name ->
        it ("skips what follows the first name " <> decodeUtf8 name <> ", a later name of the release included") $
            kept 64 (indexOf [fileOf ["\"filename\":" <> name, badEscape, ownName], fileOf [ownName]]) `shouldBe` Right (Right [1])

    it "reads every member of a file of the release, after a later name of another release too" $ do
        kept 64 (indexOf [fileOf [ownName, otherName]]) `shouldBe` Right (Right [0])
        kept 64 (indexOf [fileOf [ownName, otherName, badEscape]]) `shouldBe` Right (Left False)

    it "drops a file with no name" $
        kept 64 (indexOf [fileOf [sound], fileOf [ownName]]) `shouldBe` Right (Right [1])

    it "skips by bracket count, so a member with no value and a file closed by the other bracket do not fail the read" $ do
        kept 64 (indexOf [fileOf [otherName, "\"size\":"], fileOf [ownName]]) `shouldBe` Right (Right [1])
        kept 64 (indexOf ["{" <> otherName <> "]", fileOf [ownName]]) `shouldBe` Right (Right [1])

    for_ unlexable $ \broken ->
        it ("fails where the lexer rejects " <> decodeUtf8 broken <> " in the skipped rest") $
            kept 64 (indexOf [fileOf [otherName, broken], fileOf [ownName]]) `shouldBe` Right (Left False)

    it "fails when the body ends anywhere in the skipped rest" $ do
        let lead = "{\"name\":\"thing\",\"files\":[{" <> otherName <> ","
            rest = sound <> ",\"hashes\":{\"sha256\":\"00\"}"
        for_ [0 .. BS.length rest] $ \size ->
            kept 64 (lead <> BS.take size rest) `shouldBe` Right (Left False)

    it "counts the skipped rest toward the body limit" $ do
        let body = indexOf [fileOf [otherName, "\"url\":\"" <> BS.replicate 4096 120 <> "\""], fileOf [ownName]]
            bound = MetadataBodyLimit 1024
        walked (Reading 64 wanted Nothing True) bound [BS.take 512 body, BS.drop 512 body] `shouldBe` Left (BodyTooLarge bound)

    it "meets the nesting limit at a hash value of a rejected file when the depth has no level for one" $ do
        let body = indexOf [fileOf [otherName, "\"hashes\":{\"sha256\":\"00\"}"], fileOf [ownName]]
        kept 4 body `shouldBe` Right (Left True)
        kept 5 body `shouldBe` Right (Right [1])

{- | The rule over generated indexes. A read with a level for a hash value reads an index as
json-stream's reader reads it once each undecodable member after a rejecting name is repaired. Any
other undecodable member, and anything the lexer rejects, still ends the read without a result.
-}
repairSpec :: Spec
repairSpec = modifyMaxSuccess (const 2000) $
    it "reads as json-stream's reader reads the index with the members after each rejecting name repaired" $
        hedgehog $ do
            files <- forAll (Gen.list (Range.constant 0 5) genFile)
            depth <- forAll (Gen.frequency [(5, pure 64), (1, pure 5), (2, pure 4), (1, Gen.int (Range.constant 0 3))])
            cap <- forAll (Gen.frequency [(5, pure Nothing), (1, Just <$> Gen.int (Range.linear 0 20))])
            let skips = depth > 4
                body = indexOf (map (fileBytes False) files)
                repaired = indexOf (map (fileBytes skips) files)
            chunks <- forAll (genChunks body)
            let reading = Reading depth wanted cap True
                bound = MetadataBodyLimit (BS.length body)
                actual = walked reading bound chunks
                skipped = [fault | file <- files, fault@Undecodable{} <- afterName False file]
                beforeName = [fault | File leading (Just _) <- files, fault@Undecodable{} <- leading]
                inRelease = [fault | file <- files, fault@Undecodable{} <- afterName True file]
                unnamed = [fault | File members Nothing <- files, fault@Undecodable{} <- members]
                rejected = [broken | File leading named <- files, broken@Unlexable{} <- leading <> maybe [] snd named]
                decoded = beforeName <> inRelease <> unnamed <> (if skips then [] else skipped)
            cover 20 "every member after a rejecting name is sound" (null skipped)
            cover 5 "an undecodable member after a rejecting name, and the read has a result" (skips && not (null skipped) && succeeded actual)
            cover 3 "an undecodable member after a rejecting name, at a depth with no level for a hash value" (not skips && not (null skipped))
            cover 3 "an undecodable member before a name" (not (null beforeName))
            cover 3 "an undecodable member in a file of the release" (not (null inRelease))
            cover 2 "an undecodable member in a file with no name" (not (null unnamed))
            cover 2 "a member the lexer rejects" (not (null rejected))
            reference reading bound (cutLike chunks repaired) === actual
            if null decoded && null rejected
                then when (skips && isNothing cap) (assert (succeeded actual))
                else assert (not (succeeded actual))

-- | A read's refusal, or the bytes it read with its fields or whether its failure is the nesting limit.
type Outcome = Either LimitError (Int, Either Bool [PyPIField])

{- | How a read runs: its nesting depth, its mode, a cap on emitted fields, and whether it keeps
files as production does.
-}
data Reading = Reading Int PyPIRead (Maybe Int) Bool

walked :: Reading -> BodyLimit -> [ByteString] -> Outcome
walked reading@(Reading depth mode _ production) bound =
    emitted . walkJsonChunks bound (pypiWalk depth mode (collect reading) keeps (testTable fileUniqueFields) start)
  where
    keeps (projection, _) = not production || keepsFile projection

-- | The read through json-stream's field parser, which decodes every member of every file.
reference :: Reading -> BodyLimit -> [ByteString] -> Outcome
reference reading@(Reading depth mode _ _) bound = emitted . parseJsonChunks bound (pypiFields depth mode) (collect reading) start

start :: (PyPIProjection, [PyPIField])
start = (emptyProjection thing, [])

collect :: Reading -> (PyPIProjection, [PyPIField]) -> PyPIField -> Either LimitError (PyPIProjection, [PyPIField])
collect (Reading _ mode cap _) (projection, events) field = do
    projected <- collectField defaultLimits mode projection field
    case cap of
        Just most | length events >= most -> Left (TooManyVersions (length events) most)
        _ -> Right (projected, field : events)

emitted :: Either LimitError (StreamResult (PyPIProjection, [PyPIField])) -> Outcome
emitted = fmap (second (fmap snd)) . readOutcome

succeeded :: Outcome -> Bool
succeeded = either (const False) (isRight . snd)

-- | The positions of the files a read of release 1.2.3 keeps, or its refusal or failure class.
kept :: Int -> ByteString -> Either LimitError (Either Bool [Int])
kept depth body = fmap (fmap positions . snd) (walked (Reading depth wanted Nothing True) (MetadataBodyLimit (BS.length body)) [body])
  where
    positions fields = reverse [position | FileField position (Just _) <- fields]

thing :: PackageName
thing = unscopedPyPI "thing"

wanted :: PyPIRead
wanted = SelectedRead thing "1.2.3"

indexOf :: [ByteString] -> ByteString
indexOf files = "{\"name\":\"thing\",\"files\":[" <> BS.intercalate "," files <> "],\"meta\":{\"api-version\":\"1.1\"}}"

fileOf :: [ByteString] -> ByteString
fileOf members = "{" <> BS.intercalate "," members <> "}"

ownName, otherName, sound, badEscape :: ByteString
ownName = "\"filename\":\"thing-1.2.3.tar.gz\""
otherName = "\"filename\":\"thing-2.0.0.tar.gz\""
sound = "\"url\":\"https://files.example/thing\""
badEscape = "\"url\":\"\\x\""

-- | Members a read fails to decode, each with a repair of the same length.
undecodable :: [(String, ByteString, ByteString)]
undecodable =
    [ ("a string with an invalid escape", badEscape, "\"url\":\"\\n\"")
    , ("a string that is not UTF-8", "\"yanked\":\"\xff\"", "\"yanked\":\"y\"")
    , ("a key with an invalid escape", "\"\\x\":1", "\"\\n\":1")
    , ("a hash value with an invalid escape", "\"hashes\":{\"sha256\":\"\\x\"}", "\"hashes\":{\"sha256\":\"\\n\"}")
    , ("a hash name with an invalid escape", "\"hashes\":{\"\\x\":\"00\"}", "\"hashes\":{\"\\n\":\"00\"}")
    , ("a key that is not a string", "7  :1", "\"7\":1")
    ]

-- | Members the lexer rejects, which no skip passes.
unlexable :: [ByteString]
unlexable = ["\"yanked\":tru", "\"size\":@"]

-- | A member as written. An undecodable one carries its repair.
data Member = Sound ByteString | Undecodable ByteString ByteString | Unlexable ByteString
    deriving stock (Show)

-- | The members before a file's first name, then that name's value and the members after it.
data File = File [Member] (Maybe (ByteString, [Member]))
    deriving stock (Show)

-- Faults are rare before a name, so most reads reach the members after one.
genFile :: Gen File
genFile =
    Gen.frequency
        [ (9, File <$> members [sound] 1 0 <*> (Just <$> ((,) <$> Gen.element nameValues <*> members [sound, ownName, otherName] 8 1)))
        , (1, File <$> members [sound] 4 1 <*> pure Nothing)
        ]
  where
    members extra faults breaks =
        Gen.list (Range.constant 0 3) $
            Gen.frequency
                [ (16, Sound <$> Gen.element (extra <> soundMembers))
                , (faults, (\(_, written, repair) -> Undecodable written repair) <$> Gen.element undecodable)
                , (breaks, Unlexable <$> Gen.element unlexable)
                ]

soundMembers :: [ByteString]
soundMembers =
    [ "\"hashes\":{\"sha256\":\"00\",\"custom\":\"11\"}"
    , "\"hashes\":null"
    , "\"requires-python\":\">=3.9\""
    , "\"size\":12"
    , "\"upload-time\":\"2026-05-14T19:25:26Z\""
    , "\"yanked\":\"retir\\u00e9\""
    , "\"provenance\":null"
    , "\"core-metadata\":{\"sha256\":[[\"00\"]]}"
    ]

-- Values of a first name: of the release under several spellings, of other releases and projects, and not a name.
nameValues :: [ByteString]
nameValues =
    [ "\"thing-1.2.3.tar.gz\""
    , "\"thing-1.2.3-py3-none-any.whl\""
    , "\"Thing-1.2.3.zip\""
    , "\"thing-01.2.3.tar.gz\""
    , "\"thing-2.0.0.tar.gz\""
    , "\"thing-1.2.3rc1.tar.gz\""
    , "\"thing-1.2.3.0.tar.gz\""
    , "\"other-1.2.3.tar.gz\""
    , "\"thing-1.2.3.exe\""
    , "\"not a release\""
    , "null"
    , "7"
    , "[]"
    , "{}"
    ]

-- | Whether a first name's value puts its file in release 1.2.3, by the parser a full read groups files with.
ofRelease :: ByteString -> Bool
ofRelease value = case decodeStrict value of
    Just (String name) -> fmap fcVersionKey (fileCoordinate thing name) == Just "1.2.3"
    _ -> False

-- | The members after a file's first name, when that name is of the release or when it rejects the file.
afterName :: Bool -> File -> [Member]
afterName accepted (File _ named) = concat [trailing | Just (name, trailing) <- [named], ofRelease name == accepted]

-- | A file's bytes as written, or with each undecodable member after a rejecting name repaired.
fileBytes :: Bool -> File -> ByteString
fileBytes repairing (File leading named) = fileOf (map written leading <> maybe [] nameAndAfter named)
  where
    nameAndAfter (name, trailing) = ("\"filename\":" <> name) : map (if repairing && not (ofRelease name) then repaired else written) trailing
    written = \case
        Sound bytes -> bytes
        Undecodable bytes _ -> bytes
        Unlexable bytes -> bytes
    repaired = \case
        Undecodable _ repair -> repair
        member -> written member

-- | Cut a body where another of the same length was cut, so both reads see a failure in the same chunk.
cutLike :: [ByteString] -> ByteString -> [ByteString]
cutLike chunks body = snd (mapAccumL (\rest chunk -> swap (BS.splitAt (BS.length chunk) rest)) body chunks)
