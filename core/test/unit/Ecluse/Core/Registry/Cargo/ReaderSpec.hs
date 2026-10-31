-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native record framing, complete selected scans and source-wide resource refusals.
module Ecluse.Core.Registry.Cargo.ReaderSpec (spec) where

import Data.Aeson (Value (..), encode, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.JsonStream.Parser qualified as J
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Hedgehog (MonadTest, cover, evalEither, evalMaybe, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Package (Artifact (..), Availability (Available, Yanked), CodeExecSignal (CodeExecUnknown), CoreMetadata, HashAlg (SHA256), PackageDetails (..), PackageInfo (..), hashAlg, hashValue, renderPackageName)
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Package.InvalidEntry (InvalidEntryKind (InvalidVersionManifest), invalidKey, invalidKind, invalidReason, invalidValue)
import Ecluse.Core.Registry.CachedDocument (cargoCached)
import Ecluse.Core.Registry.Cargo.Document (cargoRecords)
import Ecluse.Core.Registry.Cargo.Reader (CargoIndexResult (..), CargoRead (..), readCargoIndexCharged)
import Ecluse.Core.Registry.Cargo.Serving (serialiseCargoDocument)
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge, TooDeeplyNested, TooManyVersions), Limits (maxMetadataBytes, maxNestingDepth, maxVersionCount), defaultLimits)
import Ecluse.Core.Server.MemoryModel (bufferedChunkCharge, decodedDocumentCharge)
import Ecluse.Core.Version (renderVersion)
import Ecluse.Server.Admission.Support (ChargeRefused (..), refuseCharge)
import Ecluse.Test.Package (validSha256)
import Ecluse.Test.Registry.Cargo.Generated (CargoInputs (..), GeneratedIndex (..), NativeRow (..), genCargoIndex)
import Ecluse.Test.Registry.Cargo.Metadata (cargoTestDiscovery, cargoTestName, cargoTestRecord, cargoTestVersion, readCargoChunks, readCargoIndex, readCargoIndexChunks)
import Ecluse.Test.Registry.Cargo.Reference (IndexLine (indexPosition, indexValue), referenceIndex)
import Ecluse.Test.Registry.JsonBytes qualified as JsonBytes
import Ecluse.Test.Registry.JsonStream (heldChunks, parseJsonChunks, sliceChunks, testTable)
import Ecluse.Test.Registry.Source (assertCancelledRead)
import Ecluse.Test.Security.Limits (checkNestingDepth)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "readCargoIndex" $ do
    differentialSpec
    chargingSpec
    defaultedRowsSpec
    it "accepts CRLF, blank lines and a final row without LF at every chunk boundary" $ do
        let body = "\n" <> row "2.0.0" <> "\r\n \t\r\n" <> row "1.0.0"
        expected <- readCargoChunks defaultLimits FullRead [body]
        for_ [1 .. BS.length body - 1] $ \cut -> do
            actual <- readCargoChunks defaultLimits FullRead [BS.take cut body, BS.drop cut body]
            actual `shouldBe` expected
        parsed <- expectRight . streamValue =<< expectRight expected
        fmap (map fst . cargoRecords) (snd cargoCached (cargoIndexDocument parsed)) `shouldBe` Just [RecordEntry 1, RecordEntry 3]
    it "keeps healthy siblings beside malformed, truncated and unsupported schema rows" $ do
        let unsupported = case cargoTestRecord "3.0.0" of
                Object fields -> Object (KeyMap.insert "v" (Number 3) fields)
                value -> value
            body = "{broken}\n" <> row "1.0.0" <> "\n" <> toStrict (encode unsupported) <> "\n{"
        parsed <- readResult FullRead body
        Map.keys (infoVersions (cargoIndexInfo parsed)) `shouldBe` ["1.0.0"]
        length (infoInvalidEntries (cargoIndexInfo parsed)) `shouldBe` 3
    it "quarantines every duplicate build-metadata identity after scanning the full source" $ do
        let body = row "1.0.0+first" <> "\n" <> row "2.0.0" <> "\n" <> row "1.0.0+second"
        full <- readResult FullRead body
        Map.keys (infoVersions (cargoIndexInfo full)) `shouldBe` ["2.0.0"]
        length (infoInvalidEntries (cargoIndexInfo full)) `shouldBe` 2
        selected <- readResult (SelectedRead (cargoTestVersion "1.0.0+first")) body
        Map.keys (infoVersions (cargoIndexInfo selected)) `shouldBe` []
        fmap cargoRecords (snd cargoCached (cargoIndexDocument selected)) `shouldBe` Just []
    it "lets an unsupported row's valid native version quarantine a selected sibling" $ do
        let unsupported = case cargoTestRecord "1.0.0+other" of
                Object fields -> Object (KeyMap.insert "v" (Number 3) fields)
                value -> value
        selected <- readResult (SelectedRead (cargoTestVersion "1.0.0")) (row "1.0.0" <> "\n" <> toStrict (encode unsupported))
        Map.keys (infoVersions (cargoIndexInfo selected)) `shouldBe` []
    it "counts malformed and unselected records before returning a selected result" $ do
        let limits = defaultLimits{maxVersionCount = 1}
        result <- readCargoChunks limits (SelectedRead (cargoTestVersion "1.0.0")) [row "1.0.0" <> "\n{broken}"]
        result `shouldBe` Left (TooManyVersions 2 1)
    it "enforces the complete source byte limit" $ do
        let body = row "1.0.0" <> "\n" <> row "2.0.0"
        bytes <- readCargoChunks defaultLimits{maxMetadataBytes = BS.length body - 1} FullRead [body]
        bytes `shouldBe` Left (BodyTooLarge (MetadataBodyLimit (BS.length body - 1)))
    it "does not intercept asynchronous cancellation while waiting for the source" $ do
        discovery <- expectRight cargoTestDiscovery
        assertCancelledRead (readCargoIndex discovery defaultLimits cargoTestName FullRead (MetadataBodyLimit 65536) (testTable []))

row :: Text -> ByteString
row = toStrict . encode . cargoTestRecord

readResult :: CargoRead -> ByteString -> IO CargoIndexResult
readResult mode body = do
    streamed <- expectRight =<< readCargoChunks defaultLimits mode [body]
    expectRight (streamValue streamed)

chargingSpec :: Spec
chargingSpec = describe "selected row reservations" $ do
    for_ ["1.0.0", "2.0.0"] $ \version ->
        it ("charges a growing " <> (if version == "1.0.0" then "selected" else "unrelated") <> " row before retaining each fragment") $ do
            peaks <- forM [4096, 256 * 1024, 2 * 1024 * 1024] $ \size -> do
                let large = featureRow version size
                    selectedVersion = "1.0.0"
                    input = if version == selectedVersion then row "2.0.0" <> "\n" <> large else row selectedVersion <> "\n" <> large
                paid <- newIORef (0 :: Int)
                parsed <- chargedResult (modifyIORef' paid . max) selectedVersion input
                Map.keys (infoVersions (cargoIndexInfo parsed)) `shouldBe` [selectedVersion]
                peak <- readIORef paid
                peak `shouldSatisfy` (>= 12 * size)
                pure peak
            zipWith (<) peaks (drop 1 peaks) `shouldBe` [True, True]
    it "adds the retained target row to a later unrelated row's workspace" $ do
        let target = featureRow "1.0.0" 8192
            other = featureRow "2.0.0" 8192
        peak <- newIORef (0 :: Int)
        _ <- chargedResult (modifyIORef' peak . max) "1.0.0" (target <> "\n" <> other)
        readIORef peak `shouldReturn` rowCost target + rowCost other
    it "does not charge the sum of discarded rows" $ do
        let target = row "1.0.0"
            other = featureRow "2.0.0" 8192
        peak <- newIORef (0 :: Int)
        _ <- chargedResult (modifyIORef' peak . max) "1.0.0" (BS.intercalate "\n" (target : replicate 100 other))
        readIORef peak >>= (`shouldSatisfy` (< 3 * (rowCost target + rowCost other)))
    for_ ["1.0.0", "2.0.0"] $ \version ->
        it ("stops before reading the rest of a refused row " <> toString version) $ do
            discovery <- expectRight cargoTestDiscovery
            pulls <- newIORef (0 :: Int)
            next <- heldChunks (sliceChunks 32768 (featureRow version (2 * 1024 * 1024)))
            let source = modifyIORef' pulls (+ 1) >> next
                pay cost = when (cost > 1024 * 1024) (refuseCharge cost)
                readSelected = readCargoIndexCharged pay discovery defaultLimits cargoTestName (SelectedRead (cargoTestVersion "1.0.0")) (MetadataBodyLimit (maxMetadataBytes defaultLimits)) (testTable []) source
            readSelected `shouldThrow` (== ChargeRefused)
            readIORef pulls >>= (`shouldSatisfy` (< 8))
    it "keeps the pure held reader identical to the charged production parser" $ do
        discovery <- expectRight cargoTestDiscovery
        let input = ["\n" <> row "2.0.0", "\n" <> row "1.0.0" <> "\r\n"]
            mode = SelectedRead (cargoTestVersion "1.0.0")
            bound = MetadataBodyLimit (maxMetadataBytes defaultLimits)
        next <- heldChunks input
        actual <- readCargoIndexCharged (const pass) discovery defaultLimits cargoTestName mode bound (testTable []) next
        actual `shouldBe` readCargoIndexChunks discovery defaultLimits cargoTestName mode bound input

defaultedRowsSpec :: Spec
defaultedRowsSpec = it "preserves omitted defaults and explicit nulls in full and selected native rows" $ do
    let dependencies =
            [ object ["name" .= ("omitted" :: Text), "req" .= ("^1" :: Text)]
            , object ["name" .= ("nullable" :: Text), "req" .= ("^1" :: Text), "target" .= Null, "registry" .= Null, "package" .= Null]
            ]
        native = case cargoTestRecord "1.0.0" of
            Object fields -> Object (KeyMap.insert "deps" (Array (fromList dependencies)) (KeyMap.delete "features" fields))
            value -> value
        body = toStrict (encode native)
        reference bytes = map (fmap Object . indexValue) (referenceIndex bytes)
    for_ [FullRead, SelectedRead (cargoTestVersion "1.0.0")] $ \mode -> do
        parsed <- readResult mode body
        Map.keys (infoVersions (cargoIndexInfo parsed)) `shouldBe` ["1.0.0"]
        fmap cargoRecords (snd cargoCached (cargoIndexDocument parsed)) `shouldBe` Just [(RecordEntry 0, native)]
        rendered <- expectRight (serialiseCargoDocument (cargoIndexDocument parsed))
        reference (toStrict rendered) `shouldBe` reference body

featureRow :: Text -> Int -> ByteString
featureRow version size = toStrict . encode $ case cargoTestRecord version of
    Object fields -> Object (KeyMap.insert "features" (object ["large" .= [T.replicate size "x"]]) fields)
    value -> value

rowCost :: ByteString -> Int
rowCost bytes = sum [decodedDocumentCharge piece + bufferedChunkCharge (BS.length piece) | piece <- sliceChunks 32768 bytes]

chargedResult :: (Int -> IO ()) -> Text -> ByteString -> IO CargoIndexResult
chargedResult pay version input = do
    discovery <- expectRight cargoTestDiscovery
    next <- heldChunks (sliceChunks 32768 input)
    streamed <- expectRight =<< readCargoIndexCharged pay discovery defaultLimits cargoTestName (SelectedRead (cargoTestVersion version)) (MetadataBodyLimit (maxMetadataBytes defaultLimits)) (testTable []) next
    expectRight (streamValue streamed)

differentialSpec :: Spec
differentialSpec = modifyMaxSuccess (const 2000) $
    it "matches declared native meanings and exact limits across generated full and selected reads" $
        hedgehog $ do
            boundary <- forAll (Gen.element [BodyAt, BodyBelow, VersionsAt, VersionsBelow, DepthAt, DepthBelow])
            generated <- forAll (genCargoIndex (if boundary `elem` [DepthAt, DepthBelow] then SoundRows else MixedRows))
            chunks <- forAll (JsonBytes.genChunks (generatedBody generated))
            cover 20 "multiple nonempty chunks" (length (filter (not . BS.null) chunks) > 1)
            discovery <- evalEither cargoTestDiscovery
            let rows = physicalRows generated
                selected = SelectedRead (cargoTestVersion (generatedSelection generated))
                claims = Map.fromListWith (+) [(claim, 1 :: Int) | (_, nativeRow) <- rows, Just claim <- [nativeClaim nativeRow]]
                values = [value | (_, nativeRow) <- rows, Just (value, _, _, _) <- [nativeExpected nativeRow]]
            map fst rows === map indexPosition (referenceIndex (generatedBody generated))
            depth <- evalMaybe (find (\budget -> all (isRight . checkNestingDepth defaultLimits{maxNestingDepth = budget}) values) [0 .. maxNestingDepth defaultLimits])
            let (limits, refusal) = generatedLimits depth boundary generated
            cover 5 "malformed native records" (any (nativeMalformed . snd) rows)
            cover 5 "duplicate native identities" (any (> 1) (Map.elems claims) && isNothing refusal)
            cover 3 "a selected release survives" (not (null (fst (semanticRows generated selected))) && isNothing refusal)
            cover 1 "an empty native source" (null rows)
            cover 20 "a final row without LF" (not (BS.null (generatedBody generated)) && not (BS.isSuffixOf "\n" (generatedBody generated)))
            cover 20 "CRLF rows" (generatedEnding generated == "\r\n")
            cover 10 "omitted optional fields" (any (\case Object fields -> not (KeyMap.member "features" fields); _ -> False) values)
            cover 10 "explicit null fields" (any (\case Object fields -> KeyMap.lookup "links" fields == Just Null; _ -> False) values)
            cover 10 "nested additive fields" (any (\case Object fields -> KeyMap.member "native-extra" fields; _ -> False) values)
            cover 3 "body refusal" (boundary == BodyBelow && isJust refusal)
            cover 3 "version refusal" (boundary == VersionsBelow && isJust refusal)
            cover 3 "nesting refusal" (boundary == DepthBelow && isJust refusal)
            for_ rows $ \(_, nativeRow) -> for_ (nativeExpected nativeRow) $ \(value, _, _, _) -> do
                parsed <- evalEither (parseJsonChunks (MetadataBodyLimit (BS.length (nativeBytes nativeRow))) (J.value :: J.Parser Value) (\held item -> Right (item : held)) [] [nativeBytes nativeRow])
                streamValue parsed === Right [value]
            for_ [FullRead, selected] $ \mode -> do
                let actual = readCargoIndexChunks discovery limits cargoTestName mode (MetadataBodyLimit (maxMetadataBytes limits)) chunks
                case refusal of
                    Just fault -> void actual === Left fault
                    Nothing -> do
                        streamed <- evalEither actual
                        streamBytes streamed === BS.length (generatedBody generated)
                        result <- evalEither (streamValue streamed)
                        matchesGenerated generated mode result

data NativeBoundary = BodyAt | BodyBelow | VersionsAt | VersionsBelow | DepthAt | DepthBelow
    deriving stock (Eq, Show)

generatedLimits :: Int -> NativeBoundary -> GeneratedIndex -> (Limits, Maybe LimitError)
generatedLimits depth boundary generated = case boundary of
    BodyAt -> (base{maxMetadataBytes = bytes}, Nothing)
    BodyBelow -> (base{maxMetadataBytes = short bytes}, BodyTooLarge (MetadataBodyLimit (short bytes)) <$ guard (bytes > 0))
    VersionsAt -> (base{maxVersionCount = length rows}, Nothing)
    VersionsBelow -> (base{maxVersionCount = short (length rows)}, TooManyVersions (length rows) (short (length rows)) <$ guard (not (null rows)))
    DepthAt -> (base{maxNestingDepth = depth}, Nothing)
    DepthBelow -> (base{maxNestingDepth = short depth}, TooDeeplyNested (short depth) <$ guard (depth > 0))
  where
    rows = physicalRows generated
    bytes = BS.length (generatedBody generated)
    short count = max 0 (count - 1)
    base = defaultLimits{maxMetadataBytes = bytes + 1}

physicalRows :: GeneratedIndex -> [(Int, NativeRow)]
physicalRows generated =
    [ (position, nativeRow)
    | framed <- referenceIndex (generatedBody generated)
    , let position = indexPosition framed
    , Just nativeRow <- [Map.lookup position declared]
    ]
  where
    declared = Map.fromList (zip [0 ..] (generatedLines generated))

semanticRows :: GeneratedIndex -> CargoRead -> ([(Int, (Value, Text, Bool, Bool))], [(Int, Bool)])
semanticRows generated mode = (kept, dropped)
  where
    rows = physicalRows generated
    seen = Map.fromListWith (+) [(claim, 1 :: Int) | (_, nativeRow) <- rows, Just claim <- [nativeClaim nativeRow]]
    candidates = [(position, nativeRow, meaning) | (position, nativeRow) <- rows, Just meaning@(_, version, _, _) <- [nativeExpected nativeRow], mode == FullRead || version == generatedSelection generated]
    retained = if mode == FullRead then candidates else take 1 candidates
    unique nativeRow = maybe False (\claim -> Map.lookup claim seen == Just 1) (nativeClaim nativeRow)
    kept = [(position, meaning) | (position, nativeRow, meaning) <- retained, unique nativeRow]
    dropped = [(position, False) | mode == FullRead, (position, nativeRow) <- rows, isNothing (nativeExpected nativeRow)] <> [(position, True) | (position, nativeRow, _) <- retained, not (unique nativeRow)]

matchesGenerated :: (MonadTest m) => GeneratedIndex -> CargoRead -> CargoIndexResult -> m ()
matchesGenerated generated mode result = do
    let info = cargoIndexInfo result
        (kept, dropped) = semanticRows generated mode
    infoName info === cargoTestName
    infoDistTags info === Map.empty
    Map.keys (infoVersions info) === sort [version | (_, (_, version, _, _)) <- kept]
    fmap cargoRecords (snd cargoCached (cargoIndexDocument result)) === Just [(RecordEntry position, value) | (position, (value, _, _, _)) <- kept]
    [(invalidKind invalid, invalidKey invalid, invalidValue invalid, invalidReason invalid == "duplicate Cargo release identity") | invalid <- infoInvalidEntries info]
        === [(InvalidVersionManifest, show (RecordEntry position), Null, duplicate) | (position, duplicate) <- dropped]
    for_ kept $ \(position, (_, version, yanked, published)) -> do
        details <- evalMaybe (Map.lookup version (infoVersions info))
        (renderPackageName (pkgName details), renderVersion (pkgVersion details), pkgPublishedAt details, pkgInstallCode details, pkgAvailability details)
            === ("fixture", version, if published then Just (UTCTime (fromGregorian 2020 1 1) 0) else Nothing, CodeExecUnknown, if yanked then Yanked else Available)
        map artifactFacts (toList (pkgArtifacts details))
            === [(RecordEntry position, "fixture-" <> version <> ".crate", "https://static.crates.io/crates/fixture/fixture-" <> T.replace "+" "%2B" version <> ".crate", [(SHA256, validSha256)], Nothing, Nothing)]

artifactFacts :: Artifact -> (EntryKey, Text, Text, [(HashAlg, Text)], Maybe CoreMetadata, Maybe Int)
artifactFacts artifact = (artEntryKey artifact, artFilename artifact, artUrl artifact, [(hashAlg digest, hashValue digest) | digest <- artHashes artifact], artCoreMetadata artifact, artSize artifact)
