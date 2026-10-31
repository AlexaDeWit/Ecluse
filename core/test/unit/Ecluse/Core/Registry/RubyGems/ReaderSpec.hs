-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Complete reads preserve platform grouping and reject late conflicts and resource breaches.
module Ecluse.Core.Registry.RubyGems.ReaderSpec (spec) where

import Data.ByteString qualified as BS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Hedgehog (MonadTest, cover, evalEither, evalIO, evalMaybe, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Package (Artifact (..), Availability (Available), CodeExecSignal (CodeExecUnknown), HashAlg (SHA256), PackageDetails (..), PackageInfo (..), hashAlg, hashValue, renderPackageName)
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Registry.RubyGems.Document (GemDependency (..), GemDocument (..), GemRow (..))
import Ecluse.Core.Registry.RubyGems.Project (coordinateFilename, coordinateName, coordinatePlatform, coordinateVersion, projectName, unGemPlatform)
import Ecluse.Core.Registry.RubyGems.Reader (GemRead (..), GemReadResult (..), readGemInfoCharged)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (..), Limits (..), defaultLimits)
import Ecluse.Core.Server.Path (unFilename)
import Ecluse.Core.Text.Iso8601 (readIso8601Utc)
import Ecluse.Core.Version (mkVersion, renderVersion)
import Ecluse.Test.Registry.JsonBytes qualified as JsonBytes
import Ecluse.Test.Registry.JsonStream (byteChunks, heldChunks, testTable)
import Ecluse.Test.Registry.RubyGems.Generated (GeneratedInfo (..), genRubyInfo)
import Ecluse.Test.Registry.RubyGems.Reference (RubyInfoRow (..), referenceInfo)
import Ecluse.Test.Registry.Source (assertCancelledRead)
import Ecluse.Test.Support (expectRight)

-- | Exercise framing at every split and scan selected versions through the final source record.
spec :: Spec
spec = describe "RubyGems complete named reads" $ do
    differentialSpec
    it "keeps all platform rows and the newest original publication time at every byte split" $ do
        let bytes = "opaque header\n---\n" <> row "1.0" "2026-01-01T00:00:00Z" <> "\n\n" <> row "1.0-java" "2026-01-03T00:00:00Z"
        reference <- parse defaultLimits FullRead [bytes]
        for_ [1 .. BS.length bytes - 1] $ \cut -> parse defaultLimits FullRead [BS.take cut bytes, BS.drop cut bytes] `shouldReturn` reference
        result <- expectRight . streamValue =<< expectRight reference
        map grEntryKey (gemRows (gemReadDocument result)) `shouldBe` [RecordEntry 2, RecordEntry 4]
        details <- maybe (expectationFailure "version absent" >> fail "version absent") pure (Map.lookup "1.0" (infoVersions (gemReadInfo result)))
        length (pkgArtifacts details) `shouldBe` 2
        pkgPublishedAt details `shouldBe` readIso8601Utc "2026-01-03T00:00:00Z"
    it "keeps grouped age unknown when one platform timestamp is missing" $ do
        result <- readResult FullRead ("---\n" <> row "1.0" "2026-01-01T00:00:00Z" <> "\n1.0-java |checksum:" <> digest)
        fmap pkgPublishedAt (Map.lookup "1.0" (infoVersions (gemReadInfo result))) `shouldBe` Just Nothing
    it "quarantines a selected version when a malformed duplicate appears after another version" $ do
        let bytes = "---\n" <> row "1.0" "2026-01-01T00:00:00Z" <> "\n" <> row "2.0" "2026-01-01T00:00:00Z" <> "\n1.0 |checksum:invalid"
        result <- readResult (SelectedRead (mkVersion RubyGems "1.0")) bytes
        infoVersions (gemReadInfo result) `shouldBe` Map.empty
        infoInvalidEntries (gemReadInfo result) `shouldSatisfy` (not . null)
    for_ [("invalid UTF-8 metadata", "1.0 |checksum:" <> digest <> ",opaque:" <> BS.singleton 255), ("unsupported platform", "1.0-unknown-platform |checksum:" <> digest), ("invalid UTF-8 platform", "1.0-" <> BS.singleton 255 <> " |checksum:" <> digest)] $ \(label, malformed) ->
        it ("quarantines an EOF duplicate with " <> label <> " without removing unrelated versions") $ do
            let bytes = "---\n" <> row "1.0" "2026-01-01T00:00:00Z" <> "\n" <> row "2.0" "2026-01-01T00:00:00Z" <> "\n" <> malformed
            full <- readResult FullRead bytes
            Map.keys (infoVersions (gemReadInfo full)) `shouldBe` ["2.0"]
            infoInvalidEntries (gemReadInfo full) `shouldSatisfy` (not . null)
            selected <- readResult (SelectedRead (mkVersion RubyGems "1.0")) bytes
            infoVersions (gemReadInfo selected) `shouldBe` Map.empty
            gemRows (gemReadDocument selected) `shouldBe` []
            unrelated <- readResult (SelectedRead (mkVersion RubyGems "2.0")) bytes
            Map.keys (infoVersions (gemReadInfo unrelated)) `shouldBe` ["2.0"]
    it "counts trailing unselected artifacts and complete source bytes" $ do
        let bytes = "---\n" <> row "1.0" "2026-01-01T00:00:00Z" <> "\n" <> row "2.0" "2026-01-01T00:00:00Z"
            platforms = "---\n" <> row "1.0" "2026-01-01T00:00:00Z" <> "\n" <> row "1.0-java" "2026-01-01T00:00:00Z"
        parse defaultLimits{maxVersionCount = 1} (SelectedRead (mkVersion RubyGems "1.0")) [bytes] `shouldReturn` Left (TooManyVersions 2 1)
        parse defaultLimits{maxVersionCount = 1} FullRead [bytes] `shouldReturn` Left (TooManyVersions 2 1)
        parse defaultLimits{maxVersionCount = 1} FullRead [platforms] `shouldReturn` Left (TooManyVersions 2 1)
        parse defaultLimits{maxMetadataBytes = BS.length bytes - 1} FullRead [bytes] `shouldReturn` Left (BodyTooLarge (MetadataBodyLimit (BS.length bytes - 1)))
    it "keeps selected workspace independent of eightfold distinct dropped history" $ do
        let target = row "1.0" "2026-01-01T00:00:00Z"
            body count = "---\n" <> target <> BS.concat ["\n" <> row (encodeUtf8 ("2." <> T.justifyRight 6 '0' (show at))) "2026-01-01T00:00:00Z" | at <- [1 .. count :: Int]]
            selected = SelectedRead (mkVersion RubyGems "1.0")
        (small, smallPeak) <- charged selected (body 250)
        (large, largePeak) <- charged selected (body 2000)
        gemReadDocument large `shouldBe` gemReadDocument small
        largePeak `shouldSatisfy` (< 2 * smallPeak)
    it "propagates asynchronous cancellation from a blocked source" $ do
        name <- expectRight (projectName "native-gem")
        assertCancelledRead (readGemInfoCharged (const pass) defaultLimits name "https://rubygems.org" FullRead (MetadataBodyLimit (maxMetadataBytes defaultLimits)) (testTable []))
    it "requires an actual header delimiter" $ do
        parsed <- expectRight =<< parse defaultLimits FullRead [row "1.0" "2026-01-01T00:00:00Z"]
        streamValue parsed `shouldSatisfy` isLeft
  where
    row coordinate timestamp = coordinate <> " |checksum:" <> digest <> ",created_at:" <> timestamp
    digest = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
    parse limits mode chunks = do
        name <- expectRight (projectName "native-gem")
        next <- heldChunks chunks
        readGemInfoCharged (const pass) limits name "https://rubygems.org" mode (MetadataBodyLimit (maxMetadataBytes limits)) (testTable []) next
    readResult mode bytes = expectRight . streamValue =<< expectRight =<< parse defaultLimits mode [bytes]

    charged mode body = do
        name <- expectRight (projectName "native-gem")
        peak <- newIORef (0 :: Int)
        next <- heldChunks [body]
        streamed <- expectRight =<< readGemInfoCharged (modifyIORef' peak . max) defaultLimits name "https://rubygems.org" mode (MetadataBodyLimit (maxMetadataBytes defaultLimits)) (testTable []) next
        result <- expectRight (streamValue streamed)
        (result,) <$> readIORef peak

differentialSpec :: Spec
differentialSpec = modifyMaxSuccess (const 2000) $
    it "matches independent generated native facts in complete full and selected reads" $
        hedgehog $ do
            generated <- forAll genRubyInfo
            chunks <- forAll (Gen.frequency [(9, JsonBytes.genChunks (generatedBody generated)), (1, pure (byteChunks (generatedBody generated)))])
            reference <- evalEither (referenceInfo (generatedBody generated))
            reference === map fst (generatedRows generated)
            let selected = generatedSelection generated
                groups = Map.fromListWith (<>) [(rowVersion row, [row]) | row <- reference]
                firstDataPosition = BS.count 10 (generatedHeader generated)
                consecutive = [firstDataPosition .. firstDataPosition + length reference - 1]
            cover 20 "multiple chunks" (length chunks > 1)
            cover 1 "byte-wise chunks" (length chunks > 1 && all ((== 1) . BS.length) chunks)
            cover 10 "multi-byte UTF-8" (BS.any (> 127) (generatedBody generated))
            cover 10 "multiple raw versions" (Map.size groups > 1)
            cover 10 "prerelease coordinates" (any (\row -> ".pre." `T.isInfixOf` rowVersion row || ".rc." `T.isInfixOf` rowVersion row) reference)
            cover 10 "additive native metadata" (any (isJust . lookup "extra1" . rowFields) reference)
            cover 10 "multiple platform artifacts" (any ((> 1) . length) (Map.elems groups))
            cover 10 "missing selected version" (not (Map.member selected groups))
            cover 10 "opaque header" (generatedHeader generated /= "---\n")
            cover 20 "final row without LF" (not (generatedFinalLf generated))
            cover 10 "physical blank rows" (map rowPosition reference /= consecutive)
            cover 10 "dependency conjunctions" (any (any ((> 1) . length . snd) . rowDependencies) reference)
            cover 10 "omitted required Ruby field" (any (isNothing . lookup "ruby" . rowFields) reference)
            cover 10 "missing publication time" (any (isNothing . lookup "created_at" . rowFields) reference)
            cover 10 "known publication time" (any (isJust . lookup "created_at" . rowFields) reference)
            cover 5 "mixed known and missing platform ages" (any mixedAges (Map.elems groups))
            name <- evalEither (projectName (generatedName generated))
            for_ [Nothing, Just selected] $ \selection -> do
                next <- evalIO (heldChunks chunks)
                let mode = maybe FullRead (SelectedRead . mkVersion RubyGems) selection
                streamed <- evalEither =<< evalIO (readGemInfoCharged (const pass) defaultLimits name (generatedBase generated) mode (MetadataBodyLimit (BS.length (generatedBody generated))) (testTable []) next)
                streamBytes streamed === BS.length (generatedBody generated)
                actual <- evalEither (streamValue streamed)
                matchesReference generated selection reference actual
  where
    mixedAges rows = any (isNothing . referenceTime) rows && any (isJust . referenceTime) rows

matchesReference :: (MonadTest m) => GeneratedInfo -> Maybe Text -> [RubyInfoRow] -> GemReadResult -> m ()
matchesReference generated selection reference actual = do
    let expected = filter (\row -> maybe True (== rowVersion row) selection) reference
        grouped = Map.fromListWith (<>) [(rowVersion row, [row]) | row <- expected]
        info = gemReadInfo actual
        document = gemReadDocument actual
        sourceBytes = Map.fromList [(rowPosition row, bytes) | (row, bytes) <- generatedRows generated]
    renderPackageName (infoName info) === generatedName generated
    infoInvalidEntries info === []
    infoDistTags info === Map.empty
    Map.keys (infoVersions info) === Map.keys grouped
    renderPackageName (gemName document) === generatedName generated
    gemHeader document === generatedHeader generated
    length (gemRows document) === length expected
    for_ (zip (gemRows document) expected) $ \(row, declared) -> do
        let coordinate = grCoordinate row
        (entryPosition (grEntryKey row), renderPackageName (coordinateName coordinate), renderVersion (coordinateVersion coordinate), unGemPlatform (coordinatePlatform coordinate), unFilename (coordinateFilename coordinate))
            === (Just (rowPosition declared), generatedName generated, rowVersion declared, rowPlatform declared, referenceFilename (generatedName generated) declared)
        [(renderPackageName (gdName dependency), toList (gdRequirement dependency)) | dependency <- grDependencies row] === rowDependencies declared
        toList (grRequiredRuby row) === referenceRequirement "ruby" declared
        toList (grRequiredRubyGems row) === referenceRequirement "rubygems" declared
        (hashAlg (grChecksum row), Just (hashValue (grChecksum row))) === (SHA256, lookup "checksum" (rowFields declared))
        grPublishedAt row === referenceTime declared
        grExtras row === Map.fromList (rowFields declared)
        original <- evalMaybe (Map.lookup (rowPosition declared) sourceBytes)
        grSource row === original
    for_ (Map.toList grouped) $ \(version, rows) -> do
        details <- evalMaybe (Map.lookup version (infoVersions info))
        (renderPackageName (pkgName details), renderVersion (pkgVersion details), pkgPublishedAt details, pkgInstallCode details, pkgAvailability details)
            === (generatedName generated, version, referenceNewest rows, CodeExecUnknown, Available)
        let artifacts = toList (pkgArtifacts details)
            ordered = filter ((== version) . rowVersion) expected
        length artifacts === length ordered
        for_ (zip artifacts ordered) $ \(artifact, declared) -> do
            let filename = referenceFilename (generatedName generated) declared
            (entryPosition (artEntryKey artifact), artFilename artifact, artUrl artifact)
                === (Just (rowPosition declared), filename, generatedBase generated <> "/gems/" <> filename)
            [(hashAlg digest, Just (hashValue digest)) | digest <- artHashes artifact] === [(SHA256, lookup "checksum" (rowFields declared))]
            artCoreMetadata artifact === Nothing
            artSize artifact === Nothing

referenceRequirement :: Text -> RubyInfoRow -> [Text]
referenceRequirement key = maybe [">= 0"] (T.splitOn "&") . lookup key . rowFields

referenceTime :: RubyInfoRow -> Maybe UTCTime
referenceTime row = lookup "created_at" (rowFields row) >>= iso8601ParseM . toString

referenceNewest :: [RubyInfoRow] -> Maybe UTCTime
referenceNewest rows = case traverse referenceTime rows of
    Just (initialTime : rest) -> Just (foldl' max initialTime rest)
    _ -> Nothing

referenceFilename :: Text -> RubyInfoRow -> Text
referenceFilename name row = name <> "-" <> rowVersion row <> (if rowPlatform row == "ruby" then "" else "-" <> rowPlatform row) <> ".gem"

entryPosition :: EntryKey -> Maybe Int
entryPosition = \case
    RecordEntry position -> Just position
    _ -> Nothing
