-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Genuine gem envelopes and independent Ruby observations bind metadata to complete payloads.
module Ecluse.Core.Registry.RubyGems.ArchiveSpec (spec) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Exception (throw)
import Data.Aeson (Value (..), eitherDecodeStrict', object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.List (lookup)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Exts.Heap qualified as Heap
import Test.Hspec

import Ecluse.Core.Package (renderPackageName)
import Ecluse.Core.Package.Hash (sha256Hex)
import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.RubyGems.Archive
import Ecluse.Core.Registry.RubyGems.CompactSpec (GemSpecification (specificationPlatform, specificationVersion), specificationCoordinate)
import Ecluse.Core.Registry.RubyGems.Project (mkCoordinate, projectName, unGemPlatform)
import Ecluse.Core.Security.Limits (Limits (..), defaultLimits)
import Ecluse.Core.Version (renderVersion)
import Ecluse.Test.Registry.Archive (patchFirstHeader, pax, paxAt, paxRecord, regular, tarBytes, tarEntries)
import Ecluse.Test.Registry.RubyGems.Reference (runRubyReference)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "native gem validation" $ do
    it "matches independent Ruby identity and digest observations for every genuine gem" $ do
        manifest <- readFileBS "test/fixtures/rubygems/manifest.json" >>= expectRight . eitherDecodeStrict'
        releases <- case manifest of
            Object fields | Just (Array rows) <- KeyMap.lookup "releases" fields -> pure (toList rows)
            _ -> fail "native manifest has no releases"
        for_ releases $ \case
            Object fields | Just (String filename) <- KeyMap.lookup "filename" fields -> do
                bytes <- readFileBS ("test/fixtures/rubygems/gems/" <> toString filename)
                archive <- inspect defaultLimits bytes >>= expectRight
                reference <- runRubyReference "archive" bytes
                gemArchiveBytes archive `shouldBe` bytes
                let observed = object ["name" .= renderPackageName (gemArchiveName archive), "version" .= renderVersion (specificationVersion (gemArchiveSpecification archive)), "platform" .= unGemPlatform (specificationPlatform (gemArchiveSpecification archive)), "sha256" .= gemArchiveSha256 archive]
                    expected = Object (KeyMap.filterWithKey (\key _ -> key `elem` ["name", "version", "platform", "sha256"]) fields)
                observed `shouldBe` expected
                case reference of
                    Object native -> do
                        Object (KeyMap.filterWithKey (\key _ -> key `elem` ["name", "version", "platform", "sha256"]) native) `shouldBe` observed
                        KeyMap.lookup "files" native `shouldBe` Just (toJSON (Set.toAscList (gemArchiveInventory archive)))
                    _ -> fail "native archive reference is not an object"
                gemArchiveInventory archive `shouldSatisfy` (not . Set.null)
            _ -> fail "native manifest contains an invalid release"
    it "returns an evaluated successful archive without forcing it in the caller" $ do
        hasConstructor 32 (Heap.asBox (throw DeferredArchive :: GemArchive)) `shouldReturn` False
        bytes <- readFileBS fixture
        result <- inspect defaultLimits bytes
        archiveBox <- Heap.asBox <$> expectRight result
        hasConstructor 32 archiveBox `shouldReturn` True
        archive <- expectRight result
        gemArchiveSha256 archive `shouldBe` sha256Hex bytes
    it "refuses duplicate, missing and unknown envelope members and nonzero trailing data" $ do
        bytes <- readFileBS fixture
        members <- nativeMembers bytes
        for_ [members <> take 1 members, drop 1 members, members <> [("unexpected", "data")]] $ \entries -> do
            changed <- BSL.toStrict <$> tarBytes (map (first toString) entries)
            inspect defaultLimits changed >>= (`shouldSatisfy` isLeft)
        inspect defaultLimits (bytes <> "junk") >>= (`shouldSatisfy` isLeft)
    it "checks member checksums and traverses the complete nested compressed stream" $ do
        bytes <- readFileBS fixture
        members <- nativeMembers bytes
        let mutate key f = map (\(name, body) -> (name, if name == key then f body else body)) members
        for_ [mutate "metadata.gz" (<> "junk"), mutate "data.tar.gz" (<> "junk"), mutate "checksums.yaml.gz" (<> "junk"), mutate "data.tar.gz" (\raw -> BS.take (BS.length raw - 1) raw)] $ \entries -> do
            changed <- BSL.toStrict <$> tarBytes (map (first toString) entries)
            inspect defaultLimits changed >>= (`shouldSatisfy` isLeft)
    it "verifies every checksum algorithm and member in well-formed members" $ do
        bytes <- readFileBS fixture
        members <- nativeMembers bytes
        metadataGzip <- fixtureMember "metadata.gz" members
        dataGzip <- fixtureMember "data.tar.gz" members
        let sha256 = "SHA256:\n  metadata.gz: " <> sha256Hex metadataGzip <> "\n  data.tar.gz: " <> sha256Hex dataGzip <> "\n"
            zeros = T.replicate 128 "0"
        for_
            [ ("---\nSHA256:\n  metadata.gz: " <> sha256Hex metadataGzip <> "\n  data.tar.gz: " <> T.replicate 64 "0" <> "\n", "gem member checksum disagrees")
            , ("---\n" <> sha256 <> "SHA512:\n  metadata.gz: " <> zeros <> "\n  data.tar.gz: " <> zeros <> "\n", "gem member checksum disagrees")
            , ("---\n" <> sha256 <> "MD5:\n  metadata.gz: " <> zeros <> "\n  data.tar.gz: " <> zeros <> "\n", "missing or unsupported gem checksum algorithm")
            , ("--- !ruby/object:Gem::Specification\n" <> sha256, "gem checksum mapping has an unsupported native tag")
            , ("---\nSHA256:\n  metadata.gz: " <> sha256Hex metadataGzip <> "\n", "gem checksum member inventory disagrees")
            ]
            $ \(checksums, reason) -> do
                changed <- repackChecksums members checksums
                void <$> inspect defaultLimits changed `shouldReturn` Left (ParseError reason)
    it "reaches nested tar validation after accepting recomputed member checksums" $ do
        bytes <- readFileBS fixture
        members <- nativeMembers bytes
        dataGzip <- fixtureMember "data.tar.gz" members
        let nativeTar = GZip.decompress (BSL.fromStrict dataGzip)
        trailing <- repack members (BSL.toStrict (GZip.compress (nativeTar <> "junk")))
        void <$> inspect defaultLimits trailing `shouldReturn` Left (ParseError "tar archive has trailing nonzero bytes")
        missing <- repack members (BSL.toStrict (GZip.compress (Tar.write [])))
        void <$> inspect defaultLimits missing `shouldReturn` Left (ParseError "gem data inventory disagrees with metadata files")
        padded <- repack members (BSL.toStrict (GZip.compress (nativeTar <> BSL.replicate 32768 0)))
        let cap = 2 * BS.length bytes
        BS.length padded `shouldSatisfy` (< cap)
        void <$> inspect defaultLimits{maxMirrorArtifactBytes = cap} padded `shouldReturn` Left (ParseError "archive inflated bytes exceed their bound")
    it "rejects malformed tagged metadata even when the outer tar is valid" $ do
        bytes <- readFileBS fixture
        members <- nativeMembers bytes
        let malicious = BSL.toStrict (GZip.compress "--- !ruby/object:Kernel {}")
        changed <- BSL.toStrict <$> tarBytes (map (\(name, body) -> (toString name, if name == "metadata.gz" then malicious else body)) members)
        inspect defaultLimits changed >>= (`shouldSatisfy` isLeft)
    it "reaches artifact and metadata byte bounds" $ do
        bytes <- readFileBS fixture
        inspect defaultLimits{maxMirrorArtifactBytes = BS.length bytes - 1} bytes >>= (`shouldSatisfy` isLeft)
        inspect defaultLimits{maxMetadataBytes = 32} bytes >>= (`shouldSatisfy` isLeft)
    it "refuses the third outer-envelope member at the outer count bound" $ do
        bytes <- readFileBS fixture
        void <$> inspect defaultLimits{archiveEntryCeiling = 2} bytes `shouldReturn` Left (ParseError "archive entry count exceeds its bound")
    it "reaches the inner member count after the envelope and YAML fit their bounds" $ do
        bytes <- readFileBS fixture
        members <- nativeMembers bytes
        length members `shouldBe` 3
        dataGzip <- fixtureMember "data.tar.gz" members
        entries <- tarEntries (GZip.decompress (BSL.fromStrict dataGzip))
        directories <- forM [1 .. 512 :: Int] $ \index -> do
            path <- expectRight (Tar.toTarPath True ("inventory-" <> show index))
            pure (Tar.simpleEntry path Tar.Directory)
        let memberCount = length entries + length directories
            payload = BSL.toStrict (GZip.compress (Tar.write (entries <> directories)))
        changed <- repack members payload
        reference <- runRubyReference "archive" changed
        archive <- inspect defaultLimits{archiveEntryCeiling = memberCount} changed >>= expectRight
        case reference of
            Object native
                | Just (Array contents) <- KeyMap.lookup "files" native
                , Just (Object regularFiles) <- KeyMap.lookup "file_sizes" native -> do
                    length contents `shouldBe` memberCount
                    gemArchiveInventory archive `shouldBe` Set.fromList (map Key.toText (KeyMap.keys regularFiles))
            _ -> fail "native archive reference lacks its physical and regular-file inventories"
        void <$> inspect defaultLimits{archiveEntryCeiling = memberCount - 1} changed `shouldReturn` Left (ParseError "archive entry count exceeds its bound")
    it "accepts the gem build envelope and refuses a PAX or GNU extended header in front of it" $ do
        bytes <- readFileBS fixture
        void <$> published bytes `shouldReturn` Right ()
        entries <- tarEntries (BSL.fromStrict bytes)
        void <$> published (written entries) `shouldReturn` Right ()
        gnu <- case entries of
            entry : _ -> pure [Tar.longLinkEntry (Tar.entryPath entry), Tar.longSymLinkEntry "unused"]
            [] -> fail "native fixture has no envelope entries"
        posix <- sequence [pax 'x' (paxRecord "mtime=0\n"), pax 'g' (paxRecord "comment=opaque\n")]
        for_ (posix <> gnu) $ \header ->
            void <$> published (written (header : entries)) `shouldReturn` Left extendedRefusal
    it "refuses an outer member named through a ustar prefix" $ do
        bytes <- readFileBS fixture
        metadataGzip <- nativeMembers bytes >>= fixtureMember "metadata.gz"
        entries <- tarEntries (BSL.fromStrict bytes)
        prefixed <- regular (replicate 120 'a' <> "/metadata.gz") metadataGzip
        -- No envelope member name holds the slash that joins a prefix, so inspection itself refuses the member.
        void <$> inspect defaultLimits (written (entries <> [prefixed])) `shouldReturn` Left (ParseError "unsupported gem envelope member")
    it "refuses a PAX rename that would show RubyGems another metadata.gz" $ do
        root <- readFileBS fixture >>= nativeMembers
        leafMetadata <- readFileBS "test/fixtures/rubygems/gems/ecluse-fixture-leaf-1.0.0.gem" >>= nativeMembers >>= fixtureMember "metadata.gz"
        rootMetadata <- fixtureMember "metadata.gz" root
        rootData <- fixtureMember "data.tar.gz" root
        rootChecksums <- fixtureMember "checksums.yaml.gz" root
        -- RubyGems reads the raw names: the leaf's metadata.gz, and no checksums.yaml.gz to check it against.
        crafted <-
            written
                <$> sequence
                    [ paxAt "PaxHeaders/metadata" 'x' (paxRecord "path=metadata.gz\n")
                    , regular "junk-metadata" rootMetadata
                    , paxAt "PaxHeaders/signature" 'x' (paxRecord "path=metadata.gz.sig\n")
                    , regular "metadata.gz" leafMetadata
                    , regular "data.tar.gz" rootData
                    , paxAt "PaxHeaders/checksums" 'x' (paxRecord "path=checksums.yaml.gz\n")
                    , regular "junk-checksums" rootChecksums
                    ]
        runRubyReference "archive" crafted >>= \case
            Object native -> KeyMap.lookup "name" native `shouldBe` Just (String "ecluse-fixture-leaf")
            _ -> fail "native archive reference is not an object"
        fmap (renderPackageName . gemArchiveName) <$> inspect defaultLimits crafted `shouldReturn` Right "ecluse-fixture-root"
        void <$> published crafted `shouldReturn` Left extendedRefusal
    it "refuses an outer member whose name or prefix field holds bytes after its first NUL" $ do
        bytes <- readFileBS fixture
        named <- fmap gemArchiveName <$> inspect defaultLimits bytes
        -- metadata.gz comes first and fills 11 bytes of its name field. The prefix field starts at byte 345.
        -- RubyGems unpacks both fields with A100 and A155, which keep those bytes, so it reads another name.
        for_ [11, 345] $ \offset -> do
            let changed = patchFirstHeader offset "\NULx" bytes
            fmap gemArchiveName <$> inspect defaultLimits changed `shouldReturn` named
            void <$> published changed `shouldReturn` Left extendedRefusal
    it "binds the checked archive to an exact admitted coordinate and digest" $ do
        bytes <- readFileBS fixture
        archive <- inspect defaultLimits bytes >>= expectRight
        let wrongDigest = "0000000000000000000000000000000000000000000000000000000000000000"
        coordinate <- expectRight (specificationCoordinate (gemArchiveSpecification archive))
        checkGemArchive coordinate (gemArchiveSha256 archive) archive `shouldBe` Right archive
        checkGemArchive coordinate wrongDigest archive `shouldSatisfy` isLeft
        otherName <- expectRight (projectName "other-owned")
        otherCoordinate <- expectRight (mkCoordinate otherName (specificationVersion (gemArchiveSpecification archive)) (specificationPlatform (gemArchiveSpecification archive)))
        checkGemArchive otherCoordinate (gemArchiveSha256 archive) archive `shouldSatisfy` isLeft

fixture :: FilePath
fixture = "test/fixtures/rubygems/gems/ecluse-fixture-root-1.0.0.gem"

-- RubyGems' reader yields extended headers as members and keeps bytes after a NUL, so publication and mirroring refuse both and any name prefix.
extendedRefusal :: ParseError
extendedRefusal = ParseError "gem envelope carries an extended or prefixed tar header"

written :: [Tar.Entry] -> ByteString
written = BSL.toStrict . Tar.write

fixtureMember :: Text -> [(Text, ByteString)] -> IO ByteString
fixtureMember name members = maybe (fail ("native fixture has no " <> toString name <> " member")) pure (lookup name members)

inspect :: Limits -> ByteString -> IO (Either ParseError GemArchive)
inspect limits = inspectGemArchive limits (const (pure ()))

-- Inspection followed by the envelope check that publication and mirroring add.
published :: ByteString -> IO (Either ParseError GemArchive)
published bytes = (>>= checkRawEnvelope) <$> inspect defaultLimits bytes

nativeMembers :: ByteString -> IO [(Text, ByteString)]
nativeMembers bytes = tarEntries (BSL.fromStrict bytes) >>= traverse member
  where
    member entry = case Tar.entryContent entry of
        Tar.NormalFile body _ -> pure (toText (Tar.entryPath entry), BSL.toStrict body)
        _ -> fail "native fixture contains a non-file envelope entry"

repack :: [(Text, ByteString)] -> ByteString -> IO ByteString
repack members payload = do
    metadataGzip <- fixtureMember "metadata.gz" members
    let checksums = TE.encodeUtf8 ("---\nSHA256:\n  metadata.gz: " <> sha256Hex metadataGzip <> "\n  data.tar.gz: " <> sha256Hex payload <> "\n")
        checksumGzip = BSL.toStrict (GZip.compress (BSL.fromStrict checksums))
    BSL.toStrict <$> tarBytes [("metadata.gz", metadataGzip), ("data.tar.gz", payload), ("checksums.yaml.gz", checksumGzip)]

-- The genuine metadata and data members under the given checksums document, every member well-formed.
repackChecksums :: [(Text, ByteString)] -> Text -> IO ByteString
repackChecksums members checksums = do
    metadataGzip <- fixtureMember "metadata.gz" members
    dataGzip <- fixtureMember "data.tar.gz" members
    let checksumGzip = BSL.toStrict (GZip.compress (BSL.fromStrict (TE.encodeUtf8 checksums)))
    BSL.toStrict <$> tarBytes [("metadata.gz", metadataGzip), ("data.tar.gz", dataGzip), ("checksums.yaml.gz", checksumGzip)]

hasConstructor :: Int -> Heap.Box -> IO Bool
hasConstructor remaining box
    | remaining <= 0 = pure False
    | otherwise =
        Heap.getBoxedClosureData box >>= \case
            Heap.ConstrClosure{} -> pure True
            Heap.IndClosure{Heap.indirectee = target} -> hasConstructor (remaining - 1) target
            Heap.BlackholeClosure{Heap.indirectee = target} -> hasConstructor (remaining - 1) target
            _ -> pure False

data DeferredArchive = DeferredArchive
    deriving stock (Show)

instance Exception DeferredArchive
