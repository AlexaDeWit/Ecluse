-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Native archive fixtures exercise complete traversal and bounded metadata extraction.
module Ecluse.Core.Registry.PyPI.ArchiveSpec (spec) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.BZip qualified as BZip
import Codec.Compression.GZip qualified as GZip
import Codec.Compression.Lzma qualified as Lzma
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Package (mkPackageName)
import Ecluse.Core.Registry.PyPI.Archive (archiveMetadata)
import Ecluse.Core.Security.Limits (Limits (..), defaultLimits)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Osv (osvZipOf)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "archiveMetadata" $ do
    for_ [(".tar.gz", GZip.compress), (".tgz", GZip.compress), (".tar.bz2", BZip.compress), (".tar.xz", Lzma.compress)] $ \(suffix, compress) -> do
        it ("extracts exact PKG-INFO from " <> toString suffix) $ do
            tar <- tarBytes [("example-1/PKG-INFO", metadata), ("example-1/package.py", "print('never execute')")]
            extract defaultLimits ("example-1" <> suffix) (BSL.toStrict (compress tar)) `shouldReturn` Right metadata
        it ("rejects truncated " <> toString suffix) $ do
            tar <- tarBytes [("example-1/PKG-INFO", metadata)]
            let compressed = BSL.toStrict (compress tar)
            extract defaultLimits ("example-1" <> suffix) (BS.take (BS.length compressed - 5) compressed) >>= (`shouldSatisfy` isLeft)
        it ("rejects an inflated-byte bomb in " <> toString suffix) $ do
            tar <- tarBytes [("example-1/PKG-INFO", metadata), ("example-1/padding", BS.replicate 65536 65)]
            extract defaultLimits{maxMirrorArtifactBytes = 8192} ("example-1" <> suffix) (BSL.toStrict (compress tar)) >>= (`shouldSatisfy` isLeft)
    for_ [("example-1-py3-none-any.whl", "example-1.dist-info/METADATA"), ("example-1.zip", "example-1/PKG-INFO")] $ \(filename, path) ->
        it ("reads metadata from " <> toString filename) $ do
            bytes <- zipBytes [(path, metadata), ("example/data", "unchanged")]
            extract defaultLimits filename bytes `shouldReturn` Right metadata
    it "rejects duplicate, unsafe, and conflicting metadata entries"
        $ for_
            [ [("example-1.dist-info/METADATA", metadata), ("example-1.dist-info/METADATA", metadata)]
            , [("../example-1.dist-info/METADATA", metadata)]
            , [("other-1.dist-info/METADATA", metadata)]
            , [("example-1.dist-info/METADATA", metadata), ("example-1.0.dist-info/METADATA", metadata)]
            ]
        $ \entries -> do
            bytes <- zipBytes entries
            extract defaultLimits "example-1-py3-none-any.whl" bytes >>= (`shouldSatisfy` isLeft)
    it "rejects missing metadata and exceeded metadata or entry budgets" $ do
        bytes <- zipBytes [("example-1.dist-info/METADATA", metadata), ("example/data", "x")]
        extract defaultLimits{maxMetadataBytes = 20} "example-1-py3-none-any.whl" bytes >>= (`shouldSatisfy` isLeft)
        extract defaultLimits{maxArtifactCount = 1} "example-1-py3-none-any.whl" bytes >>= (`shouldSatisfy` isLeft)
        absent <- zipBytes [("example/data", "x")]
        extract defaultLimits "example-1-py3-none-any.whl" absent >>= (`shouldSatisfy` isLeft)
    it "checks the whole ZIP directory and compressed trailer" $ do
        bytes <- zipBytes [("example-1.dist-info/METADATA", metadata)]
        extract defaultLimits "example-1-py3-none-any.whl" (BS.take (BS.length bytes - 10) bytes) >>= (`shouldSatisfy` isLeft)
        extract defaultLimits "example-1-py3-none-any.whl" (bytes <> "junk") >>= (`shouldSatisfy` isLeft)
    it "refuses a tar without its trailer even when metadata is complete" $ do
        tar <- tarBytes [("example-1/PKG-INFO", metadata)]
        extract defaultLimits "example-1.tar.gz" (BSL.toStrict (GZip.compress (BSL.take 1024 tar))) >>= (`shouldSatisfy` isLeft)
    it "refuses nonzero tar trailing bytes and bounded zero padding bombs" $ do
        tar <- tarBytes [("example-1/PKG-INFO", metadata)]
        extract defaultLimits "example-1.tar.gz" (BSL.toStrict (GZip.compress (tar <> "junk"))) >>= (`shouldSatisfy` isLeft)
        extract defaultLimits{maxMirrorArtifactBytes = 8192} "example-1.tar.gz" (BSL.toStrict (GZip.compress (tar <> BSL.replicate 65536 0))) >>= (`shouldSatisfy` isLeft)
    it "reads a bounded pax path override and refuses an escaping override" $ do
        for_ [("example-1/PKG-INFO", True), ("../PKG-INFO", False)] $ \(path, accepted) -> do
            headerPath <- expectRight (Tar.toTarPath False "PaxHeaders/metadata")
            filePath <- expectRight (Tar.toTarPath False "placeholder")
            let record = paxRecord ("path=" <> path <> "\n")
                archive = Tar.write [Tar.simpleEntry headerPath (Tar.OtherEntryType 'x' record (BSL.length record)), Tar.fileEntry filePath (BSL.fromStrict metadata)]
            result <- extract defaultLimits "example-1.tar.gz" (BSL.toStrict (GZip.compress archive))
            if accepted then result `shouldBe` Right metadata else result `shouldSatisfy` isLeft
    it "rejects tar links and a corrupted header checksum" $ do
        path <- expectRight (Tar.toTarPath False "example-1/PKG-INFO")
        link <- maybe (fail "invalid fixture link target") pure (Tar.toLinkTarget "other")
        let linked = Tar.write [Tar.simpleEntry path (Tar.SymbolicLink link)]
        extract defaultLimits "example-1.tar.gz" (BSL.toStrict (GZip.compress linked)) >>= (`shouldSatisfy` isLeft)
        tar <- tarBytes [("example-1/PKG-INFO", metadata)]
        extract defaultLimits "example-1.tar.gz" (BSL.toStrict (GZip.compress ("X" <> BSL.drop 1 tar))) >>= (`shouldSatisfy` isLeft)
    it "refuses an XZ dictionary above its memory ceiling before inflation" $ do
        extract defaultLimits "example-1.tar.xz" ordinaryDictionaryArchive `shouldReturn` Right metadata
        extract defaultLimits "example-1.tar.xz" oversizedDictionaryArchive >>= (`shouldSatisfy` isLeft)
    it "refuses an unsupported suffix and an inconsistent filename" $ do
        extract defaultLimits "example-1.egg" "archive" >>= (`shouldSatisfy` isLeft)
        extract defaultLimits "other-1.tar.gz" "archive" >>= (`shouldSatisfy` isLeft)
  where
    extract limits = archiveMetadata limits (mkPackageName PyPI Nothing "example") (mkVersion PyPI "1")

metadata :: ByteString
metadata = "Metadata-Version: 2.4\nName: example\nVersion: 1\n\nDescription.\n"

zipBytes :: [(Text, ByteString)] -> IO ByteString
zipBytes entries = BSL.toStrict <$> osvZipOf (map (second BSL.fromStrict) entries)

tarBytes :: [(FilePath, ByteString)] -> IO BSL.ByteString
tarBytes entries = Tar.write <$> traverse entry entries
  where
    entry (path, bytes) = do
        tarPath <- expectRight (Tar.toTarPath False path)
        pure (Tar.fileEntry tarPath (BSL.fromStrict bytes))

paxRecord :: ByteString -> BSL.ByteString
paxRecord payload = BSL.fromStrict (encodeUtf8 (show size :: Text) <> " " <> payload)
  where
    size = settle (BS.length payload + 2)
    settle candidate =
        let next = BS.length payload + 1 + length (show candidate :: String)
         in if next == candidate then candidate else settle next

-- Python tarfile/lzma fixture. Its partner changes only the dictionary property and the block-header CRC.
ordinaryDictionaryArchive :: ByteString
ordinaryDictionaryArchive = BS.pack [0xfd, 0x37, 0x7a, 0x58, 0x5a, 0x00, 0x00, 0x04, 0xe6, 0xd6, 0xb4, 0x46, 0x02, 0x00, 0x21, 0x01, 0x0c, 0x00, 0x00, 0x00, 0x8f, 0x98, 0x41, 0x9c, 0xe0, 0x27, 0xff, 0x00, 0x98, 0x5d, 0x00, 0x32, 0x9e, 0x08, 0x46, 0x81, 0x33, 0x76, 0xe4, 0x2b, 0x65, 0x2b, 0x45, 0xfd, 0xd2, 0xe4, 0x28, 0x73, 0xd8, 0xe2, 0xeb, 0x69, 0x5d, 0x7c, 0x93, 0x62, 0x41, 0x40, 0xf2, 0x2c, 0x94, 0x2b, 0xae, 0x49, 0x3a, 0x96, 0xaa, 0x31, 0x77, 0xf0, 0x53, 0x76, 0x92, 0xae, 0x38, 0xae, 0xf4, 0x79, 0x57, 0xee, 0x26, 0x2f, 0xf1, 0x7e, 0x5d, 0x28, 0xde, 0x65, 0xbe, 0x53, 0x1c, 0x80, 0x66, 0x98, 0x90, 0x90, 0x00, 0xe6, 0xc4, 0xdd, 0xda, 0xa8, 0xe5, 0x06, 0xe8, 0xd4, 0xac, 0x2f, 0x28, 0x19, 0x14, 0xcc, 0x63, 0x69, 0x47, 0x6d, 0x07, 0x69, 0x28, 0x0b, 0x28, 0x15, 0xb8, 0xca, 0xa2, 0x6d, 0x88, 0xbb, 0x6a, 0x7e, 0xbb, 0xc8, 0x53, 0xc5, 0xd1, 0x32, 0xd9, 0x0a, 0x16, 0x53, 0xde, 0x64, 0xc7, 0xb0, 0x72, 0x7f, 0x30, 0x8d, 0x20, 0xcc, 0xb5, 0xd9, 0xe9, 0x4d, 0xba, 0xff, 0xab, 0xf4, 0x36, 0x98, 0xc2, 0x47, 0x99, 0x3b, 0xf4, 0x04, 0x01, 0x07, 0x7f, 0xfa, 0x12, 0x03, 0x71, 0x60, 0x43, 0x1a, 0x45, 0x85, 0x4c, 0x27, 0x81, 0xc1, 0x00, 0x00, 0xa4, 0x98, 0x66, 0x8f, 0xa2, 0xdc, 0xcc, 0xc8, 0x00, 0x01, 0xb4, 0x01, 0x80, 0x50, 0x00, 0x00, 0x85, 0x34, 0x8b, 0xfe, 0xb1, 0xc4, 0x67, 0xfb, 0x02, 0x00, 0x00, 0x00, 0x00, 0x04, 0x59, 0x5a]

oversizedDictionaryArchive :: ByteString
oversizedDictionaryArchive = BS.pack [0xfd, 0x37, 0x7a, 0x58, 0x5a, 0x00, 0x00, 0x04, 0xe6, 0xd6, 0xb4, 0x46, 0x02, 0x00, 0x21, 0x01, 0x28, 0x00, 0x00, 0x00, 0xe6, 0xa0, 0x11, 0xb3, 0xe0, 0x27, 0xff, 0x00, 0x98, 0x5d, 0x00, 0x32, 0x9e, 0x08, 0x46, 0x81, 0x33, 0x76, 0xe4, 0x2b, 0x65, 0x2b, 0x45, 0xfd, 0xd2, 0xe4, 0x28, 0x73, 0xd8, 0xe2, 0xeb, 0x69, 0x5d, 0x7c, 0x93, 0x62, 0x41, 0x40, 0xf2, 0x2c, 0x94, 0x2b, 0xae, 0x49, 0x3a, 0x96, 0xaa, 0x31, 0x77, 0xf0, 0x53, 0x76, 0x92, 0xae, 0x38, 0xae, 0xf4, 0x79, 0x57, 0xee, 0x26, 0x2f, 0xf1, 0x7e, 0x5d, 0x28, 0xde, 0x65, 0xbe, 0x53, 0x1c, 0x80, 0x66, 0x98, 0x90, 0x90, 0x00, 0xe6, 0xc4, 0xdd, 0xda, 0xa8, 0xe5, 0x06, 0xe8, 0xd4, 0xac, 0x2f, 0x28, 0x19, 0x14, 0xcc, 0x63, 0x69, 0x47, 0x6d, 0x07, 0x69, 0x28, 0x0b, 0x28, 0x15, 0xb8, 0xca, 0xa2, 0x6d, 0x88, 0xbb, 0x6a, 0x7e, 0xbb, 0xc8, 0x53, 0xc5, 0xd1, 0x32, 0xd9, 0x0a, 0x16, 0x53, 0xde, 0x64, 0xc7, 0xb0, 0x72, 0x7f, 0x30, 0x8d, 0x20, 0xcc, 0xb5, 0xd9, 0xe9, 0x4d, 0xba, 0xff, 0xab, 0xf4, 0x36, 0x98, 0xc2, 0x47, 0x99, 0x3b, 0xf4, 0x04, 0x01, 0x07, 0x7f, 0xfa, 0x12, 0x03, 0x71, 0x60, 0x43, 0x1a, 0x45, 0x85, 0x4c, 0x27, 0x81, 0xc1, 0x00, 0x00, 0xa4, 0x98, 0x66, 0x8f, 0xa2, 0xdc, 0xcc, 0xc8, 0x00, 0x01, 0xb4, 0x01, 0x80, 0x50, 0x00, 0x00, 0x85, 0x34, 0x8b, 0xfe, 0xb1, 0xc4, 0x67, 0xfb, 0x02, 0x00, 0x00, 0x00, 0x00, 0x04, 0x59, 0x5a]
