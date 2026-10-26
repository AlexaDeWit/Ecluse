-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Verified native Cargo archives exercise complete traversal and selected text retention.
module Ecluse.Core.Registry.Cargo.ArchiveSpec (spec) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Set qualified as Set
import Test.Hspec
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Package.Hash (sha256Hex)
import Ecluse.Core.Registry (ParseError)
import Ecluse.Core.Registry.Cargo.Archive
import Ecluse.Core.Registry.Cargo.Manifest (publishMetadataValue)
import Ecluse.Core.Security.Limits (Limits (..), defaultLimits)
import Ecluse.Test.Registry.Archive (paxRecord, tarBytes)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    describe "inspectCargoArchive" $ do
        it "counts complete inflated bytes across headers, payloads and the trailer" $ do
            tar <- tarBytes [("example-1.2.3/Cargo.toml", manifest), ("example-1.2.3/src/lib.rs", "pub fn value() {}")]
            metadata <- inspect defaultLimits (compress tar) >>= expectRight
            cargoArchiveInflatedBytes metadata `shouldBe` fromIntegral (BSL.length tar)
        it "checks compressed SHA-256 before allocating decoder workspace" $ do
            bytes <- fixture [("Cargo.toml", manifest)]
            requests <- newIORef ([] :: [Int])
            result <- inspectCargoArchive defaultLimits (\wanted -> modifyIORef' requests (wanted :)) "example" "1.2.3" (BS.replicate 64 48 & decodeUtf8) bytes
            result `shouldSatisfy` isLeft
            readIORef requests `shouldReturn` []
        it "uses only the effective manifest and enumerates later build scripts" $ do
            bytes <- fixture [("Cargo.toml", manifest), ("Cargo.toml.orig", "unparsed original"), ("build.rs", "never execute")]
            metadata <- inspect defaultLimits bytes >>= expectRight
            cargoArchiveExecution metadata `shouldBe` Right True
            cargoArchiveInventory metadata `shouldBe` Set.fromList ["Cargo.toml", "Cargo.toml.orig", "build.rs"]
        it "requires exact manifest identity and the one admitted archive root" $ do
            for_ ["[package]\nname='Example'\nversion='1.2.3'", "[package]\nname='example'\nversion='1.2.3+other'"] $ \other -> do
                bytes <- fixture [("Cargo.toml", other)]
                inspect defaultLimits bytes >>= (`shouldSatisfy` isLeft)
            tar <- tarBytes [("other-1.2.3/Cargo.toml", manifest)]
            inspect defaultLimits (compress tar) >>= (`shouldSatisfy` isLeft)
        it "rejects duplicates and a missing effective root manifest" $ do
            for_ [[("Cargo.toml", manifest), ("Cargo.toml", manifest)], [("nested/Cargo.toml", manifest)], [("Cargo.toml.orig", manifest)]] $ \entries -> do
                bytes <- fixture entries
                inspect defaultLimits bytes >>= (`shouldSatisfy` isLeft)
        it "checks the complete tar trailer and the gzip checksum" $ do
            tar <- tarBytes [("example-1.2.3/Cargo.toml", manifest)]
            let valid = compress tar
                badCrc = BS.take (BS.length valid - 8) valid <> BS.replicate 4 0 <> BS.drop (BS.length valid - 4) valid
            for_ [compress (BSL.take 1024 tar), compress (tar <> "nonzero"), BS.take (BS.length valid - 3) valid, valid <> "junk", badCrc] $
                inspect defaultLimits >=> (`shouldSatisfy` isLeft)
        it "rejects a traversal hidden in a PAX path override" $ do
            header <- expectRight (Tar.toTarPath False "PaxHeaders/path")
            placeholder <- expectRight (Tar.toTarPath False "placeholder")
            let record = paxRecord "path=../Cargo.toml\n"
                tar = Tar.write [Tar.simpleEntry header (Tar.OtherEntryType 'x' record (BSL.length record)), Tar.fileEntry placeholder (BSL.fromStrict manifest)]
            inspect defaultLimits (compress tar) >>= (`shouldSatisfy` isLeft)
        it "rejects unflattened links and files beneath regular-file ancestors" $ do
            path <- expectRight (Tar.toTarPath False "example-1.2.3/linked")
            manifestPath <- expectRight (Tar.toTarPath False "example-1.2.3/Cargo.toml")
            target <- maybe (fail "invalid fixture link target") pure (Tar.toLinkTarget "Cargo.toml")
            let tar = Tar.write [Tar.simpleEntry path (Tar.SymbolicLink target), Tar.fileEntry manifestPath (BSL.fromStrict manifest)]
            inspect defaultLimits (compress tar) >>= (`shouldSatisfy` isLeft)
            bytes <- fixture [("Cargo.toml", manifest), ("parent", "x"), ("parent/child", "x")]
            inspect defaultLimits bytes >>= (`shouldSatisfy` isLeft)
        it "bounds compressed bytes, inflated bytes, entries and manifest retention" $ do
            bytes <- fixture [("Cargo.toml", manifest), ("padding", BS.replicate 65536 65)]
            for_ [defaultLimits{maxMirrorArtifactBytes = 10}, defaultLimits{maxMirrorArtifactBytes = 8192}, defaultLimits{maxArtifactCount = 1}, defaultLimits{maxMetadataBytes = 32}] $ \limits ->
                inspect limits bytes >>= (`shouldSatisfy` isLeft)
        it "does not retain README payloads during the execution evidence pass" $ do
            small <- fixture [("Cargo.toml", manifest), ("README.md", BS.replicate 65536 65)]
            large <- fixture [("Cargo.toml", manifest), ("README.md", BS.replicate (1024 * 1024) 65)]
            a <- peakCharge small
            b <- peakCharge large
            b `shouldBe` a
        it "propagates a resource charge refusal as a fault" $ do
            bytes <- fixture [("Cargo.toml", manifest)]
            let refuse _ = throwIO ChargeRefused
            inspectCargoArchive defaultLimits refuse "example" "1.2.3" (sha256Hex bytes) bytes `shouldThrow` (== ChargeRefused)
    describe "cargoPublicationMetadata" $ do
        it "reads a selected README both before and after Cargo.toml" $
            for_ [[("README.md", "readme first"), ("Cargo.toml", manifest)], [("Cargo.toml", manifest), ("README.md", "readme first")]] $ \entries -> do
                bytes <- fixture entries
                metadata <- inspect defaultLimits bytes >>= expectRight
                upload <- cargoPublicationMetadata defaultLimits (const (pure ())) metadata >>= expectRight
                case publishMetadataValue upload of
                    Object fields -> KeyMap.lookup "readme" fields `shouldBe` Just (String "readme first")
                    _ -> expectationFailure "Cargo publication is not an object"
        it "keeps invalid README text irrelevant to execution but refuses publication" $ do
            bytes <- fixture [("Cargo.toml", manifest), ("README.md", BS.singleton 255)]
            metadata <- inspect defaultLimits bytes >>= expectRight
            cargoArchiveExecution metadata `shouldBe` Right False
            cargoPublicationMetadata defaultLimits (const (pure ())) metadata >>= (`shouldSatisfy` isLeft)
        it "does not substitute empty text for an absent explicitly declared file" $ do
            bytes <- fixture [("Cargo.toml", manifest <> "readme='missing.md'\n")]
            metadata <- inspect defaultLimits bytes >>= expectRight
            cargoPublicationMetadata defaultLimits (const (pure ())) metadata >>= (`shouldSatisfy` isLeft)

    describe "cargoPublicationAgreement" $ do
        it "reserves comparison workspace before accepting an available source claim" $ do
            bytes <- fixture [("Cargo.toml", manifest)]
            archive <- inspect defaultLimits bytes >>= expectRight
            metadata <- cargoPublicationMetadata defaultLimits (const (pure ())) archive >>= expectRight
            payments <- newIORef ([] :: [Int])
            let source = object ["name" .= ("example" :: Text), "vers" .= ("1.2.3" :: Text), "deps" .= ([] :: [Value])]
            checked <- cargoPublicationAgreement defaultLimits (\cost -> modifyIORef' payments (cost :)) archive source metadata >>= expectRight
            publishMetadataValue checked `shouldBe` publishMetadataValue metadata
            paid <- readIORef payments
            paid `shouldSatisfy` (\costs -> length costs == 1 && all (> 0) costs)
            cargoPublicationAgreement defaultLimits (const (throwIO ChargeRefused)) archive source metadata `shouldThrow` (== ChargeRefused)

manifest :: ByteString
manifest = "[package]\nname='example'\nversion='1.2.3'\n"

fixture :: [(FilePath, ByteString)] -> IO ByteString
fixture entries = compress <$> tarBytes [("example-1.2.3/" <> path, content) | (path, content) <- entries]

compress :: BSL.ByteString -> ByteString
compress = BSL.toStrict . GZip.compress

inspect :: Limits -> ByteString -> IO (Either ParseError CargoArchiveMetadata)
inspect limits bytes = inspectCargoArchive limits (const (pure ())) "example" "1.2.3" (sha256Hex bytes) bytes

peakCharge :: ByteString -> IO Int
peakCharge bytes = do
    peak <- newIORef 0
    let charge wanted = modifyIORef' peak (max wanted)
    void (inspectCargoArchive defaultLimits charge "example" "1.2.3" (sha256Hex bytes) bytes >>= expectRight)
    readIORef peak

data ChargeRefused = ChargeRefused
    deriving stock (Eq, Show)

instance Exception ChargeRefused
