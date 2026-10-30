-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

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

import Ecluse.Core.Package.Hash (sha256Hex)
import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.Cargo.Archive
import Ecluse.Core.Registry.Cargo.Archive.Internal (caInventory)
import Ecluse.Core.Registry.Cargo.Manifest (publishMetadataValue)
import Ecluse.Core.Security.Limits (Limits (..), defaultLimits)
import Ecluse.Server.Admission.Support (ChargeRefused (..), refuseCharge)
import Ecluse.Test.Registry.Archive (paxRecord, tarBytes)
import Ecluse.Test.Registry.Cargo.Fixtures (cargoCrateArchive)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    describe "inspectCargoArchive" $ do
        it "checks compressed SHA-256 before allocating decoder workspace" $ do
            bytes <- fixture [("Cargo.toml", manifest)]
            requests <- newIORef ([] :: [Int])
            result <- inspectCargoArchive defaultLimits (\wanted -> modifyIORef' requests (wanted :)) "example" "1.2.3" (Just (BS.replicate 64 48 & decodeUtf8)) bytes
            void result `shouldBe` Left (ParseError "Cargo compressed archive disagrees with its admitted SHA-256")
            readIORef requests `shouldReturn` []
        it "uses only the effective manifest and enumerates every regular file" $ do
            bytes <- fixture [("Cargo.toml", manifest), ("Cargo.toml.orig", "unparsed original"), ("build.rs", "never execute")]
            metadata <- inspect defaultLimits bytes >>= expectRight
            caInventory metadata `shouldBe` Set.fromList ["Cargo.toml", "Cargo.toml.orig", "build.rs"]
        it "requires exact manifest identity and the one admitted archive root" $ do
            for_ ["[package]\nname='Example'\nversion='1.2.3'", "[package]\nname='example'\nversion='1.2.3+other'"] $ \other -> do
                bytes <- fixture [("Cargo.toml", other)]
                result <- inspect defaultLimits bytes
                void result `shouldBe` Left (ParseError "Cargo manifest disagrees with the admitted native identity")
            tar <- tarBytes [("other-1.2.3/Cargo.toml", manifest)]
            result <- inspect defaultLimits (compress tar)
            void result `shouldBe` Left (ParseError "Cargo archive member disagrees with its admitted package root")
        it "accepts an explicit root directory and refuses a regular file at that root" $ do
            root <- expectRight (Tar.toTarPath True "example-1.2.3/")
            rootFile <- expectRight (Tar.toTarPath False "example-1.2.3")
            path <- expectRight (Tar.toTarPath False "example-1.2.3/Cargo.toml")
            let info = Tar.fileEntry path (BSL.fromStrict manifest)
            metadata <- inspect defaultLimits (compress (Tar.write [Tar.simpleEntry root Tar.Directory, info])) >>= expectRight
            caInventory metadata `shouldBe` Set.singleton "Cargo.toml"
            result <- inspect defaultLimits (compress (Tar.write [Tar.fileEntry rootFile BSL.empty, info]))
            void result `shouldBe` Left (ParseError "Cargo archive root is not a directory")
        it "rejects duplicates and a missing effective root manifest" $ do
            for_ [([("Cargo.toml", manifest), ("Cargo.toml", manifest)], "duplicate archive entry"), ([("nested/Cargo.toml", manifest)], "Cargo archive lacks its effective Cargo.toml"), ([("Cargo.toml.orig", manifest)], "Cargo archive lacks its effective Cargo.toml")] $ \(entries, reason) -> do
                bytes <- fixture entries
                result <- inspect defaultLimits bytes
                void result `shouldBe` Left (ParseError reason)
        it "checks the complete tar trailer and the gzip checksum" $ do
            tar <- tarBytes [("example-1.2.3/Cargo.toml", manifest)]
            let valid = compress tar
                badCrc = BS.take (BS.length valid - 8) valid <> BS.replicate 4 0 <> BS.drop (BS.length valid - 4) valid
            for_ [(compress (BSL.take 1024 tar), "tar archive has no complete trailer"), (compress (tar <> "nonzero"), "tar archive has trailing nonzero bytes"), (BS.take (BS.length valid - 3) valid, "malformed or truncated compressed stream"), (valid <> "junk", "malformed or truncated compressed stream"), (badCrc, "malformed or truncated compressed stream")] $ \(bytes, reason) -> do
                result <- inspect defaultLimits bytes
                void result `shouldBe` Left (ParseError reason)
        it "rejects a traversal hidden in a PAX path override" $ do
            header <- expectRight (Tar.toTarPath False "PaxHeaders/path")
            placeholder <- expectRight (Tar.toTarPath False "placeholder")
            let record = paxRecord "path=../Cargo.toml\n"
                tar = Tar.write [Tar.simpleEntry header (Tar.OtherEntryType 'x' record (BSL.length record)), Tar.fileEntry placeholder (BSL.fromStrict manifest)]
            result <- inspect defaultLimits (compress tar)
            void result `shouldBe` Left (ParseError "unsafe archive path")
        it "rejects unflattened links and files beneath regular-file ancestors" $ do
            path <- expectRight (Tar.toTarPath False "example-1.2.3/linked")
            manifestPath <- expectRight (Tar.toTarPath False "example-1.2.3/Cargo.toml")
            target <- maybe (fail "invalid fixture link target") pure (Tar.toLinkTarget "Cargo.toml")
            let tar = Tar.write [Tar.simpleEntry path (Tar.SymbolicLink target), Tar.fileEntry manifestPath (BSL.fromStrict manifest)]
            result <- inspect defaultLimits (compress tar)
            void result `shouldBe` Left (ParseError "Cargo packaged archive contains an unflattened link")
            bytes <- fixture [("Cargo.toml", manifest), ("parent", "x"), ("parent/child", "x")]
            nested <- inspect defaultLimits bytes
            void nested `shouldBe` Left (ParseError "Cargo archive file traverses another regular file")
        it "bounds compressed bytes, inflated bytes and entries" $ do
            bytes <- fixture [("Cargo.toml", manifest), ("padding", BS.replicate 65536 65)]
            for_
                [ (defaultLimits{maxMirrorArtifactBytes = 10}, ParseError "Cargo compressed archive exceeds its byte bound")
                , (defaultLimits{maxMirrorArtifactBytes = 8192}, ParseError "archive inflated bytes exceed their bound")
                , (defaultLimits{maxArtifactCount = 1}, ParseError "archive entry count exceeds its bound")
                ]
                $ \(limits, fault) -> do
                    result <- inspect limits bytes
                    void result `shouldBe` Left fault
        it "bounds retained manifest bytes at the exact ceiling" $ do
            let limits = defaultLimits{maxMetadataBytes = 512}
                padded size = manifest <> "#" <> BS.replicate (size - BS.length manifest - 2) 120 <> "\n"
            atLimit <- fixture [("Cargo.toml", padded 512)]
            inspect limits atLimit >>= (`shouldSatisfy` isRight)
            overLimit <- fixture [("Cargo.toml", padded 513)]
            result <- inspect limits overLimit
            void result `shouldBe` Left (ParseError "tar metadata or extended header exceeds its bound")
        it "does not retain README payloads during the manifest pass" $ do
            small <- fixture [("Cargo.toml", manifest), ("README.md", BS.replicate 65536 65)]
            large <- fixture [("Cargo.toml", manifest), ("README.md", BS.replicate (1024 * 1024) 65)]
            a <- peakCharge small
            b <- peakCharge large
            b `shouldBe` a
        it "propagates a resource charge refusal as a fault" $ do
            bytes <- fixture [("Cargo.toml", manifest)]
            inspectCargoArchive defaultLimits refuseCharge "example" "1.2.3" (Just (sha256Hex bytes)) bytes `shouldThrow` (== ChargeRefused)
    describe "cargoPublicationMetadata" $ do
        it "preserves publication claims and their reservation when README capture is disabled" $ do
            bytes <- fixture [("Cargo.toml", manifest <> "readme=false\nlicense='MIT'\n"), ("README.md", BS.singleton 255)]
            retained <- newIORef 0
            archive <- inspectCargoArchive defaultLimits (writeIORef retained) "example" "1.2.3" (Just (sha256Hex bytes)) bytes >>= expectRight
            held <- readIORef retained
            held `shouldSatisfy` (> 0)
            payments <- newIORef ([] :: [Int])
            upload <- cargoPublicationMetadata defaultLimits (\cost -> modifyIORef' payments (cost :)) archive >>= expectRight
            case publishMetadataValue upload of
                Object fields ->
                    for_
                        [ ("name", String "example")
                        , ("vers", String "1.2.3")
                        , ("license", String "MIT")
                        , ("readme", Null)
                        , ("readme_file", Null)
                        , ("license_file", Null)
                        ]
                        $ \(key, value) -> KeyMap.lookup key fields `shouldBe` Just value
                _ -> expectationFailure "Cargo publication is not an object"
            paid <- readIORef payments
            paid `shouldSatisfy` (\costs -> not (null costs) && all (>= held) costs)
            cargoPublicationMetadata defaultLimits refuseCharge archive `shouldThrow` (== ChargeRefused)
        it "reads a selected README both before and after Cargo.toml" $
            for_ [[("README.md", "readme first"), ("Cargo.toml", manifest)], [("Cargo.toml", manifest), ("README.md", "readme first")]] $ \entries -> do
                bytes <- fixture entries
                metadata <- inspect defaultLimits bytes >>= expectRight
                upload <- cargoPublicationMetadata defaultLimits (const (pure ())) metadata >>= expectRight
                case publishMetadataValue upload of
                    Object fields -> KeyMap.lookup "readme" fields `shouldBe` Just (String "readme first")
                    _ -> expectationFailure "Cargo publication is not an object"
        it "accepts invalid README text at inspection but refuses publication" $ do
            bytes <- fixture [("Cargo.toml", manifest), ("README.md", BS.singleton 255)]
            metadata <- inspect defaultLimits bytes >>= expectRight
            cargoPublicationMetadata defaultLimits (const (pure ())) metadata `shouldReturn` Left (ParseError "Cargo publication text is not UTF-8")
        it "does not substitute empty text for an absent explicitly declared file" $ do
            bytes <- fixture [("Cargo.toml", manifest <> "readme='missing.md'\n")]
            metadata <- inspect defaultLimits bytes >>= expectRight
            cargoPublicationMetadata defaultLimits (const (pure ())) metadata `shouldReturn` Left (ParseError "Cargo manifest declares an absent text file")
        it "bounds the selected-text pass after a valid archive inspection" $ do
            let limits = defaultLimits{maxMetadataBytes = 512}
            for_ [(512, True), (513, False)] $ \(size, accepted) -> do
                bytes <- fixture [("Cargo.toml", manifest), ("README.md", BS.replicate size 65)]
                archive <- inspect defaultLimits bytes >>= expectRight
                result <- cargoPublicationMetadata limits (const (pure ())) archive
                if accepted
                    then do
                        metadata <- expectRight result
                        case publishMetadataValue metadata of
                            Object fields -> KeyMap.lookup "readme" fields `shouldBe` Just (String (decodeUtf8 (BS.replicate size 65)))
                            _ -> expectationFailure "Cargo publication is not an object"
                    else result `shouldBe` Left (ParseError "tar metadata or extended header exceeds its bound")

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
            cargoPublicationAgreement defaultLimits refuseCharge archive source metadata `shouldThrow` (== ChargeRefused)
        it "refuses a source identity disagreement after archive and publication preparation" $ do
            bytes <- fixture [("Cargo.toml", manifest)]
            archive <- inspect defaultLimits bytes >>= expectRight
            metadata <- cargoPublicationMetadata defaultLimits (const (pure ())) archive >>= expectRight
            let source = object ["name" .= ("another" :: Text), "vers" .= ("1.2.3" :: Text), "deps" .= ([] :: [Value])]
            cargoPublicationAgreement defaultLimits (const (pure ())) archive source metadata `shouldReturn` Left (ParseError "Cargo publication and source index identities disagree")

manifest :: ByteString
manifest = "[package]\nname='example'\nversion='1.2.3'\n"

fixture :: [(FilePath, ByteString)] -> IO ByteString
fixture = cargoCrateArchive "example" "1.2.3"

compress :: BSL.ByteString -> ByteString
compress = BSL.toStrict . GZip.compress

inspect :: Limits -> ByteString -> IO (Either ParseError CargoArchiveMetadata)
inspect limits bytes = inspectCargoArchive limits (const (pure ())) "example" "1.2.3" (Just (sha256Hex bytes)) bytes

peakCharge :: ByteString -> IO Int
peakCharge bytes = do
    peak <- newIORef 0
    let charge wanted = modifyIORef' peak (max wanted)
    void (inspectCargoArchive defaultLimits charge "example" "1.2.3" (Just (sha256Hex bytes)) bytes >>= expectRight)
    readIORef peak
