-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native documents for each adapter running the shared metadata transport contract.
module Ecluse.Core.Registry.Metadata.FetchSpec (spec) where

import Data.Aeson (Value (..), encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Universe.Class (Universe (universe))
import Ecluse.Core.Ecosystem (Ecosystem (Cargo, RubyGems))
import Ecluse.Core.Package (PackageInfo (infoInvalidEntries, infoVersions), mkPackageName)
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Package.InvalidEntry (InvalidEntryKind (InvalidVersionManifest), invalidKey, invalidKind)
import Ecluse.Core.Registry.Adapter (adapterFor)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataRead))
import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterEcosystem, adapterMetadata))
import Ecluse.Core.Registry.Cargo.Request (cargoArtifactHosts)
import Ecluse.Core.Registry.Metadata (Manifest (manifestInfo), MetadataError (MetadataUndecodable))
import Ecluse.Core.Registry.Metadata.Fetch.Preparation (PrepareRead (..), prepareRead)
import Ecluse.Core.Registry.Npm.Adapter (npmAdapter)
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter)
import Ecluse.Core.Registry.RubyGems.Metadata (gemReadWith)
import Ecluse.Core.Security (defaultLimits, ecosystemArtifactAuthorities)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Metadata.Fetch (ReadFixture (..), metadataFetchContract, metadataStatusLaw)
import Ecluse.Test.Contract.Metadata.Projection (MetadataFraming (..))
import Ecluse.Test.Registry.Cargo.Fixtures qualified as Cargo
import Ecluse.Test.Registry.Cargo.Metadata (cargoRead, parseCargoDiscovery)
import Ecluse.Test.Registry.Metadata.Fetch (heldManifest)
import Ecluse.Test.Registry.Npm (packumentValue, versionSpec, versionValue)
import Ecluse.Test.Registry.Npm.Metadata (npmRead)
import Ecluse.Test.Registry.PyPI (filesNamed, simpleIndex)
import Ecluse.Test.Registry.PyPI.Metadata (pypiRead)
import Ecluse.Test.Support (expectRight)
import Test.Hspec
import Prelude hiding (universe)

spec :: Spec
spec = do
    it "holds documents for every ecosystem with an adapter" $
        map rdEcosystem registered `shouldMatchList` filter (isJust . adapterFor) universe
    for_ registered $ \eco -> describe (show (rdEcosystem eco)) $ do
        metadataFetchContract eco
        when (rdEcosystem eco == RubyGems) $
            it (caseLabel "core.metadata.fetch.rubygems.record-framing" "requires caller-bound framing for the native fetch provider") $ do
                rdFraming eco `shouldBe` CallerBoundRecords
                quarantinesMalformedRecord eco ("---\n" <> rubyRow "1.0.0" 0 <> "\n{broken\n" <> rubyRow "2.0.0" 0) (RecordEntry 2)
        when (rdEcosystem eco == Cargo) $
            it (caseLabel "core.metadata.fetch.cargo.record-framing" "requires record quarantine for the native fetch provider") $ do
                rdFraming eco `shouldBe` RecordStream
                quarantinesMalformedRecord eco (rdReleases eco fetchBase ["1.0.0"] <> "\n{broken\n" <> rdReleases eco fetchBase ["2.0.0"]) (RecordEntry 1)
        it "rejects a broken registered factory despite a valid held-body reader" $ do
            metadataStatusLaw eco (200, Right ())
            calls <- newIORef (0 :: Int)
            let refused = PrepareRead $ \_ -> do
                    modifyIORef' calls (+ 1)
                    pure (Left MetadataUndecodable)
            metadataStatusLaw eco{rdPrepareRead = refused} (200, Right ()) `shouldThrow` anyException
            readIORef calls `shouldReturn` 2

registered :: [ReadFixture]
registered =
    [ ReadFixture
        { rdEcosystem = adapterEcosystem npmAdapter
        , rdPrepareRead = metadataRead (adapterMetadata npmAdapter)
        , rdRead = const (Right npmRead)
        , rdFraming = SingleDocument
        , rdCallerBoundPath = Nothing
        , rdDiscovery = Nothing
        , rdDocument = \name padding -> "{\"name\":\"" <> name <> "\",\"versions\":{}" <> paddingMember padding
        , rdReleases = \base versions ->
            toStrict . encode $
                packumentValue
                    "thing"
                    "1.2.3"
                    [(version, versionValue (versionSpec "thing" version (base <> "/thing/-/thing-" <> version <> ".tgz"))) | version <- versions]
                    [Key.fromText version .= ("2020-01-01T00:00:00.000Z" :: Text) | version <- versions]
                    []
        }
    , ReadFixture
        { rdEcosystem = adapterEcosystem pypiAdapter
        , rdPrepareRead = metadataRead (adapterMetadata pypiAdapter)
        , rdRead = const (Right pypiRead)
        , rdFraming = SingleDocument
        , rdCallerBoundPath = Nothing
        , rdDiscovery = Nothing
        , rdDocument = \name padding -> "{\"meta\":{\"api-version\":\"1.0\"},\"name\":\"" <> name <> "\",\"files\":[]" <> paddingMember padding
        , rdReleases = \_ versions -> toStrict (encode (simpleIndex "thing" (filesNamed ["thing-" <> version <> ".tar.gz" | version <- versions])))
        }
    , ReadFixture
        { rdEcosystem = Cargo
        , rdPrepareRead = PrepareRead $ \origin -> do
            adapter <- requiredAdapter Cargo
            prepareRead (metadataRead (adapterMetadata adapter)) origin
        , rdRead = \base -> cargoRead <$> parseCargoDiscovery (ecosystemArtifactAuthorities cargoArtifactHosts) base (discovery base)
        , rdFraming = RecordStream
        , rdCallerBoundPath = Nothing
        , rdDiscovery = Just discovery
        , rdDocument = \name padding -> cargoRow (decodeUtf8 name) "0.0.0" padding
        , rdReleases = \_ versions -> BS.intercalate "\n" [cargoRow "thing" version 0 | version <- versions]
        }
    , ReadFixture
        { rdEcosystem = RubyGems
        , rdPrepareRead = PrepareRead $ \origin -> do
            adapter <- requiredAdapter RubyGems
            prepareRead (metadataRead (adapterMetadata adapter)) origin
        , rdRead = Right . gemReadWith (const pass) (const pass)
        , rdFraming = CallerBoundRecords
        , rdCallerBoundPath = Just "/info/thing"
        , rdDiscovery = Nothing
        , rdDocument = \_ padding -> rubyRows ["0.0.0"] padding
        , rdReleases = \_ versions -> rubyRows versions 0
        }
    ]
  where
    rubyRows :: [Text] -> Int -> ByteString
    rubyRows versions padding = "---\n" <> BS.intercalate "\n" [rubyRow version padding | version <- versions]
    discovery base = toStrict (encode (object ["dl" .= base]))
    cargoRow name version padding = toStrict . encode $ case Cargo.cargoIndexRecord (Cargo.minimalCargoRelease name version) of
        Object fields -> Object (KeyMap.insert "_padding" (String (decodeUtf8 (BS.replicate padding 0x78))) fields)
        other -> other
    paddingMember padding = ",\"_padding\":\"" <> BS.replicate padding 0x78 <> "\"}"

rubyRow :: Text -> Int -> ByteString
rubyRow version padding = encodeUtf8 version <> " |checksum:" <> BS.replicate 64 0x30 <> ",created_at:2020-01-01T00:00:00Z,padding:" <> BS.replicate padding 0x78

fetchBase :: Text
fetchBase = "https://registry.example.test"

-- The production read keeps both healthy releases and leaves evidence at the malformed record's position.
quarantinesMalformedRecord :: ReadFixture -> ByteString -> EntryKey -> Expectation
quarantinesMalformedRecord eco bytes malformed = do
    nativeRead <- expectRight (rdRead eco fetchBase)
    manifest <- expectRight =<< heldManifest nativeRead defaultLimits fetchBase (mkPackageName (rdEcosystem eco) Nothing "thing") [bytes]
    Map.keys (infoVersions (manifestInfo manifest)) `shouldBe` ["1.0.0", "2.0.0"]
    [(invalidKind invalid, invalidKey invalid) | invalid <- infoInvalidEntries (manifestInfo manifest)] `shouldContain` [(InvalidVersionManifest, show malformed)]
