-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Native Cargo manifest interpretation and publication translation contracts.
module Ecluse.Core.Registry.Cargo.ManifestSpec (spec) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Registry (ParseError)
import Ecluse.Core.Registry.Cargo.Manifest
import Ecluse.Core.Security.Limits (Limits (..), defaultLimits)
import Ecluse.Test.Registry.Cargo.Publish (cargoResolverContradictions, cargoResolverManifest, cargoResolverMetadata)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    describe "decodeManifest" $ do
        it "requires exact native name and version spelling" $ do
            decodeManifest defaultLimits "example" "1.2.3" basic `shouldSatisfy` isRight
            decodeManifest defaultLimits "Example" "1.2.3" basic `shouldSatisfy` isLeft
            decodeManifest defaultLimits "example" "1.2.3+other" basic `shouldSatisfy` isLeft
        it "rejects duplicate TOML keys and unresolved workspace metadata" $ do
            parse (basic <> "name = 'another'\n") `shouldSatisfy` isLeft
            parse (basic <> "[workspace]\nmembers=[]\n") `shouldSatisfy` isLeft
            parse "[package]\nname='example'\nversion.workspace=true\n" `shouldSatisfy` isLeft
        it "bounds bytes and both inline and dotted nesting before semantic parsing" $ do
            decodeManifest defaultLimits{maxMetadataBytes = 10} "example" "1.2.3" basic `shouldSatisfy` isLeft
            parse (basic <> "[package.metadata]\nnested=" <> BS.replicate 80 91 <> "0" <> BS.replicate 80 93) `shouldSatisfy` isLeft
            parse (basic <> "[package.metadata]\n" <> encodeUtf8 (T.intercalate "." (replicate 80 "a")) <> "=true\n") `shouldSatisfy` isLeft
        it "does not count brackets in quoted strings as structural nesting" $
            parse (basic <> "description='" <> BS.replicate 100 91 <> "'\n") `shouldSatisfy` isRight
        it "rejects enabled unstable manifest features" $
            parse ("cargo-features=['future-build']\n" <> basic) `shouldSatisfy` isLeft
    describe "manifestExecution" $ do
        it "uses the complete inventory for default build.rs detection" $ do
            manifest <- expectRight (parse basic)
            manifestExecution manifest Set.empty `shouldBe` Right False
            manifestExecution manifest (Set.singleton "build.rs") `shouldBe` Right True
        it "honours explicit disabling even with a root build script" $ do
            manifest <- expectRight (parse (basic <> "build=false\n"))
            manifestExecution manifest (Set.singleton "build.rs") `shouldBe` Right False
        it "requires every declared build script to name a regular archive member" $ do
            manifest <- expectRight (parse (basic <> "build='scripts/custom.rs'\n"))
            manifestExecution manifest Set.empty `shouldSatisfy` isLeft
            manifestExecution manifest (Set.singleton "scripts/custom.rs") `shouldBe` Right True
            auto <- expectRight (parse (basic <> "build=true\n"))
            manifestExecution auto Set.empty `shouldSatisfy` isLeft
            manifestExecution auto (Set.singleton "build.rs") `shouldBe` Right True
        it "normalises local script references without allowing an archive escape" $ do
            manifest <- expectRight (parse (basic <> "build='./scripts/../build.rs'\n"))
            manifestExecution manifest (Set.singleton "build.rs") `shouldBe` Right True
        it "keeps unsupported build lists and escaping paths unknown" $
            for_ ["build=['a.rs','b.rs']\n", "build='../build.rs'\n"] $ \declaration -> do
                manifest <- expectRight (parse (basic <> declaration))
                manifestExecution manifest Set.empty `shouldSatisfy` isLeft
        it "recognises each native procedural macro spelling and crate type form" $
            for_ ["proc-macro=true", "proc_macro=true", "crate-type=['proc-macro']", "crate_type=['proc-macro']"] $ \declaration -> do
                manifest <- expectRight (parse (basic <> "[lib]\n" <> declaration <> "\n"))
                manifestExecution manifest Set.empty `shouldBe` Right True
        it "rejects ambiguous or unsupported native library declarations" $
            for_ ["proc-macro=true\nproc_macro=false", "proc-macro='false'", "crate-type=['unknown']"] $ \declaration -> do
                manifest <- expectRight (parse (basic <> "[lib]\n" <> declaration <> "\n"))
                manifestExecution manifest Set.empty `shouldSatisfy` isLeft
    describe "publicationMetadata" $ do
        it "uses Cargo README defaults and preserves publication-only data" $ do
            manifest <- expectRight (parse (basic <> "authors=['Alice']\nlicense='MIT'\nrust-version='1.74'\n"))
            paths <- expectRight (manifestTextPaths manifest (Set.fromList ["README", "README.txt", "README.md"]))
            paths `shouldBe` ["README.md"]
            metadata <- expectRight (publicationMetadata manifest (Map.singleton "README.md" "native readme"))
            field "authors" metadata `shouldBe` Just (toJSON ["Alice" :: Text])
            field "readme" metadata `shouldBe` Just (String "native readme")
            field "readme_file" metadata `shouldBe` Just (String "README.md")
            field "rust_version" metadata `shouldBe` Just (String "1.74")
        it "preserves a declared README filename while resolving its local reference" $ do
            manifest <- expectRight (parse (basic <> "readme='./docs/../README.md'\n"))
            manifestTextPaths manifest (Set.singleton "README.md") `shouldBe` Right ["README.md"]
            metadata <- expectRight (publicationMetadata manifest (Map.singleton "README.md" "readme"))
            field "readme_file" metadata `shouldBe` Just (String "./docs/../README.md")
        it "fails when a declared README or licence file is absent" $
            for_ ["readme='GUIDE.md'", "license-file='COPYING'"] $ \declaration -> do
                manifest <- expectRight (parse (basic <> declaration <> "\n"))
                manifestTextPaths manifest Set.empty `shouldSatisfy` isLeft
        it "preserves renamed targeted dependencies and crates.io source identity on relocation" $ do
            manifest <- expectRight (parse renamedManifest)
            metadata <- expectRight (publicationMetadata manifest Map.empty)
            checked <- expectRight (checkPublicationIndex metadata renamedIndex)
            field "deps" checked
                `shouldBe` Just
                    ( toJSON
                        [ object
                            [ "name" .= ("original" :: Text)
                            , "version_req" .= ("^1.2" :: Text)
                            , "features" .= ["extra" :: Text]
                            , "optional" .= True
                            , "default_features" .= False
                            , "target" .= ("cfg(unix)" :: Text)
                            , "kind" .= ("build" :: Text)
                            , "registry" .= ("https://github.com/rust-lang/crates.io-index" :: Text)
                            , "explicit_name_in_toml" .= ("alias" :: Text)
                            ]
                        ]
                    )
            field "features" checked `shouldBe` Just (object ["modern" .= ["dep:alias" :: Text]])
        it "compares native comparator and wildcard spellings while preserving index requirements" $
            for_ [("1.2, 1", "^1.2, ^1"), ("1.x", "1.*"), ("1.X", "1.*")] $ \(authored, indexed) -> do
                let bytes = encodeUtf8 (T.replace "version='1.2'" ("version='" <> authored <> "'") (decodeUtf8 renamedManifest))
                manifest <- expectRight (parse bytes)
                metadata <- expectRight (publicationMetadata manifest Map.empty)
                checkPublicationIndex metadata (renamedIndexWith indexed) `shouldSatisfy` isRight
        it "translates omitted native defaults into complete upload metadata" $ do
            manifest <- expectRight (parse (basic <> "[dependencies]\ndependency='1'\n"))
            metadata <- expectRight (publicationMetadata manifest Map.empty)
            let source = object ["name" .= ("example" :: Text), "vers" .= ("1.2.3" :: Text), "deps" .= [object ["name" .= ("dependency" :: Text), "req" .= ("^1" :: Text)]]]
            checked <- expectRight (checkPublicationIndex metadata source)
            field "features" checked `shouldBe` Just (object [])
            field "deps" checked `shouldBe` Just (toJSON [object ["name" .= ("dependency" :: Text), "version_req" .= ("^1" :: Text), "features" .= ([] :: [Text]), "optional" .= False, "default_features" .= True, "target" .= Null, "kind" .= ("normal" :: Text), "registry" .= ("https://github.com/rust-lang/crates.io-index" :: Text), "explicit_name_in_toml" .= Null]])
        it "keeps mirror feature metadata unchanged when an optional dependency has an implicit feature" $ do
            manifest <- expectRight (parse (basic <> "[dependencies]\ndependency={version='1',optional=true}\n"))
            metadata <- expectRight (publicationMetadata manifest Map.empty)
            let source = object ["name" .= ("example" :: Text), "vers" .= ("1.2.3" :: Text), "deps" .= [object ["name" .= ("dependency" :: Text), "req" .= ("^1" :: Text), "optional" .= True]], "features" .= object []]
            checked <- expectRight (checkPublicationIndex metadata source)
            field "features" checked `shouldBe` Just (object [])
        it "refuses explicit null for native defaulted publication facts" $ do
            manifest <- expectRight (parse (basic <> "[dependencies]\ndependency='1'\n"))
            metadata <- expectRight (publicationMetadata manifest Map.empty)
            let dep = KeyMap.fromList [("name", String "dependency"), ("req", String "^1")]
                source = KeyMap.fromList [("name", String "example"), ("vers", String "1.2.3"), ("deps", toJSON [Object dep])]
            checkPublicationIndex metadata (Object (KeyMap.insert "features" Null source)) `shouldSatisfy` isLeft
            for_ ["features", "optional", "default_features"] $ \key ->
                checkPublicationIndex metadata (Object (KeyMap.insert "deps" (toJSON [Object (KeyMap.insert key Null dep)]) source)) `shouldSatisfy` isLeft
        it "rejects a dependency mismatch rather than publish a reduced document" $ do
            manifest <- expectRight (parse renamedManifest)
            metadata <- expectRight (publicationMetadata manifest Map.empty)
            let changed = case renamedIndex of
                    Object fields -> Object (KeyMap.insert "deps" (toJSON ([] :: [Value])) fields)
                    value -> value
            checkPublicationIndex metadata changed `shouldSatisfy` isLeft

    describe "checkUploadAgreement" $ do
        it "accepts literal native source, rename, kind, target, feature and Rust-version normalisations" $ do
            manifest <- expectRight (decodeManifest defaultLimits "fixture" "1.0.0" cargoResolverManifest)
            checkUploadAgreement selectedRegistry manifest cargoResolverMetadata `shouldBe` Right ()
        it "refuses each wire-valid resolver contradiction against the same effective manifest" $ do
            manifest <- expectRight (decodeManifest defaultLimits "fixture" "1.0.0" cargoResolverManifest)
            for_ cargoResolverContradictions $ \metadata ->
                checkUploadAgreement selectedRegistry manifest metadata `shouldSatisfy` isLeft
        it "keeps current publication registry identity distinct from transport and foreign sources" $ do
            manifest <- expectRight (decodeManifest defaultLimits "fixture" "1.0.0" cargoResolverManifest)
            for_ ["sparse+https://target.test/registry/", "sparse+https://proxy.test/other/", "sparse+https://foreign.test/index/"] $ \registry ->
                checkUploadAgreement registry manifest cargoResolverMetadata `shouldSatisfy` isLeft
        it "accepts omitted default kind and alias, implicit optional features, and null or absent minimum Rust version" $ do
            manifest <- expectRight (parse (basic <> "[dependencies]\ndependency={version='1.X',optional=true}\n"))
            let dependency = object ["name" .= ("dependency" :: Text), "version_req" .= ("1.*" :: Text), "features" .= ([] :: [Text]), "optional" .= True, "default_features" .= True, "registry" .= cratesIoIndex]
                metadata = object ["deps" .= [dependency], "features" .= object []]
            checkUploadAgreement selectedRegistry manifest metadata `shouldBe` Right ()
            case metadata of
                Object fields -> checkUploadAgreement selectedRegistry manifest (Object (KeyMap.insert "rust_version" Null fields)) `shouldBe` Right ()
                _ -> expectationFailure "literal metadata is not an object"
        it "preserves prerelease constraints, comparator precision and quoted target values" $ do
            for_ [("1.2.3-alpha", "^1.2.3"), (">1.2", ">=1.2"), ("=1.2", "=1.2.0"), ("1 .2", "^1.2"), ("1.2.3-a b", "^1.2.3-ab"), ("1.*.2", "1.*")] $ \(authored, uploaded) -> do
                manifest <- expectRight (parse (basic <> "[dependencies]\ndependency='" <> encodeUtf8 authored <> "'\n"))
                let dependency = object ["name" .= ("dependency" :: Text), "version_req" .= uploaded, "features" .= ([] :: [Text]), "optional" .= False, "default_features" .= True, "registry" .= cratesIoIndex]
                checkUploadAgreement selectedRegistry manifest (object ["deps" .= [dependency], "features" .= object []]) `shouldSatisfy` isLeft
            for_ ["cfg(all(unix,target_os = \"lin ux\"))", "cfg(all(unix,target_os = \"windows\"))", "cfg(all(un ix,target_os = \"linux\"))"] $ \target -> do
                let bytes = encodeUtf8 (T.replace "cfg(all(unix,target_os = \"linux\"))" target (decodeUtf8 cargoResolverManifest))
                manifest <- expectRight (decodeManifest defaultLimits "fixture" "1.0.0" bytes)
                checkUploadAgreement selectedRegistry manifest cargoResolverMetadata `shouldSatisfy` isLeft

selectedRegistry :: Text
selectedRegistry = "sparse+https://proxy.test/cargo/private/"

basic :: ByteString
basic = "[package]\nname='example'\nversion='1.2.3'\n"

parse :: ByteString -> Either ParseError CargoManifest
parse = decodeManifest defaultLimits "example" "1.2.3"

field :: Key.Key -> CargoPublishMetadata -> Maybe Value
field key metadata = case publishMetadataValue metadata of
    Object fields -> KeyMap.lookup key fields
    _ -> Nothing

renamedManifest :: ByteString
renamedManifest = basic <> "[features]\nmodern=['dep:alias']\n[target.'cfg(unix)'.build-dependencies.alias]\npackage='original'\nversion='1.2'\nfeatures=['extra']\noptional=true\ndefault-features=false\n"

renamedIndex :: Value
renamedIndex = renamedIndexWith "^1.2"

renamedIndexWith :: Text -> Value
renamedIndexWith requirement =
    object
        [ "name" .= ("example" :: Text)
        , "vers" .= ("1.2.3" :: Text)
        , "features" .= object []
        , "features2" .= object ["modern" .= ["dep:alias" :: Text]]
        , "deps"
            .= [ object
                    [ "name" .= ("alias" :: Text)
                    , "package" .= ("original" :: Text)
                    , "req" .= requirement
                    , "features" .= ["extra" :: Text]
                    , "optional" .= True
                    , "default_features" .= False
                    , "target" .= ("cfg(unix)" :: Text)
                    , "kind" .= ("build" :: Text)
                    , "registry" .= Null
                    ]
               ]
        ]
