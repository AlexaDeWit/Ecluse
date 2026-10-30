-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

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

import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.Cargo.Manifest
import Ecluse.Core.Security.Limits (Limits (..), defaultLimits)
import Ecluse.Core.Version.Cargo (renderCargo)
import Ecluse.Test.Registry.Cargo.Publish (cargoResolverAgreements, cargoResolverContradictions, cargoResolverManifest, cargoResolverMetadata)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    describe "decodeManifest" $ do
        it "requires exact native name and version spelling" $ do
            decodeManifest defaultLimits "example" "1.2.3" basic `shouldSatisfy` isRight
            decodeManifest defaultLimits "Example" "1.2.3" basic `shouldBe` Left (ParseError "Cargo manifest disagrees with the admitted native identity")
            decodeManifest defaultLimits "example" "1.2.3+other" basic `shouldBe` Left (ParseError "Cargo manifest disagrees with the admitted native identity")
        it "rejects duplicate TOML keys and unresolved workspace metadata" $ do
            parse (basic <> "name = 'another'\n") `shouldBe` Left (ParseError "invalid Cargo TOML manifest")
            parse (basic <> "[workspace]\nmembers=[]\n") `shouldBe` Left (ParseError "Cargo packaged manifest retains workspace inheritance")
            parse "[package]\nname='example'\nversion.workspace=true\n" `shouldBe` Left (ParseError "Cargo manifest field is not a string")
            parse (basic <> "workspace='..'\n") `shouldBe` Left (ParseError "Cargo packaged manifest retains workspace inheritance")
        it "bounds bytes and both inline and dotted nesting before semantic parsing" $ do
            decodeManifest defaultLimits{maxMetadataBytes = 10} "example" "1.2.3" basic `shouldBe` Left (ParseError "Cargo manifest exceeds its byte bound")
            let limits = defaultLimits{maxMetadataBytes = BS.length basic}
            atLimit <- expectRight (decodeManifest limits "example" "1.2.3" basic)
            metadata <- expectRight (publicationMetadata atLimit Map.empty)
            field "name" metadata `shouldBe` Just (String "example")
            decodeManifest limits "example" "1.2.3" (basic <> "\n") `shouldBe` Left (ParseError "Cargo manifest exceeds its byte bound")
            parse (basic <> "[package.metadata]\nnested=" <> BS.replicate 80 91 <> "0" <> BS.replicate 80 93) `shouldBe` Left (ParseError "Cargo TOML nesting exceeds its bound")
            parse (basic <> "[package.metadata]\n" <> encodeUtf8 (T.intercalate "." (replicate 80 "a")) <> "=true\n") `shouldBe` Left (ParseError "Cargo TOML nesting exceeds its bound")
        it "checks parsed table depth after the token-depth check" $ do
            let decode = decodeManifest defaultLimits{maxNestingDepth = 2} "example" "1.2.3"
            void (decode basic) `shouldBe` Right ()
            decode (basic <> "[package.metadata]\nflag=true\n") `shouldBe` Left (ParseError "Cargo TOML nesting exceeds its bound")
        it "distinguishes encoding and missing or scalar package metadata" $ do
            parse (basic <> BS.singleton 255) `shouldBe` Left (ParseError "Cargo manifest is not UTF-8")
            parse "[metadata]\nflag=true\n" `shouldBe` Left (ParseError "Cargo manifest lacks package metadata")
            parse "package='example'\n" `shouldBe` Left (ParseError "Cargo manifest field is not a table")
            parse "[package]\nversion='1.2.3'\n" `shouldBe` Left (ParseError "Cargo manifest lacks name")
            parse "[package]\nname='example'\n" `shouldBe` Left (ParseError "Cargo manifest lacks version")
        it "accepts supported editions and distinguishes unsupported values from wrong types" $ do
            for_ ["2015", "2018", "2021", "2024"] $ \edition ->
                void (parse (basic <> "edition='" <> edition <> "'\n")) `shouldBe` Right ()
            parse (basic <> "edition='2027'\n") `shouldBe` Left (ParseError "unsupported Cargo edition")
            parse (basic <> "edition=2021\n") `shouldBe` Left (ParseError "Cargo manifest field is not a string")
        it "does not count brackets in quoted strings as structural nesting" $
            parse (basic <> "description='" <> BS.replicate 100 91 <> "'\n") `shouldSatisfy` isRight
        it "rejects enabled unstable manifest features" $ do
            void (parse ("cargo-features=[]\n" <> basic)) `shouldBe` Right ()
            parse ("cargo-features=['future-build']\n" <> basic) `shouldBe` Left (ParseError "Cargo unstable manifest features are unsupported")
        it "ignores harmless array-of-tables metadata during publication" $ do
            manifest <- expectRight (parse basic)
            annotated <- expectRight (parse (basic <> "[[package.metadata.notes]]\ntext='retained only in the manifest'\n"))
            expected <- expectRight (publicationMetadata manifest Map.empty)
            publicationMetadata annotated Map.empty `shouldBe` Right expected
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
            manifestExecution manifest Set.empty `shouldBe` Left (ParseError "Cargo build declaration names an absent regular file")
            manifestExecution manifest (Set.singleton "scripts/custom.rs") `shouldBe` Right True
            auto <- expectRight (parse (basic <> "build=true\n"))
            manifestExecution auto Set.empty `shouldBe` Left (ParseError "Cargo build declaration names an absent regular file")
            manifestExecution auto (Set.singleton "build.rs") `shouldBe` Right True
        it "normalises local script references without allowing an archive escape" $ do
            manifest <- expectRight (parse (basic <> "build='./scripts/../build.rs'\n"))
            manifestExecution manifest (Set.singleton "build.rs") `shouldBe` Right True
        it "keeps unsupported build lists and escaping paths unknown" $
            for_ [("build=['a.rs','b.rs']\n", "unsupported Cargo build declaration"), ("build='../build.rs'\n", "tar link escapes its archive")] $ \(declaration, reason) -> do
                manifest <- expectRight (parse (basic <> declaration))
                manifestExecution manifest Set.empty `shouldBe` Left (ParseError reason)
        it "recognises each native procedural macro spelling and crate type form" $
            for_ ["proc-macro=true", "proc_macro=true", "crate-type=['proc-macro']", "crate_type=['proc-macro']"] $ \declaration -> do
                manifest <- expectRight (parse (basic <> "[lib]\n" <> declaration <> "\n"))
                manifestExecution manifest Set.empty `shouldBe` Right True
        it "rejects ambiguous or unsupported native library declarations" $
            for_ [("proc-macro=true\nproc_macro=false", "ambiguous Cargo manifest spelling aliases"), ("proc-macro='false'", "Cargo manifest field is not a boolean"), ("crate-type=['unknown']", "unsupported Cargo library crate type")] $ \(declaration, reason) -> do
                manifest <- expectRight (parse (basic <> "[lib]\n" <> declaration <> "\n"))
                manifestExecution manifest Set.empty `shouldBe` Left (ParseError reason)
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
                manifestTextPaths manifest Set.empty `shouldBe` Left (ParseError "Cargo manifest declares an absent text file")
        it "requires README.md for readme=true and honours readme=false with a README present" $ do
            enabled <- expectRight (parse (basic <> "readme=true\n"))
            manifestTextPaths enabled Set.empty `shouldBe` Left (ParseError "Cargo manifest declares an absent text file")
            manifestTextPaths enabled (Set.singleton "README.md") `shouldBe` Right ["README.md"]
            enabledMetadata <- expectRight (publicationMetadata enabled (Map.singleton "README.md" "selected"))
            field "readme" enabledMetadata `shouldBe` Just (String "selected")
            disabled <- expectRight (parse (basic <> "readme=false\n"))
            manifestTextPaths disabled (Set.singleton "README.md") `shouldBe` Right []
            disabledMetadata <- expectRight (publicationMetadata disabled (Map.singleton "README.md" (BS.singleton 255)))
            field "readme" disabledMetadata `shouldBe` Just Null
        it "requires selected README and licence content to exist and contain UTF-8" $
            for_ ["readme='SELECTED'\n", "license-file='SELECTED'\n"] $ \declaration -> do
                manifest <- expectRight (parse (basic <> declaration))
                manifestTextPaths manifest (Set.singleton "SELECTED") `shouldBe` Right ["SELECTED"]
                publicationMetadata manifest Map.empty `shouldBe` Left (ParseError "Cargo declared text file has no regular content")
                publicationMetadata manifest (Map.singleton "SELECTED" (BS.singleton 255)) `shouldBe` Left (ParseError "Cargo publication text is not UTF-8")
                void (publicationMetadata manifest (Map.singleton "SELECTED" "selected")) `shouldBe` Right ()
        it "preserves nested badge values and refuses scalar badge declarations" $ do
            manifest <- expectRight (parse (basic <> "[badges.maintenance]\nstatus='actively-developed'\n"))
            metadata <- expectRight (publicationMetadata manifest Map.empty)
            field "badges" metadata `shouldBe` Just (object ["maintenance" .= object ["status" .= ("actively-developed" :: Text)]])
            scalar <- expectRight (parse ("badges='active'\n" <> basic))
            publicationMetadata scalar Map.empty `shouldBe` Left (ParseError "Cargo manifest field is not a table")
            for_ [("[badges]\nmaintenance='active'\n", "Cargo manifest field is not a table"), ("[badges.maintenance]\nstatus=true\n", "Cargo manifest field is not a string")] $ \(declaration, reason) -> do
                invalid <- expectRight (parse (basic <> declaration))
                publicationMetadata invalid Map.empty `shouldBe` Left (ParseError reason)
        it "refuses scalar dependency tables and non-array author or feature declarations" $ do
            for_ [("dependencies='1'\n" <> basic, "Cargo manifest field is not a table"), (basic <> "authors='Alice'\n", "Cargo manifest field is not an array of strings"), (basic <> "[features]\nmodern='dep:alias'\n", "Cargo manifest field is not an array of strings")] $ \(bytes, reason) -> do
                manifest <- expectRight (parse bytes)
                publicationMetadata manifest Map.empty `shouldBe` Left (ParseError reason)
            manifest <- expectRight (parse (basic <> "authors=['Alice']\n[dependencies]\nalias='1'\n[features]\nmodern=['alias/extra']\n"))
            metadata <- expectRight (publicationMetadata manifest Map.empty)
            field "authors" metadata `shouldBe` Just (toJSON ["Alice" :: Text])
            field "features" metadata `shouldBe` Just (object ["modern" .= ["alias/extra" :: Text]])
        it "accepts each manifest alias alone and refuses simultaneous spellings" $ do
            preferred <- expectRight (parse (basic <> "rust-version='1.74'\nlicense-file='COPYING'\n"))
            legacy <- expectRight (parse "[project]\nname='example'\nversion='1.2.3'\nrust_version='1.74'\nlicense_file='COPYING'\n")
            metadata <- expectRight (publicationMetadata preferred (Map.singleton "COPYING" "licence"))
            publicationMetadata legacy (Map.singleton "COPYING" "licence") `shouldBe` Right metadata
            parse (basic <> "[project]\nname='example'\nversion='1.2.3'\n") `shouldBe` Left (ParseError "ambiguous Cargo manifest spelling aliases")
            for_ ["rust-version='1.74'\nrust_version='1.74'\n", "license-file='COPYING'\nlicense_file='COPYING'\n", "[dev-dependencies]\na='1'\n[dev_dependencies]\nb='1'\n", "[dependencies.a]\nversion='1'\ndefault-features=true\ndefault_features=true\n"] $ \declaration -> do
                manifest <- expectRight (parse (basic <> declaration))
                publicationMetadata manifest Map.empty `shouldBe` Left (ParseError "ambiguous Cargo manifest spelling aliases")
        it "preserves renamed targeted dependencies and crates.io source identity on relocation" $ do
            manifest <- expectRight (parse renamedManifest)
            metadata <- expectRight (publicationMetadata manifest Map.empty)
            checked <- expectRight (checkPublicationIndex defaultLimits metadata renamedIndex)
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
                checkPublicationIndex defaultLimits metadata (renamedIndexWith indexed) `shouldSatisfy` isRight
        it "translates omitted native defaults into complete upload metadata" $ do
            manifest <- expectRight (parse (basic <> "[dependencies]\ndependency='1'\n"))
            metadata <- expectRight (publicationMetadata manifest Map.empty)
            let source = object ["name" .= ("example" :: Text), "vers" .= ("1.2.3" :: Text), "deps" .= [object ["name" .= ("dependency" :: Text), "req" .= ("^1" :: Text)]]]
            checked <- expectRight (checkPublicationIndex defaultLimits metadata source)
            field "features" checked `shouldBe` Just (object [])
            field "deps" checked `shouldBe` Just (toJSON [object ["name" .= ("dependency" :: Text), "version_req" .= ("^1" :: Text), "features" .= ([] :: [Text]), "optional" .= False, "default_features" .= True, "target" .= Null, "kind" .= ("normal" :: Text), "registry" .= ("https://github.com/rust-lang/crates.io-index" :: Text), "explicit_name_in_toml" .= Null]])
        it "keeps mirror feature metadata unchanged when an optional dependency has an implicit feature" $ do
            manifest <- expectRight (parse (basic <> "[dependencies]\ndependency={version='1',optional=true}\n"))
            metadata <- expectRight (publicationMetadata manifest Map.empty)
            let source = object ["name" .= ("example" :: Text), "vers" .= ("1.2.3" :: Text), "deps" .= [object ["name" .= ("dependency" :: Text), "req" .= ("^1" :: Text), "optional" .= True]], "features" .= object []]
            checked <- expectRight (checkPublicationIndex defaultLimits metadata source)
            field "features" checked `shouldBe` Just (object [])
        it "refuses explicit null for native defaulted publication facts" $ do
            manifest <- expectRight (parse (basic <> "[dependencies]\ndependency='1'\n"))
            metadata <- expectRight (publicationMetadata manifest Map.empty)
            let dep = KeyMap.fromList [("name", String "dependency"), ("req", String "^1")]
                source = KeyMap.fromList [("name", String "example"), ("vers", String "1.2.3"), ("deps", toJSON [Object dep])]
            checkPublicationIndex defaultLimits metadata (Object (KeyMap.insert "features" Null source)) `shouldSatisfy` isLeft
            for_ ["features", "optional", "default_features"] $ \key ->
                checkPublicationIndex defaultLimits metadata (Object (KeyMap.insert "deps" (toJSON [Object (KeyMap.insert key Null dep)]) source)) `shouldSatisfy` isLeft
        it "compares parsed mirror targets while preserving source target spelling" $ do
            manifest <- expectRight (parse (encodeUtf8 (T.replace "cfg(unix)" "cfg(any(unix,windows,))" (decodeUtf8 renamedManifest))))
            metadata <- expectRight (publicationMetadata manifest Map.empty)
            let changeDependency (Object dependency) = Object (KeyMap.insert "target" (String "cfg(any(unix, windows))") dependency)
                changeDependency other = other
                source = case renamedIndex of
                    Object fields -> case KeyMap.lookup "deps" fields of
                        Just (Array values) -> Object (KeyMap.insert "deps" (Array (fmap changeDependency values)) fields)
                        _ -> Null
                    other -> other
            checked <- expectRight (checkPublicationIndex defaultLimits metadata source)
            let targets = case field "deps" checked of
                    Just (Array values) -> [KeyMap.lookup "target" dependency | Object dependency <- toList values]
                    _ -> []
            targets `shouldBe` [Just (String "cfg(any(unix, windows))")]
        it "compares omitted and null source links as the native default" $ do
            linked <- expectRight (parse (basic <> "links='native_foo'\nbuild='build.rs'\n"))
            linkedMetadata <- expectRight (publicationMetadata linked Map.empty)
            plain <- expectRight (parse basic)
            plainMetadata <- expectRight (publicationMetadata plain Map.empty)
            let source = KeyMap.fromList [("name", String "example"), ("vers", String "1.2.3"), ("deps", toJSON ([] :: [Value]))]
                omitted = Object source
                explicitNull = Object (KeyMap.insert "links" Null source)
            for_ [omitted, explicitNull, Object (KeyMap.insert "links" (String "native_bar") source)] $ \row ->
                checkPublicationIndex defaultLimits linkedMetadata row `shouldSatisfy` isLeft
            checkPublicationIndex defaultLimits linkedMetadata (Object (KeyMap.insert "links" (String "native_foo") source)) `shouldSatisfy` isRight
            for_ [omitted, explicitNull] $ \row ->
                checkPublicationIndex defaultLimits plainMetadata row `shouldSatisfy` isRight
        it "rejects a dependency mismatch rather than publish a reduced document" $ do
            manifest <- expectRight (parse renamedManifest)
            metadata <- expectRight (publicationMetadata manifest Map.empty)
            let changed = case renamedIndex of
                    Object fields -> Object (KeyMap.insert "deps" (toJSON ([] :: [Value])) fields)
                    value -> value
            checkPublicationIndex defaultLimits metadata changed `shouldBe` Left (ParseError "Cargo manifest and index dependency counts disagree")
        it "attributes a repeated dependency identity to the side that repeats it" $ do
            let dependencyRow name = object ["name" .= (name :: Text), "req" .= ("^1" :: Text), "target" .= ("cfg(unix)" :: Text)]
                row deps = object ["name" .= ("example" :: Text), "vers" .= ("1.2.3" :: Text), "deps" .= map dependencyRow deps]
            single <- expectRight (parse (basic <> "[target.'cfg(unix)'.dependencies]\nshared='1'\n"))
            singleMetadata <- expectRight (publicationMetadata single Map.empty)
            checkPublicationIndex defaultLimits singleMetadata (row ["shared"]) `shouldSatisfy` isRight
            spaced <- expectRight (parse (basic <> "[target.'cfg(unix)'.dependencies]\nshared='1'\n[target.'cfg( unix )'.dependencies]\nshared='1'\n"))
            spacedMetadata <- expectRight (publicationMetadata spaced Map.empty)
            checkPublicationIndex defaultLimits spacedMetadata (row ["shared", "unrelated"]) `shouldBe` Left (ParseError "Cargo manifest repeats a dependency identity")
            pair <- expectRight (parse (basic <> "[target.'cfg(unix)'.dependencies]\nshared='1'\nother='1'\n"))
            pairMetadata <- expectRight (publicationMetadata pair Map.empty)
            checkPublicationIndex defaultLimits pairMetadata (row ["shared", "other"]) `shouldSatisfy` isRight
            checkPublicationIndex defaultLimits pairMetadata (row ["shared", "shared"]) `shouldBe` Left (ParseError "Cargo index repeats a dependency identity")
        it "refuses unresolved dependency sources after a valid version declaration" $ do
            for_ ["path='local'", "git='https://registry.test/source'", "branch='main'", "rev='abc123'", "tag='v1'", "registry='private'", "workspace=true"] $ \source -> do
                manifest <- expectRight (parse (basic <> "[dependencies.alias]\nversion='1'\n" <> source <> "\n"))
                publicationMetadata manifest Map.empty `shouldBe` Left (ParseError "Cargo dependency retains an unresolved source declaration")
            manifest <- expectRight (parse (basic <> "[dependencies.alias]\nversion='1'\nregistry-index='https://registry.test/index'\n"))
            metadata <- expectRight (publicationMetadata manifest Map.empty)
            field "deps" metadata `shouldBe` Just (toJSON [object ["name" .= ("alias" :: Text), "version_req" .= ("1" :: Text), "features" .= ([] :: [Text]), "optional" .= False, "default_features" .= True, "target" .= Null, "kind" .= ("normal" :: Text), "registry" .= ("https://registry.test/index" :: Text), "explicit_name_in_toml" .= Null]])
        it "refuses an empty dependency requirement after parsing its table" $ do
            manifest <- expectRight (parse (basic <> "[dependencies.alias]\nversion=''\n"))
            publicationMetadata manifest Map.empty `shouldBe` Left (ParseError "Cargo dependency has an empty version requirement")
            control <- expectRight (parse (basic <> "[dependencies.alias]\nversion='*'\n"))
            void (publicationMetadata control Map.empty) `shouldBe` Right ()
        it "classifies malformed source rows after matching the manifest identity" $ do
            manifest <- expectRight (parse renamedManifest)
            metadata <- expectRight (publicationMetadata manifest Map.empty)
            checkPublicationIndex defaultLimits metadata Null `shouldBe` Left (ParseError "Cargo source index entry is not an object")
            case renamedIndex of
                Object fields -> do
                    let source key value = Object (KeyMap.insert key value fields)
                    void (checkPublicationIndex defaultLimits metadata renamedIndex) `shouldBe` Right ()
                    checkPublicationIndex defaultLimits metadata (source "name" (String "another")) `shouldBe` Left (ParseError "Cargo publication and source index identities disagree")
                    checkPublicationIndex defaultLimits metadata (source "links" (String "another")) `shouldBe` Left (ParseError "Cargo manifest and index links declarations disagree")
                    checkPublicationIndex defaultLimits metadata (source "rust_version" (String "1.74")) `shouldBe` Left (ParseError "Cargo manifest and index Rust versions disagree")
                    checkPublicationIndex defaultLimits metadata (source "deps" (toJSON [Null])) `shouldBe` Left (ParseError "Cargo index dependency is not an object")
                    checkPublicationIndex defaultLimits metadata (Object (KeyMap.delete "deps" fields)) `shouldBe` Left (ParseError "Cargo index lacks a required publication field")
                    checkPublicationIndex defaultLimits metadata (source "deps" (String "invalid")) `shouldBe` Left (ParseError "Cargo index publication field is not an array")
                    case KeyMap.lookup "deps" fields of
                        Just (Array dependencies) -> for_ (toList dependencies) $ \case
                            Object dependency -> do
                                let changed key value = source "deps" (toJSON [Object (KeyMap.insert key value dependency)])
                                checkPublicationIndex defaultLimits metadata (source "deps" (toJSON [Object (KeyMap.delete "req" dependency)])) `shouldBe` Left (ParseError "Cargo index lacks a required publication field")
                                checkPublicationIndex defaultLimits metadata (changed "req" (Bool False)) `shouldBe` Left (ParseError "Cargo index publication field is not a string")
                                for_ ["artifact", "bindep_target", "lib", "public"] $ \key ->
                                    checkPublicationIndex defaultLimits metadata (changed key Null) `shouldBe` Left (ParseError "unsupported Cargo index dependency capability")
                                checkPublicationIndex defaultLimits metadata (changed "kind" (String "future")) `shouldBe` Left (ParseError "unsupported Cargo dependency kind")
                                for_ [("name", String "another", "Cargo manifest and index dependency identities disagree"), ("optional", Bool False, "Cargo manifest and index dependency facts disagree"), ("req", String ">1.2", "Cargo manifest and index dependency requirements disagree"), ("features", toJSON ["other" :: Text], "Cargo manifest and index feature declarations disagree"), ("optional", String "false", "Cargo index publication field is not a boolean")] $ \(key, value, reason) ->
                                    checkPublicationIndex defaultLimits metadata (changed key value) `shouldBe` Left (ParseError reason)
                            _ -> expectationFailure "source dependency fixture is not an object"
                        _ -> expectationFailure "source dependency fixture is not an array"
                _ -> expectationFailure "source index fixture is not an object"
        it "merges overlapping source feature maps and preserves their values" $ do
            manifest <- expectRight (parse renamedManifest)
            metadata <- expectRight (publicationMetadata manifest Map.empty)
            case renamedIndex of
                Object fields -> do
                    let source = Object (KeyMap.insert "features" (object ["modern" .= ["dep:alias" :: Text]]) fields)
                    checked <- expectRight (checkPublicationIndex defaultLimits metadata source)
                    field "features" checked `shouldBe` Just (object ["modern" .= ["dep:alias", "dep:alias" :: Text]])
                    case field "deps" checked of
                        Just (Array dependencies) -> [KeyMap.lookup "version_req" dependency | Object dependency <- toList dependencies] `shouldBe` [Just (String "^1.2")]
                        _ -> expectationFailure "published dependencies are not an array"
                    for_ [(Bool False, "Cargo index publication field is not an object"), (object ["modern" .= ("dep:alias" :: Text)], "Cargo index publication field is not an array"), (object ["modern" .= [Bool False]], "Cargo index publication field is not a string")] $ \(value, reason) ->
                        for_ ["features", "features2"] $ \key ->
                            checkPublicationIndex defaultLimits metadata (Object (KeyMap.insert key value fields)) `shouldBe` Left (ParseError reason)
                    checkPublicationIndex defaultLimits metadata (Object (KeyMap.insert "features2" (object []) fields)) `shouldBe` Left (ParseError "Cargo index omits a manifest feature")
                _ -> expectationFailure "source index fixture is not an object"

    describe "checkUploadAgreement" $ do
        it "accepts literal native source, rename, kind, target, feature and Rust-version normalisations" $ do
            manifest <- expectRight (decodeManifest defaultLimits "fixture" "1.0.0" cargoResolverManifest)
            for_ cargoResolverAgreements $ \metadata -> checkUploadAgreement defaultLimits selectedRegistry manifest metadata `shouldBe` Right ()
        it "refuses non-object uploads and dependency entries against a valid manifest" $ do
            manifest <- expectRight (decodeManifest defaultLimits "fixture" "1.0.0" cargoResolverManifest)
            checkUploadAgreement defaultLimits selectedRegistry manifest Null `shouldBe` Left (ParseError "Cargo upload metadata is not an object")
            case cargoResolverMetadata of
                Object fields ->
                    checkUploadAgreement defaultLimits selectedRegistry manifest (Object (KeyMap.insert "deps" (toJSON [Null]) fields)) `shouldBe` Left (ParseError "Cargo upload dependency is not an object")
                _ -> expectationFailure "upload fixture is not an object"
        it "refuses each wire-valid resolver contradiction against the same effective manifest" $ do
            manifest <- expectRight (decodeManifest defaultLimits "fixture" "1.0.0" cargoResolverManifest)
            for_ cargoResolverContradictions $ \metadata ->
                checkUploadAgreement defaultLimits selectedRegistry manifest metadata `shouldSatisfy` isLeft
        it "distinguishes feature, links and Rust-version contradictions after dependency agreement" $ do
            manifest <- expectRight (decodeManifest defaultLimits "fixture" "1.0.0" cargoResolverManifest)
            case cargoResolverMetadata of
                Object fields -> do
                    for_ [("features", object [], "Cargo upload and manifest feature graphs disagree"), ("links", String "another", "Cargo upload and manifest links declarations disagree"), ("rust_version", String "1.75", "Cargo upload and manifest Rust versions disagree")] $ \(key, value, reason) ->
                        checkUploadAgreement defaultLimits selectedRegistry manifest (Object (KeyMap.insert key value fields)) `shouldBe` Left (ParseError reason)
                    for_ [("features", Bool False, "Cargo index publication field is not an object"), ("features", object ["default" .= ("full" :: Text)], "Cargo index publication field is not an array"), ("features", object ["default" .= [Bool False]], "Cargo index publication field is not a string")] $ \(key, value, reason) ->
                        checkUploadAgreement defaultLimits selectedRegistry manifest (Object (KeyMap.insert key value fields)) `shouldBe` Left (ParseError reason)
                _ -> expectationFailure "upload fixture is not an object"
        it "keeps current publication registry identity distinct from transport and foreign sources" $ do
            manifest <- expectRight (decodeManifest defaultLimits "fixture" "1.0.0" cargoResolverManifest)
            for_ ["sparse+https://target.test/registry/", "sparse+https://proxy.test/other/", "sparse+https://foreign.test/index/"] $ \registry ->
                checkUploadAgreement defaultLimits registry manifest cargoResolverMetadata `shouldSatisfy` isLeft
        it "accepts omitted default kind and alias, implicit optional features, and null or absent minimum Rust version" $ do
            manifest <- expectRight (parse (basic <> "[dependencies]\ndependency={version='1.X',optional=true}\n"))
            let dependency = object ["name" .= ("dependency" :: Text), "version_req" .= ("1.*" :: Text), "features" .= ([] :: [Text]), "optional" .= True, "default_features" .= True, "registry" .= cratesIoIndex]
                metadata = object ["deps" .= [dependency], "features" .= object []]
            checkUploadAgreement defaultLimits selectedRegistry manifest metadata `shouldBe` Right ()
            case metadata of
                Object fields -> checkUploadAgreement defaultLimits selectedRegistry manifest (Object (KeyMap.insert "rust_version" Null fields)) `shouldBe` Right ()
                _ -> expectationFailure "literal metadata is not an object"
        it "refuses a repeated dependency identity on either side with the text its client receives" $ do
            let dependency = object ["name" .= ("dependency" :: Text), "version_req" .= ("1" :: Text), "features" .= ([] :: [Text]), "optional" .= False, "default_features" .= True, "registry" .= cratesIoIndex]
                upload deps = object ["deps" .= deps, "features" .= object []]
            manifest <- expectRight (parse (basic <> "[dependencies]\ndependency='1'\n"))
            checkUploadAgreement defaultLimits selectedRegistry manifest (upload [dependency]) `shouldBe` Right ()
            checkUploadAgreement defaultLimits selectedRegistry manifest (upload [dependency, dependency]) `shouldBe` Left (ParseError "Cargo index repeats a dependency identity")
            spaced <- expectRight (parse (basic <> "[target.'cfg(unix)'.dependencies]\ndependency='1'\n[target.'cfg( unix )'.dependencies]\ndependency='1'\n"))
            checkUploadAgreement defaultLimits selectedRegistry spaced (upload [dependency]) `shouldBe` Left (ParseError "Cargo index repeats a dependency identity")
        it "preserves prerelease constraints, comparator precision and quoted target values" $ do
            let requirementPairs :: [(Text, Text)]
                requirementPairs = [("1.2.3-alpha", "^1.2.3"), (">1.2", ">=1.2"), ("=1.2", "=1.2.0"), ("1 .2", "^1.2"), ("1.2.3-a b", "^1.2.3-ab"), ("1.*.2", "1.*")]
            for_ requirementPairs $ \(authored, uploaded) -> do
                manifest <- expectRight (parse (basic <> "[dependencies]\ndependency='" <> encodeUtf8 authored <> "'\n"))
                let dependency = object ["name" .= ("dependency" :: Text), "version_req" .= uploaded, "features" .= ([] :: [Text]), "optional" .= False, "default_features" .= True, "registry" .= cratesIoIndex]
                checkUploadAgreement defaultLimits selectedRegistry manifest (object ["deps" .= [dependency], "features" .= object []]) `shouldSatisfy` isLeft
            for_ ["cfg(all(unix,target_os = \"lin ux\"))", "cfg(all(unix,target_os = \"windows\"))", "cfg(all(un ix,target_os = \"linux\"))"] $ \target -> do
                let bytes = encodeUtf8 (T.replace "cfg(all(unix,target_os = \"linux\",))" target (decodeUtf8 cargoResolverManifest))
                manifest <- expectRight (decodeManifest defaultLimits "fixture" "1.0.0" bytes)
                checkUploadAgreement defaultLimits selectedRegistry manifest cargoResolverMetadata `shouldSatisfy` isLeft
        it "accepts wildcard and explicit comparator normalisations without changing their meaning" $ do
            let requirementPairs :: [(Text, Text)]
                requirementPairs = [("*", "X"), (">=1.X", ">=1"), ("=1.2", "= 1.2")]
            for_ requirementPairs $ \(authored, uploaded) -> do
                manifest <- expectRight (parse (basic <> "[dependencies]\ndependency='" <> encodeUtf8 authored <> "'\n"))
                let dependency = object ["name" .= ("dependency" :: Text), "version_req" .= uploaded, "features" .= ([] :: [Text]), "optional" .= False, "default_features" .= True, "registry" .= cratesIoIndex]
                checkUploadAgreement defaultLimits selectedRegistry manifest (object ["deps" .= [dependency], "features" .= object []]) `shouldBe` Right ()

    describe "parseRustRelease" $ do
        it "fills omitted numeric components with zeroes" $
            for_ [("1", "1.0.0"), ("1.74", "1.74.0"), ("1.74.1", "1.74.1")] $ \(raw, expected) ->
                renderCargo <$> parseRustRelease raw `shouldBe` Right expected
        it "refuses prerelease, build, overflow and excess components" $
            for_ ["1.74.0-alpha", "1.74.0+build", "18446744073709551616", "1.2.3.4"] $ \raw ->
                parseRustRelease raw `shouldBe` Left (ParseError "Cargo Rust version is invalid")

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
