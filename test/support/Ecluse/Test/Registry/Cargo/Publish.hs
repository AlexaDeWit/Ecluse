-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | An owned archive and paired index snapshot shared by native publication and recovery tests.
module Ecluse.Test.Registry.Cargo.Publish (
    cargoPublicationFixture,
    cargoResolverArchive,
    cargoResolverManifest,
    cargoResolverMetadata,
    cargoResolverAgreements,
    cargoResolverContradictions,
    cargoResolverDependencyField,
    cargoSourceVersion,
    observeCargoRelay,
) where

import Data.Aeson (Key, Value (..), object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types.Status (status200)
import Test.Hspec (shouldBe)

import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.Adapter.Capability (AdapterPublish (publishRelay), PublishExtras (NoPublishExtras), PublishRequestBody (PublishRequestBody))
import Ecluse.Core.Registry.CachedDocument (cargoCached)
import Ecluse.Core.Registry.Cargo.Document (cargoDocument)
import Ecluse.Core.Registry.Cargo.Project (projectName)
import Ecluse.Core.Registry.Cargo.Projection (projectCargoRecord)
import Ecluse.Core.Registry.Cargo.Publish (cargoPublish)
import Ecluse.Core.Registry.Metadata (VersionDoc (..))
import Ecluse.Core.Registry.Origin (originClient)
import Ecluse.Core.Registry.ServedDocument (stringField)
import Ecluse.Core.Security (defaultLimits, maxNestingDepth)
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (ChargeFactors))
import Ecluse.Test.Deadline (withExchangeClient)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Registry.Cargo.Fixtures (CargoRelease (..), cargoCrateArchive, minimalCargoRelease)
import Ecluse.Test.Registry.Cargo.Metadata (cargoTestDiscovery, cargoTestName)
import Ecluse.Test.Registry.Cargo.Reference (UploadFrame, referenceUpload)
import Ecluse.Test.Stub (Captured (capBody, capMethod), allCaptured, stubBaseUrl, withRoutedStub)
import Ecluse.Test.Support (expectRight)

-- | Observe production relay bytes at the destination before decoding their native metadata independently.
observeCargoRelay :: (Text -> RegistryUrl) -> ByteString -> IO UploadFrame
observeCargoRelay registryAt bytes =
    withRoutedStub (const (status200, [], "{}")) $ \target ->
        withExchangeClient (newManager defaultManagerSettings) $ \client -> do
            let origin = originClient defaultLimits client (registryAt (stubBaseUrl target)) Nothing
            void (publishRelay (cargoPublish (ChargeFactors 1000 1000 1000)) origin cargoTestName (PublishRequestBody bytes Nothing NoPublishExtras) >>= expectRight)
            captured <-
                allCaptured target >>= \case
                    [written] -> pure written
                    _ -> fail "Cargo relay did not issue exactly one request"
            capMethod captured `shouldBe` "PUT"
            capBody captured `shouldBe` bytes
            expectRight (referenceUpload (capBody captured))

-- | The README precedes Cargo.toml so publication must perform its bounded selected-text pass.
cargoPublicationFixture :: IO CargoRelease
cargoPublicationFixture = do
    let manifest = "[package]\nname='fixture'\nversion='1.0.0'\nauthors=['Ecluse contributors']\ndescription='Owned Cargo registry contract fixture'\nlicense='MIT'\nrust-version='1.74'\n"
    bytes <- cargoCrateArchive "fixture" "1.0.0" [("README.md", "owned readme"), ("Cargo.toml", manifest), ("src/lib.rs", "pub fn owned() {}\n")]
    pure (minimalCargoRelease "fixture" "1.0.0"){releaseArchive = bytes, releaseChecksum = hexSha256Of bytes, releaseReadme = "owned readme"}

-- | Literal resolver declarations pair native normalisation controls with an independently authored upload.
cargoResolverManifest :: ByteString
cargoResolverManifest =
    "[package]\nname='fixture'\nversion='1.0.0'\nrust-version='1.74'\nlinks='native_foo'\nbuild='build.rs'\n"
        <> "[dependencies]\nlocal={version='1.2',registry-index='sparse+https://proxy.test/cargo/private/',features=['beta','alpha'],default-features=false}\n"
        <> "foreign={package='original',version='1.X',registry-index='sparse+https://foreign.test/index/',optional=true}\nauto={version='0.3',optional=true}\n"
        <> "[dev-dependencies]\nhelper='1.0.0+build'\n"
        <> "[target.'cfg(all(unix,target_os = \"linux\",))'.build-dependencies]\ntool={version='>=1.2.3-alpha, <2',features=['derive']}\n"
        <> "[features]\ndefault=['full','auto']\nfull=['dep:foreign','foreign?/extra','local/alpha']\n"

-- | The upload uses current-registry null, canonical requirements, and an explicit implicit optional feature.
cargoResolverMetadata :: Value
cargoResolverMetadata =
    object
        [ "name" .= ("fixture" :: Text)
        , "vers" .= ("1.0.0" :: Text)
        , "deps"
            .= [ dep "tool" ">=1.2.3-alpha, <2" ["derive"] False True (Just "cfg(all(unix, target_os=\"linux\"))") "build" (Just cratesIo) Nothing
               , dep "original" "1.*" [] True True Nothing "normal" (Just "sparse+https://foreign.test/index/") (Just "foreign")
               , dep "helper" "^1.0.0" [] False True Nothing "dev" (Just cratesIo) (Just "helper")
               , dep "local" "^1.2" ["alpha", "beta"] False False Nothing "normal" Nothing Nothing
               , dep "auto" "^0.3" [] True True Nothing "normal" (Just "sparse+https://index.crates.io/") Nothing
               ]
        , "features" .= object ["default" .= ["auto", "full" :: Text], "full" .= ["local/alpha", "foreign?/extra", "dep:foreign" :: Text], "auto" .= ["dep:auto" :: Text]]
        , "rust_version" .= ("1.74.0" :: Text)
        , "links" .= ("native_foo" :: Text)
        , "authors" .= ["Unrestricted author text" :: Text]
        , "keywords" .= ([] :: [Text])
        , "categories" .= ([] :: [Text])
        , "description" .= ("Descriptive fields need not equal the manifest" :: Text)
        , "future-extension" .= object ["retained" .= True]
        ]
  where
    cratesIo = "https://github.com/rust-lang/crates.io-index"
    dep :: Text -> Text -> [Text] -> Bool -> Bool -> Maybe Text -> Text -> Maybe Text -> Maybe Text -> Value
    dep name requirement features isOptional defaults target kind registry alias =
        object ["name" .= name, "version_req" .= requirement, "features" .= features, "optional" .= isOptional, "default_features" .= defaults, "target" .= target, "kind" .= kind, "registry" .= registry, "explicit_name_in_toml" .= alias]

-- | The effective manifest remains independent of the literal upload claims.
cargoResolverArchive :: IO ByteString
cargoResolverArchive =
    cargoCrateArchive "fixture" "1.0.0" [("README.md", "owned readme"), ("Cargo.toml", cargoResolverManifest), ("build.rs", "fn main() {}\n"), ("src/lib.rs", "pub fn owned() {}\n")]

-- | Native registry interning can retain a different equivalent spelling from the archived declaration.
cargoResolverAgreements :: [Value]
cargoResolverAgreements =
    cargoResolverMetadata : [cargoResolverDependencyField "registry" (String source) | source <- ["https://github.com/rust-lang/crates.io-index/", "https://GITHUB.com/RUST-LANG/CRATES.IO-INDEX.git/"]]

-- | The literal upload with one field of its first dependency, the crates.io build dependency, replaced.
cargoResolverDependencyField :: Key -> Value -> Value
cargoResolverDependencyField key value = case cargoResolverMetadata of
    Object fields
        | Just (Array dependencies) <- KeyMap.lookup "deps" fields
        , Object firstDependency : rest <- toList dependencies ->
            Object (KeyMap.insert "deps" (toJSON (Object (KeyMap.insert key value firstDependency) : rest)) fields)
    _ -> Null

-- | Each wire-valid mutation changes one resolver fact against the same literal agreeing control.
cargoResolverContradictions :: [Value]
cargoResolverContradictions = case cargoResolverMetadata of
    Object fields ->
        let dependencies = case KeyMap.lookup "deps" fields of
                Just (Array values) -> toList values
                _ -> []
            changed key value = Object (KeyMap.insert key value fields)
            features = case KeyMap.lookup "features" fields of
                Just (Object values) -> values
                _ -> mempty
            extra = [Object (KeyMap.insert "name" (String "extra") fields') | Object fields' <- take 1 dependencies]
         in [changed "deps" (toJSON (drop 1 dependencies)), changed "deps" (toJSON (dependencies <> take 1 dependencies)), changed "deps" (toJSON (dependencies <> extra))]
                <> [cargoResolverDependencyField key value | (key, value) <- [("name", String "another"), ("explicit_name_in_toml", String "another"), ("target", String "cfg(windows)"), ("target", String "cfg(any(unix,,windows))"), ("target", String ("cfg(" <> T.replicate (maxNestingDepth defaultLimits) "not(" <> "unix" <> T.replicate (maxNestingDepth defaultLimits) ")" <> ")")), ("kind", String "dev"), ("registry", Null), ("registry", String "git+https://github.com/rust-lang/crates.io-index"), ("registry", String "not a registry URL"), ("registry", String "sparse+https://index.crates.io/.git/"), ("version_req", String ">1.2.3-alpha, <2"), ("optional", Bool True), ("default_features", Bool False), ("features", toJSON ["other" :: Text])]]
                <> [ changed "features" (Object (KeyMap.delete "full" features))
                   , changed "features" (Object (KeyMap.insert "extra" (toJSON ([] :: [Text])) features))
                   , changed "features" (Object (KeyMap.insert "full" (toJSON ["dep:foreign", "foreign/extra", "local/alpha" :: Text]) features))
                   , changed "features" (Object (KeyMap.insert "full" (toJSON ["local/alpha" :: Text]) features))
                   , changed "rust_version" (String "1.75")
                   , changed "rust_version" Null
                   , changed "links" (String "native_bar")
                   , changed "links" Null
                   , Object (KeyMap.delete "links" fields)
                   ]
    _ -> []

-- | Project the supplied native row with a nonzero original source coordinate.
cargoSourceVersion :: Value -> IO VersionDoc
cargoSourceVersion row@(Object fields) = do
    name <- maybe (fail "fixture row lacks its name") (expectRight . projectName) (stringField "name" fields)
    discovery <- expectRight cargoTestDiscovery
    details <- expectRight (projectCargoRecord name (RecordEntry 7) discovery row)
    pure (VersionDoc details (Just (fst cargoCached (cargoDocument [(RecordEntry 7, row)]))))
cargoSourceVersion _ = fail "fixture row is not an object"
