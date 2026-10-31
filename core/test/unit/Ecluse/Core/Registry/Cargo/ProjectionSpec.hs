-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native facts retain registry time, yank and exact archive identity while execution stays unknown.
module Ecluse.Core.Registry.Cargo.ProjectionSpec (spec) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Test.Hspec

import Ecluse.Core.Package (Artifact (artEntryKey, artFilename, artUrl), Availability (Yanked), CodeExecSignal (CodeExecUnknown), DependencySources (FetchesOutsideRegistry, RegistryDependencies), PackageDetails (..))
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.Cargo.Projection (projectCargoRecord)
import Ecluse.Test.Registry.Cargo.Metadata (cargoTestDiscovery, cargoTestName, cargoTestRecord)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "projectCargoRecord" $ do
    it "binds archive identity and source position without declaring an execution result" $ do
        discovery <- expectRight cargoTestDiscovery
        details <- expectRight (projectCargoRecord cargoTestName (RecordEntry 7) discovery (cargoTestRecord "1.0.0+build"))
        pkgInstallCode details `shouldBe` CodeExecUnknown
        pkgPublishedAt details `shouldSatisfy` isJust
        map artEntryKey (toList (pkgArtifacts details)) `shouldBe` [RecordEntry 7]
        map artFilename (toList (pkgArtifacts details)) `shouldBe` ["fixture-1.0.0+build.crate"]
        map artUrl (toList (pkgArtifacts details)) `shouldBe` ["https://static.crates.io/crates/fixture/fixture-1.0.0%2Bbuild.crate"]
    it "keeps invalid or absent native publication times unknown and retains yank" $ do
        discovery <- expectRight cargoTestDiscovery
        let changed = case cargoTestRecord "1.0.0" of
                Object fields -> Object (KeyMap.insert "yanked" (Bool True) (KeyMap.insert "pubtime" (String "not-a-time") fields))
                value -> value
        details <- expectRight (projectCargoRecord cargoTestName (RecordEntry 0) discovery changed)
        pkgPublishedAt details `shouldBe` Nothing
        pkgAvailability details `shouldBe` Yanked
    it "rejects a checksum or native name that disagrees with the requested package" $ do
        discovery <- expectRight cargoTestDiscovery
        for_ [("name", String "foreign"), ("cksum", String "bad")] $ \(key, replacement) -> do
            let changed = case cargoTestRecord "1.0.0" of Object fields -> Object (KeyMap.insert key replacement fields); value -> value
            projectCargoRecord cargoTestName (RecordEntry 0) discovery changed `shouldSatisfy` isLeft
    it "reports dependencies a consumer resolves from crates.io or the current index as resolving through the registry" $ do
        discovery <- expectRight cargoTestDiscovery
        for_ throughRegistry $ \dependencies ->
            pkgDependencySources <$> projectCargoRecord cargoTestName (RecordEntry 0) discovery (withDependencies dependencies) `shouldBe` Right RegistryDependencies
    it "reports the first normal or build dependency from another registry by the crate it names" $ do
        discovery <- expectRight cargoTestDiscovery
        for_ fromOtherRegistry $ \(dependencies, reason) ->
            pkgDependencySources <$> projectCargoRecord cargoTestName (RecordEntry 0) discovery (withDependencies dependencies) `shouldBe` Right (FetchesOutsideRegistry reason)

-- The test record with its dependency list replaced.
withDependencies :: [Value] -> Value
withDependencies dependencies = case cargoTestRecord "1.0.0" of
    Object fields -> Object (KeyMap.insert "deps" (toJSON dependencies) fields)
    value -> value

-- An index dependency with the given further members, such as its kind and registry.
dependency :: Text -> [Pair] -> Value
dependency name members = object (["name" .= name, "req" .= ("^1" :: Text), "features" .= ([] :: [Text]), "optional" .= False, "default_features" .= True, "target" .= Null] <> members)

throughRegistry :: [[Value]]
throughRegistry =
    [ []
    , [dependency "plain" ["kind" .= ("normal" :: Text)]]
    , [dependency "current" ["kind" .= ("normal" :: Text), "registry" .= Null]]
    , [dependency "git-spelling" ["kind" .= ("normal" :: Text), "registry" .= ("https://github.com/rust-lang/crates.io-index" :: Text)]]
    , [dependency "git-suffix" ["kind" .= ("build" :: Text), "registry" .= ("https://github.com/rust-lang/crates.io-index.git" :: Text)]]
    , [dependency "sparse" ["registry" .= ("sparse+https://index.crates.io/" :: Text)]]
    , [dependency "helper" ["kind" .= ("dev" :: Text), "registry" .= foreignIndex]]
    , [dependency "helper" ["kind" .= ("dev" :: Text), "registry" .= ("not a registry URL" :: Text)]]
    ]

fromOtherRegistry :: [([Value], Text)]
fromOtherRegistry =
    [ ([dependency "original" ["kind" .= ("normal" :: Text), "registry" .= foreignIndex]], outside "original" "dependencies")
    , ([dependency "original" ["registry" .= foreignIndex]], outside "original" "dependencies")
    , ([dependency "original" ["kind" .= Null, "registry" .= ("https://foreign.test/index" :: Text)]], outside "original" "dependencies")
    , ([dependency "tool" ["kind" .= ("build" :: Text), "registry" .= foreignIndex]], outside "tool" "build-dependencies")
    , ([dependency "renamed" ["package" .= ("original" :: Text), "kind" .= ("normal" :: Text), "registry" .= foreignIndex]], outside "original" "dependencies")
    , ([dependency "original" ["kind" .= ("normal" :: Text), "registry" .= ("not a registry URL" :: Text)]], outside "original" "dependencies")
    , ([dependency "helper" ["kind" .= ("dev" :: Text), "registry" .= foreignIndex], dependency "safe" ["kind" .= ("normal" :: Text)], dependency "tool" ["kind" .= ("build" :: Text), "registry" .= foreignIndex], dependency "original" ["kind" .= ("normal" :: Text), "registry" .= foreignIndex]], outside "tool" "build-dependencies")
    , -- The reader knows no address for Écluse, so a row naming its index by URL counts as another registry.
      ([dependency "first-party" ["kind" .= ("normal" :: Text), "registry" .= ("sparse+https://proxy.test/cargo/" :: Text)]], outside "first-party" "dependencies")
    ]
  where
    outside name table = "dependency '" <> name <> "' in " <> table <> " names a registry other than crates.io"

foreignIndex :: Text
foreignIndex = "sparse+https://foreign.test/index/"
