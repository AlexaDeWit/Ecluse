-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native Cargo fixture scheduling and independent source expectations.
module Ecluse.E2E.Fixtures.Cargo (
    cargoFamilyMetadata,
    cargoReadFixture,
    cargoWriteFixture,
    cargoNativeFixture,
    cargoArchiveRoute,
    cargoFixtureReleases,
    cargoReadIndex,
    cargoReadVersions,
    cargoRelease,
) where

import Data.Aeson (Value (String))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS

import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (..))
import Ecluse.E2E.Harness.Docker (withDataPlane)
import Ecluse.E2E.Harness.Proxy (proxyGet)
import Ecluse.E2E.Harness.Types (E2E, Topology (topologyAws), cargoTopology)
import Ecluse.Test.Contract.Ecosystem (FamilyMetadata (..))
import Ecluse.Test.Contract.Types
import Ecluse.Test.Registry.Cargo.Fixtures
import Ecluse.Test.Registry.Cargo.Reference (IndexLine (..), referenceIndex)

-- | Native toolchains are mandatory. A missing executable fails acquisition.
cargoFamilyMetadata :: Text -> Text -> [Text] -> FamilyMetadata
cargoFamilyMetadata family profile capabilities =
    FamilyMetadata
        ("cargo/" <> family)
        ("cargo/" <> profile)
        capabilities
        ["docker", "image", "openssl", "cargo", "cargo-1.74", "rustc"]
        ["proxy", "nginx", "cargo-registry", "ministack"]
        ["docker-daemon-exclusive", "group-mutable-state", "serial-cases"]

-- | Read families share the standard sparse registry lifecycle.
cargoReadFixture :: Text -> [Text] -> ConcreteFixture FamilyMetadata
cargoReadFixture family capabilities = ConcreteFixture (cargoFamilyMetadata family "read" capabilities) (withDataPlane cargoTopology)

-- | Writes and native restart controls share S3 and SQS with the other ecosystems.
cargoWriteFixture :: Text -> [Text] -> ConcreteFixture FamilyMetadata
cargoWriteFixture family capabilities = ConcreteFixture (cargoFamilyMetadata family "write" capabilities) (withDataPlane cargoTopology{topologyAws = True})

-- | Fixed native obligations remain independent of the scenario registration.
cargoNativeFixture :: ConcreteFixture Group
cargoNativeFixture =
    ConcreteFixture
        ( Group
            "cargo/native"
            "cargo/native"
            "cargo/write"
            E2E
            "cargo"
            [ Case ("cargo/native/" <> name) ["cargo/native"]
            | name <-
                ["current-client", "floor-client", "asset-routing", "metadata-no-archives", "denial-control", "features", "targets", "lock-checksums", "msrv", "discovery-views", "private-no-public", "raw-auth", "private-auth-required", "public-token-control", "private-anonymous", "publication-framing", "foreign-registry"]
            ]
            ["read", "mirror", "publication", "client-install"]
            ["docker", "image", "openssl", "cargo", "cargo-1.74", "rustc"]
            ["proxy", "nginx", "cargo-registry", "ministack"]
            ["docker-daemon-exclusive", "group-mutable-state", "serial-cases"]
        )
        (withDataPlane cargoTopology{topologyAws = True})

-- | Local archive paths retain each explicit native coordinate.
cargoArchiveRoute :: Text -> Text -> Text
cargoArchiveRoute name version = "/cargo/" <> cargoArchivePathFor name version

-- | Load only repository-owned source bytes through the independent fixture builder.
cargoFixtureReleases :: IO [CargoRelease]
cargoFixtureReleases = loadCargoGraph "test/fixtures/cargo/graph"

-- | Parse served rows with the independent reader and reject any malformed emitted row.
cargoReadIndex :: E2E -> Text -> Text -> IO [KeyMap.KeyMap Value]
cargoReadIndex proxy view name = do
    (status, body) <- proxyGet proxy (view <> cargoShard name)
    unless (status == 200) (fail (toString ("native index returned " <> show status <> " for " <> name)))
    traverse (either (fail . toString) pure . indexValue) (referenceIndex (LBS.toStrict body))

-- | Native version spelling comes from the served record without production decoding.
cargoReadVersions :: E2E -> Text -> Text -> IO [Text]
cargoReadVersions proxy view name = mapM version =<< cargoReadIndex proxy view name
  where
    version row = case KeyMap.lookup "vers" row of
        Just (String value) -> pure value
        _ -> fail "native row has no raw version"

-- | Empty or ambiguous fixture coordinates fail setup.
cargoRelease :: [CargoRelease] -> Text -> Text -> IO CargoRelease
cargoRelease releases name version = case filter (\release -> crateName (releaseCrate release) == name && crateVersion (releaseCrate release) == version) releases of
    [release] -> pure release
    _ -> fail (toString ("Cargo fixture coordinate is absent or ambiguous: " <> name <> "@" <> version))
