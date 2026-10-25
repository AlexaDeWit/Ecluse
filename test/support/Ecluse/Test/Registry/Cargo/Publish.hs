-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | An owned archive and paired index snapshot shared by native publication and recovery tests.
module Ecluse.Test.Registry.Cargo.Publish (
    cargoPublicationFixture,
    cargoSourceVersion,
) where

import Codec.Compression.GZip qualified as GZip
import Data.Aeson (Value (..))
import Data.ByteString.Lazy qualified as LBS

import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.CachedDocument (cargoCached)
import Ecluse.Core.Registry.Cargo.Document (cargoDocument)
import Ecluse.Core.Registry.Cargo.Project (projectName)
import Ecluse.Core.Registry.Cargo.Projection (projectCargoRecord)
import Ecluse.Core.Registry.Metadata (VersionDoc (..))
import Ecluse.Core.Registry.ServedDocument (stringField)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Registry.Cargo.Fixtures (CargoRelease (..), minimalCargoRelease)
import Ecluse.Test.Registry.Cargo.Metadata (cargoTestDiscovery)
import Ecluse.Test.Registry.PyPI.Archive (tarBytes)
import Ecluse.Test.Support (expectRight)

-- | The README precedes Cargo.toml so publication must perform its bounded selected-text pass.
cargoPublicationFixture :: IO CargoRelease
cargoPublicationFixture = do
    let manifest = "[package]\nname='fixture'\nversion='1.0.0'\nauthors=['Ecluse contributors']\ndescription='Owned Cargo registry contract fixture'\nlicense='MIT'\nrust-version='1.74'\n"
    bytes <- LBS.toStrict . GZip.compress <$> tarBytes [("fixture-1.0.0/README.md", "owned readme"), ("fixture-1.0.0/Cargo.toml", manifest), ("fixture-1.0.0/src/lib.rs", "pub fn owned() {}\n")]
    pure (minimalCargoRelease "fixture" "1.0.0"){releaseArchive = bytes, releaseChecksum = hexSha256Of bytes, releaseReadme = "owned readme"}

-- | Project the supplied native row with a nonzero original source coordinate.
cargoSourceVersion :: Value -> IO VersionDoc
cargoSourceVersion row@(Object fields) = do
    name <- maybe (fail "fixture row lacks its name") (expectRight . projectName) (stringField "name" fields)
    discovery <- expectRight cargoTestDiscovery
    details <- expectRight (projectCargoRecord name (RecordEntry 7) discovery row)
    pure (VersionDoc details (Just (fst cargoCached (cargoDocument [(RecordEntry 7, row)]))))
cargoSourceVersion _ = fail "fixture row is not an object"
