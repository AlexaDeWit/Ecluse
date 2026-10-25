-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Production Cargo reads over explicit chunks, beside independent native fixture builders.
module Ecluse.Test.Registry.Cargo.Metadata (
    cargoTestName,
    cargoTestVersion,
    cargoTestRecord,
    cargoTestDiscovery,
    readCargoChunks,
) where

import Data.Aeson (Value)

import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Registry.Cargo.Discovery (CargoDiscovery, parseCargoDiscovery)
import Ecluse.Core.Registry.Cargo.Reader (CargoIndexResult, CargoRead, readCargoIndex)
import Ecluse.Core.Registry.Cargo.Request (cargoArtifactHosts)
import Ecluse.Core.Registry.JsonStream (StreamResult)
import Ecluse.Core.Registry.Metadata (MetadataError)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError, Limits (maxMetadataBytes), ecosystemArtifactAuthorities)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Registry.Cargo.Fixtures qualified as Fixtures
import Ecluse.Test.Registry.JsonStream (heldChunks, testTable)
import Ecluse.Test.Support (expectRight)

-- | A native identity with no filesystem or network fixture dependency.
cargoTestName :: PackageName
cargoTestName = mkPackageName Cargo Nothing "fixture"

-- | Retain each supplied raw version in the native ecosystem.
cargoTestVersion :: Text -> Version
cargoTestVersion = mkVersion Cargo

-- | Reuse the independent fixture's complete supported installation record.
cargoTestRecord :: Text -> Value
cargoTestRecord version = Fixtures.cargoIndexRecord (Fixtures.minimalCargoRelease "fixture" version)

-- | A fixed anonymous source with an independently declared download authority.
cargoTestDiscovery :: Either MetadataError CargoDiscovery
cargoTestDiscovery = parseCargoDiscovery (ecosystemArtifactAuthorities cargoArtifactHosts) "https://index.crates.io" (Fixtures.cargoDiscovery "https://static.crates.io/crates/{crate}/{crate}-{version}.crate" Nothing False)

-- | Run the real framing and body-bound driver with a deterministic test-only intern table.
readCargoChunks :: Limits -> CargoRead -> [ByteString] -> IO (Either LimitError (StreamResult CargoIndexResult))
readCargoChunks limits mode chunks = do
    discovery <- expectRight cargoTestDiscovery
    next <- heldChunks chunks
    readCargoIndex discovery limits cargoTestName mode (MetadataBodyLimit (maxMetadataBytes limits)) (testTable ["vers", "cksum"]) next
