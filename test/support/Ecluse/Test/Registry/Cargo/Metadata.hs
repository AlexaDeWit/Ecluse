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
    parseCargoDiscovery,
    cargoRead,
    projectCargoIndex,
    readCargoIndexChunks,
    cargoArtifactPath,
) where

import Control.Monad.State.Strict qualified as State
import Data.Aeson (Value)
import Data.ByteString qualified as BS

import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Package (PackageInfo, PackageName, mkPackageName, renderPackageName)
import Ecluse.Core.Registry.CachedDocument (CachedDoc)
import Ecluse.Core.Registry.Cargo.Discovery (CargoDiscovery, discoveryIndex, parseDiscoveryWithDepth)
import Ecluse.Core.Registry.Cargo.Metadata (cargoReadWith, finishCargoFull)
import Ecluse.Core.Registry.Cargo.Reader (CargoIndexResult, CargoRead (FullRead), cargoIndexSteps, finishCargoSteps, readCargoIndex)
import Ecluse.Core.Registry.Cargo.Request (cargoArtifactHosts)
import Ecluse.Core.Registry.Cargo.Route.Internal (artifactCoordinate)
import Ecluse.Core.Registry.JsonStream (StreamResult, readStepsWithEnd)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataBoundExceeded))
import Ecluse.Core.Registry.Metadata.Fetch.Types (EcosystemRead)
import Ecluse.Core.Security (AllowedHostPorts, BodyLimit (MetadataBodyLimit), LimitError, Limits (maxMetadataBytes), ecosystemArtifactAuthorities)
import Ecluse.Core.Version (Version, mkVersion, renderVersion)
import Ecluse.Test.Registry.Cargo.Fixtures qualified as Fixtures
import Ecluse.Test.Registry.JsonStream (heldChunks, testTable)
import Ecluse.Test.Support (expectRight)

-- | Validate fixture archive captures with the runtime coordinate gate before constructing a load URL.
cargoArtifactPath :: PackageName -> Version -> Maybe Text
cargoArtifactPath name version = do
    let file = renderPackageName name <> "-" <> renderVersion version <> ".crate"
    _ <- artifactCoordinate name version file
    pure (Fixtures.cargoArchivePathFor (renderPackageName name) (renderVersion version))

-- | Fix the same discovery depth for held native fixtures, retaining production authority checks.
parseCargoDiscovery :: AllowedHostPorts -> Text -> ByteString -> Either MetadataError CargoDiscovery
parseCargoDiscovery = parseDiscoveryWithDepth 32

-- | Held fixtures have no admission ticket. Runtime preparation supplies its own selected-row charge.
cargoRead :: CargoDiscovery -> EcosystemRead
cargoRead = cargoReadWith (const pass)

-- | Project held bytes through the production framing and complete-index finisher.
projectCargoIndex :: CargoDiscovery -> Limits -> PackageName -> ByteString -> Either MetadataError (PackageInfo, CachedDoc)
projectCargoIndex discovery limits name bytes = do
    streamed <- first MetadataBoundExceeded (readCargoIndexChunks discovery limits name FullRead (MetadataBodyLimit (maxMetadataBytes limits)) [bytes])
    finishCargoFull discovery limits (discoveryIndex discovery) streamed

-- | Run captured chunks through production framing without another native parser.
readCargoIndexChunks :: CargoDiscovery -> Limits -> PackageName -> CargoRead -> BodyLimit -> [ByteString] -> Either LimitError (StreamResult CargoIndexResult)
readCargoIndexChunks discovery limits name mode bound =
    State.evalState (readStepsWithEnd (pure . runIdentity) (pure . runIdentity . finishCargoSteps) bound (cargoIndexSteps (const (pure ())) discovery limits name mode) nextChunk)
  where
    nextChunk =
        State.get >>= \case
            [] -> pure BS.empty
            chunk : rest -> State.put rest $> chunk

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
cargoTestDiscovery = parseCargoDiscovery (ecosystemArtifactAuthorities cargoArtifactHosts) "https://index.crates.io" "{\"dl\":\"https://static.crates.io/crates/{crate}/{crate}-{version}.crate\"}"

-- | Run the real framing and body-bound driver with a deterministic test-only intern table.
readCargoChunks :: Limits -> CargoRead -> [ByteString] -> IO (Either LimitError (StreamResult CargoIndexResult))
readCargoChunks limits mode chunks = do
    discovery <- expectRight cargoTestDiscovery
    next <- heldChunks chunks
    readCargoIndex discovery limits cargoTestName mode (MetadataBodyLimit (maxMetadataBytes limits)) (testTable ["vers", "cksum"]) next
