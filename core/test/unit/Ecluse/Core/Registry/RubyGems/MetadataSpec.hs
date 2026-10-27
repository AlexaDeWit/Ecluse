-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Named compact metadata binds request identity and origin without inventing native name fields.
module Ecluse.Core.Registry.RubyGems.MetadataSpec (spec) where

import Data.ByteString qualified as BS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Test.Hspec
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Package (PackageDetails, PackageInfo (..), hashValue, renderPackageName)
import Ecluse.Core.Package.Entry (EntryKey (RecordEntry))
import Ecluse.Core.Registry.CachedDocument (rubyGemsCached)
import Ecluse.Core.Registry.Metadata (Manifest (..), MetadataError, VersionDoc (..), VersionRead (..))
import Ecluse.Core.Registry.Metadata.Fetch (readManifest, readVersion)
import Ecluse.Core.Registry.Metadata.Fetch.Types (EcosystemRead, ReadTerms (..))
import Ecluse.Core.Registry.RubyGems.Document (GemDependency (..), GemDocument (..), GemRow (..))
import Ecluse.Core.Registry.RubyGems.Metadata (gemReadWith)
import Ecluse.Core.Registry.RubyGems.Project (coordinatePlatform, coordinateVersion, projectName, unGemPlatform)
import Ecluse.Core.Security (Limits, defaultLimits)
import Ecluse.Core.Version (mkVersion, renderVersion)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Metadata.Projection (CallerBoundProjectionDriver (..), MetadataFraming (CallerBoundRecords), ProjectionDriver (..), callerBoundProjectionContract)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Registry.Metadata.Fetch (heldBody)
import Ecluse.Test.Registry.RubyGems.Reference (RubyInfoRow (..), referenceInfo)
import Ecluse.Test.Snapshot (digestOf)
import Ecluse.Test.Support (expectRight)

-- | Apply the shared count and selection laws with native caller-bound identity assertions.
spec :: Spec
spec = do
    callerBoundProjectionContract "rubygems" projectionDriver
    it (caseLabel "core.metadata.projection.rubygems.record-framing" "uses native caller-bound records") $
        projectionFraming (callerProjection projectionDriver) `shouldBe` CallerBoundRecords
    it (caseLabel "core.metadata.projection.rubygems.invalid-evidence-control" "detects a reader that discards a malformed sibling without evidence") $ do
        let bytes = "---\nbroken\n" <> fixtureRow "1.0.0" <> "\n" <> fixtureRow "2.0.0"
        let evidenceLaw readInfo = do
                info <- expectRight =<< readInfo bytes
                Map.keys (infoVersions info) `shouldBe` ["1.0.0", "2.0.0"]
                infoInvalidEntries info `shouldSatisfy` (not . null)
            permissive input = fmap (\info -> info{infoInvalidEntries = []}) <$> fullAt origin defaultLimits input
        evidenceLaw (fullAt origin defaultLimits)
        evidenceLaw permissive `shouldThrow` anyException
    it "hashes original source bytes and retains exact selected physical rows" $ do
        name <- expectRight (projectName "fixture")
        let body = "---\n" <> fixtureRow "1.0.0" <> "\n" <> fixtureRow "2.0.0"
        manifest <- expectRight =<< readManifest native passthroughTracingPort (terms origin defaultLimits) name (heldBody [body])
        manifestDigest manifest `shouldBe` digestOf body
        manifestBodyBytes manifest `shouldBe` BS.length body
        selected <- expectRight =<< readVersion native passthroughTracingPort (terms origin defaultLimits) name (mkVersion RubyGems "2.0.0") (heldBody [body])
        fmap (map grEntryKey . gemRows) (vrVersion selected >>= vdRaw >>= snd rubyGemsCached) `shouldBe` Just [RecordEntry 2]
    for_ ["ecluse-fixture-root", "ecluse-fixture-leaf", "ecluse-fixture-platform"] $ \fixture ->
        it ("agrees with the independent reader on genuine Ruby-generated rows for " <> toString fixture) $ do
            bytes <- readFileBS ("test/fixtures/rubygems/info/" <> toString fixture)
            expected <- expectRight (referenceInfo bytes)
            name <- expectRight (projectName fixture)
            manifest <- expectRight =<< readManifest native passthroughTracingPort (terms origin defaultLimits) name (heldBody [bytes])
            document <- expectRight (maybeToRight ("native document absent" :: Text) (snd rubyGemsCached (manifestRaw manifest)))
            map observe (gemRows document) `shouldBe` map referenceObservation expected
    it "stops before another fragment when the native workspace reservation refuses" $ do
        name <- expectRight (projectName "fixture")
        let refused = gemReadWith (const (throwIO WorkspaceRefused)) (const pass) origin
        readManifest refused passthroughTracingPort (terms origin defaultLimits) name (heldBody ["---\n", fixtureRow "1.0.0"]) `shouldThrow` (== WorkspaceRefused)

projectionDriver :: CallerBoundProjectionDriver
projectionDriver =
    CallerBoundProjectionDriver
        { callerProjection =
            ProjectionDriver
                { projectFull = fullAt origin
                , projectSelected = selectedAt origin
                , projectionFraming = CallerBoundRecords
                , projectionDocument = \_ versions -> "---\n" <> BS.intercalate "\n" (map fixtureRow versions)
                , projectionPackageName = "fixture"
                , projectionOtherName = "foreign"
                , projectionVersions = ["1.0.0", "2.0.0"]
                , projectionMissingName = "missing delimiter"
                }
        , callerOrigin = origin
        , callerFullAt = fullAt
        , callerSelectedAt = selectedAt
        }

fullAt :: Text -> Limits -> ByteString -> IO (Either MetadataError PackageInfo)
fullAt supplied limits bytes = do
    name <- expectRight (projectName "fixture")
    fmap manifestInfo <$> readManifest native passthroughTracingPort (terms supplied limits) name (heldBody [bytes])

selectedAt :: Text -> Limits -> Text -> ByteString -> IO (Either MetadataError (Maybe PackageDetails))
selectedAt supplied limits version bytes = do
    name <- expectRight (projectName "fixture")
    fmap (fmap vdDetails . vrVersion) <$> readVersion native passthroughTracingPort (terms supplied limits) name (mkVersion RubyGems version) (heldBody [bytes])

terms :: Text -> Limits -> ReadTerms
terms supplied limits = ReadTerms limits supplied (const pass)

native :: EcosystemRead
native = gemReadWith (const pass) (const pass) origin

origin :: Text
origin = "https://rubygems.org"

fixtureRow :: Text -> ByteString
fixtureRow version = encodeUtf8 (version <> " |checksum:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef,created_at:2020-01-01T00:00:00Z")

observe :: GemRow -> (EntryKey, Text, Text, [(Text, [Text])], Maybe Text)
observe row = (grEntryKey row, renderVersion (coordinateVersion (grCoordinate row)), unGemPlatform (coordinatePlatform (grCoordinate row)), [(renderPackageName (gdName dependency), toList (gdRequirement dependency)) | dependency <- grDependencies row], Just (hashValue (grChecksum row)))

referenceObservation :: RubyInfoRow -> (EntryKey, Text, Text, [(Text, [Text])], Maybe Text)
referenceObservation row = (RecordEntry (rowPosition row), rowVersion row, rowPlatform row, rowDependencies row, lookup "checksum" (rowFields row))

data WorkspaceRefused = WorkspaceRefused
    deriving stock (Eq, Show)

instance Exception WorkspaceRefused
