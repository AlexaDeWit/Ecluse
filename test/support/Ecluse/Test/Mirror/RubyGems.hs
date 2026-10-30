-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native state probes reopen checked source rows and independently stored compact objects.
module Ecluse.Test.Mirror.RubyGems (
    RubyStateProbeFixture (..),
    rubyStateProbeFixture,
    makeRubyStateProbeFixture,
    padRubyStateProbeSource,
    rubyStateSourceOmissionCharge,
    rubyStateCompanionOmissionCharge,
) where

import Control.Monad (foldM)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import System.FilePath ((</>))

import Ecluse.Core.Mirror.State
import Ecluse.Core.Mirror.State.Codec (chargedDecodeProjectRecord)
import Ecluse.Core.Mirror.State.Internal (MirrorVersion (..), ProjectMirrorRecord (..), StoredMirrorFragment (..), VerifiedCompanionBytes (..), retainVerifiedIdentity)
import Ecluse.Core.Mirror.State.Weight (retainedRecordWeight)
import Ecluse.Core.Package (Artifact (artFilename), PackageDetails (..), PackageInfo (..), PackageName, hashValue, renderPackageName)
import Ecluse.Core.Registry.CachedDocument (rubyGemsCached)
import Ecluse.Core.Registry.Metadata (VersionDoc (..))
import Ecluse.Core.Registry.MirrorUpload
import Ecluse.Core.Registry.Publish (MirrorCompanion (mcValidate))
import Ecluse.Core.Registry.RubyGems.Archive (checkGemArchive, gemArchiveSpecification, inspectGemArchive)
import Ecluse.Core.Registry.RubyGems.CompactSpec (checkGemSpecification)
import Ecluse.Core.Registry.RubyGems.Document (GemDocument (..), GemRow (..), selectGemVersion)
import Ecluse.Core.Registry.RubyGems.Mirror (mirrorRow, rubyMirrorCompanion)
import Ecluse.Core.Registry.RubyGems.Project (coordinateFilename, coordinatePlatform, coordinateVersion, projectName, unGemPlatform)
import Ecluse.Core.Registry.RubyGems.Wire (decodeInfoRow)
import Ecluse.Core.Security (Limits (maxMetadataBytes), defaultLimits)
import Ecluse.Core.Server.Path (unFilename)
import Ecluse.Core.Version (renderVersion)
import Ecluse.Test.Corpus.RubyGems (projectRubyGemsInfo)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Registry.RubyGems.Fixtures (RubyRelease (..), loadRubyReleasesFrom, rubyRelease)
import Ecluse.Test.Support (expectRight, expectRightIO)

-- | Encoded records own rows and references. The object map owns the original compressed specifications.
data RubyStateProbeFixture = RubyStateProbeFixture
    { rubyStateTarget :: StateTarget
    , rubyStatePackage :: PackageName
    , rubyStateEncodedRecord :: ByteString
    , rubyStateCompanionObjects :: Map Text ByteString
    }

-- | The committed fixture contains four genuine artifacts for one exact version.
rubyStateProbeFixture :: IO RubyStateProbeFixture
rubyStateProbeFixture = makeRubyStateProbeFixture "test/fixtures/rubygems" "ecluse-fixture-platform"

-- | Generated native fixture directories use the same archive, specification and membership checks.
makeRubyStateProbeFixture :: FilePath -> Text -> IO RubyStateProbeFixture
makeRubyStateProbeFixture directory rawName = do
    name <- expectRight (projectName rawName)
    releases <- filter ((== renderPackageName name) . releaseName) <$> loadRubyReleasesFrom directory
    when (null releases) (fail "native state fixture has no releases")
    bytes <- readFileBS (directory </> "info" </> toString (renderPackageName name))
    (info, cached) <- expectRight (projectRubyGemsInfo "https://public.test" defaultLimits name bytes)
    unless (null (infoInvalidEntries info)) (fail "native state fixture contains invalid source entries")
    source <- maybe (fail "native state fixture lost its original rows") pure (snd rubyGemsCached cached)
    let sourceFiles = map (unFilename . coordinateFilename . grCoordinate) (gemRows source)
    unless (length sourceFiles == length releases && Set.fromList sourceFiles == Set.fromList (map releaseFilename releases)) (fail "native state fixture releases disagree with source membership")
    let target = stateTarget "https://private.test"
    (record, objects) <- foldM (retainVersion target name source releases) (Nothing, Map.empty) (Map.elems (infoVersions info))
    complete <- maybe (fail "native state fixture has no expected versions") pure record
    encoded <- expectRight (encodeProjectRecord defaultLimits complete)
    pure (RubyStateProbeFixture target name encoded objects)

retainVersion :: StateTarget -> PackageName -> GemDocument -> [RubyRelease] -> (Maybe ProjectMirrorRecord, Map Text ByteString) -> PackageDetails -> IO (Maybe ProjectMirrorRecord, Map Text ByteString)
retainVersion target name source releases (previous, objects) details = do
    selected <- maybe (fail "native state fixture lost a selected version") pure (selectGemVersion (pkgVersion details) source)
    let doc = VersionDoc details (Just (fst rubyGemsCached selected))
    inventory <- expectRight (checkedMirrorInventory name (pkgVersion details) doc (RubyGemsMirrorRead selected))
    record <- expectRight (extendProjectRecord defaultLimits target inventory previous)
    (retained, addedObjects) <- foldM (retainFile inventory releases) (record, objects) (toList (inventoryFiles inventory))
    version <- maybe (fail "native state fixture lost its retained version") pure (recordVersion (pkgVersion details) retained)
    unless (checkMirrorCompletion version (toList (pkgArtifacts details)) == MirrorComplete) (fail "native state fixture is incomplete")
    pure (Just retained, addedObjects)

-- Profiled GHC 9.10 exhausts simplifier ticks while expanding native fixture construction.
-- Keep this per-file action separate from the nested folds.
{-# NOINLINE retainFile #-}
retainFile :: MirrorInventory -> [RubyRelease] -> (ProjectMirrorRecord, Map Text ByteString) -> ExpectedMirrorFile -> IO (ProjectMirrorRecord, Map Text ByteString)
retainFile inventory releases (record, objects) file = do
    (verified, checked@(VerifiedCompanionBytes _ reference _)) <- checkedFixtureFile inventory releases file
    withIdentity <- expectRight (retainVerifiedIdentity (inventoryVersion inventory) verified record)
    retained <- expectRight (retainCompanionReference (inventoryVersion inventory) checked reference withIdentity)
    addedObjects <- retainFixtureCompanionObject checked objects
    pure (retained, addedObjects)

checkedFixtureFile :: MirrorInventory -> [RubyRelease] -> ExpectedMirrorFile -> IO (VerifiedArtifactBytes, VerifiedCompanionBytes)
checkedFixtureFile inventory releases file = do
    row <- expectRight (mirrorRow inventory file)
    let coordinate = grCoordinate row
    release <- rubyRelease releases (renderPackageName (inventoryPackage inventory)) (renderVersion (coordinateVersion coordinate)) (unGemPlatform (coordinatePlatform coordinate))
    unless (releaseFilename release == artFilename (expectedArtifact file)) (fail "native state fixture filename disagrees with source")
    verified <- expectRight (verifyMirrorBytes file (releaseArchive release))
    inspected <- expectRightIO (inspectGemArchive defaultLimits (const pass) (verifiedBytes verified))
    archive <- expectRight (checkGemArchive coordinate (hashValue (grChecksum row)) inspected)
    void (expectRight (checkGemSpecification coordinate row (gemArchiveSpecification archive)))
    compact <- expectRightIO (withSweeper (\deadlines -> mcValidate rubyMirrorCompanion deadlines defaultLimits (const pass) inventory file (releaseCompact release)))
    unless (compact == releaseCompact release) (fail "native state fixture changed the original compact bytes")
    checked <- expectRight (verifySourceCompanionBytes defaultLimits (inventoryReadData inventory) file compact)
    pure (verified, checked)

retainFixtureCompanionObject :: VerifiedCompanionBytes -> Map Text ByteString -> IO (Map Text ByteString)
retainFixtureCompanionObject checked@(VerifiedCompanionBytes _ reference _) objects = do
    let digest = companionDigest reference
        compact = companionBody checked
    case Map.lookup digest objects of
        Nothing -> pure ()
        Just existing -> unless (existing == compact) (fail "native state fixture has conflicting companion objects")
    pure (Map.insert digest (BS.copy compact) objects)

-- | Add labelled opaque source metadata for omission controls, preserving native archive and specification identities.
padRubyStateProbeSource :: Int -> RubyStateProbeFixture -> IO RubyStateProbeFixture
padRubyStateProbeSource padding fixture = do
    when (padding <= 0) (fail "native state source padding must be positive")
    record <- chargedDecodeProjectRecord (const pass) defaultLimits (rubyStateTarget fixture) (rubyStatePackage fixture) (rubyStateEncodedRecord fixture) >>= expectRight
    let rows = [row | version <- Map.elems (recordVersions record), StoredMirrorFragment _ (RubyGemsMirrorRead source) _ <- toList (versionFragments version), row <- gemRows source]
        suffixBytes = toInteger padding + toInteger (BS.length paddingPrefix)
        growth = sum [base64Length (toInteger (BS.length (grSource row)) + suffixBytes) - base64Length (toInteger (BS.length (grSource row))) | row <- rows]
        companions = sum [toInteger (companionSize reference) | version <- Map.elems (recordVersions record), CompanionRetained reference <- Map.elems (versionCompanions version)]
    when (toInteger (BS.length (rubyStateEncodedRecord fixture)) + growth + companions > toInteger (maxMetadataBytes defaultLimits)) (fail "native state source padding exceeds its metadata bound")
    when (null rows) (fail "native state source padding has no native rows")
    let suffix = paddingPrefix <> BS.replicate padding 120
    versions <- traverse (padVersion suffix) (projectVersions record)
    encoded <- expectRight (encodeProjectRecord defaultLimits record{projectVersions = versions})
    pure fixture{rubyStateEncodedRecord = encoded}

paddingPrefix :: ByteString
paddingPrefix = ",ecluse_probe_padding:"

base64Length :: Integer -> Integer
base64Length size = 4 * ((size + 2) `div` 3)

padVersion :: ByteString -> MirrorVersion -> IO MirrorVersion
padVersion suffix version = do
    fragments <- traverse (padFragment suffix) (storedFragments version)
    pure version{storedFragments = fragments}

padFragment :: ByteString -> StoredMirrorFragment -> IO StoredMirrorFragment
padFragment suffix (StoredMirrorFragment details (RubyGemsMirrorRead source) expected) = do
    rows <- traverse (\row -> expectRight (decodeInfoRow (gemName source) (grEntryKey row) (grSource row <> suffix))) (gemRows source)
    pure (StoredMirrorFragment details (RubyGemsMirrorRead source{gemRows = rows}) expected)
padFragment _ _ = fail "native source padding encountered another ecosystem"

-- | Weigh an omitted native source while the probe retains the unchanged, complete record.
rubyStateSourceOmissionCharge :: ProjectMirrorRecord -> Integer
rubyStateSourceOmissionCharge record = retainedRecordWeight record{projectVersions = fmap (\version -> version{storedFragments = fmap omitFragmentSource (storedFragments version)}) (projectVersions record)}

omitFragmentSource :: StoredMirrorFragment -> StoredMirrorFragment
omitFragmentSource (StoredMirrorFragment details (RubyGemsMirrorRead source) expected) = StoredMirrorFragment details (RubyGemsMirrorRead source{gemHeader = mempty, gemRows = []}) expected
omitFragmentSource fragment = fragment

-- | Weigh omitted companion references without changing the actual probe root or external object map.
rubyStateCompanionOmissionCharge :: ProjectMirrorRecord -> Integer
rubyStateCompanionOmissionCharge record = retainedRecordWeight record{projectVersions = fmap (\version -> version{storedCompanions = Map.empty}) (projectVersions record)}
