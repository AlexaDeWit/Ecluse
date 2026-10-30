-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Paired PyPI inventories, completed Cargo records and a managed view shared by state and serving tests.
module Ecluse.Test.Mirror.State (
    mirrorPackage,
    mirrorVersion,
    mirrorFilename,
    mirrorSample,
    mirrorSource,
    withMirrorEnvelope,
    cargoMirrorRecord,
    mirrorViewWith,
) where

import Control.Monad (foldM)
import Data.Aeson (Object, Value (Object, String), object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, PyPI))
import Ecluse.Core.Mirror.State (MirrorState (MirrorState), MirrorStateFault (StateInvalid, StateMissingCompanion), ProjectMirrorRecord, StateRead, StateTarget, extendProjectRecord)
import Ecluse.Core.Mirror.State.Internal (StateRevision, retainVerifiedIdentity)
import Ecluse.Core.Package
import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry, RecordEntry))
import Ecluse.Core.Registry.Cargo.Document (cargoDocument)
import Ecluse.Core.Registry.MirrorUpload (MirrorInventory, MirrorReadData (..), inventoryDetails, inventoryFiles, inventoryReadData, verifyMirrorBytes)
import Ecluse.Core.Registry.MirrorUpload.Internal (restoreMirrorInventory)
import Ecluse.Core.Registry.PyPI.Document (simpleDocument, simpleFiles)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Server.Cache.Backend.Local (newLocalPool)
import Ecluse.Core.Server.Cache.Types (StoreBudget (StoreBudget))
import Ecluse.Core.Server.MirrorView (MirrorView, newMirrorRetention, newMirrorView)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Support (expectRight)

-- | The package identity encoded by the fixture filenames.
mirrorPackage :: PackageName
mirrorPackage = mkPackageName PyPI Nothing "demo"

-- | The canonical release spelling of fixture version 1.0.
mirrorVersion :: Version
mirrorVersion = mkVersion PyPI "1"

-- | Distinct build tags add physical wheels to the same release.
mirrorFilename :: Int -> Text
mirrorFilename build = "demo-1.0-" <> show build <> "-py3-none-any.whl"

-- | Each fixture pairs original source entries with their admitted artifact facts.
mirrorSample :: [Int] -> Bool -> Text -> Text -> IO MirrorInventory
mirrorSample numbers companion location note = do
    digest <- expectRight (mkHash SHA256 (T.replicate 64 "a"))
    artifacts <-
        maybe (fail "empty test inventory") pure . nonEmpty $
            [Artifact (ArrayEntry position) (mirrorFilename number) (location <> mirrorFilename number) [digest] (if companion then Just (MetadataAvailable True) else Nothing) (Just 5) | (position, number) <- zip [0 ..] numbers]
    let details = PackageDetails mirrorPackage mirrorVersion Nothing NoCodeOnInstall RegistryDependencies Available artifacts
    expectRight (restoreMirrorInventory details (mirrorSource artifacts note))

-- | Keep source coordinates and custom served fields when a test changes artifact facts.
mirrorSource :: NonEmpty Artifact -> Text -> MirrorReadData
mirrorSource artifacts note =
    PyPIMirrorRead
        ( simpleDocument
            (KeyMap.fromList [("name", String "demo"), ("meta", object ["api-version" .= ("1.0" :: Text)])])
            [ ( artEntryKey artifact
              , object
                    ( ["filename" .= artFilename artifact, "url" .= artUrl artifact, "hashes" .= object [Key.fromText (renderHashAlg (hashAlg digest)) .= hashValue digest | digest <- artHashes artifact], "size" .= artSize artifact, "requires-python" .= (">=3.8" :: Text), "yanked" .= False, "note" .= note]
                        <> ["core-metadata" .= companionValue declaration | declaration <- maybeToList (artCoreMetadata artifact)]
                    )
              )
            | artifact <- toList artifacts
            ]
        )

companionValue :: CoreMetadata -> Value
companionValue (MetadataAvailable available) = toJSON available
companionValue (MetadataDigests values) = toJSON values

-- | Replace project fields while preserving the checked file-to-source pairing.
withMirrorEnvelope :: Object -> MirrorInventory -> IO MirrorInventory
withMirrorEnvelope envelope inventory = case inventoryReadData inventory of
    PyPIMirrorRead document -> expectRight (restoreMirrorInventory (inventoryDetails inventory) (PyPIMirrorRead (simpleDocument envelope (simpleFiles document))))
    _ -> fail "expected PyPI inventory"

-- | Complete one Cargo version per original index row at its position. Rows carry the fixture publication time and this archive's checksum.
cargoMirrorRecord :: StateTarget -> PackageName -> ByteString -> [(Int, Value)] -> IO ProjectMirrorRecord
cargoMirrorRecord target name archive rows = do
    checksum <- expectRight (mkHash SHA256 (hexSha256Of archive))
    foldM (retainRow checksum) Nothing rows >>= maybe (fail "no Cargo rows to retain") pure
  where
    retainRow checksum previous (position, row) = do
        version <- case row of
            Object fields | Just (String vers) <- KeyMap.lookup "vers" fields -> pure vers
            _ -> fail "Cargo row lacks its version"
        let filename = renderPackageName name <> "-" <> version <> ".crate"
            artifact = Artifact (RecordEntry position) filename ("https://static.crates.io/crates/" <> renderPackageName name <> "/" <> filename) [checksum] Nothing Nothing
            details = PackageDetails name (mkVersion Cargo version) (Just (UTCTime (fromGregorian 2020 1 1) 0)) CodeExecUnknown RegistryDependencies Available (artifact :| [])
        inventory <- expectRight (restoreMirrorInventory details (CargoMirrorRead (cargoDocument [(RecordEntry position, row)])))
        expected <- expectRight (extendProjectRecord defaultLimits target inventory previous)
        let file :| _ = inventoryFiles inventory
        verified <- expectRight (verifyMirrorBytes file archive)
        Just <$> expectRight (retainVerifiedIdentity (mkVersion Cargo version) verified expected)

-- | A bounded managed view with a supplied reader and no write or companion capabilities.
mirrorViewWith :: StateTarget -> (PackageName -> Maybe StateRevision -> IO (Either MirrorStateFault StateRead)) -> IO MirrorView
mirrorViewWith target readState = do
    pool <- newLocalPool 20 10_000_000
    cache <- newMirrorRetention pool 60 (StoreBudget 0 0)
    let store = MirrorState (const readState) (\_ _ _ _ -> pure (Left (StateInvalid "unexpected write"))) (\_ _ -> pure (Left StateMissingCompanion)) (\_ _ -> pure (Left StateMissingCompanion))
    pure (newMirrorView defaultLimits target store cache)
