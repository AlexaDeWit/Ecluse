-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Paired PyPI inventories shared by state unit and persistent-backend tests.
module Ecluse.Test.Mirror.State (
    mirrorPackage,
    mirrorVersion,
    mirrorFilename,
    mirrorSample,
    mirrorSource,
    withMirrorEnvelope,
    mirrorViewWith,
) where

import Data.Aeson (Object, Value (String), object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Mirror.State (MirrorState (MirrorState), MirrorStateFault (StateInvalid, StateMissingCompanion), StateRead, StateTarget)
import Ecluse.Core.Mirror.State.Internal (StateRevision)
import Ecluse.Core.Package
import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry))
import Ecluse.Core.Registry.MirrorUpload (MirrorInventory, MirrorReadData (..), inventoryDetails, inventoryReadData)
import Ecluse.Core.Registry.MirrorUpload.Internal (restoreMirrorInventory)
import Ecluse.Core.Registry.PyPI.Document (simpleDocument, simpleFiles)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Server.Cache.Backend.Local (newLocalPool)
import Ecluse.Core.Server.Cache.Types (StoreBudget (StoreBudget))
import Ecluse.Core.Server.MirrorView (MirrorView, newMirrorRetention, newMirrorView)
import Ecluse.Core.Version (Version, mkVersion)
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

-- | A bounded managed view with a supplied reader and no write or companion capabilities.
mirrorViewWith :: StateTarget -> (PackageName -> Maybe StateRevision -> IO (Either MirrorStateFault StateRead)) -> IO MirrorView
mirrorViewWith target readState = do
    pool <- newLocalPool 20 10_000_000
    cache <- newMirrorRetention pool 60 (StoreBudget 0 0)
    let store = MirrorState (const readState) (\_ _ _ _ -> pure (Left (StateInvalid "unexpected write"))) (\_ _ -> pure (Left StateMissingCompanion)) (\_ _ -> pure (Left StateMissingCompanion))
    pure (newMirrorView defaultLimits target store cache)
