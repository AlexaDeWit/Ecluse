-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native compact companions use the checked artifact coordinate and source authority.
module Ecluse.Core.Registry.RubyGems.Mirror (
    rubyMirrorCompanion,
    mirrorRow,
) where

import Data.Text qualified as T

import Ecluse.Core.Package (Artifact (artEntryKey, artFilename))
import Ecluse.Core.Registry (ParseError (parseErrorMessage), PublishFault (PublishSourceUnavailable))
import Ecluse.Core.Registry.MirrorUpload
import Ecluse.Core.Registry.Publish (MirrorCompanion (..))
import Ecluse.Core.Registry.RubyGems.CompactSpec (compactSpecBytes, validateCompactSpec)
import Ecluse.Core.Registry.RubyGems.Document (GemDocument (gemRows), GemRow (..))
import Ecluse.Core.Registry.RubyGems.Project (coordinateStem)

-- | A companion cannot supply authority or substitute another platform's specification.
rubyMirrorCompanion :: MirrorCompanion
rubyMirrorCompanion =
    MirrorCompanion
        { mcLocation = \file source -> do
            let filename = artFilename (expectedArtifact file)
            base <- maybeToRight (PublishSourceUnavailable "RubyGems artifact URL disagrees with its native filename") (T.stripSuffix ("/gems/" <> filename) source)
            stem <- maybeToRight (PublishSourceUnavailable "RubyGems artifact lacks its native extension") (T.stripSuffix ".gem" filename)
            pure (base <> "/quick/Marshal.4.8/" <> stem <> ".gemspec.rz")
        , mcValidate = \limits charge inventory file bytes -> case mirrorRow inventory file of
            Left fault -> pure (Left fault)
            Right row -> fmap (first (PublishSourceUnavailable . parseErrorMessage) . fmap compactSpecBytes) (validateCompactSpec limits charge (grCoordinate row) row bytes)
        }

-- | Require exactly one original source row for the expected physical member.
mirrorRow :: MirrorInventory -> ExpectedMirrorFile -> Either PublishFault GemRow
mirrorRow inventory file = case inventoryReadData inventory of
    RubyGemsMirrorRead source -> case filter ((== artEntryKey (expectedArtifact file)) . grEntryKey) (gemRows source) of
        [row] | coordinateStem (grCoordinate row) <> ".gem" == artFilename (expectedArtifact file) -> Right row
        _ -> Left (PublishSourceUnavailable "RubyGems upload has no unique paired source row")
    _ -> Left (PublishSourceUnavailable "RubyGems upload has no native source document")
