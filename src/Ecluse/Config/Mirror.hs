-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Validate configured mirror endpoints and credential declarations.
module Ecluse.Config.Mirror (validateMirrorSettings) where

import Data.Map.Strict qualified as Map

import Ecluse.Config.Target (resolveStoreBackend, resolveUploadBackend, vetPublicationRepository)
import Ecluse.Config.Types

{- | Reject conflicting repository and credential declarations before role-specific checks. Upload
API hosts are vetted with the other endpoint rules in "Ecluse.Composition.Endpoints".
-}
validateMirrorSettings :: AppConfig -> Either [ConfigError] ()
validateMirrorSettings app = traverse_ validate (Map.toList (cfgMounts app))
  where
    validate (eco, mount) = do
        for_ (mntMirrorTarget mount) $ \target -> do
            case (meWrite target, meUploadUrl target) of
                (WriteCodeArtifact{}, Just upload) -> do
                    original <- first pure (resolveStoreBackend eco target)
                    uploaded <- first pure (resolveUploadBackend eco target upload)
                    unless (original == uploaded) (refuse "CodeArtifact uploadUrl must address the same mirror repository")
                _ -> pure ()
            case meWrite target of
                WriteCodeArtifact{} -> unless (maybe True (== "aws") (meUsername target)) (refuse "CodeArtifact requires the aws credential username")
                _ -> pure ()
        for_ (mntPublicationTarget mount) $ \target -> do
            first pure (vetPublicationRepository eco target)
            when (isJust (peUsername target) && isNothing (peToken target)) $
                refuse "publicationTarget.username requires a static token"
            when (tgtTag (peTarget target) == TagCodeArtifact && maybe False (/= "aws") (peUsername target)) $
                refuse "CodeArtifact requires the aws credential username"

    refuse = Left . pure . ParseError
