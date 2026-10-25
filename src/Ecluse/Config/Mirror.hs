-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Validate configured mirror endpoints and credential declarations.
module Ecluse.Config.Mirror (validateMirrorSettings) where

import Data.Map.Strict qualified as Map

import Ecluse.Config.Target (resolveStoreBackend, resolveUploadBackend, vetPublicationRepository)
import Ecluse.Config.Types
import Ecluse.Core.Security (hostPortAddress)
import Ecluse.Core.Security.Egress (registryUrlText)

-- | Reject conflicting endpoint and credential declarations before role-specific checks.
validateMirrorSettings :: AppConfig -> Either [ConfigError] ()
validateMirrorSettings app = traverse_ validate (Map.toList (cfgMounts app))
  where
    validate (eco, mount) = do
        for_ (mntMirrorTarget mount) $ \target -> do
            for_ (meUploadUrl target) $ \upload -> do
                case meWrite target of
                    WriteCodeArtifact{} -> do
                        original <- first pure (resolveStoreBackend eco target)
                        uploaded <- first pure (resolveUploadBackend eco target upload)
                        unless (original == uploaded) (refuse "CodeArtifact uploadUrl must address the same mirror repository")
                    _ -> pure ()
                when (hostPortAddress (registryUrlText upload) `elem` publicHosts) $
                    refuse "mirrorTarget.uploadUrl must not address a configured public upstream host"
            case meWrite target of
                WriteCodeArtifact{} -> unless (maybe True (== "aws") (meUsername target)) (refuse "CodeArtifact requires the aws credential username")
                _ -> pure ()
        for_ (mntPublicationTarget mount) $ \target -> do
            first pure (vetPublicationRepository eco target)
            for_ (peUploadUrl target) $ \upload ->
                when (hostPortAddress (registryUrlText upload) `elem` publicHosts) $
                    refuse "publicationTarget.uploadUrl must not address a configured public upstream host"
            when (isJust (peUsername target) && isNothing (peToken target)) $
                refuse "publicationTarget.username requires a static token"
            when (tgtTag (peTarget target) == TagCodeArtifact && maybe False (/= "aws") (peUsername target)) $
                refuse "CodeArtifact requires the aws credential username"

    publicHosts = map (hostPortAddress . registryUrlText . mntPublicUpstream) (Map.elems (cfgMounts app))
    refuse = Left . pure . ParseError
