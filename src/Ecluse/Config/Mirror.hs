-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Validate configured mirror endpoints and credential declarations.
module Ecluse.Config.Mirror (validateMirrorSettings) where

import Data.Map.Strict qualified as Map

import Ecluse.Config.Target (resolveStoreBackend, resolveUploadBackend, vetPublicationRepository)
import Ecluse.Config.Types
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Registry.Adapter (RegistryAdapter (adapterPublish), adapterFor)
import Ecluse.Core.Registry.Adapter.Capability (AdapterPublish (publishUploadEndpoint), UploadEndpointRequirement (ReadRootUpload))

{- | Reject an @uploadUrl@ the protocol cannot use, then conflicting repository and credential
declarations, before role-specific checks. "Ecluse.Composition.Endpoints" vets upload API hosts.
-}
validateMirrorSettings :: AppConfig -> Either [ConfigError] ()
validateMirrorSettings app = case concatMap refusedUploadUrls mounts of
    [] -> traverse_ validate mounts
    refused -> Left refused
  where
    mounts = Map.toList (cfgMounts app)
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

-- A protocol that derives its write address from url takes no uploadUrl, which would silently send uploads elsewhere.
refusedUploadUrls :: (Ecosystem, MountConfig) -> [ConfigError]
refusedUploadUrls (eco, mount) =
    [ UploadUrlRefused eco (key <> "." <> storeTagName (tgtTag target) <> ".uploadUrl")
    | derivesUploadAddress
    , (key, target, Just _) <- mirrorTarget <> publicationTarget
    ]
  where
    derivesUploadAddress = (publishUploadEndpoint <$> (adapterPublish =<< adapterFor eco)) == Just ReadRootUpload
    mirrorTarget = [("mirrorTarget", meTarget endpoint, meUploadUrl endpoint) | endpoint <- maybeToList (mntMirrorTarget mount)]
    publicationTarget = [("publicationTarget", peTarget endpoint, peUploadUrl endpoint) | endpoint <- maybeToList (mntPublicationTarget mount)]
