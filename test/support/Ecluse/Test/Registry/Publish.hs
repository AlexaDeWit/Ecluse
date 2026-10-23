-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Publication fixtures pass through production inventory and byte verification.
module Ecluse.Test.Registry.Publish (anonymousPublishAt, publishOnlyFile, checkedNpmUpload, isMalformedBody) where

import Network.HTTP.Client (defaultManagerSettings, newManager)

import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Package (PackageDetails (pkgName, pkgVersion))
import Ecluse.Core.Registry (PublishBodyFault (PublishMalformed), PublishFault (PublishSourceUnavailable))
import Ecluse.Core.Registry.Metadata (VersionDoc (vdDetails))
import Ecluse.Core.Registry.MirrorUpload
import Ecluse.Core.Registry.Npm.Publish (NpmUploadMetadata, prepareNpmSource, prepareNpmUpload)
import Ecluse.Core.Registry.Publish
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Test.Deadline (untracedClient)
import Ecluse.Test.Port (passthroughTracingPort)

-- | An anonymous publication of one codec to a target through the shared transport, over deadlines the caller sweeps.
anonymousPublishAt :: Deadlines -> PublishCodec -> RegistryUrl -> IO MirrorPublish
anonymousPublishAt deadlines codec target = do
    manager <- newManager defaultManagerSettings
    let transport = MirrorTransport{ptClient = untracedClient manager deadlines, ptMintCredential = pure Nothing, ptLimits = defaultLimits, ptTracing = passthroughTracingPort}
    pure (newMirrorPublish transport target target codec)

-- | Exercise a single-file codec through the bound transport without bypassing byte verification.
publishOnlyFile :: MirrorPublish -> VersionDoc -> PublishPlan -> ByteString -> IO (Either PublishFault ())
publishOnlyFile publish doc plan bytes = case mpPrepareVersion publish (pkgName details) (pkgVersion details) doc of
    Left fault -> pure (Left fault)
    Right prepared -> case inventoryFiles (pmvInventory prepared) of
        file :| [] -> case verifyMirrorBytes file bytes of
            Left fault -> pure (Left fault)
            Right verified -> pmvPrepareUpload prepared (const (pure ())) file verified >>= either (pure . Left) (`publishPrepared` plan)
        _ -> pure (Left (PublishSourceUnavailable "the publication fixture requires one physical file"))
  where
    details = vdDetails doc

-- | Build npm encoder inputs with the same checked bytes as the worker.
checkedNpmUpload :: VersionDoc -> ByteString -> Either PublishFault (MirrorUploadData NpmUploadMetadata)
checkedNpmUpload doc bytes = do
    (inventory, metadata) <- prepareNpmSource (pkgName details) (pkgVersion details) doc
    case inventoryFiles inventory of
        file :| [] -> verifyMirrorBytes file bytes >>= \verified -> prepareNpmUpload inventory metadata file verified
        _ -> Left (PublishSourceUnavailable "the npm fixture requires one physical file")
  where
    details = vdDetails doc

-- | Whether the upload guard refused a client's publish body as unreadable, the @400@ case.
isMalformedBody :: Either PublishBodyFault a -> Bool
isMalformedBody = \case
    Left (PublishMalformed _) -> True
    _ -> False
