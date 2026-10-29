-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}

-- | Python Core Metadata selection over the shared complete tar traversal.
module Ecluse.Core.Registry.PyPI.Archive.Tar (extractTar) where

import Conduit
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (ParseError, parseRefusal)
import Ecluse.Core.Registry.Archive.Path (checkPath)
import Ecluse.Core.Registry.Archive.Tar (TarDecode, TarMember (Regular), resolveTarMember, scanTar, tarContents, tarMembers)
import Ecluse.Core.Registry.PyPI.Archive.Path (metadataPath)
import Ecluse.Core.Registry.PyPI.Project (DistributionKind (Sdist))
import Ecluse.Core.Security.Limits (Limits)
import Ecluse.Core.Version (Version)

-- | A linked metadata file needs at most one more local pass over the same source.
extractTar :: Limits -> (Int -> IO ()) -> PackageName -> Version -> (forall a. (ConduitT () ByteString TarDecode () -> IO a) -> IO a) -> IO (Either ParseError ByteString)
extractTar limits charge package version withSource = do
    firstPass <- scanTar limits charge select withSource
    case firstPass >>= locate of
        Left fault -> pure (Left fault)
        Right (Left bytes) -> pure (Right bytes)
        Right (Right target) -> do
            secondPass <- scanTar limits charge (Right . (== target) . T.intercalate "/") withSource
            pure $ secondPass >>= maybe (parseRefusal "metadata link target has no regular-file content") Right . Map.lookup target . tarContents
  where
    select = metadataPath package version Sdist
    locate scan = do
        selected <- filterM (checkPath >=> select) (Map.keys (tarMembers scan))
        path <- case selected of
            [] -> parseRefusal "archive has no Core Metadata"
            [path] -> Right path
            _ -> parseRefusal "ambiguous archive Core Metadata"
        target <- resolveTarMember limits scan path
        case Map.lookup target (tarMembers scan) of
            Just Regular -> pure (maybe (Right target) Left (Map.lookup target (tarContents scan)))
            _ -> parseRefusal "Core Metadata link does not name a regular file"
