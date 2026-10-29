-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Archive member paths and distribution metadata coordinates.
module Ecluse.Core.Registry.PyPI.Archive.Path (checkPath, metadataPath) where

import Data.Text qualified as T
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (ParseError, parseRefusal)
import Ecluse.Core.Registry.Archive.Path (checkPath)
import Ecluse.Core.Registry.PyPI.Project (DistributionKind (..), FileCoordinate (..), fileCoordinate)
import Ecluse.Core.Version (Version)

-- | Select only the admitted distribution metadata directory.
metadataPath :: PackageName -> Version -> DistributionKind -> [Text] -> Either ParseError Bool
metadataPath package version kind parts = case (kind, parts) of
    (Wheel, [root, "METADATA"]) | Just stem <- T.stripSuffix ".dist-info" root -> matchesIdentity stem
    (Sdist, [root, "PKG-INFO"]) -> matchesIdentity root
    _ -> Right False
  where
    matchesIdentity stem = case fileCoordinate package (stem <> ".tar.gz") of
        Just coordinate | fcVersion coordinate == version -> Right True
        _ -> parseRefusal "archive metadata directory disagrees with admitted identity"
