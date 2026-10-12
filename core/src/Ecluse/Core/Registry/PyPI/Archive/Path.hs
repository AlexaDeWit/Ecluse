-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Archive member paths and distribution metadata coordinates.
module Ecluse.Core.Registry.PyPI.Archive.Path (checkPath, metadataPath) where

import Data.Text qualified as T
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.PyPI.Project (DistributionKind (..), FileCoordinate (..), fileCoordinate)
import Ecluse.Core.Version (Version)

-- | Reject absolute, escaping, and platform-dependent member names.
checkPath :: Text -> Either ParseError [Text]
checkPath path = do
    let parts = T.splitOn "/" (fromMaybe path (T.stripSuffix "/" path))
    when (T.length path > 4096 || T.any (\c -> c < ' ' || c == '\\' || c == ':' || c == '\DEL') path) (bad "unsafe archive path")
    when (any (`elem` ["", ".", ".."]) parts) (bad "unsafe archive path")
    pure parts

-- | Select only the admitted distribution metadata directory.
metadataPath :: PackageName -> Version -> DistributionKind -> [Text] -> Either ParseError Bool
metadataPath package version kind parts = case (kind, parts) of
    (Wheel, [root, "METADATA"]) | Just stem <- T.stripSuffix ".dist-info" root -> identity stem
    (Sdist, [root, "PKG-INFO"]) -> identity root
    _ -> Right False
  where
    identity stem = case fileCoordinate package (stem <> ".tar.gz") of
        Just coordinate | fcVersion coordinate == version -> Right True
        _ -> bad "archive metadata directory disagrees with admitted identity"

bad :: Text -> Either ParseError a
bad = Left . ParseError
