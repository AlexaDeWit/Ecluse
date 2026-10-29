-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Platform-independent paths for in-memory archive inspection.
module Ecluse.Core.Registry.Archive.Path (checkPath, resolveArchivePath) where

import Control.Monad (foldM)
import Data.Text qualified as T
import Ecluse.Core.Registry (ParseError, parseRefusal)

-- | Reject absolute, escaping, and platform-dependent member names.
checkPath :: Text -> Either ParseError [Text]
checkPath path = do
    let parts = T.splitOn "/" (fromMaybe path (T.stripSuffix "/" path))
    when (T.compareLength path 4096 == GT || T.any (\c -> c < ' ' || c == '\\' || c == ':' || c == '\DEL') path) (parseRefusal "unsafe archive path")
    when (any (`elem` ["", ".", ".."]) parts) (parseRefusal "unsafe archive path")
    pure parts

-- | Resolve package-relative references or links while refusing an escape from the archive.
resolveArchivePath :: [Text] -> Text -> Either ParseError Text
resolveArchivePath base target = do
    when (T.null target || T.isPrefixOf "/" target || T.compareLength target 4096 == GT || T.any (\c -> c < ' ' || c == '\\' || c == ':' || c == '\DEL') target) (parseRefusal "unsafe tar link target")
    parts <- reverse <$> foldM step (reverse base) (T.splitOn "/" target)
    void (checkPath (T.intercalate "/" parts))
    pure (T.copy (T.intercalate "/" parts))
  where
    step held "" = Right held
    step held "." = Right held
    step [] ".." = parseRefusal "tar link escapes its archive"
    step (_ : rest) ".." = Right rest
    step held part = Right (part : held)
