-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Whole-tree PyPI reference projection for fixtures and baseline measurements, and a read of file
names through a memo.
-}
module Ecluse.Test.Registry.PyPI.Project (projectSimpleIndexFromValue, readThrough) where

import Data.Aeson (Value)
import Data.Aeson.Types (parseEither, parseJSON)

import Ecluse.Core.Package (PackageInfo, PackageName)
import Ecluse.Core.Registry (ParseError (ParseError))
import Ecluse.Core.Registry.PyPI.Project (FileCoordinate, FilenameMemo, fileCoordinate, projectName, projectSimpleIndex)
import Ecluse.Core.Registry.PyPI.Wire (IndexFile (ifFilename), SimpleIndex (..))
import Ecluse.Core.Registry.WireSupport (Projection, checkNameAgreement)

-- | Project caller-owned JSON with the same typed file semantics as the streaming reader.
projectSimpleIndexFromValue :: PackageName -> Value -> Either ParseError (Projection PackageInfo)
projectSimpleIndexFromValue requested value = do
    index <- first (ParseError . toText) (parseEither parseJSON value)
    reported <- projectName (siName index)
    let files = [(file, fileCoordinate reported (ifFilename file)) | file <- siFiles index]
    pure (checkNameAgreement requested reported (projectSimpleIndex reported (siInvalidEntries index) files))

-- | Read file names in order through a memo read: the memo after the last name, and each name's coordinate.
readThrough :: (FilenameMemo -> Text -> (Maybe FileCoordinate, FilenameMemo)) -> FilenameMemo -> [Text] -> (FilenameMemo, [Maybe FileCoordinate])
readThrough readName = mapAccumL (\memo file -> swap (readName memo file))
