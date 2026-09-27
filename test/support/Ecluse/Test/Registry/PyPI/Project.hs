-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Whole-tree PyPI reference projection for fixtures and baseline measurements.
module Ecluse.Test.Registry.PyPI.Project (projectSimpleIndexFromValue) where

import Data.Aeson (Value)
import Data.Aeson.Types (parseEither, parseJSON)

import Ecluse.Core.Package (PackageInfo, PackageName)
import Ecluse.Core.Registry (ParseError (ParseError))
import Ecluse.Core.Registry.PyPI.Project (fileCoordinate, projectName, projectSimpleIndex)
import Ecluse.Core.Registry.PyPI.Wire (IndexFile (ifFilename), SimpleIndex (..))
import Ecluse.Core.Registry.WireSupport (Projection, checkNameAgreement)

-- | Project caller-owned JSON with the same typed file semantics as the streaming reader.
projectSimpleIndexFromValue :: PackageName -> Value -> Either ParseError (Projection PackageInfo)
projectSimpleIndexFromValue requested value = do
    index <- first (ParseError . toText) (parseEither parseJSON value)
    reported <- projectName (siName index)
    let files = [(file, fileCoordinate reported (ifFilename file)) | file <- siFiles index]
    pure (checkNameAgreement requested reported (projectSimpleIndex reported (siInvalidEntries index) files))
