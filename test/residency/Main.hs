-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Residency tests and their isolated metadata probe entry point.
Explicit imports keep integration fixtures from registering unrelated examples.
-}
module Main (main) where

import Ecluse.Core.Registry.JsonStreamResidencySpec qualified as JsonStreamResidencySpec
import Ecluse.Core.Registry.MetadataResidencySpec qualified as MetadataResidencySpec
import Ecluse.Core.Registry.Npm.ReaderResidencySpec qualified as NpmReaderResidencySpec
import Ecluse.Core.Registry.PyPI.ReaderResidencySpec qualified as PyPIReaderResidencySpec
import Ecluse.Core.Server.MemoryModel.Probe (SelectedShape (SelectedControl, SelectedValue), childMain, packageMain, probe, probeEvaluation, probeListing)
import Ecluse.Core.Server.MemoryModelResidencySpec qualified as MemoryModelResidencySpec
import Ecluse.Core.Server.Pipeline.TarballResidencySpec qualified as TarballResidencySpec
import System.Environment qualified as Environment
import Test.Hspec (hspec)

main :: IO ()
main =
    Environment.getArgs >>= \case
        ["--metadata-probe", shape, path] -> childMain probe shape path
        ["--metadata-evaluation-probe", path] -> packageMain probeEvaluation path
        ["--metadata-listing-probe", path] -> packageMain probeListing path
        ["--metadata-source-probe", mode, name, version, limit, path] -> MemoryModelResidencySpec.sourceMain "npm" mode name version limit path
        ["--metadata-source-probe", ecosystem, mode, name, version, limit, path] -> MemoryModelResidencySpec.sourceMain ecosystem mode name version limit path
        ["--metadata-selected-retention-probe", ecosystem, name, version, limit, path] -> MemoryModelResidencySpec.selectedMain SelectedValue ecosystem name version limit path
        ["--metadata-selected-control-probe", ecosystem, name, version, limit, path] -> MemoryModelResidencySpec.selectedMain SelectedControl ecosystem name version limit path
        _ -> hspec $ do
            JsonStreamResidencySpec.spec
            MetadataResidencySpec.spec
            NpmReaderResidencySpec.spec
            PyPIReaderResidencySpec.spec
            TarballResidencySpec.spec
            MemoryModelResidencySpec.spec
