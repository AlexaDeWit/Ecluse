-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Residency tests and their isolated metadata probe entry point.
Explicit imports keep integration fixtures from registering unrelated examples.
-}
module Main (main) where

import Data.JsonStream.LexerResidencySpec qualified as LexerResidencySpec
import Ecluse.Core.Mirror.State.WeightResidencySpec qualified as StateWeightResidencySpec
import Ecluse.Core.Registry.JsonStreamResidencySpec qualified as JsonStreamResidencySpec
import Ecluse.Core.Registry.MetadataResidencySpec qualified as MetadataResidencySpec
import Ecluse.Core.Registry.Npm.ReaderResidencySpec qualified as NpmReaderResidencySpec
import Ecluse.Core.Registry.PyPI.ArchiveResidencySpec qualified as ArchiveResidencySpec
import Ecluse.Core.Registry.PyPI.ReaderResidencySpec qualified as PyPIReaderResidencySpec
import Ecluse.Core.Server.MemoryModel.Probe (SelectedShape (SelectedControl, SelectedValue), childMain, packageMain, probe, probeEvaluation, probeListing, probeMerge)
import Ecluse.Core.Server.MemoryModelResidencySpec qualified as MemoryModelResidencySpec
import Ecluse.Core.Server.MirrorViewResidencySpec qualified as MirrorViewResidencySpec
import Ecluse.Core.Server.Pipeline.TarballResidencySpec qualified as TarballResidencySpec
import System.Environment qualified as Environment
import Test.Hspec (hspec)

main :: IO ()
main =
    Environment.getArgs >>= \case
        ["--mirror-state-probe", mode, path] -> StateWeightResidencySpec.probeMain mode path
        ["--archive-probe", mode, path] -> ArchiveResidencySpec.probeMain mode path
        ["--mirror-read-probe", phase, directory] -> MirrorViewResidencySpec.probeMain phase directory
        ["--metadata-probe", shape, path] -> childMain probe shape path
        ["--metadata-evaluation-probe", path] -> packageMain probeEvaluation path
        ["--metadata-listing-probe", path] -> packageMain probeListing path
        ["--metadata-merge-probe", private, public, path] -> packageMain (probeMerge private public) path
        ["--metadata-source-probe", ecosystem, mode, name, version, limit, path] -> MemoryModelResidencySpec.sourceMain ecosystem mode name version limit path
        ["--metadata-selected-retention-probe", ecosystem, name, version, limit, path] -> MemoryModelResidencySpec.selectedMain SelectedValue ecosystem name version limit path
        ["--metadata-selected-control-probe", ecosystem, name, version, limit, path] -> MemoryModelResidencySpec.selectedMain SelectedControl ecosystem name version limit path
        _ -> hspec $ do
            StateWeightResidencySpec.spec
            ArchiveResidencySpec.spec
            MirrorViewResidencySpec.spec
            LexerResidencySpec.spec
            JsonStreamResidencySpec.spec
            MetadataResidencySpec.spec
            NpmReaderResidencySpec.spec
            PyPIReaderResidencySpec.spec
            TarballResidencySpec.spec
            MemoryModelResidencySpec.spec
