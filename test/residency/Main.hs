-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Residency tests and their isolated metadata probe entry point.
Explicit imports keep integration fixtures from registering unrelated examples.
-}
module Main (main) where

import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Data.JsonStream.LexerResidencySpec qualified as LexerResidencySpec
import Ecluse.Core.Mirror.State.WeightResidencySpec qualified as StateWeightResidencySpec
import Ecluse.Core.Registry.Cargo.ReaderResidencySpec qualified as CargoReaderResidencySpec
import Ecluse.Core.Registry.JsonStreamResidencySpec qualified as JsonStreamResidencySpec
import Ecluse.Core.Registry.MetadataResidencySpec qualified as MetadataResidencySpec
import Ecluse.Core.Registry.Npm.ReaderResidencySpec qualified as NpmReaderResidencySpec
import Ecluse.Core.Registry.PyPI.ArchiveResidencySpec qualified as ArchiveResidencySpec
import Ecluse.Core.Registry.PyPI.ReaderResidencySpec qualified as PyPIReaderResidencySpec
import Ecluse.Core.Registry.RubyGems.ReaderResidencySpec qualified as RubyGemsReaderResidencySpec
import Ecluse.Core.Registry.RubyGems.ServingResidencySpec qualified as RubyGemsServingResidencySpec
import Ecluse.Core.Server.Admission.ArtifactResidencySpec qualified as ArtifactAdmissionResidencySpec
import Ecluse.Core.Server.MemoryModel.CalibrationResidencySpec qualified as CalibrationResidencySpec
import Ecluse.Core.Server.MemoryModel.Probe (SelectedShape (SelectedControl, SelectedValue), childMain, packageMain, probe, probeEvaluation, probeListing, probeListingFor, probeMerge, probeMergeFor)
import Ecluse.Core.Server.MemoryModel.ProbeMode qualified as Mode
import Ecluse.Core.Server.MemoryModel.ProbeModeResidencySpec qualified as ProbeModeResidencySpec
import Ecluse.Core.Server.MemoryModelResidencySpec qualified as MemoryModelResidencySpec
import Ecluse.Core.Server.MetadataGzipResidencySpec qualified as MetadataGzipResidencySpec
import Ecluse.Core.Server.MirrorViewResidencySpec qualified as MirrorViewResidencySpec
import Ecluse.Core.Server.Pipeline.PackumentResidencySpec qualified as PackumentResidencySpec
import Ecluse.Core.Server.Pipeline.Tarball.VerifiedResidencySpec qualified as VerifiedResidencySpec
import Ecluse.Core.Server.Pipeline.TarballResidencySpec qualified as TarballResidencySpec
import Ecluse.Core.Server.Response.SummaryResidencySpec qualified as SummaryResidencySpec
import System.Environment qualified as Environment
import Test.Hspec (hspec)

main :: IO ()
main =
    Environment.getArgs >>= \case
        ["--mirror-state-probe", mode, path] -> StateWeightResidencySpec.probeMain mode path
        ["--archive-probe", mode, path] -> ArchiveResidencySpec.probeMain mode path
        ["--verified-artifact-probe", mode, path] -> VerifiedResidencySpec.probeMain mode path
        ["--gzip-probe", mode, path] -> MetadataGzipResidencySpec.probeMain mode path
        ["--mirror-read-probe", phase, directory] -> MirrorViewResidencySpec.probeMain phase directory
        arguments | Just (mode, path) <- Mode.parseProbeMode arguments -> runProbe mode path
        _ -> hspec $ do
            CalibrationResidencySpec.spec
            ProbeModeResidencySpec.spec
            StateWeightResidencySpec.spec
            ArchiveResidencySpec.spec
            MetadataGzipResidencySpec.spec
            MirrorViewResidencySpec.spec
            LexerResidencySpec.spec
            JsonStreamResidencySpec.spec
            SummaryResidencySpec.spec
            MetadataResidencySpec.spec
            NpmReaderResidencySpec.spec
            PyPIReaderResidencySpec.spec
            CargoReaderResidencySpec.spec
            RubyGemsReaderResidencySpec.spec
            RubyGemsServingResidencySpec.spec
            TarballResidencySpec.spec
            VerifiedResidencySpec.spec
            ArtifactAdmissionResidencySpec.spec
            PackumentResidencySpec.spec
            MemoryModelResidencySpec.spec

runProbe :: Mode.ProbeMode -> FilePath -> IO ()
runProbe mode path = case mode of
    Mode.Retained shape -> childMain probe shape path
    Mode.Evaluation -> packageMain probeEvaluation path
    Mode.Listing -> packageMain probeListing path
    Mode.ListingFor ecosystem name -> do
        (package, _) <- MemoryModelResidencySpec.probeIdentity ecosystem name "1.0.0"
        probeListingFor package path >>= LBS.putStr . encode
    Mode.MergeFor ecosystem name private -> do
        (package, _) <- MemoryModelResidencySpec.probeIdentity ecosystem name "1.0.0"
        probeMergeFor package private path >>= LBS.putStr . encode
    Mode.ListingCensus -> packageMain PackumentResidencySpec.probeListingCensus path
    Mode.Merge private public -> packageMain (probeMerge private public) path
    Mode.Source ecosystem source name version limit -> MemoryModelResidencySpec.sourceMain ecosystem source name version limit path
    Mode.SelectedRetention ecosystem name version limit -> MemoryModelResidencySpec.selectedMain SelectedValue ecosystem name version limit path
    Mode.SelectedControl ecosystem name version limit -> MemoryModelResidencySpec.selectedMain SelectedControl ecosystem name version limit path
