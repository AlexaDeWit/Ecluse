-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Complete native captures pin production outputs and compare independently decoded source rows.
module Ecluse.Core.Registry.Cargo.StreamingSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Package (PackageInfo (infoVersions))
import Ecluse.Core.Registry.CachedDocument (cargoCached)
import Ecluse.Core.Registry.Cargo.Document (cargoRecords)
import Ecluse.Core.Registry.Cargo.Metadata (cargoRead)
import Ecluse.Core.Registry.Cargo.Serving (assembleCargoDocument, serialiseCargoDocument)
import Ecluse.Core.Registry.Metadata (Manifest (..))
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage), cargoCaptureUpstream, cargoCorpusPackages, cpName, readCapture)
import Ecluse.Test.Corpus.Cargo (cargoCaptureDiscovery, projectCargoTree)
import Ecluse.Test.Corpus.Outputs (CorpusRead (..), captureOutputs, recordedOutputs, rendered, selectedFacts)
import Ecluse.Test.Registry.Metadata.Fetch (captureManifest)
import Ecluse.Test.Registry.Metadata.Rendering (MetadataRendering (..))
import Ecluse.Test.Support (expectRight)

-- | Missing output fixtures fail with the complete computed lines for review in normal CI.
spec :: Spec
spec = describe "Cargo corpus captures" $ for_ cargoCorpusPackages $ \package -> do
    it ("reproduces recorded native outputs for " <> toString (cpName package)) $ do
        discovery <- expectRight cargoCaptureDiscovery
        bytes <- readCapture package
        let provider =
                CorpusRead
                    { crUpstream = cargoCaptureUpstream
                    , crRead = cargoRead discovery
                    , crMetadata = MetadataRendering assembleCargoDocument serialiseCargoDocument
                    , crVersionReads = \selected _ _ -> [("selected", rendered (selectedFacts <$> selected))]
                    , crDocumentReads = \_ _ -> []
                    }
        actual <- captureOutputs provider package bytes >>= expectRight
        expected <- recordedOutputs package
        actual `shouldBe` expected
    it ("agrees with independent held rows for " <> toString (cpName package)) $ do
        discovery <- expectRight cargoCaptureDiscovery
        bytes <- readCapture package
        (expected, reference) <- expectRight (projectCargoTree defaultLimits (cpPackage package) bytes)
        actual <- captureManifest (cargoRead discovery) cargoCaptureUpstream (cpPackage package) [bytes] >>= expectRight
        manifestInfo actual `shouldBe` expected
        Map.size (infoVersions expected) `shouldSatisfy` (> 0)
        fmap cargoRecords (snd cargoCached (manifestRaw actual)) `shouldBe` fmap cargoRecords (snd cargoCached reference)
