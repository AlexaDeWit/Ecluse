-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Capture-key controls preserve v1 ownership without recording any native expected output values.
module Ecluse.Test.Corpus.OutputsSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, RubyGems))
import Ecluse.Core.Package (mkPackageName)
import Ecluse.Test.Corpus (CorpusPackage (..), CorpusTier (Medium), cargoCorpusPackages, corpusPackages, cpName, pypiCorpusPackages)
import Ecluse.Test.Corpus.Outputs (captureOutputKey, recordedOutputsFrom)

spec :: Spec
spec = describe "frozen corpus output identity" $ do
    it "preserves the fixed legacy identities for all fifteen original captures" $ do
        let packages = corpusPackages <> pypiCorpusPackages <> cargoCorpusPackages
        length packages `shouldBe` 15
        for_ packages $ \package -> captureOutputKey package `shouldBe` cpName package
    it "separates identical package names by their capture paths and ecosystems" $ do
        captureOutputKey cargoCapture `shouldBe` "aws-sdk-ec2"
        captureOutputKey rubyCapture `shouldBe` "v2:bench/corpus/rubygems/aws-sdk-ec2.info"
        let legacyControl = "aws-sdk-ec2\tlegacy-selection-control"
            nativeControl = "v2:bench/corpus/rubygems/aws-sdk-ec2.info\tnative-selection-control"
            rows = [nativeControl, legacyControl]
        recordedOutputsFrom cargoCapture rows `shouldBe` [legacyControl]
        recordedOutputsFrom rubyCapture rows `shouldBe` [nativeControl]
        recordedOutputsFrom rubyCapture [legacyControl] `shouldBe` []
    it "does not grant legacy ownership to a new capture or a different package" $ do
        let moved = cargoCapture{cpPath = "bench/corpus/cargo/aws-sdk-ec2.second.ndjson"}
            renamed = cargoCapture{cpPackage = mkPackageName Cargo Nothing "different"}
            foreignCapture = cargoCapture{cpPackage = mkPackageName Npm Nothing "aws-sdk-ec2"}
        for_ [moved, renamed, foreignCapture] $ \package -> do
            captureOutputKey package `shouldSatisfy` T.isPrefixOf "v2:"
            recordedOutputsFrom package ["aws-sdk-ec2\tlegacy-selection-control"] `shouldBe` []
    it "keeps malformed owned rows and output order for the unchanged exact comparison" $ do
        let malformed = "aws-sdk-ec2"
            unknownOutput = "aws-sdk-ec2\tunknown-output\tbad-size\tbad-hash"
            rows = ["# comment", "unknown\toutput", malformed, unknownOutput]
        recordedOutputsFrom cargoCapture rows `shouldBe` [malformed, unknownOutput]
    it "does not match an identity prefix or another qualified capture" $
        recordedOutputsFrom rubyCapture ["v2:bench/corpus/rubygems/aws-sdk-ec2.info.extra\tother", "v2:bench/corpus/rubygems/rails.info\tother"] `shouldBe` []

cargoCapture :: CorpusPackage
cargoCapture = CorpusPackage (mkPackageName Cargo Nothing "aws-sdk-ec2") "bench/corpus/cargo/aws-sdk-ec2.index.ndjson" Medium 1

rubyCapture :: CorpusPackage
rubyCapture = CorpusPackage (mkPackageName RubyGems Nothing "aws-sdk-ec2") "bench/corpus/rubygems/aws-sdk-ec2.info" Medium 1
