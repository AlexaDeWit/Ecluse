-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Temporary measurement entry point. Task owns preparation and sample ordering.
module Main (main) where

import Data.ByteString.Lazy qualified as LBS
import System.Directory (createDirectory, renameFile)
import System.FilePath ((</>))

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.OsvIndex.Artifact (compareSnapshots, inspectArtifact, selectedNames)
import Ecluse.OsvIndex.Contract
import Ecluse.OsvIndex.Controls (runControls)
import Ecluse.OsvIndex.Evidence (auditExperiment)
import Ecluse.OsvIndex.Fixtures (prepareEdges)
import Ecluse.OsvIndex.Identity (writeManifest)
import Ecluse.OsvIndex.Native (recordTrial)
import Ecluse.OsvIndex.Reader (checkReaders, readerBatch)
import Ecluse.Test.Contract.Manifest (failRunner)
import Ecluse.Test.Corpus.Advisories (AdvisoryInputs (..), compileAdvisoryInputs, corpusAdvisories)

-- | Execute one named leaf operation.
main :: IO ()
main =
    getArgs >>= \arguments -> case arguments of
        ["corpus", eco, root] ->
            parseEco eco >>= \ecosystem -> do
                inputs <- corpusAdvisories ecosystem
                LBS.writeFile (root </> (eco <> "-corpus.zip")) (aiOsvZip inputs)
                LBS.writeFile (root </> (eco <> "-corpus-epss.gz")) (aiEpssFeed inputs)
        ["compile", root, _, _] -> measured root arguments
        ["reader-batch", root, _, _, _, _] -> measured root arguments
        ["inspect", db, output] -> inspectArtifact db output
        ["compare", left, right, output] -> compareSnapshots left right output
        ["select", db, output, capture] -> selectedNames db output (toText capture)
        ["reader-check", eco, left, right, selection, output] -> parseEco eco >>= \ecosystem -> checkReaders ecosystem left right selection output
        ["edges", eco, left, right, directory, output] ->
            parseEco eco >>= \ecosystem -> do
                prepareEdges left right directory
                checkReaders ecosystem (directory </> "A.db") (directory </> "B.db") (directory </> "selection.json") output
        ["manifest", root] -> writeManifest root
        ["controls", root] -> runControls root
        ["audit", root] -> auditExperiment root
        _ -> failRunner "expected corpus, compile, inspect, compare, select, reader-check, reader-batch, edges, manifest, controls or audit"

measured :: FilePath -> [String] -> IO ()
measured root arguments = do
    trial <- maybe (failRunner "sample is outside the independent plan") pure (parseTrial root arguments)
    recordTrial root trial $ case trWork trial of
        QueryBatch mode _ ->
            readerBatch
                mode
                (caseEco (trCase trial))
                (root </> trialDirectory (Trial (trCase trial) (trVariant trial) Warmup) </> "db/artifact.db")
                (root </> "readers" </> caseName (trCase trial) </> "selection-A.json")
                (root </> trialDirectory trial </> "result.json")
        _ -> compileTrial root trial

compileTrial :: FilePath -> Trial -> IO ()
compileTrial root trial = case trialInputs trial of
    [zipPath, epssPath] -> do
        let output = root </> trialDirectory trial </> "db"
        createDirectory output
        inputs <- AdvisoryInputs <$> readFileLBS (root </> zipPath) <*> readFileLBS (root </> epssPath)
        artifact <- compileAdvisoryInputs (caseEco (trCase trial)) output inputs
        renameFile artifact (output </> "artifact.db")
    _ -> failRunner "compile input obligation is invalid"

parseEco :: String -> IO Ecosystem
parseEco = \case
    "npm" -> pure Npm
    "pypi" -> pure PyPI
    _ -> failRunner "expected npm or pypi"
