-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Rebound artifacts must still satisfy the independent fixed workload obligations.
module Ecluse.BenchReport.RefactorInputControls (inputControls) where

import Data.Aeson (Value (Object), toJSON, (.:))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.Directory (copyFile, createDirectoryIfMissing, removeFile)
import System.FilePath (takeDirectory, (</>))
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.BenchReport.RefactorEvidence (Binding (..), bindingFaults)
import Ecluse.BenchReport.RefactorEvidenceControls (requireAccepted, requireRejected)
import Ecluse.BenchReport.RefactorFiles (writeComparisonText)
import Ecluse.BenchReport.RefactorInputAudit (auditInputs, expectedCaptureSizes, expectedProbeInputs)
import Ecluse.BenchReport.RefactorInputs (Input (..), listingWorkloads, prepareInputs, versionCommands)
import Ecluse.Test.Contract.Manifest (digest, digestFile, jsonLine, readJsonFile, writeJsonFile)
import Ecluse.Test.Corpus (CorpusPackage (cpPath), corpusPackages, pypiCorpusPackages)

-- | Each mutation gets fresh matching digests before the workload audit must reject it.
inputControls :: IO ()
inputControls = withSystemTempDirectory "refactor-input-controls" $ \root -> do
    prepareInputs (root </> "inputs")
    let sourcePaths = "bench/corpus/pins.json" : map cpPath (corpusPackages <> pypiCorpusPackages)
        archivePaths = ["source-inputs-" <> side </> path | side <- ["A", "B"], path <- sourcePaths]
        declarations = map ("inputs" </>) ["versions.json", "versions.tsv", "listings.json", "capture-sizes.json"]
        listingPaths = concat [[inputPath input, "inputs" </> toString (inputId input) <> ".expected"] | (input, _, _) <- listingWorkloads]
        paths = declarations <> listingPaths <> archivePaths
    forM_ ["A", "B"] $ \side -> forM_ sourcePaths $ \path -> do
        let target = root </> "source-inputs-" <> side </> path
        createDirectoryIfMissing True (takeDirectory target)
        copyFile path target
    auditInputs root >>= requireAccepted "valid fixed workloads"
    originals <- Map.fromList <$> traverse (\path -> (path,) <$> readFileBS (root </> path)) paths
    versions <- expectedProbeInputs root "versions"
    listings <- expectedProbeInputs root "listing"
    sizes <- expectedCaptureSizes root
    let restore = forM_ (Map.toList originals) $ \(path, bytes) -> writeFileBS (root </> path) bytes
        check :: Text -> IO () -> IO ()
        check label mutation = do
            mutation
            hashes <- Map.fromList <$> traverse (\path -> (path,) <$> digestFile (root </> path)) paths
            identities <- traverse (digestFile . (root </>)) declarations
            let context = Map.singleton "inputsSha256" (digest (jsonLine identities))
                binding = Binding 1 context hashes
            bindingFaults root context paths binding >>= requireAccepted (label <> " fresh binding")
            auditInputs root >>= requireRejected label
            restore
        setVersions inputs = do
            writeJsonFile (root </> "inputs/versions.json") inputs
            writeComparisonText (root </> "inputs/versions.tsv") (versionCommands inputs)
        setListings = writeJsonFile (root </> "inputs/listings.json")
        setSizes = writeJsonFile (root </> "inputs/capture-sizes.json")
    check "empty versions and commands" (setVersions [])
    check "omitted TypeScript and command" (setVersions (filter ((/= "typescript") . inputPackage) versions))
    check "duplicate version input" (setVersions (take 1 versions <> versions))
    check "empty listings" (setListings ([] :: [Input]))
    check "omitted large listing" (setListings (filter ((/= "large") . inputId) listings))
    check "duplicate listing" (setListings (take 1 listings <> listings))
    forM_ inputMutations $ \(label, mutate) -> do
        check ("version field " <> label) (setVersions (changeFirst mutate versions))
        check ("listing field " <> label) (setListings (changeFirst mutate listings))
    check "empty capture sizes" (setSizes (Map.empty :: Map Text Integer))
    check "omitted TypeScript size" (setSizes (Map.delete "typescript" sizes))
    check "altered TypeScript size" (setSizes (Map.adjust (+ 1) "typescript" sizes))
    forM_ listingPaths $ \path -> check ("altered fixed listing input " <> toText path) (alter root path)
    forM_ (take 1 listingWorkloads) $ \(input, body, names) ->
        check "self-declared changed listing body and oracle" $ do
            let changedBody = TE.encodeUtf8 (T.replace "package-000001" "a-package" (TE.decodeUtf8 body))
                changedNames = map (\name -> if name == "package-000001" then "a-package" else name) names
                changedInput = input{inputDigest = digest changedBody, inputOracleDigest = digest (jsonLine changedNames), inputBytes = BS.length changedBody}
            writeFileBS (root </> inputPath input) changedBody
            writeJsonFile (root </> "inputs" </> toString (inputId input) <> ".expected") changedNames
            setListings [if inputId value == inputId input then changedInput else value | value <- listings]
    check "self-declared changed TypeScript capture and pins" $ do
        let path = "bench/corpus/npm/typescript.full.json"
        bytes <- (<> "\n") <$> readFileBS (root </> "source-inputs-A" </> path)
        forM_ ["A", "B"] $ \side -> do
            let archive = root </> "source-inputs-" <> side
            writeFileBS (archive </> path) bytes
            changeTypeScriptPin (archive </> "bench/corpus/pins.json") bytes
        setVersions [if inputPackage input == "typescript" then input{inputDigest = digest bytes, inputBytes = BS.length bytes} else input | input <- versions]
        setSizes (Map.insert "typescript" (toInteger (BS.length bytes)) sizes)
    forM_ ["A", "B"] $ \side ->
        forM_ ["bench/corpus/pins.json", "bench/corpus/npm/typescript.full.json"] $ \path ->
            check ("altered archived input " <> toText side <> "/" <> toText path) (alter root ("source-inputs-" <> side </> path))
    removeFile (root </> "source-inputs-A/bench/corpus/pins.json")
    auditInputs root >>= requireRejected "missing authenticated pins"
    restore

inputMutations :: [(Text, Input -> Input)]
inputMutations =
    [ ("id", \input -> input{inputId = inputId input <> "-changed"})
    , ("path", \input -> input{inputPath = inputPath input <> ".changed"})
    , ("package", \input -> input{inputPackage = inputPackage input <> "-changed"})
    , ("version", \input -> input{inputVersion = inputVersion input <> "-changed"})
    , ("digest", \input -> input{inputDigest = T.replicate 64 "0"})
    , ("oracle digest", \input -> input{inputOracleDigest = T.replicate 64 "0"})
    , ("bytes", \input -> input{inputBytes = inputBytes input + 1})
    ]

changeFirst :: (a -> a) -> [a] -> [a]
changeFirst _ [] = []
changeFirst change (value : rest) = change value : rest

alter :: FilePath -> FilePath -> IO ()
alter root path = readFileBS (root </> path) >>= writeFileBS (root </> path) . (<> "\n")

changeTypeScriptPin :: FilePath -> ByteString -> IO ()
changeTypeScriptPin path bytes = do
    pins <- readJsonFile path
    captures <- either fail pure (parseEither (.: "captures") pins)
    npm <- either fail pure (parseEither (.: "npm") captures)
    typescript <- either fail pure (parseEither (.: "typescript") npm)
    let changed = KeyMap.insert "bytes" (toJSON (BS.length bytes)) (KeyMap.insert "sha256" (toJSON (digest bytes)) typescript)
    writeJsonFile path (KeyMap.insert "captures" (Object (KeyMap.insert "npm" (Object (KeyMap.insert "typescript" (Object changed) npm)) captures)) pins)
