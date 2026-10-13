-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Prepare and reverify immutable source, binary and fixture identities.
module Ecluse.OsvIndex.Identity (writeManifest, identityFaults, currentBinding) where

import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Database.SQLite.Simple (Only (fromOnly), query_, withConnection)
import GHC.Conc (getNumCapabilities, getNumProcessors)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.Process (readProcess)

import Ecluse.OsvIndex.Audit (decodeJson)
import Ecluse.OsvIndex.Contract
import Ecluse.OsvIndex.Native (executionIdentity, fileIdentity)
import Ecluse.Test.Contract.Manifest (digest, digestFile, failRunner, writeJsonFile)
import Ecluse.Test.Corpus.Advisories (AdvisoryInputs (..), corpusAdvisories)

-- | Establish the complete fixed inventory before any sample starts.
writeManifest :: FilePath -> IO ()
writeManifest root = do
    tracked <- T.lines <$> readFileText (root </> "sources/tracked.txt")
    variants <- forM [A, B] $ \variant -> do
        files <- traverse (fileIdentity (root </> "sources" </> variantName variant) . toString) tracked
        let path = variantName variant <> "-source-files.json"
        writeJsonFile (root </> path) files
        VariantIdentity variant files <$> fileIdentity root ("bin" </> variantName variant) <*> digestFile (root </> path)
    manifest <-
        Manifest productionBase
            <$> executionIdentity
            <*> pure variants
            <*> traverse (fileIdentity root) inputInventory
            <*> digestFile "flake.lock"
            <*> digestFile (root </> "sources/source-diff.patch")
            <*> digestFile (root </> "conditions.txt")
            <*> pure (map toString tracked)
            <*> getNumCapabilities
            <*> getNumProcessors
            <*> withConnection ":memory:" (\conn -> map fromOnly <$> (query_ conn "SELECT sqlite_version()" :: IO [Only Text]))
            <*> lookupEnv "GHCRTS"
    writeJsonFile (root </> "manifest.json") manifest
    writeJsonFile (root </> "trial-order.json") expectedTrials
    faults <- identityFaults root manifest
    unless (null faults) (failRunner (T.unlines faults))

-- | Re-read actual files and the signed checkout instead of trusting a recorded manifest.
identityFaults :: FilePath -> Manifest -> IO [Text]
identityFaults root manifest = do
    execution <- executionIdentity
    headCommit <- T.strip . toText <$> readProcess "git" ["rev-parse", "HEAD"] ""
    tracked <- lines <$> readProcess "git" ["ls-tree", "-r", "--name-only", "HEAD"] ""
    sourceDiff <- readProcess "git" ["diff", toString productionBase, "HEAD", "--", "core", "runtime", "src", "cabal.project", "cabal.project.freeze", "flake.lock", "bench/corpus"] ""
    workingDiff <- readProcess "git" ["diff", "HEAD", "--"] ""
    actualInputNames <- map ("inputs" </>) <$> listDirectory (root </> "inputs")
    inputs <- traverse (fileIdentity root) inputInventory
    flake <- digestFile "flake.lock"
    patch <- digestFile (root </> "sources/source-diff.patch")
    conditions <- digestFile (root </> "conditions.txt")
    capabilities <- getNumCapabilities
    processors <- getNumProcessors
    ghcrts <- lookupEnv "GHCRTS"
    sqlite <- withConnection ":memory:" (\conn -> map fromOnly <$> (query_ conn "SELECT sqlite_version()" :: IO [Only Text]))
    recordedTracked <- map toString . T.lines <$> readFileText (root </> "sources/tracked.txt")
    variantFaults <- fmap concat $ forM [A, B] $ \variant -> do
        actualFiles <- traverse (fileIdentity (root </> "sources" </> variantName variant)) tracked
        recordedFiles <- decodeJson (root </> (variantName variant <> "-source-files.json"))
        fileManifestHash <- digestFile (root </> (variantName variant <> "-source-files.json"))
        binary <- fileIdentity root ("bin" </> variantName variant)
        let actual = VariantIdentity variant actualFiles binary fileManifestHash
        pure ["variant source or binary identity differs: " <> show variant | [actual] /= filter ((== variant) . viVariant) (mfVariants manifest) || recordedFiles /= actualFiles]
    sourceFaults <- checkSourceDelta root tracked
    pins <- checkPins root inputs
    pure $
        variantFaults
            <> sourceFaults
            <> pins
            <> [ label
               | (label, ok) <-
                    [ ("wrong production base", mfBase manifest == productionBase)
                    , ("wrong hosted execution identity", mfExecution manifest == execution && exSource execution == headCommit)
                    , ("production source, fixture pin or signed checkout changed", null sourceDiff && null workingDiff)
                    , ("wrong tracked source inventory", mfTracked manifest == tracked && recordedTracked == tracked)
                    , ("missing, duplicate or extra variant", exactCoverage [A, B] (map viVariant (mfVariants manifest)))
                    , ("wrong fixed input inventory", exactCoverage inputInventory actualInputNames && exactCoverage inputInventory (map fiPath (mfInputs manifest)))
                    , ("input identity changed", mfInputs manifest == inputs)
                    , ("native runtime conditions changed", mfCapabilities manifest == capabilities && mfProcessors manifest == processors && mfGhcrts manifest == ghcrts && mfSqlite manifest == sqlite)
                    , ("flake, diff or conditions changed", mfFlake manifest == flake && mfPatch manifest == patch && mfConditions manifest == conditions)
                    ]
               , not ok
               ]

checkSourceDelta :: FilePath -> [FilePath] -> IO [Text]
checkSourceDelta root tracked = do
    signedFiles <- traverse (fileIdentity ".") tracked
    aFiles <- traverse (fileIdentity (root </> "sources/A")) tracked
    bFiles <- traverse (fileIdentity (root </> "sources/B")) tracked
    a <- readFileText (root </> "sources/A" </> compilerFile)
    b <- readFileText (root </> "sources/B" </> compilerFile)
    actualPatch <- readFileText (root </> "sources/source-diff.patch")
    let different = [fiPath x | (x, y) <- zip aFiles bFiles, x /= y]
        linesA = T.lines a
        expectedB = T.unlines (filter (/= indexLine) linesA)
    pure
        ["A source differs from the signed checkout" | aFiles /= signedFiles]
        <> ["B source delta differs from the single candidate index line" | different /= [compilerFile] || length (filter (== indexLine) linesA) /= 1 || b /= expectedB]
        <> ["source patch does not identify the single removed index" | filter (T.isPrefixOf "-") (drop 2 (T.lines actualPatch)) /= ["-" <> indexLine] || any (T.isPrefixOf "+") (drop 2 (T.lines actualPatch))]
  where
    compilerFile = "core/src/Ecluse/Core/Osv/Compile.hs"
    indexLine = "    execute_ conn \"CREATE UNIQUE INDEX uq_ranges_segment ON package_vulnerability_ranges(package_name, cve_id, introduced_version, fixed_version, last_affected_version)\""

checkPins :: FilePath -> [FileIdentity] -> IO [Text]
checkPins root inputs = do
    remote <- fmap concat $ forM pins $ \(name, bytes, etag, identityHeader, identity) -> do
        headers <- readFileText (root </> "inputs" </> (name <> ".headers"))
        let parsed = Map.fromList [(T.toLower (T.strip key), T.strip (T.drop 1 value)) | line <- T.lines headers, let (key, value) = T.breakOn ":" line, not (T.null value)]
            actual = [fiBytes file | file <- inputs, fiPath file == "inputs" </> name]
        pure ["remote pin differs: " <> toText name | actual /= [bytes] || Map.lookup "content-length" parsed /= Just (show bytes) || fmap (T.dropAround (== '"')) (Map.lookup "etag" parsed) /= Just etag || Map.lookup identityHeader parsed /= Just identity]
    corpus <- fmap concat $ forM [NpmCorpus, PyPICorpus] $ \input -> do
        bytes <- corpusAdvisories (caseEco input)
        let prefix = caseName input
            expected = [("inputs" </> prefix <> ".zip", digest (LBS.toStrict (aiOsvZip bytes))), ("inputs" </> prefix <> "-epss.gz", digest (LBS.toStrict (aiEpssFeed bytes)))]
        pure ["committed corpus differs: " <> toText path | (path, hash) <- expected, [fiSha256 file | file <- inputs, fiPath file == path] /= [hash]]
    pure (remote <> corpus)
  where
    pins =
        [ ("npm-full.zip", 218363673, "e3f9f7acf00a6f84a63a27f5ce8a6cec", "x-goog-generation", "1791860590767687")
        , ("pypi-full.zip", 35977300, "7f1ee8b695321095b5637f66effabd73", "x-goog-generation", "1791838994093506")
        , ("epss.csv.gz", 2671545, "0313af55cdf6ee83ff8d5f7d16aa5bd0", "x-amz-version-id", "GL3FB0.f4yDl8jzyyptuxspqz5LOCE6T")
        ]

-- | Reconstruct a trial binding from current files and the independently expected command.
currentBinding :: FilePath -> Trial -> IO Binding
currentBinding root trial =
    Binding trial
        <$> executionIdentity
        <*> digestFile (root </> "manifest.json")
        <*> fileIdentity root ("bin" </> variantName (trVariant trial))
        <*> traverse (fileIdentity root) (trialInputs trial)
        <*> pure (drop 1 (trialCommand root trial))
        <*> getNumCapabilities
        <*> lookupEnv "GHCRTS"
