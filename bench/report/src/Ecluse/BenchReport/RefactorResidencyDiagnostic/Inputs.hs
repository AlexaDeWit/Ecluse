-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Authenticate original helper bytes before the isolated native campaign.
module Ecluse.BenchReport.RefactorResidencyDiagnostic.Inputs (
    Generated (..),
    captureFaults,
    registerInputs,
    inputFaults,
    matchedInputs,
    inputPaths,
    expectedLiveHashes,
    resolveProbe,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory (copyFile)
import System.FilePath (isAbsolute, takeDirectory, (</>))

import Ecluse.BenchReport.RefactorEvidence (checksums, executableDigests)
import Ecluse.BenchReport.RefactorFiles (captureProblems, readComparisonText)
import Ecluse.BenchReport.RefactorInputAudit (expectedProbeInputs)
import Ecluse.BenchReport.RefactorInputs (Input (..))
import Ecluse.BenchReport.RefactorResidencyDiagnostic.Inventory
import Ecluse.Test.Contract.Manifest (digest, readJsonFile, writeJsonFile)
import Ecluse.Test.Contract.Result (auditExactIds)

-- | A generated record preserves its actual returned path and original byte identity.
data Generated = Generated
    { generatedRole :: Text
    , generatedShape :: Text
    , generatedPackage :: Text
    , generatedSource :: Text
    , generatedHelper :: Text
    , generatedOriginal :: FilePath
    , generatedArchive :: FilePath
    , generatedBytes :: Int
    , generatedSha256 :: Text
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON Generated
instance FromJSON Generated

-- | Only compiled probe identifiers can select helper or measurement obligations.
resolveProbe :: Text -> IO Probe
resolveProbe name = maybe (fail "unknown diagnostic probe") pure (find ((== name) . probeId) probes)

-- | Copy both returned byte sequences without decoding or re-encoding them.
registerInputs :: FilePath -> Text -> Text -> IO ()
registerInputs root name identifier = do
    requireDiagnostic (name `elem` passes) "unknown repetition"
    probe <- resolveProbe identifier
    requireDiagnostic (probe `elem` merges) "Shared does not use a merge helper"
    let directory = generationDirectory name probe
        side = sideOf name
        helperDirectory = takeDirectory root </> "helper-" <> toString side
    returned <- T.lines <$> readComparisonText (root </> directory </> "returned.txt")
    requireDiagnostic (length returned == 2) "helper must return exactly two paths"
    binaries <- executableDigests (root </> directory </> "helper-before.sha256")
    recorded <- checksums (root </> "helper-build-" <> toString side </> "executable.sha256")
    forM_ ["helper-before.sha256", "helper-after.sha256"] $ \file -> do
        actual <- checksums (root </> directory </> file)
        requireDiagnostic (actual == recorded) "helper changed during generation"
    binary <- maybe (fail "missing helper binary") pure (Map.lookup "ecluse-residency" binaries)
    records <- forM (zip ["private", "public"] returned) $ \(role, path) -> do
        let original = toString path
            live = if isAbsolute original then original else helperDirectory </> original
            archive = directory </> toString role <> ".json"
        requireDiagnostic (original `elem` [probePath probe, root </> directory </> "live" </> toString role <> ".json"]) "returned input path escapes fixed generation paths"
        bytes <- BS.readFile live
        copyFile live (root </> directory </> toString role <> ".original")
        copyFile live (root </> archive)
        pure (Generated role (probeShape probe) (probePackage probe) (sourceFor side) binary original archive (BS.length bytes) (digest bytes))
    writeJsonFile (root </> directory </> "manifest.json") records

-- | Rehash every original and copied input against independently authenticated committed captures.
inputFaults :: FilePath -> IO [Text]
inputFaults root = captureProblems $ do
    captures <- captureFaults root
    generated <- concat <$> forM [(name, probe) | name <- passes, probe <- merges] (\(name, probe) -> captureProblems (auditGenerated root name probe))
    pure (captures <> generated)

-- | Authenticate selected committed captures before helper generation starts.
captureFaults :: FilePath -> IO [Text]
captureFaults root = do
    expected <- expectedProbeInputs root "versions"
    faults <-
        concat
            <$> forM
                ["A", "B"]
                ( \side -> forM probes $ \probe -> do
                    input <- maybe (fail "missing trusted capture") pure (find ((== probePath probe) . inputPath) expected)
                    bytes <- BS.readFile (root </> "source-inputs-" <> toString side </> probePath probe)
                    pure ["committed capture differs: " <> side <> "/" <> probeId probe | BS.length bytes /= inputBytes input || digest bytes /= inputDigest input]
                )
    pure (concat faults)

auditGenerated :: FilePath -> Text -> Probe -> IO [Text]
auditGenerated root name probe = do
    let directory = generationDirectory name probe
        side = sideOf name
    records <- readJsonFile (root </> directory </> "manifest.json")
    returned <- T.lines <$> readComparisonText (root </> directory </> "returned.txt")
    binary <- executableDigests (root </> "helper-build-" <> toString side </> "executable.sha256")
    fields <-
        concat
            <$> forM
                (records :: [Generated])
                ( \record -> do
                    let role = generatedRole record
                        archive = directory </> toString role <> ".json"
                        allowed = [probePath probe, root </> directory </> "live" </> toString role <> ".json"]
                    requireDiagnostic (role `elem` ["private", "public"]) "unknown generated role"
                    bytes <- BS.readFile (root </> archive)
                    original <- BS.readFile (root </> directory </> toString role <> ".original")
                    pure
                        ( ["generated input bytes differ: " <> toText archive | original /= bytes || BS.length original /= generatedBytes record || digest original /= generatedSha256 record]
                            <> ["generated shape or package differs" | generatedShape record /= probeShape probe || generatedPackage record /= probePackage probe]
                            <> ["generated helper source differs" | generatedSource record /= sourceFor side || Map.lookup "ecluse-residency" binary /= Just (generatedHelper record)]
                            <> ["generated path or archive differs" | generatedArchive record /= archive || generatedOriginal record `notElem` allowed]
                        )
                )
    pure
        ( auditExactIds "generated role" ["private", "public"] (map generatedRole records)
            <> ["returned input paths differ" | returned /= map (toText . generatedOriginal) records]
            <> fields
        )

-- | Require matched input bytes within and between sources before measuring children.
matchedInputs :: FilePath -> IO [Text]
matchedInputs root = do
    faults <- inputFaults root
    matches <-
        concat
            <$> forM
                merges
                ( \probe -> forM ["private", "public"] $ \role -> do
                    bodies <- traverse (\name -> BS.readFile (root </> generationDirectory name probe </> toString role <> ".original")) passes
                    pure ["generated inputs do not match: " <> probeId probe <> "/" <> role | length (ordNub bodies) /= 1]
                )
    pure (faults <> concat matches)

-- | Native children receive the source-specific returned paths in original order.
inputPaths :: FilePath -> Text -> Probe -> IO [Text]
inputPaths root name probe
    | probeShape probe == "Shared" = pure [toText (probePath probe)]
    | otherwise = T.lines <$> readComparisonText (root </> generationDirectory name probe </> "returned.txt")

-- | Live hashes must match the original archived bytes before and after each child.
expectedLiveHashes :: FilePath -> Text -> Probe -> IO (Map FilePath Text)
expectedLiveHashes root name probe
    | probeShape probe == "Shared" = do
        captures <- expectedProbeInputs root "versions"
        capture <- maybe (fail "missing Shared capture") pure (find ((== probePath probe) . inputPath) captures)
        pure (Map.singleton (probePath probe) (inputDigest capture))
    | otherwise = do
        records <- readJsonFile (root </> generationDirectory name probe </> "manifest.json")
        pure (Map.fromList [(generatedOriginal record, generatedSha256 record) | record <- records])
