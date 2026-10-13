-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Fixed, authenticated workloads for the temporary maintenance-reader comparison.
module Ecluse.BenchReport.RefactorInputs (Input (..), prepareInputs) where

import Data.Aeson (FromJSON, ToJSON, (.:))
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeBaseName, (</>))

import Ecluse.BenchReport.RefactorFiles (writeComparisonText)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Test.Contract.Manifest (digest, jsonLine, readJsonFile, writeJsonFile)
import Ecluse.Test.Corpus (CaptureRecord (..), CorpusPackage (cpPath), corpusPackages, cpName, pypiCorpusPackages, readCaptureRecords, readCorpusPins)

-- | Input identity is prepared outside every measured process.
data Input = Input
    { inputId :: Text
    , inputPath :: FilePath
    , inputPackage :: Text
    , inputVersion :: Text
    , inputDigest :: Text
    , inputOracleDigest :: Text
    , inputBytes :: Int
    }
    deriving stock (Show, Generic)

instance ToJSON Input
instance FromJSON Input

-- | Authenticate committed captures and generate native listings with a separate name oracle.
prepareInputs :: FilePath -> IO ()
prepareInputs root = do
    createDirectoryIfMissing True root
    captures <- readCaptureRecords Npm >>= either fail pure
    pins <- readCorpusPins (.: "pins") >>= either fail pure
    versions <- forM corpusPackages $ \package -> do
        record <- maybe (fail "missing npm capture") pure (Map.lookup (cpName package) captures)
        version <- maybe (fail "missing npm version pin") pure (Map.lookup (cpName package) pins)
        bytes <- BS.readFile (cpPath package)
        unless (fromIntegral (BS.length bytes) == crBytes record && digest bytes == crSha256 record) (fail "unauthenticated npm capture")
        pure (Input (toText (takeBaseName (cpPath package))) (cpPath package) (cpName package) version (digest bytes) "" (BS.length bytes))
    listings <- forM [("small", 20), ("large", 20000)] $ \(label, count) -> do
        let names = ["package-" <> T.justifyRight 6 '0' (show n) | n <- [1 .. count :: Int]]
            path = root </> toString label <> ".json"
            member name = "\"" <> name <> "\":{\"description\":\"" <> T.replicate 512 "x" <> "\",\"versions\":[1,2,3]}"
            body = TE.encodeUtf8 ("{" <> T.intercalate "," (map member names <> ["\"_updated\":42", "\"invalid/name/extra\":null", "\"package-000001\":{}"]) <> "}")
        BS.writeFile path body
        writeJsonFile (root </> toString label <> ".expected") names
        pure (Input label path label "" (digest body) (digest (jsonLine names)) (BS.length body))
    writeJsonFile (root </> "versions.json") versions
    writeJsonFile (root </> "listings.json") listings
    writeComparisonText (root </> "versions.tsv") (T.unlines (map versionCommand versions))
    pypiRecords <- readCaptureRecords PyPI >>= either fail pure
    pypiSizes <- forM pypiCorpusPackages $ \package -> do
        record <- maybe (fail "missing PyPI capture") pure (Map.lookup (cpName package) pypiRecords)
        bytes <- BS.readFile (cpPath package)
        unless (fromIntegral (BS.length bytes) == crBytes record && digest bytes == crSha256 record) (fail "unauthenticated PyPI capture")
        pure (cpName package, toInteger (crBytes record))
    writeJsonFile (root </> "capture-sizes.json") (Map.fromList ([(inputPackage input, toInteger (inputBytes input)) | input <- versions] <> pypiSizes))
    -- Re-read the written identities before Task can start either source.
    written <- readJsonFile (root </> "versions.json") :: IO [Input]
    unless (length written == 9 && length listings == 2) (fail "unexpected maintenance workload coverage")
  where
    versionCommand input = T.intercalate "\t" [inputId input, inputPackage input, inputVersion input, toText (inputPath input)]
