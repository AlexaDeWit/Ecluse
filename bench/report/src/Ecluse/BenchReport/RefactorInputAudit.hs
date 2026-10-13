-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Workload obligations use a compiled catalogue and capture pins authenticated by a fixed digest.
module Ecluse.BenchReport.RefactorInputAudit (expectedProbeInputs, expectedCaptureSizes, auditInputs) where

import Data.Aeson (eitherDecodeStrict', withObject, (.:))
import Data.Aeson.Types (parseEither)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import System.FilePath (takeBaseName, (</>))

import Ecluse.BenchReport.RefactorFiles (captureProblems, readComparisonText)
import Ecluse.BenchReport.RefactorInputs (Input (..), listingWorkloads, versionCommands)
import Ecluse.Test.Contract.Manifest (digest, jsonLine, readJsonFile)
import Ecluse.Test.Contract.Result (auditExactIds)
import Ecluse.Test.Corpus (CorpusPackage (cpPath), corpusPackages, cpName, pypiCorpusPackages)

trustedPins :: FilePath -> IO ByteString
trustedPins root = do
    bytes <- BS.readFile (root </> "source-inputs-A/bench/corpus/pins.json")
    unless (digest bytes == "5f7a6509afb84a8269cf9e382ffa60b5255628dac9d33dece010248c480db14a") (fail "capture pins differ from the fixed baseline SHA-256")
    pure bytes

-- | Probe coverage and every input field come from the fixed workload, not the observed manifest.
expectedProbeInputs :: FilePath -> Text -> IO [Input]
expectedProbeInputs root = \case
    "versions" -> fst <$> trustedCaptures root
    "listing" -> pure [input | (input, _, _) <- listingWorkloads]
    _ -> fail "unknown maintenance probe workload"

-- | Residency byte recovery uses sizes from the authenticated pins under the evidence root.
expectedCaptureSizes :: FilePath -> IO (Map Text Integer)
expectedCaptureSizes root = do
    (npm, pypi) <- trustedCaptures root
    pure (Map.fromList [(inputPackage input, toInteger (inputBytes input)) | input <- npm <> pypi])

trustedCaptures :: FilePath -> IO ([Input], [Input])
trustedCaptures root = do
    pins <- trustedPins root
    parsed <- either fail pure (eitherDecodeStrict' pins >>= parseEither parser)
    unless (length (fst parsed) == 9 && length (snd parsed) == 3) (fail "compiled capture catalogue differs from the fixed comparison")
    pure parsed
  where
    parser = withObject "trusted pins" $ \pins -> do
        captures <- pins .: "captures"
        npm <- captures .: "npm"
        pypi <- captures .: "pypi"
        versions <- pins .: "pins"
        pypiVersions <- pins .: "pypiPins"
        (,) <$> traverse (capture npm versions) corpusPackages <*> traverse (capture pypi pypiVersions) pypiCorpusPackages
    capture records versions package = do
        record <- records .: fromString (toString (cpName package))
        version <- versions .: fromString (toString (cpName package))
        withObject "capture" (fields package version) record
    fields package version record = do
        path <- record .: "path"
        bytes <- record .: "bytes"
        sha256 <- record .: "sha256"
        unless ("bench/corpus" </> path == cpPath package && bytes > 0) (fail "compiled capture path or byte count differs")
        pure (Input (toText (takeBaseName (cpPath package))) (cpPath package) (cpName package) version sha256 "" bytes)

-- | Authenticate both source archives, all probe declarations, the command list and the residency size map.
auditInputs :: FilePath -> IO [Text]
auditInputs root = captureProblems $ do
    pins <- trustedPins root
    (versions, pypi) <- trustedCaptures root
    listings <- expectedProbeInputs root "listing"
    actualVersions <- readJsonFile (root </> "inputs/versions.json")
    actualListings <- readJsonFile (root </> "inputs/listings.json")
    commands <- readComparisonText (root </> "inputs/versions.tsv")
    sizes <- readJsonFile (root </> "inputs/capture-sizes.json")
    expectedSizes <- expectedCaptureSizes root
    archives <- concat <$> traverse (auditArchive pins (versions <> pypi)) ["A", "B"]
    bodies <- forM listingWorkloads $ \(input, expectedBody, expectedNames) -> captureProblems $ do
        body <- BS.readFile (root </> inputPath input)
        oracle <- BS.readFile (root </> "inputs" </> toString (inputId input) <> ".expected")
        pure
            ( ["listing body differs from fixed workload: " <> inputId input | body /= expectedBody]
                <> ["listing oracle differs from fixed workload: " <> inputId input | oracle /= jsonLine expectedNames]
            )
    pure
        ( manifestProblems "versions" versions actualVersions
            <> manifestProblems "listings" listings actualListings
            <> ["version command list differs from fixed workload" | commands /= versionCommands versions]
            <> ["capture size map differs from trusted pins" | sizes /= expectedSizes]
            <> archives
            <> concat bodies
        )
  where
    auditArchive authenticatedPins captures side = do
        let archive = root </> "source-inputs-" <> side
        pinFaults <- captureProblems $ do
            pins <- BS.readFile (archive </> "bench/corpus/pins.json")
            pure ["archived pins differ from authenticated baseline pins: " <> toText side | pins /= authenticatedPins]
        faults <- forM captures $ \input -> captureProblems $ do
            bytes <- BS.readFile (archive </> inputPath input)
            pure ["archived capture differs from trusted pins: " <> toText side <> "/" <> inputPackage input | BS.length bytes /= inputBytes input || digest bytes /= inputDigest input]
        pure (pinFaults <> concat faults)

manifestProblems :: Text -> [Input] -> [Input] -> [Text]
manifestProblems label expected actual =
    auditExactIds (label <> " input") (map inputId expected) (map inputId actual)
        <> [label <> " manifest fields or order differ from the fixed workload" | actual /= expected]
