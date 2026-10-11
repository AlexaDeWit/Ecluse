-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Semantic preflight and existing full-read benchmarks over one frozen shape case.
module Ecluse.Bench.ShapeValidation (run) where

import Data.Aeson (Value, eitherDecodeStrict, encode, object, (.=))
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Tasty.Bench (bgroup, defaultMain)
import Test.Tasty.HUnit (assertBool, assertFailure)

import Ecluse.Bench.ShapeValidation.Inputs
import Ecluse.Core.ColdReadBench qualified as ColdReadBench
import Ecluse.Core.Package (infoVersions)
import Ecluse.Core.Package.Hash (hexDigestText)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataSerialise))
import Ecluse.Core.Registry.Metadata (Manifest (..))
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Snapshot (Snapshot (..), digestBytes)
import Ecluse.Core.WireBench qualified as WireBench
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpPath))
import Ecluse.Test.Corpus.Outputs (rendered)
import Ecluse.Test.EcosystemBench (EcosystemBench (..), ecosystemBenches)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Registry.Metadata.Fetch (captureChunks, captureChunksOf)
import Ecluse.Test.Security.Limits (checkDocumentArtifacts, checkVersionCount)
import Ecluse.Test.Support (expectRight)

-- | Select generate, preflight, or bench. Bench processes contain exactly one replay case.
run :: String -> IO ()
run mode = do
    directory <- required "ECLUSE_SHAPE_DIR"
    case mode of
        "generate" -> generateInputs directory
        "preflight" -> do
            output <- required "ECLUSE_SHAPE_OUTPUT"
            baseline <- lookupEnv "ECLUSE_SHAPE_BASELINE"
            createDirectoryIfMissing True output
            ecosystems <- ecosystemBenches
            for_ inputCases (preflight directory output baseline ecosystems)
        "bench" -> do
            label <- toText <$> required "ECLUSE_SHAPE_CASE"
            input <- maybe (assertFailure "unknown shape case") pure (find ((== label) . inputLabel) inputCases)
            ecosystem <- ecosystemFor input =<< ecosystemBenches
            bytes <- readInput directory input
            manifest <- readManifest ecosystem input (captureChunks bytes)
            let entry = (inputPackage input, bytes, manifestInfo manifest, Snapshot (manifestDigest manifest) (manifestRaw manifest))
                measured = ecosystem{ebCorpus = [entry]}
            ColdReadBench.withBenchmarks [measured] $ \cold ->
                defaultMain [bgroup (toString label) (WireBench.benchmarks measured : cold)]
        _ -> assertFailure "ECLUSE_SHAPE_MODE must be generate, preflight, or bench"

required :: String -> IO String
required key = lookupEnv key >>= maybe (assertFailure (key <> " is required")) pure

ecosystemFor :: InputCase -> [EcosystemBench] -> IO EcosystemBench
ecosystemFor input = maybe (assertFailure "missing ecosystem benchmark adapter") pure . find ((== inputEcosystem input) . ebEcosystem)

preflight :: FilePath -> FilePath -> Maybe FilePath -> [EcosystemBench] -> InputCase -> IO ()
preflight directory output baseline ecosystems input = do
    ecosystem <- ecosystemFor input ecosystems
    bytes <- readInput directory input
    manifest <- readManifest ecosystem input (captureChunks bytes)
    semantic <- projection ecosystem manifest
    let label = toString (inputLabel input)
        actual = object ["projection" .= semantic, "projectedVersions" .= Map.size (infoVersions (manifestInfo manifest)), "bodyBytes" .= manifestBodyBytes manifest, "wireSha256" .= hexDigestText (digestBytes (manifestDigest manifest))]
    assertBool "full read byte count differs" (manifestBodyBytes manifest == BS.length bytes)
    assertBool "full read wire digest differs" (hexDigestText (digestBytes (manifestDigest manifest)) == hexSha256Of bytes)
    assertBool "full read has no versions" (not (Map.null (infoVersions (manifestInfo manifest))))
    _ <- expectRight (checkVersionCount defaultLimits (manifestInfo manifest) >>= checkDocumentArtifacts defaultLimits)
    when (preservesProjection input) $ do
        source <- BS.readFile (cpPath (inputPackage input))
        original <- readManifest ecosystem input (captureChunks source) >>= projection ecosystem
        assertBool (label <> " changed the original projection") (semantic == original)
    when (any (`T.isInfixOf` inputLabel input) boundaryCases) $
        for_ boundaryChunks $ \size -> do
            chunked <- readManifest ecosystem input (captureChunksOf size bytes) >>= projection ecosystem
            assertBool (label <> " changed at chunk size " <> show size) (chunked == semantic)
    BS.writeFile (output </> label <> ".json") (toStrict (encode actual))
    for_ baseline $ \reference -> do
        expected <- BS.readFile (reference </> label <> ".json") >>= expectRight . eitherDecodeStrict
        assertBool (label <> " differs from the main semantic reference") (actual == expected)
    putTextLn (inputLabel input <> ": semantic preflight passed")

readManifest :: EcosystemBench -> InputCase -> [ByteString] -> IO Manifest
readManifest ecosystem input chunks = ebRead ecosystem (cpPackage (inputPackage input)) chunks >>= expectRight

projection :: EcosystemBench -> Manifest -> IO Value
projection ecosystem manifest = do
    wire <- expectRight (metadataSerialise (ebMetadata ecosystem) (manifestRaw manifest))
    document <- expectRight (eitherDecodeStrict (toStrict wire) :: Either String Value)
    pure (object ["typed" .= (decodeUtf8 (toStrict (rendered (manifestInfo manifest))) :: Text), "document" .= document])

boundaryChunks :: [Int]
boundaryChunks = [32767, 32768, 32769, 4099]

boundaryCases :: [Text]
boundaryCases = ["long-retained", "escaped"]
