-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Corpus-authenticated metadata measurements in one child process per retained shape.
module Ecluse.Core.Server.MemoryModelResidencySpec (spec, childMain) where

import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (Value, eitherDecodeStrict, encode, object, withObject, (.:), (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Package (pkgEcosystem)
import Ecluse.Core.Server.MemoryModel (expandWireBytes)
import Ecluse.Core.Server.MemoryModel.Probe (Measurement (..), Shape (..), packages, probe)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpPath), cpName)

-- | Reject unauthenticated captures and roots that do not survive or release across collections.
spec :: Spec
spec = describe "metadata retained heap" $ do
    it "includes an authenticated capture above the previous 3,687,514-byte maximum" $ do
        sizes <- traverse authenticate packages
        sizes `shouldSatisfy` any ((> 3687514) . fst)
    forM_ packages $ \package ->
        forM_ [minBound .. maxBound] $ \shape ->
            it (toString (cpName package) <> "/" <> show shape) $ do
                (size, digest) <- authenticate package
                executable <- getExecutablePath
                (status, output, errors) <-
                    readProcessWithExitCode executable ["--metadata-probe", show shape, cpPath package, "+RTS", "-T", "-N1", "-RTS"] ""
                unless (status == ExitSuccess) (expectationFailure (show status <> ": " <> errors))
                result <- either fail pure (eitherDecodeStrict (encodeUtf8 (toText output)))
                report package digest shape result
                wireBytes result `shouldBe` size
                heldLive result `shouldSatisfy` (> baselineLive result)
                releasedLive result `shouldSatisfy` (< heldLive result)
                when (shape == Typed || shape == Shared) (versions result `shouldSatisfy` (> 0))

-- | Dispatch a fresh process without entering Hspec or loading any other capture.
childMain :: String -> FilePath -> IO ()
childMain rawShape path = do
    shape <- maybe (fail "unknown metadata residency shape") pure (readMaybe rawShape)
    package <- maybe (fail "unknown metadata residency corpus path") pure (find ((== path) . cpPath) packages)
    result <- probe shape package
    LBS.putStr (encode result)

authenticate :: CorpusPackage -> IO (Int, Text)
authenticate package = do
    pinsBytes <- BS.readFile "bench/corpus/pins.json"
    pins <- either fail pure (eitherDecodeStrict pinsBytes)
    (expectedBytes, expectedHash) <- either fail pure (parseEither (capture package) pins)
    bytes <- BS.readFile (cpPath package)
    BS.length bytes `shouldBe` expectedBytes
    (show (hash bytes :: Digest SHA256) :: Text) `shouldBe` expectedHash
    pure (expectedBytes, expectedHash)

capture :: CorpusPackage -> Value -> Parser (Int, Text)
capture package = withObject "corpus pins" $ \pins -> do
    captures <- pins .: "captures"
    ecosystem <- case pkgEcosystem (cpPackage package) of
        Npm -> captures .: "npm"
        PyPI -> captures .: "pypi"
        RubyGems -> fail "no RubyGems metadata residency corpus"
    entry <- ecosystem .: Key.fromText (cpName package)
    (,) <$> entry .: "bytes" <*> entry .: "sha256"

report :: CorpusPackage -> Text -> Shape -> Measurement -> IO ()
report package digest shape result = do
    let retained = toInteger (heldLive result) - toInteger (baselineLive result)
        ratio = fromInteger retained / fromIntegral (wireBytes result) :: Double
        row =
            object
                [ "package" .= cpName package
                , "path" .= cpPath package
                , "sha256" .= digest
                , "shape" .= (show shape :: Text)
                , "retained_per_wire_byte" .= ratio
                , "existing_model_bytes" .= expandWireBytes (wireBytes result)
                , "measurement" .= result
                ]
    putStrLn ("metadata-residency " <> toString (decodeUtf8 (LBS.toStrict (encode row)) :: Text))
