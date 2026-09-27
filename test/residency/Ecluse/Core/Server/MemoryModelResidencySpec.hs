-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Corpus-authenticated metadata measurements in one child process per retained shape.
module Ecluse.Core.Server.MemoryModelResidencySpec (spec, sourceMain, selectedMain, probeIdentity, probeLimits) where

import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (Object, encode, object, (.:), (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (Parser)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Test.Hspec

import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Package (PackageName, pkgEcosystem)
import Ecluse.Core.Registry.Adapter (RegistryAdapter (adapterMetadata), adapterFor)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataChargeFactors))
import Ecluse.Core.Registry.Npm.Project (projectName)
import Ecluse.Core.Registry.PyPI.Project qualified as PyPI
import Ecluse.Core.Security (Limits (maxMetadataBytes), defaultLimits)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (cfFullReadPermille, cfOutputPermille))
import Ecluse.Core.Server.MemoryModel (expandWireBytes)
import Ecluse.Core.Server.MemoryModel.Probe (Measurement (..), SelectedShape (SelectedControl, SelectedValue), Shape (..), measureInChild, packages, probeSelected, probeSource)
import Ecluse.Core.Snapshot (digestBytes)
import Ecluse.Core.Version (Version, canonicalPep440, mkVersion, renderVersion)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpPath), cpName, readCorpusPins)

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
                measureInChild ["--metadata-probe", show shape] package >>= \case
                    Left failure -> expectationFailure failure
                    Right result -> do
                        report package digest shape result
                        checkMeasurement (pkgEcosystem (cpPackage package)) shape size result

checkMeasurement :: Ecosystem -> Shape -> Int -> Measurement -> Expectation
checkMeasurement ecosystem shape size result = do
    let retained = toInteger (heldLive result) - toInteger (baselineLive result)
        residual = max 0 (toInteger (releasedLive result) - toInteger (baselineLive result))
    wireBytes result `shouldBe` size
    retained `shouldSatisfy` (> 0)
    residual `shouldSatisfy` (<= 16 * 1024)
    (10 * residual) `shouldSatisfy` (<= retained)
    (1000 * retained) `shouldSatisfy` (<= envelopePermille ecosystem shape * toInteger size)
    when (shape == Typed || shape == Shared) (versions result `shouldSatisfy` (> 0))
    -- A listing holds its encoded output and a strict copy of it, which the output charge covers.
    when (shape == Shared) $ for_ (chargeFactors ecosystem) $ \factors ->
        (2000 * toInteger (compactBytes result)) `shouldSatisfy` (<= toInteger (cfOutputPermille factors) * toInteger size)

{- A shared entry is what a listing's full read holds, so its gate is the memory gate's full-read
charge: a representation that retains more fails here before it can outgrow the charge. -}
envelopePermille :: Ecosystem -> Shape -> Integer
envelopePermille ecosystem Shared | Just factors <- chargeFactors ecosystem = toInteger (cfFullReadPermille factors)
envelopePermille Npm Typed = 750
envelopePermille _ shape = case shape of
    Wire -> 1250
    Raw -> 7000
    Typed -> 2250
    Shared -> 7500

chargeFactors :: Ecosystem -> Maybe ChargeFactors
chargeFactors = fmap (metadataChargeFactors . adapterMetadata) . adapterFor

authenticate :: CorpusPackage -> IO (Int, Text)
authenticate package = do
    (expectedBytes, expectedHash) <- readCorpusPins (capture package) >>= either fail pure
    bytes <- BS.readFile (cpPath package)
    BS.length bytes `shouldBe` expectedBytes
    (show (hash bytes :: Digest SHA256) :: Text) `shouldBe` expectedHash
    pure (expectedBytes, expectedHash)

capture :: CorpusPackage -> Object -> Parser (Int, Text)
capture package pins = do
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

-- | Run an explicit ecosystem source mode without Show-based preparation or a hidden warm-up.
sourceMain :: String -> String -> String -> String -> String -> FilePath -> IO ()
sourceMain rawEcosystem rawMode rawName rawVersion rawLimit path = do
    mode <- maybe (fail "unknown source mode") pure (readMaybe rawMode)
    (name, version) <- probeIdentity rawEcosystem rawName rawVersion
    limits <- probeLimits rawLimit
    let limit = maxMetadataBytes limits
    outcome <- probeSource mode limits name version path
    case outcome of
        Left fault -> do
            LBS.putStr (encode (object ["status" .= ("refused" :: Text), "reason" .= fault, "mode" .= rawMode, "path" .= path, "body_limit" .= limit]))
            exitFailure
        Right (result, digest, charge, elapsed) -> do
            let successful = versions result > 0
                hex = decodeUtf8 (convertToBase Base16 (digestBytes digest) :: ByteString) :: Text
            LBS.putStr
                ( encode
                    ( object
                        [ "status" .= (if successful then "ok" else "empty-result" :: Text)
                        , "mode" .= rawMode
                        , "path" .= path
                        , "package" .= rawName
                        , "ecosystem" .= rawEcosystem
                        , "selected_version" .= rawVersion
                        , "lookup_version" .= renderVersion version
                        , "body_limit" .= limit
                        , "chunk_bytes" .= (32768 :: Int)
                        , "sha256" .= hex
                        , "compact_byte_estimate" .= charge
                        , "read_project_ns" .= elapsed
                        , "measurement" .= result
                        , "scope" .= ("single read and retained-result forcing by accounting, without Show or output encoding; sample process peak externally" :: Text)
                        ]
                    )
                )
            unless successful exitFailure

-- | Parse explicit ecosystem identities independently of capture filenames.
probeIdentity :: String -> String -> String -> IO (PackageName, Version)
probeIdentity ecosystem name version = do
    (kind, parsed) <- case ecosystem of
        "npm" -> pure (Npm, projectName (toText name))
        "pypi" -> pure (PyPI, PyPI.projectName (toText name))
        _ -> fail "probe ecosystem must be npm or pypi"
    package <- either (fail . show) pure parsed
    key <- case kind of
        PyPI -> maybe (fail "invalid PEP 440 probe version") pure (canonicalPep440 (toText version))
        _ -> pure (toText version)
    pure (package, mkVersion kind key)

-- | Report selected or discard-control samples without treating unresolved growth as retention.
selectedMain :: SelectedShape -> String -> String -> String -> String -> FilePath -> IO ()
selectedMain shape ecosystem name version rawLimit path = do
    (package, selected) <- probeIdentity ecosystem name version
    limits <- probeLimits rawLimit
    result <- probeSelected shape limits package selected path
    bytes <- BS.readFile path
    LBS.putStr $
        encode $
            object
                [ "mode" .= mode
                , "ecosystem" .= ecosystem
                , "package" .= name
                , "selected_version" .= version
                , "lookup_version" .= renderVersion selected
                , "path" .= path
                , "sha256" .= (show (hash bytes :: Digest SHA256) :: Text)
                , "body_limit" .= maxMetadataBytes limits
                , "measurement" .= result
                , "scope" .= ("Diagnostic GC samples. Compare matched selected and discard-control intervals before interpreting retained bytes. Capture authentication follows measurement, so external process peaks include that read." :: Text)
                ]
  where
    mode :: Text
    mode = case shape of
        SelectedValue -> "SelectedRetention"
        SelectedControl -> "SelectedControl"

-- | Override only the metadata byte limit, preserving the shipped structural limits.
probeLimits :: String -> IO Limits
probeLimits raw = do
    limit <- maybe (fail "metadata byte limit must be positive") pure (readMaybe raw >>= \n -> if n > 0 then Just n else Nothing)
    pure defaultLimits{maxMetadataBytes = limit}
