-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Corpus-authenticated metadata measurements in one child process per retained shape or listing.
module Ecluse.Core.Server.MemoryModelResidencySpec (spec, sourceMain, selectedMain, probeIdentity, probeLimits) where

import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (Object, encode, object, (.:), (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (Parser)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Test.Hspec

import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Ecluse.Composition.MemoryPlan.Transient (meterStepBytes)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Package (PackageName, pkgEcosystem)
import Ecluse.Core.Registry.Adapter (RegistryAdapter (adapterMetadata), adapterFor)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataChargeFactors))
import Ecluse.Core.Registry.Npm.Project (projectName)
import Ecluse.Core.Registry.PyPI.Project qualified as PyPI
import Ecluse.Core.Security (Limits (maxMetadataBytes), defaultLimits)
import Ecluse.Core.Server.Admission.Budget (roundUpToStep, scaleCharge)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (cfFullReadPermille, cfOutputPermille))
import Ecluse.Core.Server.MemoryModel (expandWireBytes)
import Ecluse.Core.Server.MemoryModel.Probe (ListingPeaks (..), Measurement (..), SelectedShape (SelectedControl, SelectedValue), Shape (..), measureInChild, packages, probeSelected, probeSource)
import Ecluse.Core.Snapshot (digestBytes)
import Ecluse.Core.Version (Version, canonicalPep440, mkVersion, renderVersion)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpPath), cpName, readCorpusPins)

{- | Reject unauthenticated captures, roots that do not survive or release across collections, and
listings whose read or render outgrows what the memory gate charges.
-}
spec :: Spec
spec = do
    describe "metadata retained heap" $ do
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
    describe "listing peak heap" $ do
        it "keeps each read-peak limit below its full-read charge" $
            for_ [Npm, PyPI] $ \ecosystem ->
                for_ ((,) <$> chargeFactors ecosystem <*> readPeakEnvelopePermille ecosystem) $ \(factors, envelope) ->
                    envelope `shouldSatisfy` (< toInteger (cfFullReadPermille factors))
        forM_ packages $ \package -> it (toString (cpName package)) $ do
            (size, _) <- authenticate package
            measureInChild ("--metadata-listing-probe" : majorSampling) package >>= \case
                Left failure -> expectationFailure failure
                Right peaks -> do
                    reportListing package peaks
                    listingSourceBytes peaks `shouldBe` size
                    let ecosystem = pkgEcosystem (cpPackage package)
                    for_ ((,) <$> chargeFactors ecosystem <*> readPeakEnvelopePermille ecosystem) (uncurry (checkListing peaks))
                    when (entryBelowSource ecosystem) (rise listingEntryLive listingBaseline peaks `shouldSatisfy` (< toInteger size))
                    -- A document holds heap, and its weight, as a cache expands it, covers that heap.
                    listingDocumentLive peaks `shouldSatisfy` (> 0)
                    listingDocumentLive peaks `shouldSatisfy` (<= toInteger (listingDocumentCharge peaks))

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
    -- A listing's full read holds a shared entry, and its output holds the encoding and a strict copy.
    when (shape == Shared) $ for_ (chargeFactors ecosystem) $ \factors -> do
        (1000 * retained) `shouldSatisfy` (<= toInteger (cfFullReadPermille factors) * toInteger size)
        (2000 * toInteger (compactBytes result)) `shouldSatisfy` (<= toInteger (cfOutputPermille factors) * toInteger size)

{- A shared entry's gate is a regression limit above its measured maximum. Separately, it must stay
within the memory gate's full-read charge, so a representation cannot outgrow what admission charges. -}
envelopePermille :: Ecosystem -> Shape -> Integer
envelopePermille Npm Typed = 500
envelopePermille Npm Shared = 750
envelopePermille PyPI Typed = 1750
envelopePermille PyPI Shared = 3500
envelopePermille _ shape = case shape of
    Wire -> 1250
    Raw -> 7000
    Typed -> 2250
    Shared -> 7500

{- The old generation may not outgrow its live data and the nursery holds 128 KiB, so nearly every
collection is major and the high-water samples live data at least once per 128 KiB allocated. -}
majorSampling :: [String]
majorSampling = ["+RTS", "-F1", "-A128k", "-RTS"]

{- From one meter step of source up, a read's peak fails the tier past this limit, the smallest quarter
step at least 8% above its measured maximum. The limit sits below the full-read charge. -}
readPeakEnvelopePermille :: Ecosystem -> Maybe Integer
readPeakEnvelopePermille = \case
    Npm -> Just 2000
    PyPI -> Just 3750
    RubyGems -> Nothing

{- Whether a listing's held entry stays smaller than the source it was read from. A PyPI entry holds each
file as aeson's tree beside its typed view, which outgrows the file. -}
entryBelowSource :: Ecosystem -> Bool
entryBelowSource = \case
    Npm -> True
    PyPI -> False
    RubyGems -> False

{- A read pays whole meter steps from its entry step on, and a render pays on top. From one step of
source up, the read's peak stays under its limit and the render's working set under its charge. -}
checkListing :: ListingPeaks -> ChargeFactors -> Integer -> Expectation
checkListing peaks factors envelope = do
    rise listingReadPeak listingBaseline peaks `shouldSatisfy` (<= paid fullRead)
    rise listingPeak listingBaseline peaks `shouldSatisfy` (<= paid (fullRead + output))
    when (size >= meterStepBytes) $ do
        (1000 * rise listingReadPeak listingBaseline peaks) `shouldSatisfy` (<= envelope * toInteger size)
        -- Collections miss the instant the lazy encoding and its strict copy are both live.
        max (rise listingPeak listingEntryLive peaks) (2 * toInteger (listingServedBytes peaks)) `shouldSatisfy` (<= toInteger output)
  where
    size = listingSourceBytes peaks
    fullRead = scaleCharge (cfFullReadPermille factors) size
    output = scaleCharge (cfOutputPermille factors) size
    paid = toInteger . roundUpToStep meterStepBytes . max meterStepBytes

rise :: (ListingPeaks -> Word64) -> (ListingPeaks -> Word64) -> ListingPeaks -> Integer
rise high low peaks = toInteger (high peaks) - toInteger (low peaks)

reportListing :: CorpusPackage -> ListingPeaks -> IO ()
reportListing package peaks =
    putStrLn . ("metadata-listing " <>) . decodeUtf8 . LBS.toStrict . encode $
        object
            [ "package" .= cpName package
            , "read_peak_per_source_byte" .= perSourceByte (rise listingReadPeak listingBaseline peaks)
            , "entry_per_source_byte" .= perSourceByte (rise listingEntryLive listingBaseline peaks)
            , "peak_above_entry_per_source_byte" .= perSourceByte (rise listingPeak listingEntryLive peaks)
            , "served_per_source_byte" .= perSourceByte (toInteger (listingServedBytes peaks))
            , "document_per_source_byte" .= perSourceByte (listingDocumentLive peaks)
            , "document_charge_per_source_byte" .= perSourceByte (toInteger (listingDocumentCharge peaks))
            , "peaks" .= peaks
            ]
  where
    perSourceByte bytes = fromInteger bytes / fromIntegral (listingSourceBytes peaks) :: Double

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
