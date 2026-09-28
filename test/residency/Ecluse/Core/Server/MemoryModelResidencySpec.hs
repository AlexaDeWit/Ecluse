-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Corpus-authenticated metadata measurements in one child process per retained shape or listing.
module Ecluse.Core.Server.MemoryModelResidencySpec (spec, sourceMain, selectedMain, probeIdentity, probeLimits) where

import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (Object, encode, object, (.:), (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (Pair, Parser)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Test.Hspec
import UnliftIO.Temporary (withSystemTempDirectory)

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
import Ecluse.Core.Server.MemoryModel.Probe (ListingPeaks (..), Measurement (..), SelectedShape (SelectedControl, SelectedValue), Shape (..), measureInChild, packages, probeSelected, probeSource, writeMergeDocuments)
import Ecluse.Core.Snapshot (digestBytes)
import Ecluse.Core.Version (Version, canonicalPep440, mkVersion, renderVersion)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpPath), cpName, readCorpusPins)
import Ecluse.Test.Corpus.Merge (MergeShape (..))

{- | Reject unauthenticated captures, roots that do not survive or release across collections, and
listings, of one source or two, whose reads or render outgrow what the memory gate charges.
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
                for_ ((,) <$> chargeFactors ecosystem <*> peakLimits ecosystem) $ \(factors, limits) ->
                    readPeakLimit limits `shouldSatisfy` (< toInteger (cfFullReadPermille factors))
        forM_ packages $ \package -> it (toString (cpName package)) $ do
            (size, _) <- authenticate package
            measureInChild ("--metadata-listing-probe" : majorSampling) package >>= \case
                Left failure -> expectationFailure failure
                Right peaks -> do
                    reportListing "metadata-listing" [] package peaks
                    listingSourceBytes peaks `shouldBe` size
                    listingBasisBytes peaks `shouldBe` size
                    for_ (listingBounds package) $ \(factors, limits) -> do
                        checkPaid factors peaks
                        checkReadPeak limits peaks
                        checkOutput factors limits peaks
    describe "merged listing peak heap" $ forM_ packages $ \package ->
        forM_ [minBound .. maxBound] $ \shape ->
            it (toString (cpName package) <> "/" <> show shape) $ do
                (size, _) <- authenticate package
                withSystemTempDirectory "ecluse-merge" $ \directory -> do
                    (private, public) <- writeMergeDocuments shape directory package
                    measureInChild (["--metadata-merge-probe", private, public] <> majorSampling) package >>= \case
                        Left failure -> expectationFailure failure
                        Right peaks -> do
                            reportListing "metadata-merge" ["shape" .= (show shape :: Text)] package peaks
                            checkBasis shape size peaks
                            for_ (listingBounds package) $ \(factors, limits) -> do
                                checkPaid factors peaks
                                checkOutput factors limits peaks

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
envelopePermille Npm Shared = 1750
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

-- Regression limits per source byte, in thousandths.
data PeakLimits = PeakLimits
    { readPeakLimit :: Integer
    , outputLimit :: Integer
    -- ^ Per byte of the output basis.
    }

{- From one meter step up, a read's peak and a listing's output working set each fail the tier past
these limits, the smallest quarter step at least 8% above their measured maxima. -}
peakLimits :: Ecosystem -> Maybe PeakLimits
peakLimits = \case
    Npm -> Just PeakLimits{readPeakLimit = 2000, outputLimit = 1750}
    PyPI -> Just PeakLimits{readPeakLimit = 3750, outputLimit = 1500}
    RubyGems -> Nothing

listingBounds :: CorpusPackage -> Maybe (ChargeFactors, PeakLimits)
listingBounds package = (,) <$> chargeFactors ecosystem <*> peakLimits ecosystem
  where
    ecosystem = pkgEcosystem (cpPackage package)

-- A listing pays whole meter steps from its entry step on, for its reads and then its render.
checkPaid :: ChargeFactors -> ListingPeaks -> Expectation
checkPaid factors peaks = do
    rise listingReadPeak listingBaseline peaks `shouldSatisfy` (<= paid fullRead)
    rise listingPeak listingBaseline peaks `shouldSatisfy` (<= paid (fullRead + output))
  where
    fullRead = scaleCharge (cfFullReadPermille factors) (listingSourceBytes peaks)
    output = scaleCharge (cfOutputPermille factors) (listingBasisBytes peaks)
    paid = toInteger . roundUpToStep meterStepBytes . max meterStepBytes

-- From one step of source up, a read's peak stays under its limit, which sits below its charge.
checkReadPeak :: PeakLimits -> ListingPeaks -> Expectation
checkReadPeak limits peaks = when (size >= meterStepBytes) $ do
    (1000 * rise listingReadPeak listingBaseline peaks) `shouldSatisfy` (<= readPeakLimit limits * toInteger size)
  where
    size = listingSourceBytes peaks

-- From one step of basis up, the output working set fits the output charge and stays under its limit.
checkOutput :: ChargeFactors -> PeakLimits -> ListingPeaks -> Expectation
checkOutput factors limits peaks = when (basis >= meterStepBytes) $ do
    outputWorkingSet peaks `shouldSatisfy` (<= toInteger (scaleCharge (cfOutputPermille factors) basis))
    (1000 * outputWorkingSet peaks) `shouldSatisfy` (<= outputLimit limits * toInteger basis)
  where
    basis = listingBasisBytes peaks

-- Collections miss the instant the lazy encoding and its strict copy are both live, so both count.
outputWorkingSet :: ListingPeaks -> Integer
outputWorkingSet peaks = max (rise listingPeak listingEntryLive peaks) (2 * toInteger (listingServedBytes peaks))

-- The basis counts one copy of the versions both documents hold and every copy of the rest.
checkBasis :: MergeShape -> Int -> ListingPeaks -> Expectation
checkBasis shape size peaks = case shape of
    Identical -> do
        listingSourceBytes peaks `shouldBe` 2 * size
        listingBasisBytes peaks `shouldBe` size
    Overlapping -> listingBasisBytes peaks `shouldSatisfy` (< listingSourceBytes peaks)
    Disjoint -> listingBasisBytes peaks `shouldBe` listingSourceBytes peaks
    PublishOrder -> listingBasisBytes peaks `shouldSatisfy` (\basis -> basis >= size && basis < listingSourceBytes peaks)
    HeavyBase -> listingBasisBytes peaks `shouldSatisfy` (\basis -> basis > size && basis < listingSourceBytes peaks)

rise :: (ListingPeaks -> Word64) -> (ListingPeaks -> Word64) -> ListingPeaks -> Integer
rise high low peaks = toInteger (high peaks) - toInteger (low peaks)

reportListing :: Text -> [Pair] -> CorpusPackage -> ListingPeaks -> IO ()
reportListing label details package peaks =
    putStrLn . toString . ((label <> " ") <>) . decodeUtf8 . LBS.toStrict . encode . object $
        details
            <> [ "package" .= cpName package
               , "read_peak_per_source_byte" .= perByte listingSourceBytes (rise listingReadPeak listingBaseline peaks)
               , "entry_per_source_byte" .= perByte listingSourceBytes (rise listingEntryLive listingBaseline peaks)
               , "peak_above_entry_per_source_byte" .= perByte listingSourceBytes (rise listingPeak listingEntryLive peaks)
               , "served_per_source_byte" .= perByte listingSourceBytes (toInteger (listingServedBytes peaks))
               , "basis_per_source_byte" .= perByte listingSourceBytes (toInteger (listingBasisBytes peaks))
               , "output_working_set_per_basis_byte" .= perByte listingBasisBytes (outputWorkingSet peaks)
               , "output_working_set_per_charged_byte" .= (fromInteger (outputWorkingSet peaks) / fromIntegral charged :: Double)
               , "peaks" .= peaks
               ]
  where
    perByte bytesOf bytes = fromInteger bytes / fromIntegral (bytesOf peaks) :: Double
    charged = maybe 0 (\factors -> scaleCharge (cfOutputPermille factors) (listingBasisBytes peaks)) (chargeFactors (pkgEcosystem (cpPackage package)))

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
