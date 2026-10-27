-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Corpus-authenticated metadata measurements in one child process per retained shape or listing.
module Ecluse.Core.Server.MemoryModelResidencySpec (spec, sourceMain, selectedMain, probeIdentity, probeLimits, checkPaid, checkPhases, checkReadPeak, checkOutputCharge, checkOutputLimit) where

import Prelude hiding (universe)

import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (encode, object, (.=))
import Data.Aeson.Types (Pair)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Universe.Class (universe)
import GHC.Conc (getNumProcessors)
import Test.Hspec
import UnliftIO.Async (pooledMapConcurrentlyN)
import UnliftIO.Exception (evaluate, tryAny)
import UnliftIO.Temporary (withSystemTempDirectory)

import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Ecluse.Composition.MemoryPlan.Transient (meterStepBytes)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName, parseEcosystem)
import Ecluse.Core.Package (PackageName, pkgEcosystem)
import Ecluse.Core.Registry.Adapter (RegistryAdapter (adapterMetadata), adapterFor)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataChargeFactors))
import Ecluse.Core.Security (Limits (maxMetadataBytes), defaultLimits)
import Ecluse.Core.Server.Admission.Budget (roundUpToStep, scaleCharge)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (cfFullReadPermille, cfKeptPermille, cfOutputPermille))
import Ecluse.Core.Server.MemoryModel (expandWireBytes)
import Ecluse.Core.Server.MemoryModel.Calibration (Figures (..), fullReadCharge, heldLimit, readPeakLimit)
import Ecluse.Core.Server.MemoryModel.Probe (ListingPeaks (..), Measurement (..), Residency (..), SelectedShape (SelectedControl, SelectedValue), Shape (..), SingleListing (..), measureInChild, packages, probeSelected, probeSource, requireFigures, requireResidency, retainedFigure, writeMergeDocuments)
import Ecluse.Core.Server.MemoryModel.ProbeMode qualified as Mode
import Ecluse.Core.Snapshot (digestBytes)
import Ecluse.Core.Version (Version, renderVersion)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage, cpPath), cpName, readCapture)
import Ecluse.Test.Corpus.Merge (MergeShape (..), realisticShape)
import Ecluse.Test.Package (hexSha256Of)

-- | Reject unauthenticated captures, failed retention or release, and listings that outgrow admission charges.
spec :: Spec
spec = do
    describe "metadata residency coverage" $
        for_ universe $ \ecosystem ->
            when (isJust (adapterFor ecosystem)) $
                it ("covers registered adapter " <> show ecosystem) $ do
                    record <- requireResidency ecosystem
                    length (residencyPackages record) `shouldSatisfy` (> 0)
                    map (pkgEcosystem . cpPackage) (residencyPackages record) `shouldSatisfy` all (== ecosystem)
                    Map.keys (residencyFigures record) `shouldMatchList` map cpName (residencyPackages record)
    describe "metadata retained heap" $ do
        it "includes an authenticated capture above the previous 3,687,514-byte maximum" $ do
            sizes <- traverse authenticate packages
            sizes `shouldSatisfy` any ((> 3687514) . fst)
        forM_ packages $ \package ->
            forM_ [minBound .. maxBound] $ \shape ->
                it (toString (cpName package) <> "/" <> show shape) $ do
                    (size, digest) <- authenticate package
                    measureInChild (Mode.Retained (show shape)) package >>= \case
                        Left failure -> expectationFailure failure
                        Right result -> do
                            report package digest shape result
                            checkMeasurement package shape size result
    describe "listing peak heap" $ do
        it "keeps each regression limit below its charge" $
            for_ universe $ \ecosystem ->
                for_ (adapterFor ecosystem) $ \adapter -> do
                    record <- requireResidency ecosystem
                    let factors = metadataChargeFactors (adapterMetadata adapter)
                    for_ (residencyFigures record) $ \figures -> do
                        map readPeakLimit (maybeToList (readPeak figures)) `shouldSatisfy` all (< toInteger (cfFullReadPermille factors))
                        map heldLimit (maybeToList (singleOutput figures) <> Map.elems (mergedOutputs figures)) `shouldSatisfy` all (< toInteger (cfOutputPermille factors))
        it "keeps each shared entry's regression limit below the kept charge" $
            for_ universe $ \ecosystem ->
                for_ (adapterFor ecosystem) $ \adapter -> do
                    record <- requireResidency ecosystem
                    let kept = toInteger (cfKeptPermille (metadataChargeFactors (adapterMetadata adapter)))
                    map (heldLimit . retainedShared) (Map.elems (residencyFigures record)) `shouldSatisfy` all (< kept)
        forM_ packages $ \package -> it (toString (cpName package)) $ do
            (size, digest) <- authenticate package
            measureInChild Mode.Listing package >>= \case
                Left failure -> expectationFailure failure
                Right single -> do
                    let peaks = singlePeaks single
                        perSource bytes = fromInteger bytes / fromIntegral size :: Double
                    reportListing
                        "metadata-listing"
                        [ "document_per_source_byte" .= perSource (singleDocumentLive single)
                        , "document_charge_per_source_byte" .= perSource (toInteger (singleDocumentCharge single))
                        ]
                        package
                        digest
                        peaks
                    listingSourceBytes peaks `shouldBe` size
                    listingBasisBytes peaks `shouldBe` size
                    (record, factors) <- listingBounds package
                    figures <- requireFigures package
                    checkPaid factors peaks
                    checkPhases factors peaks
                    checkReadPeak (readPeak figures) peaks
                    checkOutputCharge factors peaks
                    checkOutputLimit (singleOutput figures) peaks
                    when (residencyEntryBelowSource record) (rise listingEntryLive listingBaseline peaks `shouldSatisfy` (< toInteger size))
                    singleDocumentLive single `shouldSatisfy` (> 0)
                    singleDocumentLive single `shouldSatisfy` (<= toInteger (singleDocumentCharge single))
    describe "merged listing peak heap" $ beforeAll measureMerges $ forM_ merges $ \(package, shape) ->
        it (toString (cpName package) <> "/" <> show shape) $ \measured -> do
            (size, digest) <- authenticate package
            case Map.lookup (cpPath package, shape) measured of
                Nothing -> expectationFailure "the merge probe did not run"
                Just (Left failure) -> expectationFailure failure
                Just (Right peaks) -> do
                    record <- requireResidency (pkgEcosystem (cpPackage package))
                    let derived = fullReadCharge <$> residencyCalibration record
                        observed = fromInteger (rise listingReadPeak listingBaseline peaks) / fromIntegral (listingSourceBytes peaks) :: Double
                    reportListing
                        "metadata-merge"
                        [ "shape" .= (show shape :: Text)
                        , "derived_full_read_permille" .= derived
                        , "full_read_headroom_per_source_byte" .= fmap (\charge -> fromInteger charge / 1000 - observed) derived
                        ]
                        package
                        digest
                        peaks
                    checkBasis shape size peaks
                    (_, factors) <- listingBounds package
                    figures <- requireFigures package
                    checkPaid factors peaks
                    checkPhases factors peaks
                    checkOutputCharge factors peaks
                    when (realisticShape shape) (checkOutputLimit (Map.lookup shape (mergedOutputs figures)) peaks)

merges :: [(CorpusPackage, MergeShape)]
merges = [(package, shape) | package <- packages, shape <- [minBound .. maxBound]]

-- Each child samples its own collections on one capability, so one child per processor can run.
measureMerges :: IO (Map (FilePath, MergeShape) (Either String ListingPeaks))
measureMerges = do
    jobs <- getNumProcessors
    Map.fromList <$> pooledMapConcurrentlyN jobs measure merges
  where
    -- A merge whose documents fail to write fails its own example, not the rest.
    measure (package, shape) = ((cpPath package, shape),) . either (Left . show) id <$> tryAny (measureMerge package shape)
    measureMerge package shape = withSystemTempDirectory "ecluse-merge" $ \directory -> do
        (private, public) <- writeMergeDocuments shape directory package
        measureInChild (Mode.Merge private public) package

checkMeasurement :: CorpusPackage -> Shape -> Int -> Measurement -> Expectation
checkMeasurement package shape size result = do
    figures <- requireFigures package
    factors <- maybe (fail "residency ecosystem has no adapter") pure (chargeFactors (pkgEcosystem (cpPackage package)))
    let retained = toInteger (heldLive result) - toInteger (baselineLive result)
        residual = max 0 (toInteger (releasedLive result) - toInteger (baselineLive result))
    wireBytes result `shouldBe` size
    retained `shouldSatisfy` (> 0)
    residual `shouldSatisfy` (<= 16 * 1024)
    (10 * residual) `shouldSatisfy` (<= retained)
    (1000 * retained) `shouldSatisfy` (<= heldLimit (retainedFigure shape figures) * toInteger size)
    when (shape == Typed || shape == Shared) (versions result `shouldSatisfy` (> 0))
    when (shape == Shared) $ do
        (1000 * retained) `shouldSatisfy` (<= toInteger (cfFullReadPermille factors) * toInteger size)
        (2000 * toInteger (compactBytes result)) `shouldSatisfy` (<= toInteger (cfOutputPermille factors) * toInteger size)

listingBounds :: CorpusPackage -> IO (Residency, ChargeFactors)
listingBounds package = (,) <$> requireResidency ecosystem <*> maybe (fail "residency ecosystem has no adapter") pure (chargeFactors ecosystem)
  where
    ecosystem = pkgEcosystem (cpPackage package)

-- | Whole meter steps cover sub-step sources and output bases, including the entry step.
checkPaid :: ChargeFactors -> ListingPeaks -> Expectation
checkPaid factors peaks = do
    rise listingReadPeak listingBaseline peaks `shouldSatisfy` (<= paid fullRead)
    rise listingPeak listingBaseline peaks `shouldSatisfy` (<= paid (fullRead + output))
    outputWorkingSet peaks `shouldSatisfy` (<= paid output)
  where
    fullRead = scaleCharge (cfFullReadPermille factors) (listingSourceBytes peaks)
    output = scaleCharge (cfOutputPermille factors) (listingBasisBytes peaks)
    paid = toInteger . paidSteps

-- What the meter holds for a charge: whole steps, at least the entry step.
paidSteps :: Int -> Int
paidSteps = roundUpToStep meterStepBytes . max meterStepBytes

-- | Keep retained entries and rendering within the existing stepped read/output charges.
checkPhases :: ChargeFactors -> ListingPeaks -> Expectation
checkPhases factors peaks = do
    entries `shouldSatisfy` (<= toInteger (kept + headroom))
    entries + outputWorkingSet peaks `shouldSatisfy` (<= toInteger (kept + sending))
    toInteger (listingServedBytes peaks) `shouldSatisfy` (<= toInteger sending)
  where
    entries = rise listingEntryLive listingBaseline peaks
    fullRead = scaleCharge (cfFullReadPermille factors) (listingSourceBytes peaks)
    kept = scaleCharge (cfKeptPermille factors) (listingSourceBytes peaks)
    headroom = paidSteps fullRead - fullRead
    output = scaleCharge (cfOutputPermille factors) (listingBasisBytes peaks)
    sending = headroom + roundUpToStep meterStepBytes (max 0 (output - headroom))

-- | From one step of source up, a read's peak stays under its capture's limit, which sits below its charge.
checkReadPeak :: Maybe Rational -> ListingPeaks -> Expectation
checkReadPeak figure peaks = when (size >= meterStepBytes) $ case figure of
    Nothing -> expectationFailure "no read peak figure for a source of at least one meter step"
    Just ratio -> (1000 * rise listingReadPeak listingBaseline peaks) `shouldSatisfy` (<= readPeakLimit ratio * toInteger size)
  where
    size = listingSourceBytes peaks

-- | From one step of basis up, the output working set fits the output charge.
checkOutputCharge :: ChargeFactors -> ListingPeaks -> Expectation
checkOutputCharge factors peaks = when (basis >= meterStepBytes) $ outputWorkingSet peaks `shouldSatisfy` (<= toInteger (scaleCharge (cfOutputPermille factors) basis))
  where
    basis = listingBasisBytes peaks

-- | From one step of basis up, a realistic listing's output working set stays under its own limit.
checkOutputLimit :: Maybe Rational -> ListingPeaks -> Expectation
checkOutputLimit figure peaks = when (basis >= meterStepBytes) $ case figure of
    Nothing -> expectationFailure "no output working set figure for a basis of at least one meter step"
    Just ratio -> (1000 * outputWorkingSet peaks) `shouldSatisfy` (<= heldLimit ratio * toInteger basis)
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
    HeavyBase -> heavier
    HeavyOldBase -> heavier
  where
    heavier = listingBasisBytes peaks `shouldSatisfy` (\basis -> basis > size && basis < listingSourceBytes peaks)

rise :: (ListingPeaks -> Word64) -> (ListingPeaks -> Word64) -> ListingPeaks -> Integer
rise high low peaks = toInteger (high peaks) - toInteger (low peaks)

reportListing :: Text -> [Pair] -> CorpusPackage -> Text -> ListingPeaks -> IO ()
reportListing label details package digest peaks =
    putStrLn . toString . ((label <> " ") <>) . decodeUtf8 . LBS.toStrict . encode . object $
        details
            <> [ "ecosystem" .= ecosystemName (pkgEcosystem (cpPackage package))
               , "package" .= cpName package
               , "path" .= cpPath package
               , "sha256" .= digest
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
    bytes <- readCapture package
    size <- evaluate (BS.length bytes)
    digest <- evaluate (hexSha256Of bytes)
    pure (size, digest)

report :: CorpusPackage -> Text -> Shape -> Measurement -> IO ()
report package digest shape result = do
    let retained = toInteger (heldLive result) - toInteger (baselineLive result)
        ratio = fromInteger retained / fromIntegral (wireBytes result) :: Double
        row =
            object
                [ "ecosystem" .= ecosystemName (pkgEcosystem (cpPackage package))
                , "package" .= cpName package
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
    kind <- maybe (fail "unknown probe ecosystem") pure (parseEcosystem (toText ecosystem))
    record <- requireResidency kind
    residencyIdentity record name version

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
