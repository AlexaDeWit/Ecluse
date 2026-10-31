-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Dense npm and PyPI listings, and versions and files with minimal bodies, each read in a fresh
child process. Their bytes pay less than their floors, so the floors cover what the read holds, and
the floors for the entries the finished document keeps cover what it holds once read. A PyPI index
of repeated envelope arrays, which count toward no floor, peaks within its per-byte charge.
-}
module Ecluse.Core.Registry.Metadata.EntryFloorResidencySpec (spec) where

import Data.Aeson (encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import System.FilePath ((</>))
import Test.Hspec
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI), ecosystemName)
import Ecluse.Core.Package (mkPackageName)
import Ecluse.Core.Registry.Adapter (RegistryAdapter (adapterMetadata))
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataChargeFactors, metadataRead))
import Ecluse.Core.Registry.Metadata (Manifest (manifestRetainedEntries))
import Ecluse.Core.Registry.Metadata.EntryFloor (RetainedEntries (..), entryFloorCharge)
import Ecluse.Core.Server.Admission.Budget (scaleCharge)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (cfFullReadPermille, cfKeptPermille))
import Ecluse.Core.Server.MemoryModel.Calibration (EntryFigures (..), entryFloorFigures, heldLimit, readPeakLimit, releaseFloorFigures)
import Ecluse.Core.Server.MemoryModel.Probe (ListingPeaks (..), SingleListing (..), measurePathInChild)
import Ecluse.Core.Server.MemoryModel.ProbeMode qualified as Mode
import Ecluse.Test.Registry.JsonBytes (commas, quoted, render)
import Ecluse.Test.Registry.Metadata.Fetch (heldWorkspacePeak)
import Ecluse.Test.Registry.Npm (minimalPackument)
import Ecluse.Test.Registry.PyPI (minimalIndex)
import Ecluse.Test.Server.Mount (requiredAdapter)
import Ecluse.Test.Support (expectRight)

-- | One dense document: the entry kind it repeats, its bytes for a count of entries, and the entries its read holds and its finished document keeps.
data Dense = Dense
    { denseEcosystem :: Ecosystem
    , denseKind :: Text
    , denseDocument :: Int -> ByteString
    , denseRead :: RetainedEntries
    , denseKept :: RetainedEntries
    }

{- | Each listing's peak and held bytes against its floors and figure limits, and the repeated arrays'
peak against the per-byte full-read charge.
-}
spec :: Spec
spec = describe "dense listing live bytes" $ do
    for_ (denseListings <> minimalBodies) $ \dense ->
        it (toString (label dense)) $ do
            let bytes = denseDocument dense entries
                size = BS.length bytes
                held = denseRead dense
                total = retainedReleases held + retainedOthers held
            (factors, workspace, kept, peaks) <- readListing (denseEcosystem dense) bytes
            let readRise = rise listingReadPeak peaks
                heldRise = rise listingEntryLive peaks
            report dense size total readRise heldRise
            workspace `shouldBe` max 0 (entryFloorCharge held - scaleCharge (cfFullReadPermille factors) size)
            kept `shouldBe` denseKept dense
            readRise `shouldSatisfy` (<= toInteger (max (scaleCharge (cfFullReadPermille factors) size) (entryFloorCharge held)))
            heldRise `shouldSatisfy` (<= toInteger (max (scaleCharge (cfKeptPermille factors) size) (entryFloorCharge kept)))
            figures <- maybe (fail "no dense listing figures") pure (Map.lookup (label dense) (entryFloorFigures <> releaseFloorFigures))
            (1000 * readRise) `shouldSatisfy` (<= readPeakLimit (entryReadPeak figures) * toInteger total + 1000 * tolerance)
            (1000 * heldRise) `shouldSatisfy` (<= heldLimit (entryHeld figures) * toInteger total + 1000 * tolerance)
    -- A repeat counts toward no floor, so the read may hold none of it.
    it "pypi/repeated-arrays keeps a repeated envelope array within the read's charge" $ do
        let items = commas (replicate repeatedItems "0")
            bytes =
                render
                    ( "{\"meta\":{\"api-version\":\"1.1\",\"tracks\":[],\"tracks\":["
                        <> items
                        <> "]},\"name\":\"thing\",\"alternate-locations\":[],\"alternate-locations\":["
                        <> items
                        <> "],\"meta\":{\"tracks\":["
                        <> items
                        <> "]},\"files\":["
                        <> validFile
                        <> "]}"
                    )
            size = BS.length bytes
        (factors, workspace, kept, peaks) <- readListing PyPI bytes
        workspace `shouldBe` 0
        kept `shouldBe` releases 1
        rise listingReadPeak peaks `shouldSatisfy` (<= toInteger (max (scaleCharge (cfFullReadPermille factors) size) (entryFloorCharge (releases 1))))

-- The production read's charge factors, the workspace it paid and the entries it keeps, and a child's peaks reading the same bytes.
readListing :: Ecosystem -> ByteString -> IO (ChargeFactors, Int, RetainedEntries, ListingPeaks)
readListing ecosystem bytes = withSystemTempDirectory "ecluse-dense-listing" $ \directory -> do
    let path = directory </> "dense.json"
    BS.writeFile path bytes
    adapter <- requiredAdapter ecosystem
    (result, workspace) <- heldWorkspacePeak (metadataRead (adapterMetadata adapter)) (registryOf ecosystem) (mkPackageName ecosystem Nothing "thing") [bytes]
    kept <- manifestRetainedEntries <$> expectRight result
    measured <- measurePathInChild (Mode.ListingFor (toString (ecosystemName ecosystem)) "thing") path
    peaks <- singlePeaks <$> expectRight measured
    pure (metadataChargeFactors (adapterMetadata adapter), workspace, kept, peaks)

-- Items in each repeated array: held whole, they would outweigh the per-byte charge several times over.
repeatedItems :: Int
repeatedItems = 30000

-- Enough entries that the read's fixed costs add under a byte to each.
entries :: Int
entries = 10000

-- The other probes' 16 KiB release tolerance, so a figure near zero, such as npm's held timestamps, fails on growth and not on one block.
tolerance :: Integer
tolerance = 16 * 1024

-- The densest form of each entry kind a full read retains: a key and a value that does not decode, or an empty item.
-- An npm timestamp under a key that names no version is dropped, so its document keeps only its one release.
denseListings :: [Dense]
denseListings =
    [ Dense Npm "versions" (\count -> render ("{\"name\":\"thing\",\"versions\":{" <> commas [quoted ("0.0." <> Builder.intDec n) <> ":{}" | n <- [1 .. count]] <> "}}")) (releases entries) (releases entries)
    , Dense Npm "timestamps" (packument "time") (RetainedEntries 1 entries) (RetainedEntries 1 0)
    , Dense Npm "dist-tags" (packument "dist-tags") (RetainedEntries 1 entries) (RetainedEntries 1 entries)
    , Dense PyPI "files" (\count -> index ("\"files\":[" <> commas (validFile : replicate count "0") <> "]")) (releases (entries + 1)) (releases (entries + 1))
    , Dense PyPI "versions" (\count -> index ("\"versions\":[" <> commas (replicate count "0") <> "],\"files\":[" <> validFile <> "]")) (RetainedEntries 1 entries) (RetainedEntries 1 entries)
    , Dense PyPI "alternate-locations" (\count -> index ("\"alternate-locations\":[" <> commas (replicate count "0") <> "],\"files\":[" <> validFile <> "]")) (RetainedEntries 1 entries) (RetainedEntries 1 entries)
    , Dense PyPI "tracks" (\count -> render ("{\"meta\":{\"api-version\":\"1.1\",\"tracks\":[" <> commas (replicate count "0") <> "]},\"name\":\"thing\",\"files\":[" <> validFile <> "]}")) (RetainedEntries 1 entries) (RetainedEntries 1 entries)
    ]
  where
    -- One release that renders, beside an object of keys whose values do not decode.
    packument key count =
        render
            ( "{\"name\":\"thing\",\"versions\":{\"1.0.0\":{\"name\":\"thing\",\"version\":\"1.0.0\",\"dist\":{\"tarball\":\"https://registry.npmjs.org/thing/-/thing-1.0.0.tgz\"}}},\""
                <> key
                <> "\":{"
                <> commas [quoted (Builder.intDec n) <> ":0" | n <- [1 .. count]]
                <> "}}"
            )
    index members = render ("{\"meta\":{\"api-version\":\"1.1\"},\"name\":\"thing\"," <> members <> "}")

-- One PyPI file that names a release, so a listing renders.
validFile :: Builder.Builder
validFile = "{\"filename\":\"thing-1.0.0.tar.gz\",\"url\":\"https://files.pythonhosted.org/thing-1.0.0.tar.gz\"}"

-- Versions and files with the smallest valid body, which hold more for each entry than any dense form.
minimalBodies :: [Dense]
minimalBodies =
    [ Dense Npm "minimal-versions" minimalPackument (releases entries) (releases entries)
    , Dense PyPI "minimal-files" minimalIndex (releases entries) (releases entries)
    ]

releases :: Int -> RetainedEntries
releases count = RetainedEntries count 0

label :: Dense -> Text
label dense = ecosystemName (denseEcosystem dense) <> "/" <> denseKind dense

registryOf :: Ecosystem -> Text
registryOf = \case
    PyPI -> "https://pypi.org/simple"
    _ -> "https://registry.npmjs.org"

rise :: (ListingPeaks -> Word64) -> ListingPeaks -> Integer
rise high peaks = toInteger (high peaks) - toInteger (listingBaseline peaks)

-- Each figure is a rise over the retained entries, so a recalibration reads it from the line.
report :: Dense -> Int -> Int -> Integer -> Integer -> IO ()
report dense size retained readRise heldRise =
    putTextLn . ("metadata-dense-listing " <>) . decodeUtf8 . LBS.toStrict . encode $
        object
            [ "listing" .= label dense
            , "source_bytes" .= size
            , "retained_entries" .= retained
            , "read_peak_bytes" .= readRise
            , "held_bytes" .= heldRise
            , "read_peak_per_entry" .= (fromInteger readRise / fromIntegral retained :: Double)
            , "held_per_entry" .= (fromInteger heldRise / fromIntegral retained :: Double)
            ]
