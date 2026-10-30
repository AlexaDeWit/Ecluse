-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Dense npm and PyPI listings, many retained entries in few bytes, each read in a fresh child
process. Their bytes pay almost nothing, so the entry floor alone covers what the read holds.
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
import Ecluse.Core.Registry.Metadata.EntryFloor (entryFloorCharge)
import Ecluse.Core.Server.Admission.Budget (scaleCharge)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (cfFullReadPermille, cfKeptPermille))
import Ecluse.Core.Server.MemoryModel.Calibration (EntryFigures (..), denseFigures, heldLimit, readPeakLimit)
import Ecluse.Core.Server.MemoryModel.Probe (ListingPeaks (..), SingleListing (..), measurePathInChild)
import Ecluse.Core.Server.MemoryModel.ProbeMode qualified as Mode
import Ecluse.Test.Registry.Metadata.Fetch (heldWorkspacePeak)
import Ecluse.Test.Server.Mount (requiredAdapter)
import Ecluse.Test.Support (expectRight)

-- | One dense document: the entry kind it repeats, and its bytes for a count of entries.
data Dense = Dense
    { denseEcosystem :: Ecosystem
    , denseKind :: Text
    , denseDocument :: Int -> ByteString
    }

-- | The read's peak and what it holds once read, against the floor and against each figure's limit.
spec :: Spec
spec = describe "dense listing live bytes" $
    for_ denseListings $ \dense ->
        it (toString (label dense)) $ withSystemTempDirectory "ecluse-dense-listing" $ \directory -> do
            let path = directory </> "dense.json"
                bytes = denseDocument dense entries
                size = BS.length bytes
            BS.writeFile path bytes
            adapter <- requiredAdapter (denseEcosystem dense)
            (result, _) <- heldWorkspacePeak (metadataRead (adapterMetadata adapter)) (registryOf (denseEcosystem dense)) (mkPackageName (denseEcosystem dense) Nothing "thing") [bytes]
            retained <- manifestRetainedEntries <$> expectRight result
            measured <- measurePathInChild (Mode.ListingFor (toString (ecosystemName (denseEcosystem dense))) "thing") path
            peaks <- singlePeaks <$> expectRight measured
            let readRise = rise listingReadPeak peaks
                heldRise = rise listingEntryLive peaks
                factors = metadataChargeFactors (adapterMetadata adapter)
            report dense size retained readRise heldRise
            retained `shouldSatisfy` (>= entries)
            readRise `shouldSatisfy` (<= toInteger (max (scaleCharge (cfFullReadPermille factors) size) (entryFloorCharge retained)))
            heldRise `shouldSatisfy` (<= toInteger (max (scaleCharge (cfKeptPermille factors) size) (entryFloorCharge retained)))
            figures <- maybe (fail "no dense listing figures") pure (Map.lookup (label dense) denseFigures)
            (1000 * readRise) `shouldSatisfy` (<= readPeakLimit (entryReadPeak figures) * toInteger retained + 1000 * tolerance)
            (1000 * heldRise) `shouldSatisfy` (<= heldLimit (entryHeld figures) * toInteger retained + 1000 * tolerance)

-- Enough entries that the read's fixed costs add under a byte to each.
entries :: Int
entries = 10000

-- The other probes' 16 KiB release tolerance, so a figure near zero, such as npm's held timestamps, fails on growth and not on one block.
tolerance :: Integer
tolerance = 16 * 1024

-- The densest form of each entry kind a full read retains: a key and a value that does not decode, or an empty item.
denseListings :: [Dense]
denseListings =
    [ Dense Npm "versions" (\count -> render ("{\"name\":\"thing\",\"versions\":{" <> commas [quoted ("0.0." <> Builder.intDec n) <> ":{}" | n <- [1 .. count]] <> "}}"))
    , Dense Npm "timestamps" (packument "time")
    , Dense Npm "dist-tags" (packument "dist-tags")
    , Dense PyPI "files" (\count -> index ("\"files\":[" <> commas (validFile : replicate count "0") <> "]"))
    , Dense PyPI "versions" (\count -> index ("\"versions\":[" <> commas (replicate count "0") <> "],\"files\":[" <> validFile <> "]"))
    , Dense PyPI "alternate-locations" (\count -> index ("\"alternate-locations\":[" <> commas (replicate count "0") <> "],\"files\":[" <> validFile <> "]"))
    , Dense PyPI "tracks" (\count -> render ("{\"meta\":{\"api-version\":\"1.1\",\"tracks\":[" <> commas (replicate count "0") <> "]},\"name\":\"thing\",\"files\":[" <> validFile <> "]}"))
    ]
  where
    -- One release that renders, beside an object of keys whose values do not decode.
    packument key count =
        render
            ( "{\"name\":\"thing\",\"versions\":{\"1.0.0\":{\"dist\":{\"tarball\":\"https://registry.npmjs.org/thing/-/thing-1.0.0.tgz\"}}},\""
                <> key
                <> "\":{"
                <> commas [quoted (Builder.intDec n) <> ":0" | n <- [1 .. count]]
                <> "}}"
            )
    index members = render ("{\"meta\":{\"api-version\":\"1.1\"},\"name\":\"thing\"," <> members <> "}")
    validFile = "{\"filename\":\"thing-1.0.0.tar.gz\",\"url\":\"https://files.pythonhosted.org/thing-1.0.0.tar.gz\"}"

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

quoted :: Builder.Builder -> Builder.Builder
quoted text = "\"" <> text <> "\""

commas :: [Builder.Builder] -> Builder.Builder
commas = mconcat . intersperse ","

render :: Builder.Builder -> ByteString
render = toStrict . Builder.toLazyByteString
