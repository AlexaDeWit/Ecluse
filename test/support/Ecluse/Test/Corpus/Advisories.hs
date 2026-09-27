-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Advisory inputs for the benchmark corpus: the OSV records and EPSS rows captured under
@bench/corpus/advisories/@, and a generated worst case with many ranges per package. Both take
the served shape, an OSV export archive and a gzipped EPSS feed, for Pilot's compiler to read.
-}
module Ecluse.Test.Corpus.Advisories (
    AdvisoryInputs (..),
    corpusAdvisories,
    SyntheticTarget (..),
    syntheticAdvisories,
) where

import Codec.Compression.GZip qualified as GZip
import Data.Aeson (Object, Value, eitherDecode, encode, object, withObject, (.:), (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (parseEither)
import Data.Text qualified as T
import System.FilePath ((</>))

import Ecluse.Core.Ecosystem (Ecosystem (Npm), ecosystemName)
import Ecluse.Core.Osv.Ecosystem (osvEcosystemFor, osvExportDirectory)
import Ecluse.Core.Version (parseVersionKey)
import Ecluse.Test.Osv (osvZipOf)
import Ecluse.Test.Support (expectRightText)

-- | One ecosystem's OSV export archive and the gzipped EPSS feed that scores it.
data AdvisoryInputs = AdvisoryInputs
    { aiOsvZip :: LByteString
    , aiEpssFeed :: LByteString
    }

advisoryRoot :: FilePath
advisoryRoot = "bench/corpus/advisories"

-- | The captured records @bench/corpus/pins.json@ pins for the ecosystem, with the captured EPSS rows.
corpusAdvisories :: Ecosystem -> IO AdvisoryInputs
corpusAdvisories eco = do
    ids <- pinnedRecordIds eco
    records <- forM ids $ \recordId ->
        (recordId <> ".json",) <$> readFileLBS (advisoryRoot </> toString (ecosystemName eco) </> toString recordId <> ".json")
    archive <- osvZipOf records
    feed <- readFileLBS (advisoryRoot </> "epss.csv")
    pure AdvisoryInputs{aiOsvZip = archive, aiEpssFeed = GZip.compress feed}

pinnedRecordIds :: Ecosystem -> IO [Text]
pinnedRecordIds eco = do
    raw <- readFileLBS "bench/corpus/pins.json"
    expectRightText (first toText (eitherDecode raw >>= parseEither records))
  where
    records = withObject "pins" $ \pins -> do
        advisories :: Object <- pins .: "advisories"
        byEcosystem :: Object <- advisories .: "records"
        byEcosystem .: Key.fromText (ecosystemName eco)

-- | A package the generated worst case names, its release keys, and how many advisories it gets.
data SyntheticTarget = SyntheticTarget
    { stPackage :: Text
    , stVersions :: [Text]
    , stAdvisories :: Int
    }

{- | Advisories over windows of each target's own releases, each fixed at a real release. Every
other advisory carries a critical CVSS vector, and EPSS scores step from 0 to 0.95.
-}
syntheticAdvisories :: Ecosystem -> [SyntheticTarget] -> IO AdvisoryInputs
syntheticAdvisories eco targets = do
    archive <- osvZipOf [(recordId <> ".json", encode record) | (recordId, _, record) <- generated]
    pure AdvisoryInputs{aiOsvZip = archive, aiEpssFeed = GZip.compress (epssPreamble <> foldMap scoreLine generated)}
  where
    generated = zipWith (syntheticRecord eco) [0 ..] [(target, window) | target <- targets, window <- windows eco target]
    epssPreamble = "#model_version:synthetic,score_date:2026-01-01T00:00:00+0000\ncve,epss,percentile\n"
    scoreLine (_, (alias, score), _) = encodeUtf8 (alias <> "," <> score <> ",0.5\n")

-- Each advisory's introduced and fixed release, in the ecosystem's order.
windows :: Ecosystem -> SyntheticTarget -> [(Text, Text)]
windows eco target = mapMaybe window [0 .. stAdvisories target - 1]
  where
    ordered = map fst (sortOn snd [(raw, key) | raw <- stVersions target, Right key <- [parseVersionKey eco raw]])
    count = length ordered
    width = max 1 (count `div` 16)
    window i = do
        let start = i * (count - width) `div` stAdvisories target
        (,) <$> ordered !!? start <*> ordered !!? (start + width)

syntheticRecord :: Ecosystem -> Int -> (SyntheticTarget, (Text, Text)) -> (Text, (Text, Text), Value)
syntheticRecord eco n (target, (introduced, fixed)) = (recordId, (alias, score), record)
  where
    recordId = "ECLUSE-BENCH-" <> show n
    alias = "CVE-2099-" <> show (10000 + n)
    score = "0." <> T.justifyRight 2 '0' (show (n `mod` 20 * 5))
    vector
        | even n = "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:H/I:H/A:H" :: Text
        | otherwise = "CVSS:3.1/AV:N/AC:H/PR:N/UI:R/S:U/C:L/I:L/A:N"
    rangeType = if eco == Npm then "SEMVER" else "ECOSYSTEM" :: Text
    record =
        object
            [ "schema_version" .= ("1.6.0" :: Text)
            , "id" .= recordId
            , "modified" .= ("2026-01-01T00:00:00Z" :: Text)
            , "aliases" .= [alias]
            , "severity" .= [object ["type" .= ("CVSS_V3" :: Text), "score" .= vector]]
            , "affected"
                .= [ object
                        [ "package" .= object ["ecosystem" .= osvExportDirectory (osvEcosystemFor eco), "name" .= stPackage target]
                        , "ranges" .= [object ["type" .= rangeType, "events" .= [object ["introduced" .= introduced], object ["fixed" .= fixed]]]]
                        ]
                   ]
            ]
