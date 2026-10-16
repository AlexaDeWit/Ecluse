-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Captured and generated OSV archives and EPSS feeds for the benchmark corpus.
Pilot compiles both into the advisory shapes the performance harnesses read.
-}
module Ecluse.Test.Corpus.Advisories (
    AdvisoryInputs (..),
    corpusAdvisories,
    compileAdvisoryInputs,
    compileCorpusAdvisories,
    checkCapturesServed,
    suggestedDenyIfCve,
    suggestedDenyIfEpss,
    shippedPolicy,
    configuredShippedPolicy,
    allAdvisoryRules,
    SyntheticTarget (..),
    fillerTargets,
    syntheticAdvisories,
    mixedMaliciousAdvisories,
    maliciousAdvisoryInputs,
) where

import Codec.Compression.GZip qualified as GZip
import Data.Aeson (Object, Value, encode, object, (.:), (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (Parser)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Short qualified as TS
import Data.Time (nominalDay)
import Network.HTTP.Types (status200)
import System.FilePath (takeFileName)

import Ecluse.Core.Cve (AdvisoryRange, CveLookup (cveAdvisoriesFor, cveCoveredNames))
import Ecluse.Core.Ecosystem (Ecosystem (Npm), ecosystemName)
import Ecluse.Core.Osv.Ecosystem (osvEcosystemFor, osvExportDirectory)
import Ecluse.Core.Osv.Schema (EpssRequirement (EpssRequired))
import Ecluse.Core.Package (PackageName, pkgCanonical, renderPackageName)
import Ecluse.Core.Rules (AdvisoryDatabase (AdvisoryDatabase, NoAdvisoryDatabase), AdvisoryGeneration (agLookup), RuleDeps (rdAdvisoryDatabase), readAdvisories, withCveLookup)
import Ecluse.Core.Rules.Types (
    DenyIfCveParams (DenyIfCveParams),
    DenyIfEpssParams (DenyIfEpssParams),
    FailureAlignment (FailDeny),
    PrecededRule,
    Rule (AllowIfOlderThan, AllowIfRemediatesCve, DenyIfCve, DenyIfEpss, DenyIfKnownMalicious),
 )
import Ecluse.Core.Version (parseVersionKey)
import Ecluse.Test.Corpus (FilePin (..), readCorpusPins, readPinned)
import Ecluse.Test.Osv (osvZipOf)
import Ecluse.Test.Osv.Withdrawal (maliciousEntries, maliciousZip)
import Ecluse.Test.OsvDb (compileOsvZipDbWithFeedTo, scoresOf)
import Ecluse.Test.Rules (atDefaultPrecedence)

-- | One ecosystem's OSV export archive and the gzipped EPSS feed that scores it.
data AdvisoryInputs = AdvisoryInputs
    { aiOsvZip :: LByteString
    , aiEpssFeed :: LByteString
    }

-- | The captured records @bench/corpus/pins.json@ pins for the ecosystem, with the captured EPSS rows.
corpusAdvisories :: Ecosystem -> IO AdvisoryInputs
corpusAdvisories eco = do
    (records, epss) <- readCorpusPins (advisoryPins eco) >>= either fail pure
    entries <- forM records $ \pin -> (toText (takeFileName (fpPath pin)),) <$> readPinned pin
    archive <- osvZipOf entries
    feed <- readPinned epss
    pure AdvisoryInputs{aiOsvZip = archive, aiEpssFeed = GZip.compress feed}

-- | Compile the captured advisories, rejecting artifacts without any range.
compileCorpusAdvisories :: Ecosystem -> FilePath -> IO FilePath
compileCorpusAdvisories eco dir = do
    compiled <- corpusAdvisories eco >>= compileAdvisoryInputs eco dir
    ranges <- scoresOf compiled
    when (null ranges) (fail ("the " <> toString (ecosystemName eco) <> " corpus advisories compiled to no range"))
    pure compiled

-- | Compile advisory inputs into the directory through Pilot's compiler, returning the artifact's path.
compileAdvisoryInputs :: Ecosystem -> FilePath -> AdvisoryInputs -> IO FilePath
compileAdvisoryInputs eco dir inputs = compileOsvZipDbWithFeedTo eco EpssRequired (status200, aiEpssFeed inputs) (aiOsvZip inputs) dir

-- | Check canonical capture names, covered packages and non-empty rows through 'readAdvisories'.
checkCapturesServed :: RuleDeps -> [PackageName] -> IO (Either Text ())
checkCapturesServed deps packages = case filter (\package -> renderPackageName package /= lookupKey package) packages of
    package : _ -> pure (Left ("the capture " <> renderPackageName package <> " differs from its lookup key " <> lookupKey package))
    [] -> checkCoveredCaptures deps packages
  where
    lookupKey = TS.toText . pkgCanonical

-- Covered captures are matched by display name, which 'checkCapturesServed' holds equal to the lookup key.
checkCoveredCaptures :: RuleDeps -> [PackageName] -> IO (Either Text ())
checkCoveredCaptures deps packages =
    withCveLookup deps (traverse (cveCoveredNames . snd)) >>= \case
        Nothing -> pure (Left "no advisory generation is serving")
        Just covered -> case filter ((`elem` covered) . renderPackageName) packages of
            [] -> pure (Left "the served advisories cover none of the captures")
            named -> do
                unserved <- filterM (fmap null . rowsTheRulesRead deps) named
                pure (if null unserved then Right () else Left ("the served advisories return no row for " <> T.intercalate ", " (map renderPackageName unserved)))

-- Coverage alone does not prove the generation returns rows for a capture.
rowsTheRulesRead :: RuleDeps -> PackageName -> IO [AdvisoryRange]
rowsTheRulesRead deps package = do
    seen <- newIORef []
    let recording cve = cve{cveAdvisoriesFor = cveAdvisoriesFor cve >=> \rows -> rows <$ writeIORef seen rows}
        spied =
            deps
                { rdAdvisoryDatabase = case rdAdvisoryDatabase deps of
                    NoAdvisoryDatabase -> NoAdvisoryDatabase
                    AdvisoryDatabase borrow -> AdvisoryDatabase $ \use ->
                        borrow (\generation -> use generation{agLookup = \readLookup -> agLookup generation (readLookup . fmap (second recording))})
                }
    void (withCveLookup spied (readAdvisories package))
    readIORef seen

-- | The shared shipped policy: the minimum-age quarantine and the remediation fast lane.
shippedPolicy :: [PrecededRule]
shippedPolicy = map atDefaultPrecedence [AllowIfOlderThan (7 * nominalDay), AllowIfRemediatesCve]

-- | The shared shipped policy selected when advisories.url is present.
configuredShippedPolicy :: [PrecededRule]
configuredShippedPolicy = atDefaultPrecedence (DenyIfKnownMalicious FailDeny) : shippedPolicy

-- | The source-configured shared policy with both scored advisory denies, at the suggested thresholds.
allAdvisoryRules :: [PrecededRule]
allAdvisoryRules = configuredShippedPolicy <> map atDefaultPrecedence [DenyIfCve suggestedDenyIfCve, DenyIfEpss suggestedDenyIfEpss]

-- | @DenyIfCve@ at the CVSS threshold @config/default.yaml@ suggests, failing closed.
suggestedDenyIfCve :: DenyIfCveParams
suggestedDenyIfCve = DenyIfCveParams 8 FailDeny

-- | @DenyIfEpss@ at the EPSS threshold @config/default.yaml@ suggests, failing closed.
suggestedDenyIfEpss :: DenyIfEpssParams
suggestedDenyIfEpss = DenyIfEpssParams 0.5 FailDeny

advisoryPins :: Ecosystem -> Object -> Parser ([FilePin], FilePin)
advisoryPins eco pins = do
    advisories <- pins .: "advisories"
    byEcosystem <- advisories .: "records"
    records :: Map Text FilePin <- byEcosystem .: Key.fromText (ecosystemName eco)
    (Map.elems records,) <$> advisories .: "epss"

-- | A package the generated worst case names, its release keys, and how many advisories it gets.
data SyntheticTarget = SyntheticTarget
    { stPackage :: Text
    , stVersions :: [Text]
    , stAdvisories :: Int
    }

-- | Packages with one advisory each, which fill the generated table so a lookup's cost scales as in production.
fillerTargets :: Int -> [SyntheticTarget]
fillerTargets count = [SyntheticTarget ("ecluse-bench-filler-" <> show k) ["1.0.0", "2.0.0"] 1 | k <- [1 .. count]]

-- | Generate release windows with real fixes, alternating critical CVSS vectors and EPSS scores from 0 to 0.95.
syntheticAdvisories :: Ecosystem -> [SyntheticTarget] -> IO AdvisoryInputs
syntheticAdvisories = syntheticAdvisoriesWith []

-- | Add labelled captured MAL points to the unchanged scored synthetic reference.
mixedMaliciousAdvisories :: Ecosystem -> [SyntheticTarget] -> IO AdvisoryInputs
mixedMaliciousAdvisories eco targets = do
    entries <- maliciousEntries eco [(stPackage target, take 1 (stVersions target)) | target <- targets, stAdvisories target > 1] Nothing
    syntheticAdvisoriesWith entries eco targets

-- | Use attributed MAL-derived exact points with the existing captured EPSS input.
maliciousAdvisoryInputs :: Ecosystem -> [(Text, [Text])] -> IO AdvisoryInputs
maliciousAdvisoryInputs eco targets = do
    source <- corpusAdvisories eco
    archive <- maliciousZip eco targets Nothing
    pure source{aiOsvZip = archive}

syntheticAdvisoriesWith :: [(Text, LByteString)] -> Ecosystem -> [SyntheticTarget] -> IO AdvisoryInputs
syntheticAdvisoriesWith extra eco targets = do
    archive <- osvZipOf (extra <> [(recordId n <> ".json", encode (syntheticRecord eco n target window)) | (n, (target, window)) <- indexed])
    pure AdvisoryInputs{aiOsvZip = archive, aiEpssFeed = GZip.compress (epssPreamble <> foldMap (scoreLine . fst) indexed)}
  where
    -- Only the windows stay shared, so each record is encoded and dropped as the archive streams.
    indexed = zip [0 ..] [(target, window) | target <- targets, window <- windows eco target]
    epssPreamble = "#model_version:synthetic,score_date:2026-01-01T00:00:00+0000\ncve,epss,percentile\n"
    scoreLine n = encodeUtf8 (cveAlias n <> "," <> epssScore n <> ",0.5\n")

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

recordId, cveAlias, epssScore :: Int -> Text
recordId n = "ECLUSE-BENCH-" <> show n
cveAlias n = "CVE-2099-" <> show (10000 + n)
epssScore n = "0." <> T.justifyRight 2 '0' (show (n `mod` 20 * 5))

syntheticRecord :: Ecosystem -> Int -> SyntheticTarget -> (Text, Text) -> Value
syntheticRecord eco n target (introduced, fixed) =
    object
        [ "schema_version" .= ("1.6.0" :: Text)
        , "id" .= recordId n
        , "modified" .= ("2026-01-01T00:00:00Z" :: Text)
        , "aliases" .= [cveAlias n]
        , "severity" .= [object ["type" .= ("CVSS_V3" :: Text), "score" .= vector]]
        , "affected"
            .= [ object
                    [ "package" .= object ["ecosystem" .= osvExportDirectory (osvEcosystemFor eco), "name" .= stPackage target]
                    , "ranges" .= [object ["type" .= rangeType, "events" .= [object ["introduced" .= introduced], object ["fixed" .= fixed]]]]
                    ]
               ]
        ]
  where
    vector
        | even n = "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:H/I:H/A:H" :: Text
        | otherwise = "CVSS:3.1/AV:N/AC:H/PR:N/UI:R/S:U/C:L/I:L/A:N"
    rangeType = if eco == Npm then "SEMVER" else "ECOSYSTEM" :: Text
