-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Advisory variants of the load scenarios. Before its proxy boots, a variant compiles the
captured corpus advisories through Pilot's compiler and serves the artifact from a loopback stub
of the object store, outside the proxy's cgroup. The proxy syncs it as it would from S3, and the
harness measures only once the database is installed.
-}
module Ecluse.BenchLoad.Advisories (
    shippedAdvisories,
    allRulesAdvisories,
    advisoryDenyRules,
    maliciousQualification,
    advisoryStoreReply,
) where

import Data.Aeson (object, (.=))
import Data.Aeson.Types (Pair)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Network.HTTP.Client (defaultManagerSettings, httpLbs, newManager, parseRequest, responseBody, responseStatus)
import Network.HTTP.Types (Header, Status, hContentType, status200, status404, statusCode)
import Network.HTTP.Types.Header (hETag, hLastModified)
import System.IO (stderr)
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Harness (LoadKnobs (lkAdvisories), Scenario (..), Target (targetProxy))
import Ecluse.BenchLoad.ProxyProcess (AdvisoryFeed (AdvisoryFeed), advisoryBucket, proxyPort)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI), ecosystemName)
import Ecluse.Core.Osv.Schema (osvDbFileName)
import Ecluse.Core.Package (PackageInfo (infoVersions), mkPackageName)
import Ecluse.Core.Rules (evalRules, prepare)
import Ecluse.Core.Rules.Types (Decision (Blocked), DenyIfCveParams (..), DenyIfEpssParams (..), EvalContext (EvalContext), FailureAlignment (FailDeny, FailNoDecision), Reason (KnownMalicious), identityEvidence)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Corpus.Advisories (AdvisoryInputs (aiOsvZip), SyntheticTarget (SyntheticTarget), compileAdvisoryInputs, compileCorpusAdvisories, configuredShippedPolicy, fillerTargets, maliciousAdvisoryInputs, mixedMaliciousAdvisories, suggestedDenyIfCve, suggestedDenyIfEpss)
import Ecluse.Test.OsvDb (withServedArtifact)
import Ecluse.Test.Package (hexSha1OfLazy, hexSha256Of)
import Ecluse.Test.Registry.Npm.Metadata (projectNpmManifest)
import Ecluse.Test.Registry.PyPI.Metadata (projectPyPIIndex)
import Ecluse.Test.Stub (Captured (capPath), Stub (stubPort), withRoutedStub)

-- | The scenario under the shipped policy, with its proxy syncing the corpus advisories.
shippedAdvisories :: Ecosystem -> Scenario -> Scenario
shippedAdvisories ecosystem =
    advisoryVariant ecosystem "advisories" "The proxy syncs an advisory database compiled from the captured corpus advisories, under the shipped policy." []

-- | The scenario with its proxy syncing the corpus advisories, and its policy adding both advisory denies.
allRulesAdvisories :: Ecosystem -> Scenario -> Scenario
allRulesAdvisories ecosystem =
    advisoryVariant
        ecosystem
        "all-advisory-rules"
        ( "The proxy syncs an advisory database compiled from the captured corpus advisories, and its policy adds DenyIfCve at CVSS "
            <> threshold (dicMinCvss suggestedDenyIfCve)
            <> " (onUnavailable "
            <> alignment (dicOnUnavailable suggestedDenyIfCve)
            <> ") and DenyIfEpss at "
            <> threshold (dieMinEpss suggestedDenyIfEpss)
            <> " (onUnavailable "
            <> alignment (dieOnUnavailable suggestedDenyIfEpss)
            <> ")."
        )
        advisoryDenyRules

-- The variant preserves its counterpart's settings and refuses in-process work, which boots no proxy.
advisoryVariant :: Ecosystem -> Text -> Text -> [Pair] -> Scenario -> Scenario
advisoryVariant = advisoryVariantWith compileCorpusAdvisories

-- | Opt-in MAL work with explicit labels, preserving the reference scenario's operating settings.
maliciousQualification :: Ecosystem -> Bool -> Scenario -> Scenario
maliciousQualification eco mixed original =
    qualified{scenarioBoot = \knobs use -> scenarioBoot qualified knobs (\target -> checkFiltered target >> use target)}
  where
    qualified = advisoryVariantWith compileQualified eco label "Source qualification: attributed MAL-derived affected points. Denials change admitted work." [] original
    label = if mixed then "mal-mixed-heavy" else "mal-hit"
    (package, version) = case eco of
        Npm -> ("react", "18.2.0")
        PyPI -> ("requests", "2.32.5")
        _ -> ("malicious-fixture-control", "1.0.0")
    checkFiltered target = do
        proxy <- maybe (benchFail "MAL qualification has no proxy") pure (targetProxy target)
        let path = case eco of
                Npm -> "/npm/" <> package
                _ -> "/pypi/simple/" <> package <> "/"
        manager <- newManager defaultManagerSettings
        request <- parseRequest ("http://127.0.0.1:" <> show (proxyPort proxy) <> toString path)
        response <- httpLbs request manager
        unless (statusCode (responseStatus response) == 200) (benchFail "MAL qualification metadata did not retain a successful response")
        let project = case eco of
                Npm -> fmap fst . projectNpmManifest defaultLimits (mkPackageName eco Nothing package)
                _ -> fmap fst . projectPyPIIndex defaultLimits (mkPackageName eco Nothing package)
        info <- either (benchFail . show) pure (project (LBS.toStrict (responseBody response)))
        when (Map.member version (infoVersions info) || Map.null (infoVersions info)) (benchFail "MAL qualification did not remove the affected release while retaining allowed releases")
        TIO.hPutStrLn stderr (label <> " response_sha256=" <> hexSha256Of (LBS.toStrict (responseBody response)) <> " admitted_versions=" <> T.intercalate "," (Map.keys (infoVersions info)))
    compileQualified ecosystem dir = do
        inputs <-
            if mixed
                then mixedMaliciousAdvisories ecosystem (SyntheticTarget package [version, "999.0.0"] 200 : fillerTargets 20000)
                else maliciousAdvisoryInputs ecosystem [(package, [version])]
        path <- compileAdvisoryInputs ecosystem dir inputs
        artifact <- readFileLBS path
        TIO.hPutStrLn stderr (label <> " source_zip_sha256=" <> hexSha256Of (LBS.toStrict (aiOsvZip inputs)) <> " artifact_sha256=" <> hexSha256Of (LBS.toStrict artifact) <> " serving_etag=" <> hexSha1OfLazy artifact)
        now <- getCurrentTime
        withServedArtifact ecosystem path $ \deps _ -> do
            rules <- prepare deps configuredShippedPolicy
            decision <- evalRules (EvalContext now Nothing) rules (identityEvidence (mkPackageName ecosystem Nothing package) (mkVersion ecosystem version))
            case decision of
                Blocked "DenyIfKnownMalicious" _ KnownMalicious{} -> pure ()
                other -> benchFail ("MAL qualification did not reach its intended denial: " <> show other)
        pure path

advisoryVariantWith :: (Ecosystem -> FilePath -> IO FilePath) -> Ecosystem -> Text -> Text -> [Pair] -> Scenario -> Scenario
advisoryVariantWith compile ecosystem suffix note rules s =
    s
        { scenarioName = name
        , scenarioDescription = scenarioDescription s <> " " <> note
        , scenarioBoot = \knobs k ->
            if scenarioInProcess s
                then benchFail (name <> ": an in-process scenario boots no proxy to sync an advisory database")
                else withAdvisoryStore compile ecosystem (\port -> scenarioBoot s knobs{lkAdvisories = Just (AdvisoryFeed port rules)} k)
        }
  where
    name = scenarioName s <> "-" <> suffix

-- | Both advisory denies at the shared suggested thresholds, as @ECLUSE_RULES@ entries.
advisoryDenyRules :: [Pair]
advisoryDenyRules =
    [ "deny-known-cves" .= object ["type" .= ("DenyIfCve" :: Text), "minCvss" .= dicMinCvss suggestedDenyIfCve, "onUnavailable" .= alignment (dicOnUnavailable suggestedDenyIfCve)]
    , "deny-exploitable-cves" .= object ["type" .= ("DenyIfEpss" :: Text), "minEpss" .= dieMinEpss suggestedDenyIfEpss, "onUnavailable" .= alignment (dieOnUnavailable suggestedDenyIfEpss)]
    ]

-- The @onUnavailable@ spelling the rules configuration reads.
alignment :: FailureAlignment -> Text
alignment = \case
    FailDeny -> "deny"
    FailNoDecision -> "skip"

-- A threshold as the docs write it: 8 rather than 8.0.
threshold :: Double -> Text
threshold d = maybe (show d) show (wholeNumber d)
  where
    wholeNumber x = let n = truncate x :: Int in if fromIntegral n == x then Just n else Nothing

-- Compile the ecosystem's corpus advisories and serve the artifact on a loopback port for the action.
withAdvisoryStore :: (Ecosystem -> FilePath -> IO FilePath) -> Ecosystem -> (Int -> IO a) -> IO a
withAdvisoryStore compile ecosystem use =
    withSystemTempDirectory "ecluse-bench-advisories" $ \dir -> do
        artifact <- readFileLBS =<< compile ecosystem dir
        publishedAt <- getCurrentTime
        withRoutedStub (advisoryStoreReply ecosystem publishedAt artifact) (use . stubPort)

-- | Answer the artifact's HEAD or GET with its ETag and publication time, and other paths with 404.
advisoryStoreReply :: Ecosystem -> UTCTime -> LByteString -> Captured -> (Status, [Header], LByteString)
advisoryStoreReply ecosystem publishedAt artifact request
    | capPath request == artifactPath = (status200, headers, artifact)
    | otherwise = (status404, [], "")
  where
    artifactPath = encodeUtf8 ("/" <> advisoryBucket <> "/" <> toText (osvDbFileName (ecosystemName ecosystem)))
    headers =
        [ (hETag, encodeUtf8 ("\"" <> hexSha1OfLazy artifact <> "\""))
        , (hLastModified, encodeUtf8 (formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" publishedAt))
        , (hContentType, "application/octet-stream")
        ]
