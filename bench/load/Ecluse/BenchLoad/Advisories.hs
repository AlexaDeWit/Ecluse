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
    compileCorpusAdvisories,
    advisoryStoreReply,
) where

import Data.Aeson (object, (.=))
import Data.Aeson.Types (Pair)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Network.HTTP.Types (Header, Status, hContentType, status200, status404)
import Network.HTTP.Types.Header (hETag, hLastModified)
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Harness (LoadKnobs (lkAdvisories), Scenario (..))
import Ecluse.BenchLoad.ProxyProcess (AdvisoryFeed (AdvisoryFeed), advisoryBucket)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Osv.Schema (EpssRequirement (EpssRequired), osvDbFileName)
import Ecluse.Core.Rules.Types (DenyIfCveParams (..), DenyIfEpssParams (..), FailureAlignment (FailDeny, FailNoDecision))
import Ecluse.Test.Corpus.Advisories (AdvisoryInputs (..), corpusAdvisories, suggestedDenyIfCve, suggestedDenyIfEpss)
import Ecluse.Test.OsvDb (compileOsvZipDbWithFeedTo, scoresOf)
import Ecluse.Test.Package (hexSha1OfLazy)
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
            <> show (dicMinCvss suggestedDenyIfCve)
            <> " and DenyIfEpss at "
            <> show (dieMinEpss suggestedDenyIfEpss)
            <> "."
        )
        advisoryDenyRules

{- | The no-database scenario with a name suffix, a description note, and the rules its policy adds.
Everything else is the counterpart's. An in-process counterpart boots no proxy, so its variant refuses.
-}
advisoryVariant :: Ecosystem -> Text -> Text -> [Pair] -> Scenario -> Scenario
advisoryVariant ecosystem suffix note rules s =
    Scenario
        { scenarioName = name
        , scenarioDescription = scenarioDescription s <> " " <> note
        , scenarioConcurrencyScale = scenarioConcurrencyScale s
        , scenarioServiceTime = scenarioServiceTime s
        , scenarioInProcess = scenarioInProcess s
        , scenarioBoot = \knobs k ->
            if scenarioInProcess s
                then benchFail (name <> ": an in-process scenario boots no proxy to sync an advisory database")
                else withAdvisoryStore ecosystem (\port -> scenarioBoot s knobs{lkAdvisories = Just (AdvisoryFeed port rules)} k)
        }
  where
    name = scenarioName s <> "-" <> suffix

-- | Both advisory denies at the shared suggested thresholds, as @ECLUSE_RULES@ entries.
advisoryDenyRules :: [Pair]
advisoryDenyRules =
    [ "deny-known-cves" .= object ["type" .= ("DenyIfCve" :: Text), "minCvss" .= dicMinCvss suggestedDenyIfCve, "onUnavailable" .= alignment (dicOnUnavailable suggestedDenyIfCve)]
    , "deny-exploitable-cves" .= object ["type" .= ("DenyIfEpss" :: Text), "minEpss" .= dieMinEpss suggestedDenyIfEpss, "onUnavailable" .= alignment (dieOnUnavailable suggestedDenyIfEpss)]
    ]
  where
    alignment = \case
        FailDeny -> "deny" :: Text
        FailNoDecision -> "skip"

-- Compile the ecosystem's corpus advisories and serve the artifact on a loopback port for the action.
withAdvisoryStore :: Ecosystem -> (Int -> IO a) -> IO a
withAdvisoryStore ecosystem use =
    withSystemTempDirectory "ecluse-bench-advisories" $ \dir -> do
        artifact <- readFileLBS =<< compileCorpusAdvisories ecosystem dir
        publishedAt <- getCurrentTime
        withRoutedStub (advisoryStoreReply ecosystem publishedAt artifact) (use . stubPort)

{- | Compile the ecosystem's pinned corpus advisories into the directory, returning the artifact's
path. An artifact with no range would leave every advisory rule abstaining, so it fails the harness.
-}
compileCorpusAdvisories :: Ecosystem -> FilePath -> IO FilePath
compileCorpusAdvisories ecosystem dir = do
    inputs <- corpusAdvisories ecosystem
    compiled <- compileOsvZipDbWithFeedTo ecosystem EpssRequired (status200, aiEpssFeed inputs) (aiOsvZip inputs) dir
    ranges <- scoresOf compiled
    when (null ranges) (benchFail ("bench-load: the " <> ecosystemName ecosystem <> " corpus advisories compiled to no range"))
    pure compiled

{- | Answer the ecosystem's artifact key in 'advisoryBucket' as S3 answers a path-style HEAD or
GET, with an ETag and the publication time, and every other path with 404.
-}
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
