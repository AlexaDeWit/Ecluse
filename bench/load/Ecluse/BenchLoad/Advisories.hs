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
    allAdvisoryRules,
    advisoryDenyRules,
    advisoryStoreStub,
) where

import Data.Aeson (object, (.=))
import Data.Aeson.Types (Pair)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Network.HTTP.Types (hContentType, status200, status404)
import Network.HTTP.Types.Header (hETag, hLastModified)
import Network.Wai (Application, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import UnliftIO.Temporary (withSystemTempDirectory)

import Ecluse.BenchLoad.Harness (LoadKnobs (lkAdvisories), Scenario (..))
import Ecluse.BenchLoad.ProxyProcess (AdvisoryFeed (AdvisoryFeed), advisoryBucket)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Osv.Schema (EpssRequirement (EpssRequired), osvDbFileName)
import Ecluse.Test.Corpus.Advisories (AdvisoryInputs (..), corpusAdvisories)
import Ecluse.Test.OsvDb (compileOsvZipDbWithFeedTo)
import Ecluse.Test.Package (hexSha1OfLazy)

-- | The scenario under the shipped policy, with its proxy syncing the corpus advisories.
shippedAdvisories :: Ecosystem -> Scenario -> Scenario
shippedAdvisories ecosystem =
    advisoryVariant ecosystem "advisories" "The proxy syncs an advisory database compiled from the captured corpus advisories, under the shipped policy." []

-- | The scenario with its proxy syncing the corpus advisories, and its policy adding both advisory denies.
allAdvisoryRules :: Ecosystem -> Scenario -> Scenario
allAdvisoryRules ecosystem =
    advisoryVariant
        ecosystem
        "all-advisory-rules"
        "The proxy syncs an advisory database compiled from the captured corpus advisories, and its policy adds DenyIfCve at CVSS 8 and DenyIfEpss at 0.5, both failing closed."
        advisoryDenyRules

{- | The no-database scenario with a name suffix, a description note, and the rules its policy adds.
Everything else is the counterpart's, so the two measure the same traffic.
-}
advisoryVariant :: Ecosystem -> Text -> Text -> [Pair] -> Scenario -> Scenario
advisoryVariant ecosystem suffix note rules s =
    Scenario
        { scenarioName = scenarioName s <> "-" <> suffix
        , scenarioDescription = scenarioDescription s <> " " <> note
        , scenarioConcurrencyScale = scenarioConcurrencyScale s
        , scenarioServiceTime = scenarioServiceTime s
        , scenarioInProcess = scenarioInProcess s
        , scenarioBoot = \knobs k -> withAdvisoryStore ecosystem (\port -> scenarioBoot s knobs{lkAdvisories = Just (AdvisoryFeed port rules)} k)
        }

-- | The two advisory denies at the thresholds @config/default.yaml@ suggests, failing closed.
advisoryDenyRules :: [Pair]
advisoryDenyRules =
    [ "deny-known-cves" .= object ["type" .= ("DenyIfCve" :: Text), "minCvss" .= (8 :: Int), "onUnavailable" .= ("deny" :: Text)]
    , "deny-exploitable-cves" .= object ["type" .= ("DenyIfEpss" :: Text), "minEpss" .= (0.5 :: Double), "onUnavailable" .= ("deny" :: Text)]
    ]

-- | Compile the ecosystem's corpus advisories and serve the artifact on a loopback port for the action.
withAdvisoryStore :: Ecosystem -> (Int -> IO a) -> IO a
withAdvisoryStore ecosystem use =
    withSystemTempDirectory "ecluse-bench-advisories" $ \dir -> do
        inputs <- corpusAdvisories ecosystem
        compiled <- compileOsvZipDbWithFeedTo ecosystem EpssRequired (status200, aiEpssFeed inputs) (aiOsvZip inputs) dir
        artifact <- readFileLBS compiled
        publishedAt <- getCurrentTime
        testWithApplication (pure (advisoryStoreStub ecosystem publishedAt artifact)) use

{- | Answer the ecosystem's artifact key in 'advisoryBucket' as S3 answers a path-style HEAD or
GET, with an ETag and the publication time, and every other path with 404.
-}
advisoryStoreStub :: Ecosystem -> UTCTime -> LByteString -> Application
advisoryStoreStub ecosystem publishedAt artifact = serve
  where
    serve request respond =
        respond $
            if pathInfo request == [advisoryBucket, toText (osvDbFileName (ecosystemName ecosystem))]
                then responseLBS status200 headers artifact
                else responseLBS status404 [] ""
    headers =
        [ (hETag, encodeUtf8 ("\"" <> hexSha1OfLazy artifact <> "\""))
        , (hLastModified, encodeUtf8 (formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" publishedAt))
        , (hContentType, "application/octet-stream")
        ]
