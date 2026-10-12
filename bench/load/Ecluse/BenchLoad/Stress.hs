-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Paired load keeps warm reads active while changing private listings prevent cold-side reuse.
module Ecluse.BenchLoad.Stress (WarmLoadDriver (..), pairedLoad, herdSize, rampSteps) where

import Data.Aeson ((.=))
import Data.Aeson.Types (Pair)
import Data.List (partition)
import Data.Map.Strict qualified as Map
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (status200)
import Network.Wai (Application)

import Ecluse.BenchLoad.Fixture (artifactBytes, fetchChecked, loadCorpusBodies, longCacheTtl, weightedMix, withProxyOverStubs)
import Ecluse.BenchLoad.Harness (Driver (DriveUnder), LoadKnobs (..), Target, proxied, urlLoad)
import Ecluse.BenchLoad.NpmArtifact (SelectedArtifact (..))
import Ecluse.BenchLoad.ProxyProcess (proxyPort)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Test.Corpus (CorpusPackage (cpTier, cpWeight), CorpusTier (Heavy), cpName)

-- | Artifact selection and upstream wire formats remain native to each ecosystem.
data WarmLoadDriver = WarmLoadDriver
    { wldEcosystem :: Ecosystem
    , wldPackages :: [CorpusPackage]
    , wldListingUrl :: Int -> Text -> Text
    , wldArtifactUrl :: Int -> SelectedArtifact -> Text
    , wldSelect :: [CorpusPackage] -> Map Text LByteString -> IO (Map Text SelectedArtifact)
    , wldPrivate :: (Text -> IO [Pair]) -> Int -> LByteString -> Application
    , wldPublic :: IORef (Map Text LByteString) -> Int -> Map Text LByteString -> Map ByteString LByteString -> Application
    }

-- | Warm-up and fixture allocation finish before the harness starts either generator.
pairedLoad :: WarmLoadDriver -> LoadKnobs -> (Target -> IO a) -> IO a
pairedLoad driver knobs k = do
    bodies <- loadCorpusBodies (wldPackages driver)
    selected <- wldSelect driver warmPackages bodies
    artifactRequests <- traverse (HTTP.parseRequest . toString . saUpstreamUrl) (Map.elems selected)
    served <- newIORef (0 :: Int)
    rewritten <- newIORef mempty
    let latency = lkUpstreamLatencyMicros knobs
        bytes = artifactBytes (lkPayloadBytes knobs)
        artifacts = Map.fromList [(HTTP.path request, bytes) | request <- artifactRequests]
        nonce name
            | name `elem` map cpName coldPackages = do
                n <- atomicModifyIORef' served (\count -> (count + 1, count))
                pure ["description" .= ("bench nonce " <> show n :: Text)]
            | otherwise = pure []
    withProxyOverStubs
        (wldEcosystem driver)
        knobs
        longCacheTtl
        Nothing
        (wldPrivate driver nonce latency bytes)
        (wldPublic driver rewritten latency bodies artifacts)
        (const [])
        $ \proxy _ -> do
            let port = proxyPort proxy
                listings pkgs = weightedMix cpWeight (wldListingUrl driver) pkgs port
                warmUrls = listings warmPackages <> map (wldArtifactUrl driver port) (Map.elems selected)
            for_ (ordNub warmUrls) (void . fetchChecked status200 [])
            k (proxied proxy (DriveUnder (urlLoad warmUrls) (urlLoad (listings coldPackages))))
  where
    (coldPackages, warmPackages) = partition ((== Heavy) . cpTier) (wldPackages driver)

-- | A cold burst has no warm-up requests.
herdSize :: Int
herdSize = 100

-- | Each step holds its concurrency for the configured duration.
rampSteps :: [Int]
rampSteps = [10, 25, 50, 100, 200, 400]
