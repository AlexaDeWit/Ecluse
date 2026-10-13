-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Shared live registry captures. Synchronous failures yield absence. Cancellation propagates.
module Ecluse.Test.RegistryCapture (
    -- * The curated catalogue
    Catalogue (..),
    loadCatalogue,
    decodeCatalogue,
    smokeRegistryPackages,

    -- * The live registry fetch
    registryUrl,
    fetchPackumentBody,
    fetchVersions,
    parseRegistryVersions,
) where

import Data.ByteString qualified as BS

import Data.Aeson (FromJSON (parseJSON), Object, eitherDecode, withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Client (
    Manager,
    httpLbs,
    parseUrlThrow,
    requestHeaders,
    responseBody,
    responseTimeout,
    responseTimeoutMicro,
 )
import UnliftIO.Exception (tryAny)

import Ecluse.Core.Ecosystem (Ecosystem (..), parseEcosystem)
import Ecluse.Core.Registry (RegistryResponse (RegistryResponse))

import Ecluse.Core.Version (renderVersion)
import Ecluse.Test.Corpus (readCorpusPins)
import Ecluse.Test.Registry.Npm.Project (parseVersionList)
import Ecluse.Test.Registry.PyPI.Wire qualified as PyPI
import Ecluse.Test.Registry.RubyGems.Wire qualified as RubyGems

-- | Curated smoke names and benchmark pins from the shared catalogue.
data Catalogue = Catalogue
    { catSmokeNames :: Map Ecosystem [Text]
    -- ^ Curated gnarly-version package names per ecosystem, for the version-oracle differential.
    , catBenchPins :: Map Text Text
    -- ^ Workload versions selected by benchmarks, independent of capture size.
    }
    deriving stock (Eq, Show)

instance FromJSON Catalogue where
    parseJSON = withObject "Catalogue" catalogueFields

catalogueFields :: Object -> Parser Catalogue
catalogueFields o = do
    rawNames <- o .: "smokeNames"
    pins <- o .: "pins"
    names <- Map.fromList <$> traverse parseEcoKey (Map.toList rawNames)
    pure Catalogue{catSmokeNames = names, catBenchPins = pins}
  where
    parseEcoKey :: (Text, [Text]) -> Parser (Ecosystem, [Text])
    parseEcoKey (k, vs) = case parseEcosystem k of
        Just eco -> pure (eco, vs)
        Nothing -> fail ("RegistryCapture: unknown ecosystem key in smokeNames: " <> toString k)

-- | Decode a 'Catalogue' from raw JSON bytes.
decodeCatalogue :: LByteString -> Either String Catalogue
decodeCatalogue = eitherDecode

-- | Fail on a missing or malformed committed catalogue.
loadCatalogue :: IO Catalogue
loadCatalogue = readCorpusPins catalogueFields >>= either fail pure

-- | Curated smoke names, ordered by ecosystem.
smokeRegistryPackages :: Catalogue -> [(Ecosystem, [Text])]
smokeRegistryPackages = Map.toList . catSmokeNames

-- | The versions endpoint, with scoped npm names percent-encoded.
registryUrl :: Ecosystem -> Text -> Text
registryUrl eco pkg = case eco of
    Npm -> "https://registry.npmjs.org/" <> T.replace "/" "%2F" pkg
    PyPI -> "https://pypi.org/pypi/" <> pkg <> "/json"
    RubyGems -> "https://rubygems.org/api/v1/versions/" <> pkg <> ".json"

-- The User-Agent every capture fetch identifies itself with.
captureUserAgent :: ByteString
captureUserAgent = "ecluse-registry-capture"

-- | Return 'Nothing' for synchronous fetch failures. Propagate cancellation.
fetchPackumentBody :: Manager -> Ecosystem -> Text -> IO (Maybe LByteString)
fetchPackumentBody manager eco pkg = do
    result <- tryAny $ do
        req0 <- parseUrlThrow (toString (registryUrl eco pkg))
        let req =
                req0
                    { requestHeaders = [("User-Agent", captureUserAgent)]
                    , responseTimeout = responseTimeoutMicro (30 * 1000 * 1000)
                    }
        responseBody <$> httpLbs req manager
    pure $ case result of
        Left _ -> Nothing
        Right body -> Just body

-- | Fetch a package's published version strings from its registry.
fetchVersions :: Manager -> Ecosystem -> Text -> IO (Maybe [Text])
fetchVersions manager eco pkg =
    (>>= parseRegistryVersions eco) <$> fetchPackumentBody manager eco pkg

-- | Read published versions through each ecosystem's wire decoder.
parseRegistryVersions :: Ecosystem -> LByteString -> Maybe [Text]
parseRegistryVersions eco body = case eco of
    Npm -> rightToMaybe (map renderVersion <$> parseVersionList (RegistryResponse 200 (BS.length (BSL.toStrict body)) (BSL.toStrict body)))
    PyPI -> PyPI.projectVersions <$> decode' body
    RubyGems -> RubyGems.listingVersions <$> decode' body
  where
    decode' :: (FromJSON a) => LByteString -> Maybe a
    decode' = rightToMaybe . eitherDecode
