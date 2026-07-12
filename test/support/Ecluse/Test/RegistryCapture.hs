-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The shared registry-capture support: one curated package catalogue and one
live-registry fetch path for the test tiers that reach real registries.

It exists so the curated package lists and the registry fetch live in a single
place rather than being re-spelled per consumer:

  * the __catalogue__ ('Catalogue') is the one source for the curated package
    lists -- the gnarly-version names the version-oracle smoke differential
    exercises ('smokeRegistryPackages') and the benchmark-corpus capture pins
    ('catBenchPins'). It is read from a language-neutral JSON file
    ('cataloguePath') so the Haskell consumers here and the Node corpus-capture
    script read the same committed source;

  * the __fetch__ ('fetchPackumentBody', 'fetchVersions') is the single live
    HTTP path to a registry's version-listing endpoint ('registryUrl'), with
    parsing routed through each ecosystem's canonical wire decoder
    ('parseRegistryVersions').

Per-consumer processing stays at the call site: the version-oracle differential
keeps every published version (prereleases included, since ordering is the point),
while the benchmark corpus trims to stable releases in its own Node capture script.

Every fetch is total: a network failure, a non-2xx status, or an undecodable body
yields 'Nothing', so a live tier can pend on absence rather than crash.
-}
module Ecluse.Test.RegistryCapture (
    -- * The curated catalogue
    Catalogue (..),
    cataloguePath,
    loadCatalogue,
    decodeCatalogue,
    smokeRegistryPackages,

    -- * The live registry fetch
    registryUrl,
    fetchPackumentBody,
    fetchVersions,
    parseRegistryVersions,
) where

import Control.Exception (try)
import Data.Aeson (FromJSON (parseJSON), eitherDecode, withObject, (.:))
import Data.Aeson.Types (Parser)
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

import Ecluse.Core.Ecosystem (Ecosystem (..), parseEcosystem)
import Ecluse.Core.Registry.Npm.Wire (Packument (pkmtVersions))
import Ecluse.Test.Registry.Pypi.Wire qualified as Pypi
import Ecluse.Test.Registry.Rubygems.Wire qualified as Rubygems

{- | The curated package catalogue: the per-ecosystem smoke names and the
benchmark-corpus capture pins, decoded from the shared JSON source.
-}
data Catalogue = Catalogue
    { catSmokeNames :: Map Ecosystem [Text]
    -- ^ Curated gnarly-version package names per ecosystem, for the version-oracle differential.
    , catBenchPins :: Map Text Text
    -- ^ Benchmark-corpus capture pins: an npm package name to the version it is captured at.
    }
    deriving stock (Eq, Show)

instance FromJSON Catalogue where
    parseJSON = withObject "Catalogue" $ \o -> do
        rawNames <- o .: "smokeNames"
        pins <- o .: "pins"
        names <- Map.fromList <$> traverse parseEcoKey (Map.toList rawNames)
        pure Catalogue{catSmokeNames = names, catBenchPins = pins}
      where
        parseEcoKey :: (Text, [Text]) -> Parser (Ecosystem, [Text])
        parseEcoKey (k, vs) = case parseEcosystem k of
            Just eco -> pure (eco, vs)
            Nothing -> fail ("RegistryCapture: unknown ecosystem key in smokeNames: " <> toString k)

{- | The committed catalogue's path, relative to the package root the test suites
run from. It is the same file the Node corpus-capture script reads, so both sides
share one curated source.
-}
cataloguePath :: FilePath
cataloguePath = "bench/corpus/pins.json"

-- | Decode a 'Catalogue' from raw JSON bytes.
decodeCatalogue :: LByteString -> Either String Catalogue
decodeCatalogue = eitherDecode

{- | Read and decode the committed catalogue from 'cataloguePath'. Fails loudly if
the file is missing or malformed -- a committed-data defect, not a runtime condition
a caller decides on.
-}
loadCatalogue :: IO Catalogue
loadCatalogue = do
    raw <- readFileLBS cataloguePath
    either (\e -> fail (cataloguePath <> " did not decode: " <> e)) pure (decodeCatalogue raw)

{- | The curated smoke names as @(ecosystem, names)@ pairs, ordered by ecosystem --
the shape the version-oracle differential iterates.
-}
smokeRegistryPackages :: Catalogue -> [(Ecosystem, [Text])]
smokeRegistryPackages = Map.toList . catSmokeNames

{- | The registry endpoint that lists a package's published versions. A scoped npm
name is percent-encoded (@\@types\/node@ → @\@types%2Fnode@); the other ecosystems
take a bare name.
-}
registryUrl :: Ecosystem -> Text -> Text
registryUrl eco pkg = case eco of
    Npm -> "https://registry.npmjs.org/" <> T.replace "/" "%2F" pkg
    PyPI -> "https://pypi.org/pypi/" <> pkg <> "/json"
    RubyGems -> "https://rubygems.org/api/v1/versions/" <> pkg <> ".json"

-- The User-Agent every capture fetch identifies itself with.
captureUserAgent :: ByteString
captureUserAgent = "ecluse-registry-capture"

{- | Fetch a package's raw version-listing body from its registry. 'Nothing' on any
network failure, a non-2xx status (a 404 throws via 'parseUrlThrow'), or a missing
endpoint -- so a live tier pends on absence rather than failing.
-}
fetchPackumentBody :: Manager -> Ecosystem -> Text -> IO (Maybe LByteString)
fetchPackumentBody manager eco pkg = do
    result <- try $ do
        req0 <- parseUrlThrow (toString (registryUrl eco pkg))
        let req =
                req0
                    { requestHeaders = [("User-Agent", captureUserAgent)]
                    , responseTimeout = responseTimeoutMicro (30 * 1000 * 1000)
                    }
        responseBody <$> httpLbs req manager
    pure $ case result of
        Left (_ :: SomeException) -> Nothing
        Right body -> Just body

{- | Fetch a package's published version strings from its registry -- the fetch
layered with each ecosystem's canonical decode. 'Nothing' when the fetch fails or
the body does not decode for that ecosystem. Every published version is kept,
prereleases included; trimming, where wanted, is the caller's job.
-}
fetchVersions :: Manager -> Ecosystem -> Text -> IO (Maybe [Text])
fetchVersions manager eco pkg =
    (>>= parseRegistryVersions eco) <$> fetchPackumentBody manager eco pkg

{- | Extract a registry response's published version strings through each
ecosystem's __canonical__ wire decoder -- the npm packument
('Ecluse.Core.Registry.Npm.Wire.Packument'), the PyPI project JSON
('Ecluse.Test.Registry.Pypi.Wire.ProjectJson'), or the RubyGems versions array
('Ecluse.Test.Registry.Rubygems.Wire.VersionListing') -- rather than re-parsing the
JSON here. 'Nothing' if the body does not decode for that ecosystem.
-}
parseRegistryVersions :: Ecosystem -> LByteString -> Maybe [Text]
parseRegistryVersions eco body = case eco of
    Npm -> Map.keys . pkmtVersions <$> decode' body
    PyPI -> Pypi.projectVersions <$> decode' body
    RubyGems -> Rubygems.listingVersions <$> decode' body
  where
    decode' :: (FromJSON a) => LByteString -> Maybe a
    decode' = rightToMaybe . eitherDecode
