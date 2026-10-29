-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Frozen registry corpus metadata shared by performance harnesses.
Capture policy and pins live in @bench/corpus/pins.json@.
-}
module Ecluse.Test.Corpus (
    CorpusTier (..),
    CorpusPackage (..),
    corpusPackages,
    pypiCorpusPackages,
    cargoCorpusPackages,
    rubyGemsCorpusPackages,
    cpName,
    corpusLabel,
    CaptureUpstream (..),
    npmCaptureUpstream,
    pypiCaptureUpstream,
    cargoCaptureUpstream,
    rubyGemsCaptureUpstream,
    readCorpusPins,
    FilePin (..),
    readPinned,
    readCapture,
    CaptureRecord (..),
    readCaptureRecords,
    captureTexts,
    syntheticProxyBase,
    permissiveAgeRules,
) where

import Data.Aeson (FromJSON (parseJSON), Object, Value (Object), eitherDecode, withObject, (.:))
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString qualified as BS
import Data.JsonStream.Parser qualified as J
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime, nominalDay)
import System.FilePath ((</>))
import Test.Hspec (shouldBe, shouldSatisfy)

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems), ecosystemName)
import Ecluse.Core.Package (PackageName, mkPackageName, mkScope, pkgEcosystem, renderPackageName)
import Ecluse.Core.Registry.Cargo.Request (cargoArtifactHosts)
import Ecluse.Core.Registry.JsonStream (StreamResult (streamValue))
import Ecluse.Core.Registry.Npm.Request (npmArtifactHosts)
import Ecluse.Core.Registry.PyPI.Request (pypiArtifactHosts)
import Ecluse.Core.Rules.Types (PrecededRule, Rule (AllowIfOlderThan))
import Ecluse.Core.Security (AllowedHostPorts, BodyLimit (MetadataBodyLimit), ecosystemArtifactAuthorities)
import Ecluse.Test.Package (hexSha256Of, unscopedNpm)
import Ecluse.Test.Registry.JsonStream (parseJsonChunks)
import Ecluse.Test.Rules (atDefaultPrecedence)
import Ecluse.Test.Support (expectRight)

-- | Size tiers sort from the smallest corpus tier to the heaviest.
data CorpusTier = Medium | Large | Heavy
    deriving stock (Eq, Ord, Show)

-- | A pinned registry document with its size tier and load weight.
data CorpusPackage = CorpusPackage
    { cpPackage :: PackageName
    -- ^ The requested name a projection validates the capture's self-reported name against.
    , cpPath :: FilePath
    -- ^ The capture's path, relative to the package root Cabal runs the harness from.
    , cpTier :: CorpusTier
    -- ^ The size and shape tier.
    , cpWeight :: Int
    -- ^ The package's multiplicity in the load harness's large-emphasis serve mix.
    }

-- | npm captures in the stable order used to select load working sets.
corpusPackages :: [CorpusPackage]
corpusPackages =
    [ entry Heavy 8 (scoped "types" "node") (corpusRoot <> "types-node.full.json")
    , entry Heavy 8 (unscopedNpm "webpack") (corpusRoot <> "webpack.full.json")
    , entry Heavy 6 (scoped "aws-sdk" "client-s3") (corpusRoot <> "aws-sdk-client-s3.full.json")
    , entry Large 4 (unscopedNpm "express") (corpusRoot <> "express.full.json")
    , entry Large 4 (unscopedNpm "typescript") (corpusRoot <> "typescript.full.json")
    , entry Large 3 (scoped "babel" "core") (corpusRoot <> "babel-core.full.json")
    , entry Large 2 (unscopedNpm "react") (corpusRoot <> "react.full.json")
    , entry Medium 2 (unscopedNpm "request") (corpusRoot <> "request.full.json")
    , entry Medium 2 (unscopedNpm "lodash") (corpusRoot <> "lodash.full.json")
    ]
  where
    entry tier weight name path =
        CorpusPackage{cpPackage = name, cpPath = path, cpTier = tier, cpWeight = weight}
    scoped s = mkPackageName Npm (Just (mkScope s))

corpusRoot :: FilePath
corpusRoot = "bench/corpus/npm/"

-- | PEP 691 captures in the stable order used to select load working sets.
pypiCorpusPackages :: [CorpusPackage]
pypiCorpusPackages =
    [ entry Heavy 8 "boto3"
    , entry Large 4 "numpy"
    , entry Medium 2 "requests"
    ]
  where
    entry tier weight name = CorpusPackage (mkPackageName PyPI Nothing name) ("bench/corpus/pypi/" <> toString name <> ".simple.json") tier weight

-- | Complete crates.io sparse files, ordered from the heaviest workload to the medium tier.
cargoCorpusPackages :: [CorpusPackage]
cargoCorpusPackages =
    [ entry Heavy 8 "aws-sdk-ec2"
    , entry Large 4 "windows"
    , entry Medium 2 "tokio"
    ]
  where
    entry tier weight name = CorpusPackage (mkPackageName Cargo Nothing name) ("bench/corpus/cargo/" <> toString name <> ".index.ndjson") tier weight

-- | Complete named-gem histories include dependency-heavy and many-platform releases.
rubyGemsCorpusPackages :: [CorpusPackage]
rubyGemsCorpusPackages =
    [ entry Heavy 8 "rails"
    , entry Large 4 "nokogiri"
    , entry Medium 2 "aws-sdk-ec2"
    ]
  where
    entry tier weight name = CorpusPackage (mkPackageName RubyGems Nothing name) ("bench/corpus/rubygems/" <> toString name <> ".info") tier weight

-- | A corpus package's wire name, both the request path and the body's self-reported name.
cpName :: CorpusPackage -> Text
cpName = renderPackageName . cpPackage

-- | The stable corpus label used in native benchmark rows.
corpusLabel :: CorpusPackage -> Text
corpusLabel package = cpName package <> " (" <> tier <> ")"
  where
    tier = case cpTier package of
        Medium -> "medium"
        Large -> "large"
        Heavy -> "heavy"

-- | The registry a capture came from, which production enforces artifact locations against.
data CaptureUpstream = CaptureUpstream
    { upstreamOrigin :: Text
    , upstreamAuthorities :: AllowedHostPorts
    -- ^ The artifact hosts the registry may name besides its own.
    }

-- | The npm registry the npm captures came from.
npmCaptureUpstream :: CaptureUpstream
npmCaptureUpstream = CaptureUpstream "https://registry.npmjs.org" (ecosystemArtifactAuthorities npmArtifactHosts)

-- | The PyPI Simple index the PyPI captures came from.
pypiCaptureUpstream :: CaptureUpstream
pypiCaptureUpstream = CaptureUpstream "https://pypi.org/simple" (ecosystemArtifactAuthorities pypiArtifactHosts)

-- | The public sparse root and its predeclared archive authorities.
cargoCaptureUpstream :: CaptureUpstream
cargoCaptureUpstream = CaptureUpstream "https://index.crates.io" (ecosystemArtifactAuthorities cargoArtifactHosts)

-- | Native per-gem source metadata binds archive URLs to the same registry authority.
rubyGemsCaptureUpstream :: CaptureUpstream
rubyGemsCaptureUpstream = CaptureUpstream "https://rubygems.org" (ecosystemArtifactAuthorities [])

-- | Parse @bench/corpus/pins.json@ with the given parser, or return why it did not parse.
readCorpusPins :: (Object -> Parser a) -> IO (Either String a)
readCorpusPins parser = do
    raw <- readFileLBS pinsPath
    pure (first ((pinsPath <> ": ") <>) (eitherDecode raw >>= parseEither (withObject "corpus pins" parser)))
  where
    pinsPath = "bench/corpus/pins.json"

-- | A file under @bench/corpus@ with its committed size and SHA-256 digest.
data FilePin = FilePin
    { fpPath :: FilePath
    , fpBytes :: Int64
    , fpSha256 :: Text
    }

instance FromJSON FilePin where
    parseJSON = withObject "file pin" $ \pin -> FilePin <$> pin .: "path" <*> pin .: "bytes" <*> pin .: "sha256"

-- | Read a catalogue-relative file, refusing a changed size or digest before measurements start.
readPinned :: FilePin -> IO LByteString
readPinned pin = do
    raw <- readFileBS ("bench/corpus" </> fpPath pin)
    unless (fromIntegral (BS.length raw) == fpBytes pin && hexSha256Of raw == fpSha256 pin) $
        fail ("bench/corpus/" <> fpPath pin <> " differs from its size or SHA-256 in bench/corpus/pins.json")
    pure (fromStrict raw)

-- | A committed capture's file identity and capture time.
data CaptureRecord = CaptureRecord
    { crPin :: FilePin
    , crCapturedAt :: UTCTime
    }

-- | The capture records @bench/corpus/pins.json@ keeps for the ecosystem, by package name.
readCaptureRecords :: Ecosystem -> IO (Either String (Map Text CaptureRecord))
readCaptureRecords eco = readCorpusPins $ \pins -> do
    recorded <- pins .: "captures"
    entries <- recorded .: fromString (toString (ecosystemName eco))
    traverse (withObject "capture" (\capture -> CaptureRecord <$> parseJSON (Object capture) <*> capture .: "capturedAt")) entries

-- | Authenticate a registered capture, refusing missing pins and mismatched paths.
readCapture :: CorpusPackage -> IO ByteString
readCapture package = do
    records <- readCaptureRecords (pkgEcosystem (cpPackage package)) >>= either fail pure
    record <- maybe (fail ("missing capture pin for " <> toString (cpName package))) pure (Map.lookup (cpName package) records)
    let pin = crPin record
    unless ("bench/corpus" </> fpPath pin == cpPath package) $
        fail ("capture pin path differs for " <> toString (cpName package))
    toStrict <$> readPinned pin

-- | Read each capture entry, rejecting empty captures and entries below the requested text count.
captureTexts :: Int -> J.Parser [Text] -> CorpusPackage -> IO [[Text]]
captureTexts fewest textsOf package = do
    bytes <- readFileBS (cpPath package)
    streamed <- expectRight (parseJsonChunks (MetadataBodyLimit (BS.length bytes)) textsOf (\held entry -> Right (entry : held)) [] [bytes])
    entries <- expectRight (streamValue streamed)
    entries `shouldSatisfy` (not . null)
    take 5 (filter ((< fewest) . length) entries) `shouldBe` []
    pure entries

-- | The placeholder proxy origin the serve-time rewrite puts tarball URLs under.
syntheticProxyBase :: Text
syntheticProxyBase = "https://ecluse.example"

-- | Admit releases older than one day to measure the complete serving transform.
permissiveAgeRules :: [PrecededRule]
permissiveAgeRules = [atDefaultPrecedence (AllowIfOlderThan nominalDay)]
