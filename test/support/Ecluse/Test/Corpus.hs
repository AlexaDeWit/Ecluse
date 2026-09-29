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
    cpName,
    CaptureUpstream (..),
    npmCaptureUpstream,
    pypiCaptureUpstream,
    readCorpusPins,
    CaptureRecord (..),
    readCaptureRecords,
    syntheticProxyBase,
    permissiveAgeRules,
) where

import Data.Aeson (Object, eitherDecode, withObject, (.:))
import Data.Aeson.Types (Parser, parseEither)
import Data.Time (UTCTime, nominalDay)

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI), ecosystemName)
import Ecluse.Core.Package (PackageName, mkPackageName, mkScope, renderPackageName)
import Ecluse.Core.Registry.Npm.Request (npmArtifactHosts)
import Ecluse.Core.Registry.PyPI.Request (pypiArtifactHosts)
import Ecluse.Core.Rules.Types (PrecededRule, Rule (AllowIfOlderThan))
import Ecluse.Core.Security (AllowedHostPorts, ecosystemArtifactAuthorities)
import Ecluse.Test.Package (unscopedNpm)
import Ecluse.Test.Rules (atDefaultPrecedence)

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

-- | A corpus package's wire name, both the request path and the body's self-reported name.
cpName :: CorpusPackage -> Text
cpName = renderPackageName . cpPackage

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

-- | Parse @bench/corpus/pins.json@ with the given parser, or return why it did not parse.
readCorpusPins :: (Object -> Parser a) -> IO (Either String a)
readCorpusPins parser = do
    raw <- readFileLBS pinsPath
    pure (first ((pinsPath <> ": ") <>) (eitherDecode raw >>= parseEither (withObject "corpus pins" parser)))
  where
    pinsPath = "bench/corpus/pins.json"

-- | A committed capture's recorded byte count, SHA-256 digest, and capture time.
data CaptureRecord = CaptureRecord
    { crBytes :: Int64
    , crSha256 :: Text
    , crCapturedAt :: UTCTime
    }

-- | The capture records @bench/corpus/pins.json@ keeps for the ecosystem, by package name.
readCaptureRecords :: Ecosystem -> IO (Either String (Map Text CaptureRecord))
readCaptureRecords eco = readCorpusPins $ \pins -> do
    recorded <- pins .: "captures"
    entries <- recorded .: fromString (toString (ecosystemName eco))
    traverse (withObject "capture" (\capture -> CaptureRecord <$> capture .: "bytes" <*> capture .: "sha256" <*> capture .: "capturedAt")) entries

-- | The placeholder proxy origin the serve-time rewrite puts tarball URLs under.
syntheticProxyBase :: Text
syntheticProxyBase = "https://ecluse.example"

-- | Admit releases older than one day to measure the complete serving transform.
permissiveAgeRules :: [PrecededRule]
permissiveAgeRules = [atDefaultPrecedence (AllowIfOlderThan nominalDay)]
