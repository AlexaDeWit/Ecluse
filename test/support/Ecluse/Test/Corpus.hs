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
    syntheticProxyBase,
    permissiveAgeRules,
) where

import Data.Time (nominalDay)

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Package (PackageName, mkPackageName, mkScope, renderPackageName)
import Ecluse.Core.Rules.Types (PrecededRule, Rule (AllowIfOlderThan))
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

-- | npm captures, heaviest first for the load harness's working set.
corpusPackages :: [CorpusPackage]
corpusPackages =
    [ entry Heavy 8 (scoped "types" "node") (corpusRoot <> "types-node.full.json")
    , entry Heavy 8 (unscopedNpm "webpack") (corpusRoot <> "webpack.full.json")
    , entry Heavy 6 (scoped "aws-sdk" "client-s3") (corpusRoot <> "aws-sdk-client-s3.full.json")
    , entry Large 4 (unscopedNpm "express") "core/test/unit/fixtures/npm/express.full.json"
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

-- | PEP 691 captures shared by benchmark and acceptance harnesses, heaviest first.
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

-- | The placeholder proxy origin the serve-time rewrite puts tarball URLs under.
syntheticProxyBase :: Text
syntheticProxyBase = "https://ecluse.example"

-- | Admit releases older than one day to measure the complete serving transform.
permissiveAgeRules :: [PrecededRule]
permissiveAgeRules = [atDefaultPrecedence (AllowIfOlderThan nominalDay)]
