-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The curated real-world npm packument corpus the performance harnesses share.

'corpusPackages' names every capture once: its package identity, its committed path, its
size tier, and its serve weight. The captures are frozen data. @bench\/corpus\/pins.json@
pins the version each is taken at, @make gen-bench-corpus@ re-captures them deliberately,
and each keeps its real heterogeneous shape rather than a trimmed model of one.
-}
module Ecluse.Test.Corpus (
    CorpusTier (..),
    CorpusPackage (..),
    corpusPackages,
    cpName,
    syntheticProxyBase,
    permissiveAgeRules,
) where

import Data.Time (nominalDay)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName, mkScope, renderPackageName)
import Ecluse.Core.Rules.Types (PrecededRule, Rule (AllowIfOlderThan))
import Ecluse.Test.Rules (atDefaultPrecedence)

-- | A corpus package's size and shape tier, which labels the rendered benchmark groups.
data CorpusTier = Medium | Large | Heavy
    deriving stock (Eq, Show)

-- | One curated real-world packument capture and what the harnesses do with it.
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

{- | The curated corpus, ordered __heaviest first__: the load harness's cache working set
takes the leading entries. A few-version package stresses nothing, so none are here.
-}
corpusPackages :: [CorpusPackage]
corpusPackages =
    [ entry Heavy 8 (scoped "types" "node") (corpusRoot <> "types-node.full.json")
    , entry Heavy 8 (unscoped "webpack") (corpusRoot <> "webpack.full.json")
    , entry Heavy 6 (scoped "aws-sdk" "client-s3") (corpusRoot <> "aws-sdk-client-s3.full.json")
    , entry Large 4 (unscoped "express") "core/test/unit/fixtures/npm/express.full.json"
    , entry Large 4 (unscoped "typescript") (corpusRoot <> "typescript.full.json")
    , entry Large 3 (scoped "babel" "core") (corpusRoot <> "babel-core.full.json")
    , entry Large 2 (unscoped "react") (corpusRoot <> "react.full.json")
    , entry Medium 2 (unscoped "request") (corpusRoot <> "request.full.json")
    , entry Medium 2 (unscoped "lodash") (corpusRoot <> "lodash.full.json")
    ]
  where
    entry tier weight name path =
        CorpusPackage{cpPackage = name, cpPath = path, cpTier = tier, cpWeight = weight}
    unscoped = mkPackageName Npm Nothing
    scoped s = mkPackageName Npm (Just (mkScope s))

-- Every capture but the reused express fixture lives here, relative to the package root.
corpusRoot :: FilePath
corpusRoot = "bench/corpus/npm/"

-- | A corpus package's wire name, both the request path and the body's self-reported name.
cpName :: CorpusPackage -> Text
cpName = renderPackageName . cpPackage

-- | The placeholder proxy origin the serve-time rewrite puts tarball URLs under.
syntheticProxyBase :: Text
syntheticProxyBase = "https://ecluse.example"

{- | A permissive rule set: every legitimately-aged version survives, so a harness measures
the whole transform instead of short-circuiting to a no-survivors denial.
-}
permissiveAgeRules :: [PrecededRule]
permissiveAgeRules = [atDefaultPrecedence (AllowIfOlderThan nominalDay)]
