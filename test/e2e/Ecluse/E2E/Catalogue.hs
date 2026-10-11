-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | E2E scheduling metadata is independent of Hspec construction and container acquisition.
module Ecluse.E2E.Catalogue (catalogue, prerequisites) where

import Prelude hiding (universe)

import Data.Universe.Class (universe)

import Ecluse.Core.Ecosystem (ecosystemName, parseEcosystem)
import Ecluse.Core.Registry.Adapter (RegistryAdapter (adapterPublish), adapterFor)
import Ecluse.E2E.Harness.Availability (e2ePrerequisites)
import Ecluse.E2E.Requirements (obligations)
import Ecluse.E2E.Suites.NpmServing qualified as NpmServing
import Ecluse.E2E.Suites.NpmWrites qualified as NpmWrites
import Ecluse.E2E.Suites.PyPI qualified as PyPI
import Ecluse.E2E.Suites.SharedSystem qualified as SharedSystem
import Ecluse.Test.Contract.Runner (DeclaredGroup (..))
import Ecluse.Test.Contract.Types

-- | Production adapter names, independent obligations, and concrete groups meet here.
catalogue :: Catalogue
catalogue =
    Catalogue
        { catalogueProductionEcosystems = map ecosystemName $ filter (isJust . adapterFor) universe
        , catalogueProfiles = profiles
        , catalogueObligations = map activateSupportedWrites obligations
        , catalogueGroups = map declaredGroup $ NpmServing.declarations <> NpmWrites.declarations <> PyPI.declarations <> SharedSystem.declarations
        }

profiles :: [Profile]
profiles =
    map
        withProductionFacts
        [ Profile "npm/registry" ["npm"] ["verdaccio", "memory"] ["read", "mirror", "publication", "client-install"]
        , Profile "npm/maintenance" ["npm"] ["verdaccio", "sqs", "s3"] ["read", "mirror", "maintenance", "advisories"]
        , Profile "pypi/read" ["pypi"] ["http"] ["read", "client-install"]
        , Profile "pypi/advisories" ["pypi"] ["http", "s3"] ["read", "advisories", "client-install"]
        , Profile "mixed/registry" ["npm", "pypi"] ["verdaccio", "memory"] ["read", "mirror", "client-install"]
        , Profile "npm/telemetry" ["npm"] ["verdaccio", "memory", "collector"] ["read", "mirror", "telemetry", "client-install"]
        ]

-- | Missing tools fail setup without replacing declared examples with a pending placeholder.
prerequisites :: Group -> IO (Maybe Text)
prerequisites = fmap (fmap toText) . e2ePrerequisites . map toString . groupPrerequisites

withProductionFacts :: Profile -> Profile
withProductionFacts profile = profile{profileCapabilities = filter supported (profileCapabilities profile)}
  where
    adapters = map (parseEcosystem >=> adapterFor) (profileEcosystems profile)
    supported capability = case capability of
        "read" -> all isJust adapters
        "mirror" -> any (maybe False (isJust . adapterPublish)) adapters
        "publication" -> any (maybe False (isJust . adapterPublish)) adapters
        _ -> True

activateSupportedWrites :: Obligation -> Obligation
activateSupportedWrites obligation
    | obligationId obligation `elem` ["pypi/mirror", "pypi/publication"]
    , Just adapter <- parseEcosystem "pypi" >>= adapterFor
    , isJust (adapterPublish adapter) =
        obligation{obligationSupport = Supported}
    | otherwise = obligation
