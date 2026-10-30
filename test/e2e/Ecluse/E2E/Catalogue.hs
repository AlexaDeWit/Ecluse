-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | E2E scheduling metadata is independent of Hspec construction and resource acquisition.
module Ecluse.E2E.Catalogue (catalogue, prerequisites) where

import Prelude hiding (universe)

import Data.Universe.Class (universe)

import Ecluse.Core.Ecosystem (ecosystemName)
import Ecluse.Core.Registry.Adapter (adapterFor)
import Ecluse.E2E.Cargo.DredgerE2ESpec (cargoDredgerProfile)
import Ecluse.E2E.Harness.Availability (e2ePrerequisites)
import Ecluse.E2E.PyPI.DredgerE2ESpec (pypiDredgerProfile)
import Ecluse.E2E.Requirements (obligations)
import Ecluse.E2E.RubyGems.Profiles (rubyProfiles)
import Ecluse.E2E.Suites.Cargo qualified as Cargo
import Ecluse.E2E.Suites.Npm qualified as Npm
import Ecluse.E2E.Suites.PyPI qualified as PyPI
import Ecluse.E2E.Suites.RubyGems qualified as RubyGems
import Ecluse.E2E.Suites.SharedSystem qualified as SharedSystem
import Ecluse.Test.Contract.Ecosystem (missingCapabilityObligations, missingCapabilityProfiles)
import Ecluse.Test.Contract.Runner (DeclaredGroup (..))
import Ecluse.Test.Contract.Types

-- | Fixed requirements do not shrink when a production adapter lacks an operation.
catalogue :: Catalogue
catalogue =
    Catalogue
        { catalogueProductionEcosystems = map ecosystemName $ filter (isJust . adapterFor) universe
        , catalogueProfiles = profiles <> missingCapabilityProfiles "npm" Npm.declarations <> missingCapabilityProfiles "pypi" PyPI.declarations <> missingCapabilityProfiles "cargo" Cargo.declarations <> missingCapabilityProfiles "rubygems" RubyGems.declarations
        , catalogueObligations = obligations <> missingCapabilityObligations declarations
        , catalogueGroups = map declaredGroup declarations
        }

profiles :: [Profile]
profiles =
    [ Profile "npm/read" ["npm"] ["http"] ["read", "client-install"]
    , Profile "npm/registry" ["npm"] ["verdaccio", "memory"] ["read", "mirror", "publication", "client-install"]
    , Profile "npm/maintenance" ["npm"] ["verdaccio", "sqs", "s3"] ["read", "mirror", "maintenance", "advisories"]
    , Profile "npm/advisories" ["npm"] ["http", "verdaccio", "sqs", "s3"] ["read", "mirror", "advisories", "client-install"]
    , Profile "pypi/write" ["pypi"] ["http", "sqs", "s3"] ["read", "mirror", "client-install", "telemetry"]
    , Profile "pypi/publication" ["pypi"] ["http"] ["read", "publication", "client-install"]
    , Profile "pypi/write-telemetry" ["pypi"] ["http", "sqs", "s3", "collector"] ["read", "mirror", "client-install", "telemetry"]
    , Profile "cargo/read" ["cargo"] ["http"] ["read", "client-install", "telemetry"]
    , Profile "cargo/write" ["cargo"] ["http", "sqs", "s3", "collector"] ["read", "mirror", "publication", "client-install", "telemetry", "advisories"]
    , cargoDredgerProfile
    , pypiDredgerProfile
    , Profile "pypi/read" ["pypi"] ["http"] ["read", "client-install"]
    , Profile "pypi/advisories" ["pypi"] ["http", "s3"] ["read", "advisories", "client-install"]
    , Profile "mixed/registry" ["npm", "pypi", "cargo"] ["verdaccio", "memory"] ["read", "mirror", "client-install"]
    , Profile "npm/telemetry" ["npm"] ["verdaccio", "memory", "collector"] ["read", "mirror", "telemetry", "client-install"]
    , Profile "pypi/telemetry" ["pypi"] ["http", "collector"] ["read", "telemetry", "client-install"]
    ]
        <> rubyProfiles

declarations :: [DeclaredGroup]
declarations = Npm.declarations <> PyPI.declarations <> Cargo.declarations <> RubyGems.declarations <> SharedSystem.declarations

-- | Missing tools fail setup without replacing declared examples with pending placeholders.
prerequisites :: Group -> IO (Maybe Text)
prerequisites = fmap (fmap toText) . e2ePrerequisites . map toString . groupPrerequisites
