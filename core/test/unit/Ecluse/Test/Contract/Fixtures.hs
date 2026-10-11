-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Independent requirements and registrations for catalogue failure controls.
module Ecluse.Test.Contract.Fixtures (catalogue, profile, obligation, scenarioGroup, item, executionIdentity, report) where

import Ecluse.Test.Contract.Phase (PhaseTimings (..))
import Ecluse.Test.Contract.Result
import Ecluse.Test.Contract.Types

-- | A production ecosystem with one provider and one reviewed obligation.
catalogue :: Catalogue
catalogue = Catalogue ["example"] [profile] [obligation] [scenarioGroup]

-- | A provider that supports metadata reads.
profile :: Profile
profile = Profile "example/http" ["example"] ["http"] ["read"]

-- | Expected coverage is deliberately independent of the registered case list.
obligation :: Obligation
obligation = Obligation "read/identity" "example/http" "metadata" Supported ["read"] ["example/read/identity"]

-- | One whole lifecycle belonging to the primary shard.
scenarioGroup :: Group
scenarioGroup = Group "example/read" "metadata" "example/http" Unit "primary" [item] ["read"] [] [] []

-- | The concrete example refers back to the reviewed obligation.
item :: Case
item = Case "example/read/identity" ["read/identity"]

-- | All five execution identities are independently checked.
executionIdentity :: Identity
executionIdentity = Identity "source" "image" "architecture" "digest" "run"

-- | A complete primary report for the fixture catalogue.
report :: Report
report = Report 1 executionIdentity "primary" False [GroupResult "example/read" Passed 0 [] (PhaseTimings 0 0 0)] [CaseResult "example/read/identity" "example/read" Passed 0 []] 1 [scenarioGroup]
