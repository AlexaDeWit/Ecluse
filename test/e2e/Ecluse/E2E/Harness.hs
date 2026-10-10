-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The end-to-end harness as one import for the spec modules.
module Ecluse.E2E.Harness (
    module Ecluse.E2E.Harness.Types,
    module Ecluse.E2E.Harness.Advisories,
    module Ecluse.E2E.Harness.Availability,
    module Ecluse.E2E.Harness.Client,
    module Ecluse.E2E.Harness.Collector,
    module Ecluse.E2E.Harness.Docker,
    module Ecluse.E2E.Harness.InstalledTree,
    module Ecluse.E2E.Harness.Npm,
    module Ecluse.E2E.Harness.Pip,
    module Ecluse.E2E.Harness.Proxy,
    module Ecluse.E2E.Harness.Stub,
    module Ecluse.E2E.Harness.Verdaccio,
) where

import Ecluse.E2E.Harness.Advisories
import Ecluse.E2E.Harness.Availability
import Ecluse.E2E.Harness.Client
import Ecluse.E2E.Harness.Collector
import Ecluse.E2E.Harness.Docker
import Ecluse.E2E.Harness.InstalledTree
import Ecluse.E2E.Harness.Npm
import Ecluse.E2E.Harness.Pip
import Ecluse.E2E.Harness.Proxy
import Ecluse.E2E.Harness.Stub
import Ecluse.E2E.Harness.Types
import Ecluse.E2E.Harness.Verdaccio
