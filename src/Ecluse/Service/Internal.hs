-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The runtime's data-plane managers, exported for "Ecluse.Service" and its specs. Importing it opts out of "Ecluse.Service"'s stability promise.
module Ecluse.Service.Internal (
    dataPlaneManagers,
) where

import Network.HTTP.Client (Manager)

import Ecluse.Composition.Plan (BootPlan (bpPrivateConnections, bpPublicConnections, bpValidated))
import Ecluse.Composition.Sizing (newPooledManager)
import Ecluse.Composition.Validate (ValidatedPlan (vpSettings))
import Ecluse.Config (AppConfig (cfgEgress), EgressSettings (egrOperatorContact))
import Ecluse.Core.BuildIdentity (identityManagerSettings, renderUserAgent)
import Ecluse.Core.Fault.Http (typedTlsManagerSettings)

{- | The public and the private data-plane managers, each pooled at its own bound. They stay split
because public reads are anonymous and private reads forward the caller's credential.
-}
dataPlaneManagers :: BootPlan -> IO (Manager, Manager)
dataPlaneManagers bootPlan = (,) <$> pooled (bpPublicConnections bootPlan) <*> pooled (bpPrivateConnections bootPlan)
  where
    renderedAgent = renderUserAgent (egrOperatorContact (cfgEgress (vpSettings (bpValidated bootPlan))))
    pooled connections = newPooledManager connections (identityManagerSettings renderedAgent typedTlsManagerSettings)
