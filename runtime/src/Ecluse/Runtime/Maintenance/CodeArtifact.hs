-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The AWS CodeArtifact leaf of the store maintenance handle. This is __control plane__ only,
on @amazonka@, while the data plane stays on @http-client@. The calls are a control-plane record
built once from a discovered identity and captured in the handle's closures, so the backend's
state never reaches the proxy's @Env@ and a spec can drive the sequencing without one. The
read-only calls are their own record ("Ecluse.Runtime.Maintenance.CodeArtifact.Read"), and the
decisions live in "Ecluse.Runtime.Maintenance.CodeArtifact.Decide".
-}
module Ecluse.Runtime.Maintenance.CodeArtifact (
    newCodeArtifactMaintenance,
    newCodeArtifactObservation,
    newCodeArtifactUpstreamProbe,
    newCodeArtifactCacheMaintenance,
    newCodeArtifactCacheObservation,
) where

import Ecluse.Runtime.Maintenance.CodeArtifact.Internal (
    newCodeArtifactCacheMaintenance,
    newCodeArtifactCacheObservation,
    newCodeArtifactMaintenance,
    newCodeArtifactObservation,
    newCodeArtifactUpstreamProbe,
 )
