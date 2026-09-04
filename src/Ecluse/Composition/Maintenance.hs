-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The composition root's store maintenance build, split across the boot's two tiers. The pure
half reads each mount's resolved store backend ("Ecluse.Config.Target") as one rule in the vetting
pass, so @ecluse dredger@ refuses a store this build cannot sweep and @ecluse check-config@ names
that refusal. Only that pass issues a 'ClearedBackend', and the effectful half, in the pruner's arm
of the planning phase ("Ecluse.Composition.Executable"), builds a handle from that witness alone.
-}
module Ecluse.Composition.Maintenance (
    -- * The config-decidable half
    ClearedBackend,
    clearedBackend,
    vetStoreBackends,

    -- * The environment-dependent half
    BuildStoreMaintenance,
    buildStoreMaintenance,
    planStoreMaintenance,
) where

import Data.Map.Strict qualified as Map
import Validation (eitherToValidation, validationToEither)

import Ecluse.Composition.BootError (
    BootError (StoreMaintenanceUnavailable),
    StoreMaintenanceReason (ClientBuildFailed, CodeArtifactUnaddressable, NoControlPlane),
    refuseOnThrow,
 )
import Ecluse.Composition.Types (RegistryRole (MirrorPruner, MirrorWriter))
import Ecluse.Composition.Vet (Severity (Ignore, Refuse), Vet, rule, vetRole)
import Ecluse.Config (
    ControlPlane (ControlCodeArtifact, ControlNone),
    MirrorTarget (mtBackend),
    Mount (mountRegistries),
    MountMap,
    StoreBackend (sbControl, sbTag),
    regMirrorTarget,
 )
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Registry.Maintenance (StoreMaintenance)
import Ecluse.Runtime.Maintenance.CodeArtifact (newCodeArtifactMaintenance)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (CodeArtifactStore)

{- | A store the deleting role's pass cleared. Only 'vetStoreBackends' issues one, so a handle
that can delete is built for no store that pass did not clear.
-}
newtype ClearedBackend = ClearedBackend CodeArtifactStore
    deriving stock (Eq, Show)

-- | The store a cleared witness addresses, for a caller that reads it back.
clearedBackend :: ClearedBackend -> CodeArtifactStore
clearedBackend (ClearedBackend store) = store

{- | The rule every declared mirror target meets: its resolved backend offers a control plane this
build can sweep. The deleting role refuses a target that fails it, and a writing role ignores it.
-}
vetStoreBackends :: MountMap -> Vet (Map Ecosystem ClearedBackend)
vetStoreBackends mounts = clearedFor <$> vetRole <* traverse_ (rule severity unmaintained) resolved
  where
    resolved =
        [ (eco, sweepableStore (mtBackend target))
        | (eco, mount) <- Map.toAscList mounts
        , Just target <- [regMirrorTarget (mountRegistries mount)]
        ]

    severity = \case
        MirrorPruner -> Refuse (uncurry StoreMaintenanceUnavailable)
        MirrorWriter -> Ignore

    unmaintained (eco, outcome) = (eco,) <$> leftToMaybe outcome

    -- A refused pass yields no plan, so a target the rule refused never reaches this map.
    clearedFor = \case
        MirrorWriter -> Map.empty
        MirrorPruner -> Map.fromList [(eco, ClearedBackend store) | (eco, Right store) <- resolved]

-- The store a resolved backend lets the Dredger delete from, or why this build reaches none.
sweepableStore :: StoreBackend -> Either StoreMaintenanceReason CodeArtifactStore
sweepableStore backend = case sbControl backend of
    ControlCodeArtifact addressed -> first CodeArtifactUnaddressable addressed
    ControlNone -> Left (NoControlPlane (sbTag backend))

{- | How a boot builds one store's maintenance handle. Injected, as the queue builder is, so a spec
drives the pruner's arm of the planning phase without discovering an AWS identity.
-}
type BuildStoreMaintenance = ClearedBackend -> IO StoreMaintenance

-- | The live handle for a cleared store, with its credentials discovered the standard AWS way.
buildStoreMaintenance :: BuildStoreMaintenance
buildStoreMaintenance = newCodeArtifactMaintenance . clearedBackend

{- | Build one handle per cleared store, or every refusal the live environment earns. The builds
accumulate, so one launch reports every store whose client cannot be built.
-}
planStoreMaintenance ::
    BuildStoreMaintenance ->
    Map Ecosystem ClearedBackend ->
    IO (Either [BootError] (Map Ecosystem StoreMaintenance))
planStoreMaintenance build backends =
    validationToEither . traverse eitherToValidation <$> Map.traverseWithKey planOne backends
  where
    planOne eco backend =
        refuseOnThrow (StoreMaintenanceUnavailable eco . ClientBuildFailed) (build backend)
