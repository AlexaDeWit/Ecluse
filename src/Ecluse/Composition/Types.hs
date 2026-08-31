-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The roles a boot runs under: which command started the process, which half of the mirror
pipeline that process runs, and which registry role its validation rules apply.

The command line writes these and the boot pipeline reads them, so they live in a module neither
side owns.
-}
module Ecluse.Composition.Types (
    -- * The booting command
    BootRole (..),
    registryRoleOf,
    pipelineRoleOf,

    -- * The roles it decomposes into
    MirrorRole (..),
    RegistryRole (..),
) where

{- | What a booting command does with the configured mirror targets. It decides each rule's
severity and which witnesses the boot's vetting pass issues.
-}
data BootRole
    = -- | @ecluse proxy@, @ecluse proxy --no-worker@ and @ecluse mirror@: they write.
      BootMirrorPipeline MirrorRole
    | -- | @ecluse dredger@: it permanently deletes.
      BootStorePruner
    | -- | @ecluse pilot@ and @ecluse check-config@: they neither mirror nor delete.
      BootWithoutPipeline
    deriving stock (Eq, Show)

-- | The registry role a command's rules apply. Only the Dredger deletes.
registryRoleOf :: BootRole -> RegistryRole
registryRoleOf = \case
    BootMirrorPipeline _ -> MirrorWriter
    BootStorePruner -> MirrorPruner
    BootWithoutPipeline -> MirrorWriter

-- | The mirror-pipeline half a command runs, 'Nothing' where it runs none.
pipelineRoleOf :: BootRole -> Maybe MirrorRole
pipelineRoleOf = \case
    BootMirrorPipeline role -> Just role
    BootStorePruner -> Nothing
    BootWithoutPipeline -> Nothing

-- | The mirror-pipeline halves one process runs, selected by the command line.
data MirrorRole
    = -- | @ecluse proxy@: the front door and the mirror worker in one process.
      ServeAndMirror
    | -- | @ecluse proxy --no-worker@: the front door alone, still enqueueing.
      ServeOnly
    | -- | @ecluse mirror@: the worker alone, serving only its health probes.
      MirrorOnly
    deriving stock (Eq, Show)

{- | The boot role a vetting pass vets for. The proxy and the mirror worker write to a mount's
mirror target, and the Dredger deletes from it, which is what makes a shared store unsafe.
-}
data RegistryRole
    = -- | @ecluse proxy@ and @ecluse mirror@: they read and write, and delete nothing.
      MirrorWriter
    | -- | @ecluse dredger@: it permanently deletes from every mount's mirror target.
      MirrorPruner
    deriving stock (Eq, Show)
