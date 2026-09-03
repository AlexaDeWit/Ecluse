-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The composition root's store maintenance build, split across the boot's two tiers. The pure
half reads each mount's mirror target as the backend that maintains it, as one rule in the vetting
pass, so @ecluse dredger@ refuses a store this build cannot sweep and @ecluse check-config@ names
that refusal. Only that pass issues a 'ClearedBackend', so the effectful half, in the pruner's arm
of the planning phase ("Ecluse.Composition.Executable"), builds a handle for no store it did not
clear. The discrimination is on the store URL alone, the way the mirror-write credential is derived
("Ecluse.Config.MirrorCredential"), so a store and the backend that sweeps it cannot diverge.
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

    -- * Internals exported for testing
    StoreBackend (..),
    resolveStoreBackend,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Validation (eitherToValidation, validationToEither)

import Ecluse.Composition.BootError (
    BootError (StoreMaintenanceUnavailable),
    StoreMaintenanceReason (ClientBuildFailed, NoBackendForHost, NoFormatFor, NotRepositoryEndpoint),
    refuseOnThrow,
 )
import Ecluse.Composition.Types (RegistryRole (MirrorPruner, MirrorWriter))
import Ecluse.Composition.Vet (Severity (Ignore, Refuse), Vet, rule, vetRole)
import Ecluse.Config (MountConfig (mntMirrorTarget))
import Ecluse.Config.MirrorCredential (parseCodeArtifactHost)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Registry.Maintenance (StoreMaintenance)
import Ecluse.Core.Security (hostAddress)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Text (afterFirst, nonBlank)
import Ecluse.Runtime.Maintenance.CodeArtifact (newCodeArtifactMaintenance)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (
    CodeArtifactStore (..),
    codeArtifactFormat,
    formatToken,
 )

-- | Which backend maintains a mirror store, one arm per backend this build carries.
newtype StoreBackend = CodeArtifactBackend CodeArtifactStore
    deriving stock (Eq, Show)

{- | A backend the deleting role's pass cleared. Only 'vetStoreBackends' issues one, so a handle
that can delete is built for no store that pass did not clear.
-}
newtype ClearedBackend = ClearedBackend StoreBackend
    deriving stock (Eq, Show)

-- | The backend a cleared witness carries, for a caller that reads it back.
clearedBackend :: ClearedBackend -> StoreBackend
clearedBackend (ClearedBackend backend) = backend

{- | The rule every declared mirror target meets: this build carries a backend that can sweep it.
The deleting role refuses a target that fails it. A writing role deletes nothing and ignores it.
-}
vetStoreBackends :: Map Ecosystem MountConfig -> Vet (Map Ecosystem ClearedBackend)
vetStoreBackends mounts = clearedFor <$> vetRole <* traverse_ (rule severity unmaintained) resolved
  where
    resolved =
        [ (eco, resolveStoreBackend eco url)
        | (eco, mcfg) <- Map.toAscList mounts
        , Just url <- [mntMirrorTarget mcfg]
        ]

    severity = \case
        MirrorPruner -> Refuse (uncurry StoreMaintenanceUnavailable)
        MirrorWriter -> Ignore

    unmaintained (eco, outcome) = (eco,) <$> leftToMaybe outcome

    -- A refused pass yields no plan, so a target the rule refused never reaches this map.
    clearedFor = \case
        MirrorWriter -> Map.empty
        MirrorPruner -> Map.fromList [(eco, ClearedBackend backend) | (eco, Right backend) <- resolved]

{- | Read a mirror target as the backend that maintains it, or why none in this build can. The
host decides which backend, and only then does that backend read the rest as its coordinates.
-}
resolveStoreBackend :: Ecosystem -> RegistryUrl -> Either StoreMaintenanceReason StoreBackend
resolveStoreBackend eco url = case parseCodeArtifactHost (hostAddress raw) of
    Just (domain, owner, region) ->
        CodeArtifactBackend <$> codeArtifactCoordinates eco raw domain owner region
    Nothing -> Left NoBackendForHost
  where
    raw = registryUrlText url

{- The rest of a CodeArtifact endpoint: the format the ecosystem maps to, and the repository under
it. The path's format segment has to be the mount's own, because a repository's per-format
endpoints are separate stores. -}
codeArtifactCoordinates ::
    Ecosystem -> Text -> Text -> Text -> Text -> Either StoreMaintenanceReason CodeArtifactStore
codeArtifactCoordinates eco raw domain owner region = do
    format <- maybeToRight (NoFormatFor eco) (codeArtifactFormat eco)
    repository <-
        maybeToRight
            (NotRepositoryEndpoint (formatToken format))
            (repositoryOfPath (formatToken format) raw)
    pure
        CodeArtifactStore
            { casDomain = domain
            , casDomainOwner = owner
            , casRegion = region
            , casRepository = repository
            , casFormat = format
            }

-- The repository a CodeArtifact endpoint path names, under the expected format segment.
repositoryOfPath :: Text -> Text -> Maybe Text
repositoryOfPath format url = case pathSegments url of
    [pathFormat, repository] | pathFormat == format -> nonBlank repository
    _ -> Nothing

-- The non-empty path segments of an absolute URL, which the egress boundary has already vetted.
pathSegments :: Text -> [Text]
pathSegments url = filter (not . T.null) (T.splitOn "/" (T.dropWhile (/= '/') (afterFirst "://" url)))

{- | How a boot builds one store's maintenance handle. Injected, as the queue builder is, so a spec
drives the pruner's arm of the planning phase without discovering an AWS identity.
-}
type BuildStoreMaintenance = ClearedBackend -> IO StoreMaintenance

-- | The live handle for a cleared backend, with its credentials discovered the standard AWS way.
buildStoreMaintenance :: BuildStoreMaintenance
buildStoreMaintenance (ClearedBackend (CodeArtifactBackend coordinates)) =
    newCodeArtifactMaintenance coordinates

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
