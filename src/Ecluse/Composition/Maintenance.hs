-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The composition root's store maintenance build, split across the boot's two tiers. The pure
half reads each mount's mirror target as the backend that maintains it, as one rule in the
vetting pass, so @ecluse dredger@ refuses a store this build cannot sweep and @ecluse check-config@
names that refusal. The effectful half builds that backend's handle in the pruner's arm of the
planning phase ("Ecluse.Composition.Executable"). The discrimination is on the store URL alone,
the way the mirror-write credential is derived ("Ecluse.Config.MirrorCredential"), so a store and
the backend that sweeps it cannot diverge. A URL naming no backend this build carries is a refusal
rather than a silent skip.
-}
module Ecluse.Composition.Maintenance (
    -- * The config-decidable half
    StoreBackend (..),
    vetStoreBackends,

    -- * The environment-dependent half
    BuildStoreMaintenance,
    buildStoreMaintenance,
    planStoreMaintenance,

    -- * Internals exported for testing
    resolveStoreBackend,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Validation (eitherToValidation, validationToEither)

import Ecluse.Composition.BootError (BootError (StoreMaintenanceUnavailable), refuseOnThrow)
import Ecluse.Composition.Types (RegistryRole (MirrorPruner, MirrorWriter))
import Ecluse.Composition.Vet (Severity (Ignore, Refuse), Vet, rule, vetRole)
import Ecluse.Config (MountConfig (mntMirrorTarget))
import Ecluse.Config.MirrorCredential (parseCodeArtifactHost)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Registry.Maintenance (StoreMaintenance)
import Ecluse.Core.Security (hostAddress)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Text (nonBlank)
import Ecluse.Runtime.Maintenance.CodeArtifact (newCodeArtifactMaintenance)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (
    CodeArtifactStore (..),
    codeArtifactFormat,
    formatToken,
 )

{- | Which backend maintains a mirror store, one arm per backend this build carries. A new backend
is one more arm here and one more handle behind it. Only 'vetStoreBackends' issues one onto the
plan, and only under the deleting role, so a handle that could delete is built for no store the
pass did not clear.
-}
newtype StoreBackend = CodeArtifactBackend CodeArtifactStore
    deriving stock (Eq, Show)

{- | The rule every declared mirror target meets: this build carries a backend that can sweep it.
The deleting role refuses a target that fails it, and its pass clears the backend for each that
passes. A writing role deletes nothing, so it boots on such a target and logs nothing, and the
checker names the Dredger's refusal for it.
-}
vetStoreBackends :: Map Ecosystem MountConfig -> Vet (Map Ecosystem StoreBackend)
vetStoreBackends mounts = clearedFor <$> vetRole <* traverse_ (rule severity unmaintained) targets
  where
    targets = [(eco, url) | (eco, mcfg) <- Map.toAscList mounts, Just url <- [mntMirrorTarget mcfg]]

    severity = \case
        MirrorPruner -> Refuse (uncurry StoreMaintenanceUnavailable)
        MirrorWriter -> Ignore

    unmaintained (eco, url) = (eco,) <$> leftToMaybe (resolveStoreBackend eco url)

    -- A refused pass yields no plan, so a target the rule refused never reaches this map.
    clearedFor = \case
        MirrorWriter -> Map.empty
        MirrorPruner ->
            Map.fromList [(eco, backend) | (eco, url) <- targets, Right backend <- [resolveStoreBackend eco url]]

{- | Read a mirror target as the backend that maintains it, or why none in this build can. The
host decides which backend, and only then does that backend read the rest as its coordinates.
-}
resolveStoreBackend :: Ecosystem -> RegistryUrl -> Either Text StoreBackend
resolveStoreBackend eco url = case parseCodeArtifactHost (hostAddress raw) of
    Just (domain, owner, region) ->
        CodeArtifactBackend <$> codeArtifactCoordinates eco raw domain owner region
    Nothing -> Left "its host names no store maintenance backend this build carries"
  where
    raw = registryUrlText url

{- The rest of a CodeArtifact endpoint: the format the ecosystem maps to, and the repository under
it. The path's format segment has to be the mount's own, because a repository's per-format
endpoints are separate stores. -}
codeArtifactCoordinates :: Ecosystem -> Text -> Text -> Text -> Text -> Either Text CodeArtifactStore
codeArtifactCoordinates eco raw domain owner region = do
    format <-
        maybeToRight
            ("CodeArtifact has no package format for the " <> ecosystemName eco <> " ecosystem")
            (codeArtifactFormat eco)
    repository <-
        maybeToRight
            ("its path is not a CodeArtifact repository endpoint, /" <> formatToken format <> "/{repository}/")
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
pathSegments url = filter (not . T.null) (T.splitOn "/" (T.dropWhile (/= '/') afterAuthority))
  where
    afterAuthority = T.drop 2 (snd (T.breakOn "//" url))

{- | How a boot builds one store's maintenance handle. Injected, as the queue builder is, so a spec
drives the pruner's arm of the planning phase without discovering an AWS identity.
-}
type BuildStoreMaintenance = StoreBackend -> IO StoreMaintenance

-- | The live handle for a cleared backend, with its credentials discovered the standard AWS way.
buildStoreMaintenance :: BuildStoreMaintenance
buildStoreMaintenance = \case
    CodeArtifactBackend coordinates -> newCodeArtifactMaintenance coordinates

{- | Build one handle per cleared store, or every refusal the live environment earns. The builds
accumulate, so one launch reports every store whose client cannot be built.
-}
planStoreMaintenance ::
    BuildStoreMaintenance ->
    Map Ecosystem StoreBackend ->
    IO (Either [BootError] (Map Ecosystem StoreMaintenance))
planStoreMaintenance build backends =
    validationToEither . traverse eitherToValidation <$> Map.traverseWithKey planOne backends
  where
    planOne eco backend =
        refuseOnThrow (StoreMaintenanceUnavailable eco . ("building its client failed: " <>)) (build backend)
