-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Shared configuration fixtures for store vetting and construction tests.
module Ecluse.Composition.Maintenance.Support (withoutPrivateAuthority, refusalsOf, unaddressablePrivateUpstream, vetted, noAdapter, withoutMaintenance, mountsFor, privateCaches, clearedBackendsFor, verdaccioEnv) where

import Ecluse.Composition.BootError (Advisory, BootError)
import Ecluse.Composition.Maintenance.Plan (ClearedBackend, ResolveMaintenanceAdapter, vetPrivateCaches, vetStoreBackends)
import Ecluse.Composition.Support (codeArtifactDomain, expectConfig, overrideEnv, staticEnvVars, withoutMirrorTargetToken, withoutMirrorTargetUrl)
import Ecluse.Composition.Types (RegistryRole)
import Ecluse.Composition.Vet (runVet)
import Ecluse.Config (AppConfig (cfgMounts), Config (configApp, configMounts), DeletionConsent (DeletionWithheld), MountMap, PrivateEndpoint (PrivateEndpoint, preConsent, preTarget, preToken), StoreBackend, StoreTag (TagCodeArtifact), Target (Target))
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Registry.Adapter (RegistryAdapter (adapterMaintenance), adapterFor)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMaintenance (AdapterMaintenance, maintenanceAlphabet, maintenanceListing, maintenanceVersionDelete))
import Ecluse.Core.Registry.Maintenance.NameSpace (noNameAlphabet)
import Ecluse.Test.Package (unsafeRegistryUrl)

-- | A CodeArtifact host whose path names no repository.
unaddressablePrivateUpstream :: PrivateEndpoint
unaddressablePrivateUpstream =
    PrivateEndpoint
        { preTarget = Target TagCodeArtifact (unsafeRegistryUrl (codeArtifactDomain <> "/npm/"))
        , preToken = Nothing
        , preConsent = DeletionWithheld
        }

-- | Run the maintenance pass with the shipped ecosystem adapters.
vetted :: RegistryRole -> MountMap -> ([Advisory], Either [BootError] (Map Ecosystem ClearedBackend))
vetted role mounts = runVet role (vetStoreBackends adapterFor mounts)

-- | An ecosystem this build ships no adapter for at all.
noAdapter :: ResolveMaintenanceAdapter
noAdapter _ = Nothing

-- | This build's adapters with their maintenance slice emptied: an ecosystem that fills neither verb.
withoutMaintenance :: ResolveMaintenanceAdapter
withoutMaintenance eco =
    adapterFor eco <&> \adapter ->
        adapter
            { adapterMaintenance =
                AdapterMaintenance
                    { maintenanceListing = Nothing
                    , maintenanceVersionDelete = Nothing
                    , maintenanceAlphabet = noNameAlphabet
                    }
            }

-- | Load resolved mounts from an environment fixture.
mountsFor :: [(String, String)] -> IO MountMap
mountsFor env = configMounts <$> expectConfig env Nothing

-- | Run private-cache vetting with the supplied adapter resolver.
privateCaches :: ResolveMaintenanceAdapter -> RegistryRole -> Config -> Either [BootError] (Map Ecosystem (Maybe StoreBackend, ClearedBackend))
privateCaches resolveAdapter role config =
    snd (runVet role (vetPrivateCaches resolveAdapter (cfgMounts (configApp config)) (configMounts config)))

-- | Load cleared backends for a role and fail the test on refusal.
clearedBackendsFor :: RegistryRole -> [(String, String)] -> IO (Map Ecosystem ClearedBackend)
clearedBackendsFor role env = do
    mounts <- mountsFor env
    either (\errs -> fail ("the backend rule refused: " <> show errs)) pure (snd (vetted role mounts))

-- | 'staticEnvVars' mirroring to a Verdaccio store, under the written deletion consent.
verdaccioEnv :: String -> [(String, String)]
verdaccioEnv permitDeletion =
    overrideEnv "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__PERMIT_DELETION" permitDeletion $
        overrideEnv "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__URL" "https://verdaccio.example.test/" $
            overrideEnv "ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__TOKEN" "write-token" $
                withoutMirrorTargetToken (withoutMirrorTargetUrl staticEnvVars)

-- | Remove private Verdaccio deletion authority while retaining the endpoint.
withoutPrivateAuthority :: [(String, String)] -> [(String, String)]
withoutPrivateAuthority = filter (\(key, _) -> key /= "ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__VERDACCIO__TOKEN" && key /= "ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__VERDACCIO__PERMIT_DELETION")

-- | Read typed refusals without rendering their diagnostic text.
refusalsOf :: ([Advisory], Either [BootError] a) -> Maybe [BootError]
refusalsOf = leftToMaybe . snd
