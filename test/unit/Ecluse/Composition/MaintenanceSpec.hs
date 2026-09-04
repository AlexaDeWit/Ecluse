-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.MaintenanceSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec
import UnliftIO.Exception (throwIO)

import Ecluse.Composition.BootError (
    BootError (StoreMaintenanceUnavailable),
    StoreMaintenanceReason (ClientBuildFailed, CodeArtifactUnaddressable),
    renderBootError,
 )
import Ecluse.Composition.Maintenance (
    ClearedBackend,
    clearedBackend,
    planStoreMaintenance,
    vetStoreBackends,
 )
import Ecluse.Composition.Support (
    codeArtifactEnvVars,
    expectConfig,
    noMaintenanceBackend,
    overrideEnv,
    staticEnvVars,
    withoutMirrorTargetToken,
    withoutMirrorTargetUrl,
 )
import Ecluse.Composition.Types (RegistryRole (MirrorPruner, MirrorWriter))
import Ecluse.Composition.Vet (runVet)
import Ecluse.Config (CodeArtifactAbsence (NotRepositoryEndpoint), Config (configMounts), MountMap)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (casRepository)
import Ecluse.Test.Maintenance (FakeStore (fakeMaintenance), defaultFakeStoreConfig, newFakeStore)

spec :: Spec
spec = do
    passSpec
    planSpec

{- The rule as the boot applies it: over the loaded mounts, under each role. The deleting role
is the one that refuses, and the checker's warning for it is what a writing role leaves behind. -}
passSpec :: Spec
passSpec = describe "vetStoreBackends" $ do
    it "clears the deleting role the backend for each mirror target this build can sweep" $ do
        mounts <- mountsFor codeArtifactEnvVars
        let (advisories, outcome) = runVet MirrorPruner (vetStoreBackends mounts)
        advisories `shouldBe` []
        fmap (map repositoryOf . Map.elems) outcome `shouldBe` Right ["mirror"]
        fmap Map.keys outcome `shouldBe` Right [Npm]

    it "refuses the deleting role a mirror target this build cannot sweep, naming the key" $ do
        mounts <- mountsFor staticEnvVars
        case runVet MirrorPruner (vetStoreBackends mounts) of
            ([], Left [err]) -> do
                err `shouldBe` noMaintenanceBackend
                renderBootError err `shouldSatisfy` T.isInfixOf "ECLUSE_MOUNTS__NPM__MIRROR_TARGET"
            other -> expectationFailure ("expected the one maintenance refusal, got: " <> show other)

    it "refuses the deleting role alone a CodeArtifact target whose path addresses no repository" $ do
        -- The path check is the Dredger's, not the load's, so the writing role boots on the same
        -- configuration the deleting role refuses.
        mounts <- mountsFor (overrideEnv "ECLUSE_MOUNTS__NPM__MIRROR_TARGET" repositorylessEndpoint codeArtifactEnvVars)
        runVet MirrorPruner (vetStoreBackends mounts)
            `shouldBe` ([], Left [StoreMaintenanceUnavailable Npm (CodeArtifactUnaddressable (NotRepositoryEndpoint "npm"))])
        runVet MirrorWriter (vetStoreBackends mounts) `shouldBe` ([], Right Map.empty)

    it "clears a writing role no backend, and neither refuses nor advises on a target it cannot sweep" $ do
        -- Only the Dredger deletes, so only its pass reads the rule. The checker still names
        -- the Dredger's refusal for this configuration, so an operator learns of it once.
        mounts <- mountsFor staticEnvVars
        runVet MirrorWriter (vetStoreBackends mounts) `shouldBe` ([], Right Map.empty)

    it "clears nothing and refuses nothing for a mount that declares no mirror target" $ do
        mounts <- mountsFor (withoutMirrorTargetUrl (withoutMirrorTargetToken staticEnvVars))
        runVet MirrorPruner (vetStoreBackends mounts) `shouldBe` ([], Right Map.empty)

{- The environment tier over the cleared backends. It builds one handle per store, and its
refusals accumulate rather than stopping at the first store whose client cannot be built. -}
planSpec :: Spec
planSpec = describe "planStoreMaintenance" $ do
    it "builds one handle per cleared store, keyed by the mount that declares it" $ do
        backends <- clearedBackendsFor twoStoreEnv
        outcome <- planStoreMaintenance (const (fakeMaintenance <$> newFakeStore defaultFakeStoreConfig)) backends
        fmap Map.keys outcome `shouldBe` Right (Map.keys backends)

    it "reports a refusal for every store whose client the environment cannot build" $ do
        backends <- clearedBackendsFor twoStoreEnv
        Map.keys backends `shouldBe` [Npm, PyPI]
        outcome <- planStoreMaintenance (const (throwIO NoStoreClient)) backends
        case outcome of
            Right _ -> expectationFailure "expected both store builds to refuse"
            Left errs ->
                map withoutBacktrace errs
                    `shouldBe` [ StoreMaintenanceUnavailable Npm (ClientBuildFailed "NoStoreClient")
                               , StoreMaintenanceUnavailable PyPI (ClientBuildFailed "NoStoreClient")
                               ]
  where
    -- 'displayException' appends GHC's backtrace, so the assertion reads the reason's own line.
    withoutBacktrace = \case
        StoreMaintenanceUnavailable eco (ClientBuildFailed detail) ->
            StoreMaintenanceUnavailable eco (ClientBuildFailed (T.takeWhile (/= '\n') detail))
        err -> err

-- | The typed stand-in for amazonka's credential-discovery failure.
data NoStoreClient = NoStoreClient
    deriving stock (Show)

instance Exception NoStoreClient

-- The resolved mounts an environment layer loads to: the input the rule reads.
mountsFor :: [(String, String)] -> IO MountMap
mountsFor env = configMounts <$> expectConfig env Nothing

-- The deleting role's cleared backends for an environment layer, failing the test on a refusal.
clearedBackendsFor :: [(String, String)] -> IO (Map Ecosystem ClearedBackend)
clearedBackendsFor env = do
    mounts <- mountsFor env
    either (\errs -> fail ("the backend rule refused: " <> show errs)) pure $
        snd (runVet MirrorPruner (vetStoreBackends mounts))

-- | 'codeArtifactEnvVars' with a second mirrored mount, so the plan has two stores to build.
twoStoreEnv :: [(String, String)]
twoStoreEnv =
    overrideEnv "ECLUSE_MOUNTS__PYPI__ENABLED" "true" $
        overrideEnv "ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM" pypiInternalEndpoint $
            overrideEnv "ECLUSE_MOUNTS__PYPI__MIRROR_TARGET" pypiEndpoint codeArtifactEnvVars

-- The repository a cleared backend addresses, read back through the pass's own witness.
repositoryOf :: ClearedBackend -> Text
repositoryOf = casRepository . clearedBackend

pypiEndpoint :: (IsString s) => s
pypiEndpoint = "https://acme-111122223333.d.codeartifact.eu-west-1.amazonaws.com/pypi/mirror/"

pypiInternalEndpoint :: (IsString s) => s
pypiInternalEndpoint = "https://acme-111122223333.d.codeartifact.eu-west-1.amazonaws.com/pypi/internal/"

-- A CodeArtifact endpoint that stops at the format segment, so its path names no repository.
repositorylessEndpoint :: (IsString s) => s
repositorylessEndpoint = "https://acme-111122223333.d.codeartifact.eu-west-1.amazonaws.com/npm/"
