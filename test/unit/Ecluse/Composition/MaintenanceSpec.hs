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
    StoreMaintenanceReason (ClientBuildFailed, NoBackendForHost, NoFormatFor, NotRepositoryEndpoint),
    renderBootError,
 )
import Ecluse.Composition.Maintenance (
    ClearedBackend,
    StoreBackend (CodeArtifactBackend),
    clearedBackend,
    planStoreMaintenance,
    resolveStoreBackend,
    vetStoreBackends,
 )
import Ecluse.Composition.Support (
    codeArtifactEnvVars,
    expectConfig,
    overrideEnv,
    staticEnvVars,
    withoutMirrorTargetToken,
    withoutMirrorTargetUrl,
 )
import Ecluse.Composition.Types (RegistryRole (MirrorPruner, MirrorWriter))
import Ecluse.Composition.Vet (runVet)
import Ecluse.Config (AppConfig (cfgMounts), Config (configApp), MountConfig)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Security.Egress (mkRegistryUrl)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (CodeArtifactStore (..), formatToken)
import Ecluse.Test.Maintenance (FakeStore (fakeMaintenance), defaultFakeStoreConfig, newFakeStore)

spec :: Spec
spec = do
    resolutionSpec
    passSpec
    planSpec

resolutionSpec :: Spec
resolutionSpec = describe "resolveStoreBackend" $ do
    it "reads the domain, its owner, the region, and the repository from the endpoint" $ do
        backend <- backendAt Npm npmEndpoint
        fmap casDomain backend `shouldBe` Right "acme"
        fmap casDomainOwner backend `shouldBe` Right "111122223333"
        fmap casRegion backend `shouldBe` Right "eu-west-1"
        fmap casRepository backend `shouldBe` Right "mirror"
        fmap (formatToken . casFormat) backend `shouldBe` Right "npm"

    it "keeps a domain that carries its own hyphens" $ do
        backend <- backendAt Npm hyphenatedEndpoint
        fmap casDomain backend `shouldBe` Right "acme-eu-team"

    it "reads a pypi mount from the pypi endpoint of the same repository" $ do
        backend <- backendAt PyPI pypiEndpoint
        fmap casRepository backend `shouldBe` Right "mirror"
        fmap (formatToken . casFormat) backend `shouldBe` Right "pypi"

    it "refuses a host no backend in this build recognises, whatever the ecosystem" $
        backendAt Npm "https://verdaccio.example.test/npm/mirror/"
            `shouldReturn` Left NoBackendForHost

    it "refuses an unrecognised host before it judges the ecosystem's format" $
        -- A RubyGems mirror on Verdaccio is refused for its host, not for a CodeArtifact
        -- format it was never going to need.
        backendAt RubyGems "https://verdaccio.example.test/gems/mirror/"
            `shouldReturn` Left NoBackendForHost

    it "refuses a CodeArtifact endpoint for an ecosystem CodeArtifact has no format for" $
        backendAt RubyGems npmEndpoint `shouldReturn` Left (NoFormatFor RubyGems)

    it "refuses a mount pointed at another format's endpoint of the same repository" $
        backendAt Npm pypiEndpoint `shouldReturn` Left (NotRepositoryEndpoint "npm")

    it "refuses an endpoint that names no repository" $
        backendAt Npm "https://acme-111122223333.d.codeartifact.eu-west-1.amazonaws.com/npm/"
            `shouldReturn` Left (NotRepositoryEndpoint "npm")

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
                err `shouldBe` StoreMaintenanceUnavailable Npm NoBackendForHost
                renderBootError err `shouldSatisfy` T.isInfixOf "ECLUSE_MOUNTS__NPM__MIRROR_TARGET"
            other -> expectationFailure ("expected the one maintenance refusal, got: " <> show other)

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

-- The active mounts an environment layer resolves to: the input the rule reads.
mountsFor :: [(String, String)] -> IO (Map Ecosystem MountConfig)
mountsFor env = cfgMounts . configApp <$> expectConfig env Nothing

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
repositoryOf cleared = case clearedBackend cleared of
    CodeArtifactBackend coordinates -> casRepository coordinates

-- Read one URL as a backend under the named ecosystem, reduced to its CodeArtifact coordinates.
backendAt :: Ecosystem -> Text -> IO (Either StoreMaintenanceReason CodeArtifactStore)
backendAt eco url = do
    registry <- either (fail . toString) pure (mkRegistryUrl url)
    pure (coordinatesOf <$> resolveStoreBackend eco registry)
  where
    coordinatesOf (CodeArtifactBackend coordinates) = coordinates

npmEndpoint :: (IsString s) => s
npmEndpoint = "https://acme-111122223333.d.codeartifact.eu-west-1.amazonaws.com/npm/mirror/"

pypiEndpoint :: (IsString s) => s
pypiEndpoint = "https://acme-111122223333.d.codeartifact.eu-west-1.amazonaws.com/pypi/mirror/"

pypiInternalEndpoint :: (IsString s) => s
pypiInternalEndpoint = "https://acme-111122223333.d.codeartifact.eu-west-1.amazonaws.com/pypi/internal/"

hyphenatedEndpoint :: (IsString s) => s
hyphenatedEndpoint = "https://acme-eu-team-111122223333.d.codeartifact.eu-west-1.amazonaws.com/npm/mirror/"
