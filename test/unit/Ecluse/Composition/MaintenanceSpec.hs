-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.MaintenanceSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Composition.BootError (BootError (StoreMaintenanceUnavailable), renderBootError)
import Ecluse.Composition.Maintenance (
    StoreBackend (CodeArtifactBackend),
    resolveStoreBackend,
    vetStoreBackends,
 )
import Ecluse.Composition.Support (
    codeArtifactEnvVars,
    expectConfig,
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

spec :: Spec
spec = do
    resolutionSpec
    passSpec

resolutionSpec :: Spec
resolutionSpec = describe "resolveStoreBackend" $ do
    it "reads the domain, its owner, the region, and the repository from the endpoint" $ do
        let backend = backendAt Npm npmEndpoint
        fmap casDomain backend `shouldBe` Right "acme"
        fmap casDomainOwner backend `shouldBe` Right "111122223333"
        fmap casRegion backend `shouldBe` Right "eu-west-1"
        fmap casRepository backend `shouldBe` Right "mirror"
        fmap (formatToken . casFormat) backend `shouldBe` Right "npm"

    it "keeps a domain that carries its own hyphens" $
        fmap casDomain (backendAt Npm hyphenatedEndpoint) `shouldBe` Right "acme-eu-team"

    it "reads a pypi mount from the pypi endpoint of the same repository" $ do
        let backend = backendAt PyPI pypiEndpoint
        fmap casRepository backend `shouldBe` Right "mirror"
        fmap (formatToken . casFormat) backend `shouldBe` Right "pypi"

    it "refuses a host no backend in this build recognises, whatever the ecosystem" $
        backendAt Npm "https://verdaccio.example.test/npm/mirror/"
            `shouldSatisfy` refusedBecause "names no store maintenance backend"

    it "refuses an unrecognised host before it judges the ecosystem's format" $ do
        -- A RubyGems mirror on Verdaccio is refused for its host, not for a CodeArtifact
        -- format it was never going to need.
        let rubygems = backendAt RubyGems "https://verdaccio.example.test/gems/mirror/"
        rubygems `shouldSatisfy` refusedBecause "names no store maintenance backend"
        rubygems `shouldNotSatisfy` refusedBecause "no package format"

    it "refuses a CodeArtifact endpoint for an ecosystem CodeArtifact has no format for" $
        backendAt RubyGems npmEndpoint
            `shouldSatisfy` refusedBecause "no package format for the rubygems ecosystem"

    it "refuses a mount pointed at another format's endpoint of the same repository" $
        backendAt Npm pypiEndpoint `shouldSatisfy` refusedBecause "not a CodeArtifact repository endpoint"

    it "refuses an endpoint that names no repository" $
        backendAt Npm "https://acme-111122223333.d.codeartifact.eu-west-1.amazonaws.com/npm/"
            `shouldSatisfy` refusedBecause "not a CodeArtifact repository endpoint"
  where
    refusedBecause needle = either (T.isInfixOf needle) (const False)

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
            ([], Left [err@(StoreMaintenanceUnavailable Npm reason)]) -> do
                reason `shouldBe` "its host names no store maintenance backend this build carries"
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
  where
    repositoryOf (CodeArtifactBackend coordinates) = casRepository coordinates

-- The active mounts an environment layer resolves to: the input the rule reads.
mountsFor :: [(String, String)] -> IO (Map Ecosystem MountConfig)
mountsFor env = cfgMounts . configApp <$> expectConfig env Nothing

-- Read one URL as a backend under the named ecosystem, reduced to its CodeArtifact coordinates.
backendAt :: Ecosystem -> Text -> Either Text CodeArtifactStore
backendAt eco url = do
    registry <- mkRegistryUrl url
    coordinatesOf <$> resolveStoreBackend eco registry
  where
    coordinatesOf (CodeArtifactBackend coordinates) = coordinates

npmEndpoint :: Text
npmEndpoint = "https://acme-111122223333.d.codeartifact.eu-west-1.amazonaws.com/npm/mirror/"

pypiEndpoint :: Text
pypiEndpoint = "https://acme-111122223333.d.codeartifact.eu-west-1.amazonaws.com/pypi/mirror/"

hyphenatedEndpoint :: Text
hyphenatedEndpoint = "https://acme-eu-team-111122223333.d.codeartifact.eu-west-1.amazonaws.com/npm/mirror/"
