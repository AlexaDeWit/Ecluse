-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.MaintenanceSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Composition.BootError (BootError (StoreMaintenanceUnavailable), renderBootError)
import Ecluse.Composition.Endpoints (MirrorStore, vetMirrorStores)
import Ecluse.Composition.Maintenance (StoreBackend (CodeArtifactBackend), resolveStoreBackend)
import Ecluse.Composition.Support (expectConfig, overrideEnv, staticEnvVars)
import Ecluse.Config (AppConfig (cfgMounts), Config (configApp))
import Ecluse.Config.MirrorCredential (parseCodeArtifactHost)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Security (hostAddress)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (CodeArtifactStore (..), formatToken)

{- The store a backend is resolved from is the vetted witness, never a raw URL, so every
case walks the same endpoint vetting the Dredger's boot does. -}
spec :: Spec
spec = describe "resolveStoreBackend" $ do
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

    it "refuses a host no backend in this build recognises, whatever the ecosystem" $ do
        verdaccio <- backendAt Npm "https://verdaccio.example.test/npm/mirror/"
        verdaccio `shouldSatisfy` refusedBecause "names no store maintenance backend"

    it "refuses an unrecognised host before it judges the ecosystem's format" $ do
        -- A RubyGems mirror on Verdaccio is refused for its host, not for a CodeArtifact
        -- format it was never going to need.
        rubygems <- backendAt RubyGems "https://verdaccio.example.test/gems/mirror/"
        rubygems `shouldSatisfy` refusedBecause "names no store maintenance backend"
        rubygems `shouldNotSatisfy` refusedBecause "no package format"

    it "refuses a CodeArtifact endpoint for an ecosystem CodeArtifact has no format for" $ do
        rubygems <- backendAt RubyGems npmEndpoint
        rubygems `shouldSatisfy` refusedBecause "no package format for the rubygems ecosystem"

    it "refuses a mount pointed at another format's endpoint of the same repository" $ do
        crossed <- backendAt Npm pypiEndpoint
        crossed `shouldSatisfy` refusedBecause "not a CodeArtifact repository endpoint"

    it "refuses an endpoint that names no repository" $ do
        bare <- backendAt Npm "https://acme-111122223333.d.codeartifact.eu-west-1.amazonaws.com/npm/"
        bare `shouldSatisfy` refusedBecause "not a CodeArtifact repository endpoint"

    it "names the mount key an operator has to fix" $ do
        rubygems <- backendAt RubyGems npmEndpoint
        case rubygems of
            Left err ->
                renderBootError err `shouldSatisfy` T.isInfixOf "ECLUSE_MOUNTS__RUBYGEMS__MIRROR_TARGET"
            Right _ -> expectationFailure "CodeArtifact has no rubygems format, so this must refuse"
  where
    refusedBecause needle = \case
        Left (StoreMaintenanceUnavailable _ reason) -> needle `T.isInfixOf` reason
        _ -> False

{- Resolve the npm mount's vetted mirror store at the given URL, then read it as a backend
under the named ecosystem, so the witness is the only way in. -}
backendAt :: Ecosystem -> Text -> IO (Either BootError CodeArtifactStore)
backendAt eco url = do
    store <- mirrorStoreAt url
    pure (coordinatesOf (resolveStoreBackend eco store))
  where
    coordinatesOf = fmap (\(CodeArtifactBackend coordinates) -> coordinates)

-- The vetted mirror store of an npm mount pointed at one URL. Config load derives the
-- write credential from that URL, so a CodeArtifact target carries no static token and
-- any other target must.
mirrorStoreAt :: Text -> IO MirrorStore
mirrorStoreAt url = do
    mounts <- cfgMounts . configApp <$> expectConfig (envAt url) Nothing
    case vetMirrorStores mounts >>= (maybeToRight [] . Map.lookup Npm) of
        Left errs -> fail ("no vetted mirror store: " <> show errs)
        Right store -> pure store
  where
    envAt target
        | isJust (parseCodeArtifactHost (hostAddress target)) = filter ((/= tokenKey) . fst) base
        | otherwise = base
      where
        base = overrideEnv "ECLUSE_MOUNTS__NPM__MIRROR_TARGET" (toString target) staticEnvVars

    tokenKey = "ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN"

npmEndpoint :: Text
npmEndpoint = "https://acme-111122223333.d.codeartifact.eu-west-1.amazonaws.com/npm/mirror/"

pypiEndpoint :: Text
pypiEndpoint = "https://acme-111122223333.d.codeartifact.eu-west-1.amazonaws.com/pypi/mirror/"

hyphenatedEndpoint :: Text
hyphenatedEndpoint = "https://acme-eu-team-111122223333.d.codeartifact.eu-west-1.amazonaws.com/npm/mirror/"
