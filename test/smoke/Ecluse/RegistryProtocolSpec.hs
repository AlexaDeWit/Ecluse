module Ecluse.RegistryProtocolSpec (spec) where

import Control.Exception (try)
import Data.Aeson (eitherDecodeStrict)
import Data.Map.Strict qualified as Map
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)
import Test.Hspec

import Ecluse.Ecosystem (Ecosystem (Npm))
import Ecluse.Package (PackageInfo (infoDistTags, infoName), mkPackageName, renderPackageName)
import Ecluse.Registry (RegistryClient (fetchMetadata, parsePackageInfo))
import Ecluse.Registry.Npm (defaultNpmConfig, newNpmClient)
import Ecluse.Registry.Npm.Wire (
    AbbreviatedPackument (apkmtDistTags, apkmtName, apkmtVersions),
 )

{- | Smoke tests make __live__ calls to public registries (npm, PyPI) to confirm
our JSON decoding and protocol handling match reality.

They depend on uncontrolled external services, so they are __allowed to fail by
design__ and never gate a merge (the CI @gate@ does not depend on them). A
failure is a prompt to investigate — protocol drift, or just flakiness — not an
automatic blocker.

Two cases run against the public @registry.npmjs.org@. The first fetches a real
__abbreviated__ packument and decodes it through "Ecluse.Registry.Npm.Wire"
(shelling out to @curl@), pinning the lenient decoder to reality. The second
drives the full data plane — "Ecluse.Registry.Npm"'s 'newNpmClient' and
'fetchMetadata' over real @http-client@ — and projects the response to the domain
'PackageInfo', so a protocol or projection drift surfaces end-to-end. Both
__pend__ rather than fail when the network (or @curl@) is unavailable, so a bare
or offline checkout does not see a red test.
-}
spec :: Spec
spec = describe "live registry protocol (npm / PyPI)" $ do
    it "decodes a real abbreviated packument from the public npm registry" $ do
        (code, out, _err) <-
            readProcessWithExitCode
                "curl"
                [ "-sf"
                , "-H"
                , "Accept: " <> abbreviatedAccept
                , registryBase <> "/is-odd"
                ]
                ""
        case code of
            ExitFailure _ ->
                pendingWith
                    "npm registry unreachable (offline or curl unavailable); smoke test skipped"
            ExitSuccess ->
                case eitherDecodeStrict (encodeUtf8 out) of
                    Left err ->
                        expectationFailure ("abbreviated packument failed to decode: " <> err)
                    Right pk -> do
                        -- The model still matches reality: the four top-level
                        -- fields decode, and dist-tags always carries `latest`.
                        apkmtName pk `shouldBe` "is-odd"
                        Map.member "latest" (apkmtDistTags pk) `shouldBe` True
                        Map.null (apkmtVersions pk) `shouldBe` False

    it "fetchMetadata of a real package projects to PackageInfo (full data plane)" $ do
        manager <- newManager tlsManagerSettings
        client <- newNpmClient (defaultNpmConfig manager)
        let isOdd = mkPackageName Npm Nothing "is-odd"
        outcome <- try (fetchMetadata client isOdd)
        case outcome of
            Left (_ :: SomeException) ->
                pendingWith "npm registry unreachable (offline); smoke test skipped"
            Right response ->
                case parsePackageInfo client response of
                    Left err ->
                        expectationFailure ("live packument failed to project: " <> show err)
                    Right info -> do
                        -- The data plane reached npm and the projection round-trips:
                        -- the package name comes back as published, and `latest` is
                        -- always a dist-tag.
                        renderPackageName (infoName info) `shouldBe` "is-odd"
                        Map.member "latest" (infoDistTags info) `shouldBe` True
  where
    registryBase = "https://registry.npmjs.org"
    abbreviatedAccept = "application/vnd.npm.install-v1+json"
