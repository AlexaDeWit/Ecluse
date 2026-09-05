-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Registry.NpmSmokeSpec (spec) where

import Control.Exception (try)
import Data.Aeson (Value (Object, String), eitherDecodeStrict)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)
import Test.Hspec
import UnliftIO.Exception (throwString)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    HashAlg (SHA1, SRI),
    PackageInfo (infoDistTags, infoName, infoVersions),
    PackageName,
    mkHash,
    mkPackageName,
    renderPackageName,
 )
import Ecluse.Core.Registry (RegistryResponse (responseBody))
import Ecluse.Core.Registry.Npm (fetchMetadataFormBounded)
import Ecluse.Core.Registry.Npm.Metadata (projectNpmManifest)
import Ecluse.Core.Registry.Npm.Project (Projection (NameMismatch, Projected), parsePackageInfoFromValue, projectName)
import Ecluse.Core.Registry.Npm.Request (MetadataForm (Abbreviated, Full))
import Ecluse.Core.Registry.Origin (OriginClient)
import Ecluse.Core.Registry.Request (noValidators)
import Ecluse.Core.Security (Limits (maxVersionCount), checkNestingDepth, checkVersionCount, defaultLimits)
import Ecluse.Core.Security.Egress (mkRegistryUrl)
import Ecluse.Test.Registry.Npm (defaultNpmConfig, publicRegistryBaseUrl)

{- | Smoke tests make __live__ calls to public registries (npm, PyPI) to confirm our JSON decoding
and protocol handling match reality. They depend on uncontrolled external services, so they never
gate a merge, and each case __pends__ rather than fails when the network is unavailable.
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
                case eitherDecodeStrict (encodeUtf8 out) :: Either String Value of
                    Left err ->
                        expectationFailure ("abbreviated packument failed to decode: " <> err)
                    Right value ->
                        case parsePackageInfoFromValue (mkPackageName Npm Nothing "is-odd") value of
                            Left err ->
                                expectationFailure ("abbreviated packument failed to project: " <> show err)
                            Right (NameMismatch reported) ->
                                expectationFailure ("abbreviated packument self-reported a different name: " <> toString reported)
                            Right (Projected info) -> do
                                -- The live decoder still matches reality: the packument
                                -- projects, and dist-tags always carries `latest`.
                                renderPackageName (infoName info) `shouldBe` "is-odd"
                                Map.member "latest" (infoDistTags info) `shouldBe` True
                                Map.null (infoVersions info) `shouldBe` False

    it "a bounded fetch of a real package projects to PackageInfo (full data plane)" $ do
        manager <- newManager tlsManagerSettings
        let isOdd = mkPackageName Npm Nothing "is-odd"
        config <- publicRegistryOrigin manager
        outcome <- fetchMetadataFormBounded config Abbreviated noValidators isOdd
        case outcome of
            Left _ ->
                -- The typed channel reports the unreachable-registry case as a value.
                pendingWith "npm registry unreachable (offline); smoke test skipped"
            Right response ->
                case projectNpmManifest defaultLimits isOdd (responseBody response) of
                    Left err ->
                        expectationFailure ("live packument failed to project: " <> show err)
                    Right (info, _raw) -> do
                        -- The live projection round-trips: the name comes back as published, and
                        -- `latest` is always a dist-tag.
                        renderPackageName (infoName info) `shouldBe` "is-odd"
                        Map.member "latest" (infoDistTags info) `shouldBe` True

    it "validates every real dist.shasum and dist.integrity a long-lived npm packument serves (mkHash accepts real formats)" $ do
        -- A fail-closed validator must not false-reject a digest npm actually serves: that
        -- would silently drop a legitimate version to "no integrity". The lodash packument spans
        -- the legacy `dist.shasum` (SHA-1) and modern `dist.integrity` (SRI) eras. This checks
        -- well-formedness, not the public floor.
        (code, out, _err) <-
            readProcessWithExitCode "curl" ["-sf", registryBase <> "/lodash"] ""
        case code of
            ExitFailure _ ->
                pendingWith "npm registry unreachable (offline or curl unavailable); smoke test skipped"
            ExitSuccess ->
                case eitherDecodeStrict (encodeUtf8 out) :: Either String Value of
                    Left err -> expectationFailure ("lodash packument failed to decode: " <> err)
                    Right value -> do
                        let digests = collectDistDigests value
                        -- Non-vacuous: the packument carried both digest kinds, so the
                        -- assertion spans both the legacy and modern eras.
                        any ((== SHA1) . fst) digests `shouldBe` True
                        any ((== SRI) . fst) digests `shouldBe` True
                        -- Every real digest validates through the same mkHash the projection
                        -- uses. A Left here is our validator false-rejecting a real format.
                        [(alg, d) | (alg, d) <- digests, isLeft (mkHash alg d)] `shouldBe` []

    -- The default Limits must not false-positive on real data: each large, widely-trusted
    -- package's full packument stays admissible under the defaults (security.md invariant 4).
    -- The react packument is too big to commit, so only this live case covers it.
    for_ ["react", "@types/node", "lodash"] $ \pkg ->
        it ("a real large trusted packument is admissible under the default Limits (" <> toString pkg <> ")") $ do
            manager <- newManager tlsManagerSettings
            -- The live splitter, not a harness copy: a pin the front door would refuse fails here.
            parsed <- either (fail . show) pure (projectName pkg)
            outcome <- try (admissibleUnderDefaults manager parsed)
            case outcome of
                Left (_ :: SomeException) ->
                    pendingWith "npm registry unreachable (offline); smoke test skipped"
                Right (name, versionCount) -> do
                    name `shouldBe` pkg
                    versionCount `shouldSatisfy` (> 0)
                    versionCount `shouldSatisfy` (<= maxVersionCount defaultLimits)
  where
    registryBase = "https://registry.npmjs.org"
    abbreviatedAccept = "application/vnd.npm.install-v1+json"

{- | Run the bounded fetch, decode, nesting, projection, and version-count sequence the serve path
applies, over a live full packument under the default 'Limits'. It throws when any bound refuses
the document, so an accidentally too-tight default surfaces as a failure, not a silent pass.
-}
admissibleUnderDefaults :: Manager -> PackageName -> IO (Text, Int)
admissibleUnderDefaults manager name = do
    config <- publicRegistryOrigin manager
    -- 1. Body bound: fetchMetadataFormBounded reads through boundedRead against ocLimits,
    -- reporting any fetch fault (a bound breach included) as a value this smoke helper renders.
    response <-
        fetchMetadataFormBounded config Full noValidators name
            >>= either (\fault -> throwString ("bounded fetch refused: " <> show fault)) pure
    -- 2. Decode, then 3. nesting bound, 4. projection, 5. version-count bound: the same
    -- chain the serve-path projection runs. Any refusal throws and fails the smoke case.
    value <- either (\e -> throwString ("decode failed: " <> e)) pure (eitherDecodeStrict (responseBody response))
    bounded <- either (\e -> throwString ("nesting bound refused a real package: " <> show e)) pure (checkNestingDepth defaultLimits value)
    info <- case parsePackageInfoFromValue name bounded of
        Left e -> throwString ("projection failed: " <> show e)
        Right (Projected i) -> pure i
        Right (NameMismatch reported) -> throwString ("projection self-reported a different name: " <> toString reported)
    admitted <- either (\e -> throwString ("version bound refused a real package: " <> show e)) pure (checkVersionCount defaultLimits info)
    pure (renderPackageName (infoName admitted), Map.size (infoVersions admitted))

{- | Every @dist.shasum@ (as a 'SHA1' digest) and @dist.integrity@ (as an 'SRI') a packument
carries, across all of its versions. These are the raw wire digests the projection feeds to
'mkHash'.
-}
collectDistDigests :: Value -> [(HashAlg, Text)]
collectDistDigests value =
    [ pair
    | Object top <- [value]
    , Just (Object versions) <- [KeyMap.lookup "versions" top]
    , Object versionObj <- KeyMap.elems versions
    , Just (Object dist) <- [KeyMap.lookup "dist" versionObj]
    , pair <-
        [(SHA1, s) | Just (String s) <- [KeyMap.lookup "shasum" dist]]
            <> [(SRI, i) | Just (String i) <- [KeyMap.lookup "integrity" dist]]
    ]

-- The live public registry as an origin at the secure-default bounds. Its URL is https, so
-- the production former builds the witness and a refusal here is a broken constant.
publicRegistryOrigin :: Manager -> IO OriginClient
publicRegistryOrigin manager =
    either (throwString . toString) (pure . (`defaultNpmConfig` manager)) (mkRegistryUrl publicRegistryBaseUrl)
