-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Registry.NpmSmokeSpec (spec) where

import Data.Aeson (Value (Object, String), eitherDecodeStrict)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    HashAlg (SHA1, SRI),
    PackageInfo (infoDistTags, infoName, infoVersions),
    PackageName,
    mkHash,
    mkPackageName,
    renderPackageName,
 )
import Ecluse.Core.Registry (FetchFault (FetchTransport), RegistryResponse (responseBody))
import Ecluse.Core.Registry.Metadata (Manifest (manifestInfo), MetadataError (MetadataFetch))
import Ecluse.Core.Registry.Npm.Metadata (fetchNpmManifest)
import Ecluse.Core.Registry.Npm.Project (projectName)
import Ecluse.Core.Registry.Npm.Request (MetadataForm (Abbreviated))
import Ecluse.Core.Registry.Origin (OriginClient)
import Ecluse.Core.Registry.WireSupport (Projection (NameMismatch, Projected))
import Ecluse.Core.Security (Limits (maxVersionCount), defaultLimits)
import Ecluse.Core.Security.Egress (mkRegistryUrl)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Registry.Npm (defaultNpmConfig, publicRegistryBaseUrl)
import Ecluse.Test.Registry.Npm.Metadata (fetchMetadataFormBounded, projectNpmManifest)
import Ecluse.Test.Registry.Npm.Project (parsePackageInfoFromValue)
import Ecluse.Test.Support (expectRightText)

{- | Smoke tier: __live__ calls to the public npm registry, confirming that our decoding, our
projection, and the default 'Limits' still match what the registry serves.
-}
spec :: Spec
spec = describe "live npm registry protocol" $ do
    it "decodes a real abbreviated packument from the public npm registry" $ do
        document <- liveRegistryDocument ["-H", "Accept: " <> abbreviatedAccept] "/is-odd"
        case document of
            Nothing -> pendingWith registryUnreadable
            Just value ->
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
        outcome <- fetchMetadataFormBounded config Abbreviated isOdd
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
        document <- liveRegistryDocument [] "/lodash"
        case document of
            Nothing -> pendingWith registryUnreadable
            Just value -> do
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
            outcome <- admissibleUnderDefaults manager parsed
            case outcome of
                Unreachable fault ->
                    pendingWith ("npm registry unreachable (" <> toString fault <> "); smoke test skipped")
                Refused why ->
                    expectationFailure ("the default Limits refused a real trusted packument: " <> toString why)
                Admitted name versionCount -> do
                    name `shouldBe` pkg
                    versionCount `shouldSatisfy` (> 0)
                    versionCount `shouldSatisfy` (<= maxVersionCount defaultLimits)

registryBase :: String
registryBase = "https://registry.npmjs.org"

abbreviatedAccept :: String
abbreviatedAccept = "application/vnd.npm.install-v1+json"

-- Both curl reads pend for the same two causes, so they report them the same way.
registryUnreadable :: String
registryUnreadable = "npm registry unreachable (offline or curl unavailable); smoke test skipped"

{- | A live registry document under 'registryBase'. 'Nothing' means curl or the registry was
unavailable, while a document that arrives and does not decode fails the case.
-}
liveRegistryDocument :: [String] -> String -> IO (Maybe Value)
liveRegistryDocument extraArgs path = do
    (code, out, _err) <- readProcessWithExitCode "curl" (["-sf"] <> extraArgs <> [registryBase <> path]) ""
    case code of
        ExitFailure _ -> pure Nothing
        ExitSuccess ->
            either
                (\err -> fail (path <> " failed to decode: " <> err))
                (pure . Just)
                (eitherDecodeStrict (encodeUtf8 out))

{- | What the serve path's admissibility chain answered for a live packument. Only a transport
fault is the registry's absence, so only that arm may pend a case.
-}
data Admissibility
    = -- | The exchange never produced a document, so the case has nothing to judge.
      Unreachable Text
    | -- | A bound or the projection turned a real trusted package away.
      Refused Text
    | -- | The packument's own name, and the version count the bounds admitted.
      Admitted Text Int
    deriving stock (Eq, Show)

-- | Run the shipping streamed read against a live packument under the configured default limits.
admissibleUnderDefaults :: Manager -> PackageName -> IO Admissibility
admissibleUnderDefaults manager name = do
    config <- publicRegistryOrigin manager
    fetched <- fetchNpmManifest passthroughTracingPort config name
    pure $ case fetched of
        Left (MetadataFetch (FetchTransport fault)) -> Unreachable (show fault)
        Left fault -> Refused ("the streamed fetch refused a real package: " <> show fault)
        Right manifest ->
            let info = manifestInfo manifest
             in Admitted (renderPackageName (infoName info)) (Map.size (infoVersions info))

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
    (`defaultNpmConfig` manager) <$> expectRightText (mkRegistryUrl publicRegistryBaseUrl)
