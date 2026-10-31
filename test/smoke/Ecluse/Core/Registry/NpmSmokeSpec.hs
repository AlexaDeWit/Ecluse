-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Live npm protocol checks against the shipped read and input bounds.
module Ecluse.Core.Registry.NpmSmokeSpec (spec) where

import Data.Aeson (Value (Object, String), eitherDecodeStrict)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (
    HashAlg (SHA1, SRI),
    PackageInfo (infoDistTags, infoName, infoVersions),
    mkHash,
    mkPackageName,
    renderPackageName,
 )
import Ecluse.Core.Registry (RegistryResponse (responseBody))
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient)
import Ecluse.Core.Registry.Metadata.Fetch (fetchManifest)
import Ecluse.Core.Registry.Npm.Project (projectName)
import Ecluse.Core.Registry.Npm.Request (MetadataForm (Abbreviated))
import Ecluse.Core.Registry.Origin (OriginClient)
import Ecluse.Core.Registry.WireSupport (Projection (NameMismatch, Projected))
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress (mkRegistryUrl)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.RegistrySmoke (liveMetadata)
import Ecluse.Test.Deadline (withExchangeClient)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Registry.Npm (defaultNpmConfig, publicRegistryBaseUrl)
import Ecluse.Test.Registry.Npm.Metadata (fetchMetadataFormBounded, npmRead, projectNpmManifest)
import Ecluse.Test.Registry.Npm.Project (parsePackageInfoFromValue)
import Ecluse.Test.Support (expectRightText)

-- | Check real registry data without treating transport failures as protocol failures.
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
                        renderPackageName (infoName info) `shouldBe` "is-odd"
                        Map.member "latest" (infoDistTags info) `shouldBe` True
                        Map.null (infoVersions info) `shouldBe` False

    it "a bounded fetch of a real package projects to PackageInfo (full data plane)" $ do
        withExchangeClient (newManager tlsManagerSettings) $ \exchanges -> do
            let isOdd = mkPackageName Npm Nothing "is-odd"
            config <- publicRegistryOrigin exchanges
            outcome <- fetchMetadataFormBounded config Abbreviated isOdd
            case outcome of
                Left _ ->
                    pendingWith "npm registry unreachable (offline); smoke test skipped"
                Right response ->
                    case projectNpmManifest defaultLimits isOdd (responseBody response) of
                        Left err ->
                            expectationFailure ("live packument failed to project: " <> show err)
                        Right (info, _raw) -> do
                            renderPackageName (infoName info) `shouldBe` "is-odd"
                            Map.member "latest" (infoDistTags info) `shouldBe` True

    it "validates every real dist.shasum and dist.integrity a long-lived npm packument serves (mkHash accepts real formats)" $ do
        -- Lodash covers both legacy SHA-1 and modern SRI formats. This checks syntax, not admission.
        document <- liveRegistryDocument [] "/lodash"
        case document of
            Nothing -> pendingWith registryUnreadable
            Just value -> do
                let digests = collectDistDigests value
                any ((== SHA1) . fst) digests `shouldBe` True
                any ((== SRI) . fst) digests `shouldBe` True
                [(alg, d) | (alg, d) <- digests, isLeft (mkHash alg d)] `shouldBe` []

    -- Live responses check default admission against growth since the complete corpus captures.
    for_ [("react", "react"), ("types-node", "@types/node"), ("lodash", "lodash"), ("typescript", "typescript")] $ \(identifier, pkg) ->
        it (caseLabel ("live/npm/" <> identifier) ("a real large trusted packument is admissible under the default Limits (" <> pkg <> ")")) $ do
            withExchangeClient (newManager tlsManagerSettings) $ \exchanges -> do
                parsed <- either (fail . show) pure (projectName pkg)
                config <- publicRegistryOrigin exchanges
                void (liveMetadata pkg (fetchManifest npmRead passthroughTracingPort config parsed))

registryBase :: String
registryBase = "https://registry.npmjs.org"

abbreviatedAccept :: String
abbreviatedAccept = "application/vnd.npm.install-v1+json"

registryUnreadable :: String
registryUnreadable = "npm registry unreachable (offline or curl unavailable); smoke test skipped"

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

-- Include raw digests across every version to detect false refusals by mkHash.
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

-- Reject an invalid built-in registry URL instead of reporting an offline test.
publicRegistryOrigin :: ExchangeClient -> IO OriginClient
publicRegistryOrigin client =
    (`defaultNpmConfig` client) <$> expectRightText (mkRegistryUrl publicRegistryBaseUrl)
