module Ecluse.RegistryProtocolSpec (spec) where

import Control.Exception (try)
import Data.Aeson (eitherDecodeStrict)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)
import Test.Hspec
import UnliftIO.Exception (throwString)

import Ecluse.Ecosystem (Ecosystem (Npm))
import Ecluse.Package (PackageInfo (infoDistTags, infoName, infoVersions), PackageName, Scope, mkPackageName, mkScope, renderPackageName)
import Ecluse.Registry (RegistryClient (fetchMetadata, parsePackageInfo), RegistryResponse (responseBody))
import Ecluse.Registry.Npm (
    MetadataForm (Full),
    NpmClientConfig (npmLimits, npmManager),
    defaultNpmConfig,
    fetchMetadataForm,
    newNpmClient,
    noValidators,
 )
import Ecluse.Registry.Npm.Project (parsePackageInfoFromValue)
import Ecluse.Registry.Npm.Wire (
    AbbreviatedPackument (apkmtDistTags, apkmtName, apkmtVersions),
 )
import Ecluse.Security (Limits (maxVersionCount), checkNestingDepth, checkVersionCount, defaultLimits)

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

    -- The default Limits must not false-positive on CURRENT real data: each large,
    -- widely-trusted package's full packument is admissible under the defaults
    -- (security.md invariant 4). This validates the committed-fixture proof
    -- ("Ecluse.SecuritySpec", express) against live registry data — react in
    -- particular is multi-megabyte / thousands of versions, too big to commit but the
    -- architect's headline case for "must never be refused". Non-gating: it pends on
    -- a network failure rather than reddening the gate.
    for_ ["react", "@types/node", "lodash"] $ \pkg ->
        it ("a real large trusted packument is admissible under the default Limits (" <> toString pkg <> ")") $ do
            manager <- newManager tlsManagerSettings
            outcome <- try (admissibleUnderDefaults manager (mkPackageName Npm (scopeOf pkg) (bareOf pkg)))
            case outcome of
                Left (_ :: SomeException) ->
                    pendingWith "npm registry unreachable (offline); smoke test skipped"
                Right (name, versionCount) -> do
                    -- It fetched within the body bound, decoded within the nesting
                    -- bound, projected, and cleared the version-count bound — i.e. the
                    -- whole data-plane sequence admitted a real large package.
                    name `shouldBe` pkg
                    versionCount `shouldSatisfy` (> 0)
                    versionCount `shouldSatisfy` (<= maxVersionCount defaultLimits)
  where
    registryBase = "https://registry.npmjs.org"
    abbreviatedAccept = "application/vnd.npm.install-v1+json"

    -- The bare (unscoped) name of a possibly-scoped identifier, for 'mkPackageName'.
    bareOf :: Text -> Text
    bareOf p = maybe p (T.drop 1 . snd . T.breakOn "/") (T.stripPrefix "@" p >> Just p)

    scopeOf :: Text -> Maybe Scope
    scopeOf p = case T.stripPrefix "@" p of
        Just rest | (sc, rest') <- T.breakOn "/" rest, not (T.null rest') -> Just (mkScope sc)
        _ -> Nothing

{- | Run the exact response-bound sequence the data plane applies in
@Ecluse.Server.Pipeline.fetchEntry@ over a live full packument under the default
'Limits', returning the projected @(name, versionCount)@ on success. Throws (the
bounded read's 'Ecluse.Registry.Npm.ResponseBoundExceeded', a decode error, or a
projection error) if any bound or step refuses the document — so a default that was
accidentally too tight surfaces as a failure, not a silent pass.
-}
admissibleUnderDefaults :: Manager -> PackageName -> IO (Text, Int)
admissibleUnderDefaults manager name = do
    let config = (defaultNpmConfig manager){npmManager = manager, npmLimits = defaultLimits}
    -- 1. Body bound: fetchMetadataForm reads through boundedRead against npmLimits.
    response <- fetchMetadataForm config Full noValidators name
    -- 2. Decode, then 3. nesting bound, 4. projection, 5. version-count bound — the
    -- same chain fetchEntry runs; any refusal throws and fails the smoke case.
    value <- either (\e -> throwString ("decode failed: " <> e)) pure (eitherDecodeStrict (responseBody response))
    bounded <- either (\e -> throwString ("nesting bound refused a real package: " <> show e)) pure (checkNestingDepth defaultLimits value)
    info <- either (\e -> throwString ("projection failed: " <> show e)) pure (parsePackageInfoFromValue bounded)
    admitted <- either (\e -> throwString ("version bound refused a real package: " <> show e)) pure (checkVersionCount defaultLimits info)
    pure (renderPackageName (infoName admitted), Map.size (infoVersions admitted))
