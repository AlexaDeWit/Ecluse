module Ecluse.RegistryProtocolSpec (spec) where

import Data.Aeson (eitherDecodeStrict)
import Data.Map.Strict qualified as Map
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)
import Test.Hspec

import Ecluse.Registry.Npm.Wire (
    AbbreviatedPackument (apkmtDistTags, apkmtName, apkmtVersions),
 )

{- | Smoke tests make __live__ calls to public registries (npm, PyPI) to confirm
our JSON decoding and protocol handling match reality.

They depend on uncontrolled external services, so they are __allowed to fail by
design__ and never gate a merge (the CI @gate@ does not depend on them). A
failure is a prompt to investigate — protocol drift, or just flakiness — not an
automatic blocker.

This case fetches a real __abbreviated__ packument from @registry.npmjs.org@ and
decodes it through "Ecluse.Registry.Npm.Wire". If the lenient decoder ever stops
matching what npm actually sends — a field changed shape, a required key
vanished — this is where it surfaces. The fetch shells out to @curl@ (mirroring
the existing oracle smoke test) and __pends__ rather than fails when @curl@ or
the network is unavailable, so a bare or offline checkout does not see a red
test.
-}
spec :: Spec
spec =
    describe "live registry protocol (npm / PyPI)" $
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
  where
    registryBase = "https://registry.npmjs.org"
    abbreviatedAccept = "application/vnd.npm.install-v1+json"
