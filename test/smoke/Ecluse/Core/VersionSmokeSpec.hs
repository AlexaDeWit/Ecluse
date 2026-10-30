-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Live registry ordering checks share the pinned native tools with the offline Cargo unit gate.
module Ecluse.Core.VersionSmokeSpec (spec) where

import Data.Text qualified as T
import Hedgehog (Gen)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory (getTemporaryDirectory)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Ecosystem (Ecosystem (..), ecosystemName)
import Ecluse.Core.Version
import Ecluse.Test.RegistryCapture (fetchVersions, loadCatalogue, smokeRegistryPackages)
import Ecluse.Test.Version qualified as V
import Ecluse.Test.Version.Oracle (oracleAvailable, oracleCompare, oracleSort)

-- | Network failures remain optional smoke observations. The pinned Cargo oracle must be present.
spec :: Spec
spec = do
    catalogue <- runIO loadCatalogue
    describe "version-ordering fixtures vs the live reference oracles" $
        it "regenerating from node-semver / packaging / Gem::Version reproduces the committed fixture" $ do
            tmpDir <- getTemporaryDirectory
            let regenerated = tmpDir <> "/ecluse-version-fixtures-smoke.txt"
            (code, _out, _err) <-
                readProcessWithExitCode "bash" [generatorScript, regenerated] ""
            case code of
                ExitFailure _ ->
                    pendingWith
                        "reference oracles unavailable; run via `nix develop` (node-semver / packaging / ruby)"
                ExitSuccess -> do
                    fresh <- readFileBS regenerated
                    baked <- readFileBS committedFixture
                    fresh `shouldBe` baked

    -- Modest iteration count: this tier shells out to a tool per comparison.
    describe "compareVersions agrees with the live oracle on random inputs" $
        modifyMaxSuccess (const 60) $
            for_ [(Npm, npmish), (PyPI, pypiish), (Cargo, cargoish), (RubyGems, gemish)] $ \(eco, gen) -> do
                let title = show eco <> " -- generative differential (both-accept only)"
                withLiveOracle eco title $
                    it title $
                        hedgehog $ do
                            raw1 <- H.forAll gen
                            raw2 <- H.forAll gen
                            let ours = compareVersions (mkVersion eco raw1) (mkVersion eco raw2)
                            theirs <- H.evalIO (oracleCompare eco raw1 raw2)
                            -- Assert only when both sides accepted the inputs.
                            -- Skip one-sided "what parses" disagreement.
                            case (ours, theirs) of
                                (Just o, Just t) -> do
                                    H.footnote (toString (raw1 <> " vs " <> raw2))
                                    o H.=== t
                                _ -> H.success

    -- One oracle process sorts each registry's complete version list.
    describe "compareVersions agrees with the reference oracle on live registry versions" $
        for_ (smokeRegistryPackages catalogue) $ \(eco, pkgs) ->
            withLiveOracle eco (show eco <> " -- live registry ordering") $ do
                manager <- runIO (newManager tlsManagerSettings)
                for_ pkgs $ \pkg ->
                    it (show eco <> " -- " <> toString pkg <> " (live registry versions)") $ do
                        mVersions <- fetchVersions manager eco pkg
                        case mVersions of
                            Nothing ->
                                pendingWith
                                    ( toString (ecosystemName eco)
                                        <> " registry unreachable or undecodable for "
                                        <> toString pkg
                                        <> "; smoke test skipped"
                                    )
                            Just [] ->
                                pendingWith ("no versions published for " <> toString pkg <> "; smoke test skipped")
                            Just versions -> do
                                mRef <- oracleSort eco versions
                                case mRef of
                                    Nothing ->
                                        pendingWith
                                            ("reference oracle failed to sort versions for " <> toString pkg <> "; smoke test skipped")
                                    Just [] ->
                                        pendingWith
                                            ("reference oracle accepted none of the published versions for " <> toString pkg <> "; smoke test skipped")
                                    Just refSorted ->
                                        case findDivergences eco refSorted of
                                            [] ->
                                                -- Non-vacuous: we ordered a real, reference-valid set.
                                                refSorted `shouldNotBe` []
                                            ds ->
                                                expectationFailure (renderDivergences eco pkg refSorted ds)
  where
    generatorScript = "scripts/gen-version-fixtures.sh"
    committedFixture = "core/test/unit/fixtures/version-ordering.txt"

-- Probe optional tools before generating cases, so missing tools cannot pass vacuously.
withLiveOracle :: Ecosystem -> String -> Spec -> Spec
withLiveOracle eco title body = do
    available <- runIO (oracleAvailable eco)
    if available
        then body
        else
            it title $
                if eco == Cargo
                    then expectationFailure "the pinned Cargo oracle failed its precedence probe"
                    else pendingWith ("reference oracle for " <> show eco <> " unavailable; run via `nix develop`")

data Divergence
    = Abstained Text
    | Misordered Text Text
    deriving stock (Eq, Show)

-- Consecutive pairs suffice because native order is total and equal precedence preserves input order.
findDivergences :: Ecosystem -> [Text] -> [Divergence]
findDivergences eco refSorted = abstentions <> misorders
  where
    abstentions = [Abstained v | v <- refSorted, isNothing (versionKey (mkVersion eco v))]
    misorders =
        [ Misordered a b
        | (a, b) <- zip refSorted (drop 1 refSorted)
        , compareVersions (mkVersion eco a) (mkVersion eco b) == Just GT
        ]

renderDivergences :: Ecosystem -> Text -> [Text] -> [Divergence] -> String
renderDivergences eco pkg refSorted ds =
    toString . T.unlines $ header : map render ds
  where
    header =
        "compareVersions diverges from the live reference oracle for "
            <> ecosystemName eco
            <> "/"
            <> pkg
            <> " over "
            <> show (length refSorted)
            <> " reference-valid published versions:"
    render = \case
        Abstained v ->
            "  abstain -- our parser returns no ordering key for reference-valid " <> v
        Misordered a b ->
            "  misorder -- reference orders " <> a <> " before " <> b <> ", but compareVersions says GT"

npmish :: Gen Text
npmish = Gen.choice [V.genNpm, messy]

pypiish :: Gen Text
pypiish = Gen.choice [V.genPyPI, messy]

gemish :: Gen Text
gemish = Gen.choice [V.genGem, messy]

cargoish :: Gen Text
cargoish = Gen.choice [V.genCargo, messy]

-- Near misses broaden smoke inputs without deciding parser-acceptance equivalence.
messy :: Gen Text
messy =
    Gen.text
        (Range.linear 1 10)
        (Gen.element ('.' : '-' : '+' : '_' : '!' : ['0' .. '9'] <> "abcrvdevpostpre"))
