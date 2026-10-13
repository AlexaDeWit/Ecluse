-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Fixed native and helper components share the pinned compiler and external dependencies.
module Ecluse.BenchReport.RefactorResidencyDiagnostic.Plans (auditPlan, planBinaryPath, comparePlans, fixturePlan, toolchainText) where

import Data.Aeson (Value (Object), object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T
import System.FilePath ((</>))

import Ecluse.BenchReport.BuildPlan (planUnits, projectPlanUnits)
import Ecluse.BenchReport.RefactorFiles (readComparisonText)
import Ecluse.BenchReport.RefactorResidencyDiagnostic.Inventory (requireDiagnostic)
import Ecluse.Test.Contract.Manifest (readJsonFile)

-- | The residency executable path follows the fixed Cabal component and compiler.
planBinaryPath :: FilePath -> FilePath
planBinaryPath source = source </> "dist-newstyle/build/aarch64-linux/ghc-9.10.3/ecluse-0.4.0/t/ecluse-residency/build/ecluse-residency/ecluse-residency"

-- | Toolchain versions match the pinned flake and qualified native plans.
toolchainText :: Text
toolchainText = "The Glorious Glasgow Haskell Compilation System, version 9.10.3\ncabal-install version 3.16.1.0\ncompiled using version 3.16.1.0 of the Cabal library (in-tree)\n"

planFields :: [(Text, Text)]
planFields = [("cabal-version", "3.16.1.0"), ("cabal-lib-version", "3.16.1.0"), ("compiler-id", "ghc-9.10.3"), ("compiler-abi", "ae30"), ("os", "linux"), ("arch", "aarch64")]

-- | Require one residency component, its exact checkout path, flags and identified toolchain.
auditPlan :: FilePath -> FilePath -> IO [Text]
auditPlan directory source = do
    plan <- readJsonFile (directory </> "plan.json")
    units <- either fail pure (planUnits plan)
    case plan of
        Object fields -> requireDiagnostic (all (\(key, value) -> KeyMap.lookup (fromString (toString key)) fields == Just (toJSON value)) planFields) "plan compiler, Cabal or platform differs"
        _ -> fail "plan must be an object"
    let matches = [unit | Object unit <- units, KeyMap.lookup "pkg-name" unit == Just (toJSON ("ecluse" :: Text)), KeyMap.lookup "component-name" unit == Just (toJSON ("test:ecluse-residency" :: Text))]
        unitIds = [value | Object unit <- units, Just value <- [KeyMap.lookup "id" unit]]
    requireDiagnostic (length unitIds == length units && length (ordNub unitIds) == length units) "missing or duplicate plan units"
    case matches of
        [unit] -> do
            forM_ [("pkg-version", "0.4.0"), ("type", "configured"), ("style", "local")] $ \(key, value :: Text) ->
                requireDiagnostic (KeyMap.lookup key unit == Just (toJSON value)) "residency plan component settings differ"
            requireDiagnostic (KeyMap.lookup "flags" unit == Just (object ["dev-http-egress" .= True])) "residency plan flags differ"
            requireDiagnostic (KeyMap.lookup "bin-file" unit == Just (toJSON (planBinaryPath source))) "residency plan executable path differs"
            case KeyMap.lookup "pkg-src" unit of
                Just (Object origin) -> requireDiagnostic (KeyMap.lookup "type" origin == Just (toJSON ("local" :: Text)) && KeyMap.lookup "path" origin `elem` map (Just . toJSON) [source, source </> "."]) "residency plan source path differs"
                _ -> fail "residency plan source identity missing"
        _ -> fail "missing or duplicate residency component"
    compiler <- T.unlines . map T.stripEnd . T.lines <$> readComparisonText (directory </> "toolchain.txt")
    requireDiagnostic (compiler == toolchainText) "identified toolchain differs from pinned plan"
    pure []

normaliseUnit :: Value -> Value
normaliseUnit (Object unit) = Object $ case KeyMap.lookup "pkg-src" unit of
    Just (Object origin) -> KeyMap.insert "pkg-src" (Object (KeyMap.delete "path" origin)) unit
    _ -> unit
normaliseUnit value = value

externalUnits :: FilePath -> IO [Value]
externalUnits path = do
    plan <- readJsonFile path
    units <- either fail pure (planUnits plan)
    pure (map normaliseUnit (projectPlanUnits ["ecluse"] ["dist-dir", "build-info", "bin-file"] units))

-- | Remove only source-specific paths while retaining dependency identities, hashes and flags.
comparePlans :: FilePath -> IO [Text]
comparePlans root = do
    native <- externalUnits (root </> "build-A/plan.json")
    comparisons <- forM ["build-B", "helper-build-A", "helper-build-B"] $ \directory -> do
        actual <- externalUnits (root </> directory </> "plan.json")
        pure ["external dependency plan differs: " <> toText directory | actual /= native]
    pure (concat comparisons)

-- | Synthetic plans retain the actual Cabal schema and fixed residency contract.
fixturePlan :: FilePath -> Value
fixturePlan source =
    object $
        [(fromString (toString key), toJSON value) | (key, value) <- planFields]
            <> ["install-plan" .= [object ["type" .= ("configured" :: Text), "id" .= ("ecluse-0.4.0-inplace-ecluse-residency" :: Text), "pkg-name" .= ("ecluse" :: Text), "pkg-version" .= ("0.4.0" :: Text), "flags" .= object ["dev-http-egress" .= True], "style" .= ("local" :: Text), "pkg-src" .= object ["type" .= ("local" :: Text), "path" .= (source </> ".")], "component-name" .= ("test:ecluse-residency" :: Text), "bin-file" .= planBinaryPath source], object ["type" .= ("pre-existing" :: Text), "id" .= ("base-4.20.2.0" :: Text), "pkg-name" .= ("base" :: Text), "pkg-version" .= ("4.20.2.0" :: Text)]]]
