-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Fixed diagnostic component identities and narrow external-plan comparison.
module Ecluse.BenchReport.AllocationDiagnostic.Plan (checkPlan, checkFullPlan, auditPlansFor, cabalDigest, tarDigest) where

import Data.Aeson (Object, Value (Object), object, toJSON, withObject, (.:), (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.Text qualified as T
import System.FilePath ((</>))

import Ecluse.BenchReport.AllocationDiagnostic.Inventory (Recipe (..), requireDiagnostic)
import Ecluse.Test.Contract.Manifest (readJsonFile)

-- | Preserve the qualified focused package and fit checks.
checkPlan :: FilePath -> Text -> IO ()
checkPlan root build = do
    units <- readPlanUnits root build
    (benchmark, unit) <- benchmarkLibrary units
    fit <- fitLibrary units benchmark unit
    validateFit build fit
    requireDiagnostic (KeyMap.lookup "pkg-version" unit == Just (toJSON ("0.4.1" :: Text))) "tasty-bench version differs"
    requireDiagnostic (KeyMap.lookup "flags" unit == Just (object ["tasty" .= True])) "tasty-bench flags differ"
    if "C-" `T.isPrefixOf` build
        then do
            requireDiagnostic (KeyMap.lookup "pkg-src-sha256" unit == Just (toJSON tarDigest)) "native tasty-bench source differs"
            requireDiagnostic (KeyMap.lookup "pkg-cabal-sha256" unit == Just (toJSON cabalDigest)) "native tasty-bench Cabal revision differs"
        else do
            requireDiagnostic (KeyMap.lookup "style" unit == Just (toJSON ("local" :: Text))) "diagnostic tasty-bench is not local"
            case KeyMap.lookup "pkg-src" unit of
                Just (Object origin) -> requireDiagnostic (KeyMap.lookup "path" origin == Just (toJSON (root </> "dependency/tasty-bench-0.4.1"))) "overlay source path differs"
                _ -> fail "overlay source identity is missing"

-- | The pinned upstream Cabal revision is shared by both recipes.
cabalDigest :: Text
cabalDigest = "c38a77747f14e8d26ca99869b879ee56099b04e5381a74a62255dcbab85aab84"

-- | The pinned upstream archive is shared by both recipes.
tarDigest :: Text
tarDigest = "315c77e1f0f5fddf3b8465060867bb30c6d6562867b8609afdc7f6e14e295777"

-- | Full-history checks precede the existing narrow fit substitution.
auditPlansFor :: Recipe -> FilePath -> IO ()
auditPlansFor recipe root = do
    when (recipe == FullHistory) $ do
        nativeEdges <- checkFullPlan root "C-A"
        forM_ ["C-B", "I-A", "I-B"] $ \build -> do
            actualEdges <- checkFullPlan root build
            requireDiagnostic (actualEdges == nativeEdges) ("full tasty-bench library external edges differ: " <> build)
    native <- externalPlan "C-A"
    forM_ ["C-B", "I-A", "I-B"] $ \build -> do
        actual <- externalPlan build
        requireDiagnostic (actual == native) ("external dependency plan differs: " <> build)
  where
    externalPlan :: Text -> IO [Value]
    externalPlan build = do
        units <- readPlanUnits root build
        (benchmark, library) <- benchmarkLibrary units
        fit <- fitLibrary units benchmark library
        libraryId <- unitId library
        fitId <- unitId fit
        validateFit build fit
        traverse (normaliseExternal libraryId fitId) [unit | unit <- units, KeyMap.lookup "pkg-name" unit `notElem` map (Just . toJSON) (["ecluse", "tasty-bench"] :: [Text])]

readPlanUnits :: FilePath -> Text -> IO [Object]
readPlanUnits root build = do
    plan <- readJsonFile (root </> "build-" <> toString build </> "identity/plan.json")
    either fail pure (parseEither (withObject "plan" (.: "install-plan")) plan :: Either String [Object])

uniqueComponent :: Text -> Text -> [Object] -> IO Object
uniqueComponent package component units = case [unit | unit <- units, KeyMap.lookup "pkg-name" unit == Just (toJSON package), KeyMap.lookup "component-name" unit == Just (toJSON component)] of
    [unit] -> pure unit
    _ -> fail (toString ("missing or duplicate plan component: " <> package <> "/" <> component))

benchmarkLibrary :: [Object] -> IO (Object, Object)
benchmarkLibrary units = do
    benchmark <- uniqueComponent "ecluse" "bench:ecluse-bench" units
    library <- uniqueComponent "tasty-bench" "lib" units
    identifiers <- traverse unitId units
    requireDiagnostic (length identifiers == length (ordNub identifiers)) "duplicate plan unit identifier"
    requireDependency "benchmark does not use exact tasty-bench library" benchmark library
    pure (benchmark, library)

fitLibrary :: [Object] -> Object -> Object -> IO Object
fitLibrary units benchmark library = do
    fit <- uniqueComponent "tasty-bench-fit" "lib" units
    requireDependency "benchmark does not use exact tasty-bench-fit library" benchmark fit
    requireDependency "tasty-bench-fit does not use exact tasty-bench library" fit library
    pure fit

unitId :: Object -> IO Text
unitId unit = either fail pure (parseEither (.: "id") unit)

unitDependencies :: Object -> IO [Text]
unitDependencies unit = either fail pure (parseEither (.: "depends") unit)

requireDependency :: Text -> Object -> Object -> IO ()
requireDependency problem consumer library = do
    dependencies <- unitDependencies consumer
    libraryId <- unitId library
    requireDiagnostic (length (filter (== libraryId) dependencies) == 1) problem

validateFit :: Text -> Object -> IO ()
validateFit build unit = do
    let expectedStyle :: Text
        expectedStyle = if "C-" `T.isPrefixOf` build then "global" else "inplace"
    requireDiagnostic (KeyMap.lookup "style" unit == Just (toJSON expectedStyle)) "tasty-bench-fit build style differs"
    requireDiagnostic (KeyMap.lookup "pkg-version" unit == Just (toJSON ("0.1.1" :: Text))) "tasty-bench-fit version differs"
    requireDiagnostic (KeyMap.lookup "flags" unit == Just (object ["debug" .= False])) "tasty-bench-fit flags differ"
    requireDiagnostic (KeyMap.lookup "pkg-src-sha256" unit == Just (toJSON ("3b26a892b3080be7d0298862a85d3fb116cabf6e3508ad912354cb7247162ef4" :: Text))) "tasty-bench-fit source differs"
    requireDiagnostic (KeyMap.lookup "pkg-cabal-sha256" unit == Just (toJSON ("762cb6ee6da109f9c36d3e3a0835ccdac356939c9733a0dc501ddf24102615d2" :: Text))) "tasty-bench-fit Cabal revision differs"

normaliseExternal :: Text -> Text -> Object -> IO Value
normaliseExternal libraryId fitId unit = do
    identifier <- unitId unit
    let comparable = foldr KeyMap.delete unit ["pkg-src", "dist-dir", "build-info"]
    if identifier /= fitId
        then pure (Object comparable)
        else do
            dependencies <- unitDependencies unit
            source <- maybe (fail "tasty-bench-fit source record is missing") pure (KeyMap.lookup "pkg-src" unit)
            let stableDependencies = map (\dependency -> if dependency == libraryId then "diagnostic:tasty-bench:lib" else dependency) dependencies
                normalised = KeyMap.insert "pkg-src" source (KeyMap.insert "depends" (toJSON stableDependencies) (foldr KeyMap.delete comparable ["id", "style"]))
            pure (Object normalised)

-- | Only the library and the reviewed local bench-fibo component can leave the external projection.
checkFullPlan :: FilePath -> Text -> IO [Text]
checkFullPlan root build = do
    units <- readPlanUnits root build
    let instrumented = "I-" `T.isPrefixOf` build
        components = [unit | unit <- units, KeyMap.lookup "pkg-name" unit == Just (toJSON ("tasty-bench" :: Text))]
        expectedNames = ["lib"] <> ["bench:bench-fibo" | instrumented] :: [Text]
    names <- traverse (either (const (fail "full tasty-bench component inventory differs")) pure . parseEither (.: "component-name")) components
    requireDiagnostic (sort names == sort expectedNames) "full tasty-bench component inventory differs"
    forM_ components (checkFullComponent root instrumented)
    library <- uniqueComponent "tasty-bench" "lib" units
    externalIds <- traverse (externalId units) ["base", "containers", "deepseq", "ghc-prim", "tasty"]
    actual <- unitDependencies library
    requireDiagnostic (sort actual == sort externalIds) "full tasty-bench library dependencies differ"
    when instrumented $ do
        libraryId <- unitId library
        requireDiagnostic (libraryId == "tasty-bench-0.4.1-inplace") "full tasty-bench local library identity differs"
        fibo <- uniqueComponent "tasty-bench" "bench:bench-fibo" units
        fiboId <- unitId fibo
        requireDiagnostic (fiboId == libraryId <> "-bench-fibo") "full tasty-bench bench-fibo identity differs"
        baseId <- externalId units "base"
        dependencies <- unitDependencies fibo
        requireDiagnostic (sort dependencies == sort [baseId, libraryId]) "full tasty-bench bench-fibo dependencies differ"
    pure (sort externalIds)

checkFullComponent :: FilePath -> Bool -> Object -> IO ()
checkFullComponent root instrumented unit = do
    let localSource = object ["type" .= ("local" :: Text), "path" .= (root </> "dependency/tasty-bench-0.4.1")]
        nativeSource = object ["type" .= ("repo-tar" :: Text), "repo" .= object ["type" .= ("secure-repo" :: Text), "uri" .= ("http://hackage.haskell.org/" :: Text)]]
        expectedStyle :: Text
        expectedStyle = if instrumented then "local" else "global"
    requireDiagnostic (KeyMap.lookup "type" unit == Just (toJSON ("configured" :: Text))) "full tasty-bench component type differs"
    requireDiagnostic (KeyMap.lookup "pkg-version" unit == Just (toJSON ("0.4.1" :: Text))) "full tasty-bench component version differs"
    requireDiagnostic (KeyMap.lookup "flags" unit == Just (object ["tasty" .= True])) "full tasty-bench component flags differ"
    requireDiagnostic (KeyMap.lookup "style" unit == Just (toJSON expectedStyle)) "full tasty-bench component style differs"
    requireDiagnostic (KeyMap.lookup "pkg-src" unit == Just (if instrumented then localSource else nativeSource)) "full tasty-bench component source differs"
    requireDiagnostic (KeyMap.lookup "exe-depends" unit == Just (toJSON ([] :: [Text]))) "full tasty-bench component executable dependencies differ"
    forM_ [("pkg-src-sha256", tarDigest), ("pkg-cabal-sha256", cabalDigest)] $ \(key, value) ->
        requireDiagnostic (KeyMap.lookup key unit == if instrumented then Nothing else Just (toJSON value)) "full tasty-bench component source digest differs"

externalId :: [Object] -> Text -> IO Text
externalId units package = case [unit | unit <- units, KeyMap.lookup "pkg-name" unit == Just (toJSON package), KeyMap.lookup "component-name" unit == Just (toJSON ("lib" :: Text)) || (KeyMap.lookup "type" unit == Just (toJSON ("pre-existing" :: Text)) && isNothing (KeyMap.lookup "component-name" unit))] of
    [unit] -> unitId unit
    _ -> fail (toString ("full tasty-bench external library is missing or duplicated: " <> package))
