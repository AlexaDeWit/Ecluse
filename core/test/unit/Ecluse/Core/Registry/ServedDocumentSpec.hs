-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Shared document contracts and artifact dropping through npm and PyPI assembly.
module Ecluse.Core.Registry.ServedDocumentSpec (spec) where

import Data.Aeson (Value (Array, Number, Object, String), object, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GHC.Conc (getAllocationCounter)
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)
import UnliftIO (evaluate)

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Package (InvalidEntry (invalidKey, invalidKind), InvalidEntryKind (InvalidIndexFile, InvalidVersionManifest), PackageInfo (infoInvalidEntries), mkPackageName)
import Ecluse.Core.Package.Entry (AdmittedEntry (..), EntryKey (..))
import Ecluse.Core.Package.Filter (enforceArtifactLocations)
import Ecluse.Core.Package.Merge (MergePlan (..), Provenance (GatedSource), SourceId, mergePackuments)
import Ecluse.Core.Registry.Npm.Filter (assembleMergedPackument)
import Ecluse.Core.Registry.Npm.Project (parsePackageInfoFromValue)
import Ecluse.Core.Registry.PyPI.Filter (assembleSimpleIndex)
import Ecluse.Core.Registry.PyPI.Project (projectSimpleIndexFromValue)
import Ecluse.Core.Registry.ServedDocument (overlaySurvivors, rebaseArtifactUrl, safeDocumentName)
import Ecluse.Core.Registry.WireSupport (Projection (NameMismatch, Projected))
import Ecluse.Core.Security (ecosystemArtifactAuthorities)
import Ecluse.Core.Snapshot (Snapshot (..), digestOf)
import Ecluse.Test.Registry.Npm qualified as Npm
import Ecluse.Test.Registry.PyPI (simpleFile, withFileKeys)
import Ecluse.Test.Snapshot (jsonSnapshot, syntheticSnapshot)
import Ecluse.Test.Support (expectRight)

-- | Pin source selection, name gates, and artifact rebasing after location admission.
spec :: Spec
spec = do
    overlaySpec
    entryContractSpec
    allocationSpec
    nameGateSpec
    rebaseSpec
    droppedArtifactSpec

droppedArtifactSpec :: Spec
droppedArtifactSpec = describe "served artifact filename refusals" $
    for_ ["a\\..\\..\\x", ".", "..", "", ".. ", ". ", " ", "%2e", ".%2E", "%2e.", "%2E%2e", "a%2fb", "a%5Cb"] $ \filename -> do
        it ("drops and records an npm version whose URL ends in " <> show filename) $ do
            let version = Npm.versionValue (Npm.versionSpec "lodash" "1.0.0" ("https://registry.npmjs.org/" <> filename))
                source = Npm.packumentValue "lodash" "1.0.0" [("1.0.0", version)] [] []
            info <- projectedInfo =<< expectRight (parsePackageInfoFromValue (mkPackageName Npm Nothing "lodash") source)
            let kept = enforceArtifactLocations (ecosystemArtifactAuthorities []) "https://registry.npmjs.org" info
            map invalidKind (infoInvalidEntries kept) `shouldBe` [InvalidVersionManifest]
            map invalidKey (infoInvalidEntries kept) `shouldBe` ["1.0.0"]
            case mergePackuments [(GatedSource, kept <$ jsonSnapshot source)] of
                Nothing -> expectationFailure "expected a merge plan for the empty listing"
                Just plan ->
                    field "versions" (assembleMergedPackument "https://ecluse.test/npm" (Map.singleton 0 (jsonSnapshot source)) plan source)
                        `shouldBe` Just (Object mempty)

        for_ ["absent", "distinct", "duplicate" :: Text] $ \siblingKind ->
            it ("drops and records a PyPI file whose URL ends in " <> show filename <> ", sibling=" <> toString siblingKind) $ do
                let refusedName = "requests-1.0.0.tar.gz"
                    keepSibling = siblingKind /= "absent"
                    siblingName = if siblingKind == "duplicate" then refusedName else "requests-1.0.0-py3-none-any.whl"
                    refused = withFileKeys [("url", String ("https://files.pythonhosted.org/" <> filename))] (simpleFile refusedName)
                    files = refused : [simpleFile siblingName | keepSibling]
                    source = object ["name" .= ("requests" :: Text), "meta" .= object ["api-version" .= ("1.0" :: Text)], "files" .= files]
                info <- projectedInfo =<< expectRight (projectSimpleIndexFromValue (mkPackageName PyPI Nothing "requests") source)
                let kept = enforceArtifactLocations (ecosystemArtifactAuthorities ["https://files.pythonhosted.org"]) "https://pypi.org" info
                map invalidKind (infoInvalidEntries kept) `shouldBe` [if keepSibling then InvalidIndexFile else InvalidVersionManifest]
                map invalidKey (infoInvalidEntries kept) `shouldBe` [if keepSibling then refusedName else "1"]
                case mergePackuments [(GatedSource, kept <$ jsonSnapshot source)] of
                    Nothing -> expectationFailure "expected a merge plan for the listing"
                    Just plan -> do
                        let served = assembleSimpleIndex "https://ecluse.test/pypi" (Map.singleton 0 (jsonSnapshot source)) plan source
                            sibling = withFileKeys [("url", String ("https://ecluse.test/pypi/simple/requests/" <> siblingName))] (simpleFile siblingName)
                        field "files" served `shouldBe` Just (Array (fromList [sibling | keepSibling]))
                        field "versions" served `shouldBe` Just (Array (fromList [String "1" | keepSibling]))

projectedInfo :: Projection a -> IO a
projectedInfo = \case
    Projected info -> pure info
    NameMismatch name -> fail ("unexpected name mismatch: " <> toString name)

field :: Key -> Value -> Maybe Value
field key = \case
    Object o -> KeyMap.lookup key o
    _ -> Nothing

overlaySpec :: Spec
overlaySpec = describe "overlaySurvivors" $ do
    it "takes each survivor's entry from the source that won it" $
        overlay [(0, sourceOf [("1.0.0", "private"), ("2.0.0", "private")]), (1, sourceOf [("1.0.0", "public"), ("2.0.0", "public")])] [("1.0.0", 0), ("2.0.0", 1)]
            `shouldBe` [("1.0.0", "private"), ("2.0.0", "public")]

    it "yields survivors in key order, so the assembly is deterministic" $
        overlay [(0, sourceOf [("1.0.0", "a"), ("2.0.0", "a"), ("10.0.0", "a")])] [("2.0.0", 0), ("10.0.0", 0), ("1.0.0", 0)]
            `shouldBe` [("1.0.0", "a"), ("10.0.0", "a"), ("2.0.0", "a")]

    it "drops a survivor whose winning source holds no entry for it, never fabricating one" $
        overlay [(0, sourceOf [("1.0.0", "a")])] [("1.0.0", 0), ("2.0.0", 0)]
            `shouldBe` [("1.0.0", "a")]

    it "drops a survivor whose winning source is absent from the map" $
        overlay [(0, sourceOf [("1.0.0", "a")])] [("1.0.0", 0), ("2.0.0", 7)]
            `shouldBe` [("1.0.0", "a")]

    it "yields nothing for a plan with no survivors" $
        overlay [(0, sourceOf [("1.0.0", "a")])] [] `shouldBe` []

    it "resolves every survivor one source won, at any size" $
        hedgehog $ do
            count <- forAll (Gen.int (Range.linear 1 40))
            let versions = [show n | n <- [1 .. count :: Int]]
                survivors = [(v, 0 :: SourceId) | v <- versions]
            length (overlay [(0, sourceOf [(v, v) | v <- versions])] survivors) === count

entryContractSpec :: Spec
entryContractSpec = describe "source-scoped admitted-entry contracts" $ do
    for_ [ArrayEntry 2, ObjectEntry "release", SingletonEntry] $ \key ->
        describe (show key) $ do
            let source = syntheticSnapshot [(key, "kept" :: Text)]
                sources = Map.singleton 0 source
                plan = entryPlan source key
                serve :: Map SourceId (Snapshot [(EntryKey, Text)]) -> MergePlan -> [(Text, Text)]
                serve = overlaySurvivors id
            it "serves the exact admitted entry" $
                serve sources plan `shouldBe` [("1", "kept")]
            it "refuses a missing admitted identity" $
                serve sources plan{mpArtifacts = mempty} `shouldBe` []
            it "refuses an entry whose version lost admission" $
                serve sources plan{mpSurvivors = mempty} `shouldBe` []
            it "refuses an unnamed admitted entry" $
                serve sources plan{mpArtifacts = fmap (fmap (\entry -> entry{admittedFilename = ""})) (mpArtifacts plan)} `shouldBe` []
            it "refuses a missing raw identity" $
                serve (Map.singleton 0 ([] <$ source)) plan `shouldBe` []
            it "refuses a different raw key" $
                serve (Map.singleton 0 ([(ObjectEntry "different", "other")] <$ source)) plan `shouldBe` []
            it "refuses duplicate raw keys without selecting either occurrence" $
                serve (Map.singleton 0 ([(key, "kept"), (key, "refused")] <$ source)) plan `shouldBe` []
            it "refuses duplicate admitted keys" $
                serve sources plan{mpArtifacts = fmap (\entries -> entries <> entries) (mpArtifacts plan)} `shouldBe` []
            it "refuses a different snapshot under the same source position" $
                serve (Map.singleton 0 source{snapshotDigest = digestOf "different upstream bytes"}) plan `shouldBe` []
            it "refuses a matching snapshot from a losing source" $
                serve (Map.singleton 1 source) plan `shouldBe` []
            it "does not confuse identical content from two source positions" $
                serve (Map.insert 1 ([(key, "losing")] <$ source) sources) plan `shouldBe` [("1", "kept")]

    it "refuses negative array positions" $ do
        let key = ArrayEntry (-1)
            source = syntheticSnapshot [(key, "invalid" :: Text)]
        overlaySurvivors id (Map.singleton 0 source) (entryPlan source key) `shouldBe` []

allocationSpec :: Spec
allocationSpec = describe "entry selection allocation growth" $
    for_ [("array", ArrayEntry), ("object", ObjectEntry . show)] $ \(label, keyAt) ->
        it ("bounds allocation growth for " <> label <> " coordinates") $ do
            allocations <- forM [128, 256, 512, 1024] $ \count -> do
                let entries = [(keyAt position, "raw entry" :: Text) | position <- [0 .. count - 1]]
                    source = syntheticSnapshot entries
                    basePlan = entryPlan source SingletonEntry
                    admitted = [AdmittedEntry (snapshotDigest source) key "same-filename" | (key, _) <- entries]
                kept <- expectRight (maybeToRight ("empty allocation fixture" :: Text) (nonEmpty admitted))
                let plan = basePlan{mpArtifacts = Map.singleton "1" kept}
                _ <- evaluate (T.length (show (source, plan)))
                before <- getAllocationCounter
                served <- evaluate (sum [T.length version + T.length value | (version, value) <- overlaySurvivors id (Map.singleton 0 source) plan])
                after <- getAllocationCounter
                served `shouldBe` count * 10
                let allocated = before - after
                putTextLn ("entry selection " <> toText label <> ": entries=" <> show count <> ", allocated_bytes=" <> show allocated)
                allocated `shouldSatisfy` (> 0)
                pure allocated
            for_ (zip allocations (drop 1 allocations)) $ \(smaller, larger) ->
                larger `shouldSatisfy` (< 3 * smaller + 65536)

entryPlan :: Snapshot a -> EntryKey -> MergePlan
entryPlan source key =
    MergePlan
        { mpName = mkPackageName Npm Nothing "fixture"
        , mpSurvivors = Map.singleton "1" 0
        , mpArtifacts = Map.singleton "1" (AdmittedEntry (snapshotDigest source) key "same-filename" :| [])
        , mpDistTags = mempty
        , mpTime = mempty
        , mpDivergences = mempty
        }

nameGateSpec :: Spec
nameGateSpec = describe "safeDocumentName" $ do
    it "reads the name a document claims for itself when the parser admits it" $
        safeDocumentName parseName (documentNamed (String "lodash")) `shouldBe` Just "LODASH"

    it "refuses a name the parser rejects, so nothing interpolates it" $
        safeDocumentName parseName (documentNamed (String "../etc")) `shouldBe` Nothing

    it "refuses a document whose name is not a string" $
        safeDocumentName parseName (documentNamed (Number 1)) `shouldBe` Nothing

    it "refuses a document that claims no name at all" $
        safeDocumentName parseName KeyMap.empty `shouldBe` Nothing

    it "reads the parser the caller supplies, not a grammar of its own" $
        safeDocumentName (const (Nothing :: Maybe Text)) (documentNamed (String "lodash")) `shouldBe` Nothing

rebaseSpec :: Spec
rebaseSpec = describe "rebaseArtifactUrl" $ do
    it "declines a location the renderer will not render" $
        rebaseArtifactUrl (const Nothing) "https://registry.npmjs.org/lodash/-/lodash-4.17.21.tgz"
            `shouldBe` (Nothing :: Maybe Text)

    it "points an upstream location back through the mount, keeping the file name verbatim" $
        rebaseArtifactUrl mountUrl "https://registry.npmjs.org/lodash/-/lodash-4.17.21.tgz"
            `shouldBe` Just "https://ecluse.test/npm/lodash/-/lodash-4.17.21.tgz"

    it "reads the file name past a query string a signed location carries" $
        rebaseArtifactUrl mountUrl "https://cdn.test/a/lodash-4.17.21.tgz?sig=abc"
            `shouldBe` Just "https://ecluse.test/npm/lodash/-/lodash-4.17.21.tgz"

    it "leaves a location that names no file, rather than pointing it somewhere wrong" $ do
        rebaseArtifactUrl mountUrl "https://registry.npmjs.org/lodash/" `shouldBe` Nothing
        rebaseArtifactUrl mountUrl "" `shouldBe` Nothing

    for_ ["a\\..\\..\\x", ".", "..", ".. ", ". ", " ", "bad\n", "%2e", ".%2e", "%2E.", "%2E%2e", "a%2fb", "a%5Cb"] $ \filename ->
        it ("refuses to rebase " <> show filename) $
            rebaseArtifactUrl mountUrl ("https://registry.npmjs.org/" <> filename) `shouldBe` Nothing

    for_ ["%2e%2e.tgz", "two%20words.tgz", "name+tag.tgz", "%252e%252e"] $ \filename ->
        it ("preserves an admitted encoded filename when rebasing " <> show filename) $
            rebaseArtifactUrl mountUrl ("https://registry.npmjs.org/" <> filename)
                `shouldBe` mountUrl filename

    it "is idempotent: rebasing an already-rebased URL yields the same URL" $
        hedgehog $ do
            file <- forAll (Gen.text (Range.linear 1 20) Gen.alphaNum)
            let once = rebaseArtifactUrl mountUrl ("https://upstream.test/x/" <> file <> ".tgz")
            (once >>= rebaseArtifactUrl mountUrl) === once

overlay :: [(SourceId, Value)] -> [(Text, SourceId)] -> [(Text, Value)]
overlay sources survivors =
    overlaySurvivors versionEntries scoped (planOver scoped (Map.fromList survivors))
  where
    scoped = Map.fromList (map (second jsonSnapshot) sources)
    versionEntries = \case
        Object o
            | Just (Object vs) <- KeyMap.lookup "versions" o ->
                [(ObjectEntry (Key.toText key), value) | (key, value) <- KeyMap.toAscList vs]
        _ -> []

sourceOf :: [(Text, Text)] -> Value
sourceOf entries = object ["versions" .= object [(fromString (toString version), String marker) | (version, marker) <- entries]]

planOver :: Map SourceId (Snapshot Value) -> Map Text SourceId -> MergePlan
planOver sources survivors =
    MergePlan
        { mpName = mkPackageName Npm Nothing "lodash"
        , mpSurvivors = survivors
        , mpArtifacts =
            Map.mapMaybeWithKey
                (\version sid -> (\source -> AdmittedEntry (snapshotDigest source) (ObjectEntry version) "x.tgz" :| []) <$> Map.lookup sid sources)
                survivors
        , mpDistTags = Map.empty
        , mpTime = Map.empty
        , mpDivergences = mempty
        }

documentNamed :: Value -> KeyMap.KeyMap Value
documentNamed = KeyMap.singleton "name"

parseName :: Text -> Maybe Text
parseName raw = do
    guard (not (T.null raw) && T.all (`elem` ("abcdefghijklmnopqrstuvwxyz-." :: String)) raw && not (T.isInfixOf ".." raw))
    pure (T.toUpper raw)

mountUrl :: Text -> Maybe Text
mountUrl file = Just ("https://ecluse.test/npm/lodash/-/" <> file)
