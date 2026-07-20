-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Registry.Json.AssemblySpec (spec) where

import Data.Aeson (Value (Bool, Object, String))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec (Spec, describe, it, shouldBe)

import Ecluse.Core.Package.Merge (SourceId)
import Ecluse.Core.Registry.Json.Assembly (
    plannedKeysOver,
    rebaseArtifactUrl,
    rebaseHook,
    replaySurvivors,
    safeMountPrefix,
 )

{- | Direct tests for the plan-replay and rebase skeleton JSON adapters instantiate.
'replaySurvivors' must take each surviving version from the source the plan says won it,
drop a survivor whose winning source lacks it, and apply the hook to every placed object.
'safeMountPrefix' is a security gate: it must refuse any name with an unsafe structural
component. 'rebaseArtifactUrl' must leave an undecomposable URL unchanged and stay
idempotent. 'rebaseHook' must fall back to no rewrite whenever the gate refuses. npm's
instantiation is exercised end-to-end by "Ecluse.Registry.Npm.FilterSpec"; these pin the
shared mechanics in isolation.
-}
spec :: Spec
spec = do
    replaySurvivorsSpec
    plannedKeysOverSpec
    safeMountPrefixSpec
    rebaseArtifactUrlSpec
    rebaseHookSpec

plannedKeysOverSpec :: Spec
plannedKeysOverSpec = describe "plannedKeysOver (the plan's keys win)" $ do
    it "overrides a same-named base key with the plan's rebuilt one" $
        -- The load-bearing assertion: if this inverted, a version the plan denied
        -- would be served from the upstream's own map.
        lookupKey "versions" (plannedKeysOver [("versions", String "planned")] baseDoc)
            `shouldBe` Just (String "planned")

    it "relays every base key the plan does not name" $
        lookupKey "extra" (plannedKeysOver [("versions", String "planned")] baseDoc)
            `shouldBe` Just (String "kept")

    it "adds a planned key the base does not carry" $
        lookupKey "dist-tags" (plannedKeysOver [("dist-tags", String "new")] baseDoc)
            `shouldBe` Just (String "new")

    it "yields an object over an empty base" $
        plannedKeysOver [("versions", String "planned")] mempty
            `shouldBe` Object (KeyMap.singleton "versions" (String "planned"))

replaySurvivorsSpec :: Spec
replaySurvivorsSpec = describe "replaySurvivors" $ do
    it "takes each surviving version from the source the plan says won it" $ do
        let survivors = Map.fromList [("1.0.0", 1), ("2.0.0", 0)]
            r = replaySurvivors id versionsBySource survivors
        markOf "1.0.0" r `shouldBe` Just (String "b-one")
        markOf "2.0.0" r `shouldBe` Just (String "a-two")

    it "drops a survivor whose winning source carries no object for it" $ do
        let survivors = Map.fromList [("1.0.0", 0), ("9.9.9", 1)]
            r = replaySurvivors id versionsBySource survivors
        KeyMap.keys r `shouldBe` ["1.0.0"]

    it "applies the rewrite hook to each placed object, keeping its own keys" $ do
        let survivors = Map.fromList [("1.0.0", 0)]
            r = replaySurvivors (markWith "hooked") versionsBySource survivors
        markOf "1.0.0" r `shouldBe` Just (String "a-one")
        fieldOf "1.0.0" "hooked" r `shouldBe` Just (Bool True)

safeMountPrefixSpec :: Spec
safeMountPrefixSpec = describe "safeMountPrefix (the shared safety gate)" $ do
    it "yields {base}/{name} for a name whose components are all safe" $
        safeMountPrefix single mountBase "thing"
            `shouldBe` Just "https://proxy.test/npm/thing"

    it "admits a scoped name when the grammar splits its structural separator" $
        safeMountPrefix scoped mountBase "@acme/thing"
            `shouldBe` Just "https://proxy.test/npm/@acme/thing"

    it "refuses a traversal component" $
        safeMountPrefix single mountBase ".." `shouldBe` Nothing

    it "refuses a name carrying a path separator the grammar does not make structural" $
        safeMountPrefix single mountBase "a/../b" `shouldBe` Nothing

    it "refuses an empty name" $
        safeMountPrefix single mountBase "" `shouldBe` Nothing

    it "refuses a scoped name whose base component is a traversal" $
        safeMountPrefix scoped mountBase "@acme/.." `shouldBe` Nothing

rebaseArtifactUrlSpec :: Spec
rebaseArtifactUrlSpec = describe "rebaseArtifactUrl (the shared rebase discipline)" $ do
    it "rebases through the ecosystem's path convention" $
        rebaseArtifactUrl dashDash "https://upstream.test/thing/-/thing-1.0.0.tgz"
            `shouldBe` "https://proxy.test/npm/thing/-/thing-1.0.0.tgz"

    it "leaves a URL with no derivable filename unchanged" $
        rebaseArtifactUrl dashDash "https://upstream.test/thing/"
            `shouldBe` "https://upstream.test/thing/"

    it "is idempotent for a convention that ends in the filename" $ do
        let once = rebaseArtifactUrl dashDash "https://upstream.test/thing/-/thing-1.0.0.tgz"
        rebaseArtifactUrl dashDash once `shouldBe` once

    it "carries the filename over verbatim under a different convention" $
        rebaseArtifactUrl filesInfix "https://files.test/ab/cd/thing-1.0.0.whl"
            `shouldBe` "https://proxy.test/other/thing/files/thing-1.0.0.whl"

rebaseHookSpec :: Spec
rebaseHookSpec = describe "rebaseHook (gate and rewrite, fail-closed)" $ do
    it "rewrites under the validated prefix when the gate admits the name" $
        rebaseHook single stamp mountBase (Just "thing") (Object mempty)
            `shouldBe` Object (KeyMap.singleton "prefix" (String "https://proxy.test/npm/thing"))

    it "performs no rewrite when the gate refuses the name" $
        rebaseHook single stamp mountBase (Just "..") (Object mempty)
            `shouldBe` Object mempty

    it "performs no rewrite when the document has no readable name" $
        rebaseHook single stamp mountBase Nothing (Object mempty)
            `shouldBe` Object mempty

-- | The mount's externally-visible base URL, as the assembly derives prefixes from.
mountBase :: Text
mountBase = "https://proxy.test/npm"

{- | A base document carrying a key the plan rebuilds (@versions@) and one it does not
(@extra@), so both halves of the overlay are observable.
-}
baseDoc :: KeyMap Value
baseDoc = KeyMap.fromList [("versions", String "upstream"), ("extra", String "kept")]

-- | Look a key up in a 'Value' expected to be an object.
lookupKey :: Key -> Value -> Maybe Value
lookupKey k = \case
    Object o -> KeyMap.lookup k o
    _ -> Nothing

-- | A single-component name grammar: the whole name, as an unscoped ecosystem reads it.
single :: Text -> [Text]
single name = [name]

-- | npm's scoped grammar, enough to witness that a structural separator is admitted.
scoped :: Text -> [Text]
scoped name = case T.stripPrefix "@" name of
    Just scopeAndBase ->
        let (scope, rest) = T.breakOn "/" scopeAndBase
         in if T.null rest then [name] else [scope, T.drop 1 rest]
    Nothing -> [name]

-- | npm's path convention, for the rebase cases.
dashDash :: Text -> Text
dashDash file = "https://proxy.test/npm/thing/-/" <> file

-- | A second, differently-shaped convention, to witness the builder is the only variable.
filesInfix :: Text -> Text
filesInfix file = "https://proxy.test/other/thing/files/" <> file

-- | A rewrite that records the prefix it was handed, so the gate's decision is observable.
stamp :: Text -> Value -> Value
stamp prefix = \case
    Object o -> Object (KeyMap.insert "prefix" (String prefix) o)
    v -> v

-- | Two sources; the plan names which one wins each surviving version key.
versionsBySource :: Map SourceId (KeyMap Value)
versionsBySource =
    Map.fromList
        [ (0, KeyMap.fromList [("1.0.0", tagged "a-one"), ("2.0.0", tagged "a-two")])
        , (1, KeyMap.fromList [("1.0.0", tagged "b-one")])
        ]

-- | A version object marked so the winning-source choice is observable.
tagged :: Text -> Value
tagged s = Object (KeyMap.singleton "mark" (String s))

-- | The @mark@ field of a placed version object, if present.
markOf :: Key -> KeyMap Value -> Maybe Value
markOf version = fieldOf version "mark"

-- | The value at @inner@ inside the version object at @outer@, if both are present.
fieldOf :: Key -> Key -> KeyMap Value -> Maybe Value
fieldOf outer inner r = case KeyMap.lookup outer r of
    Just (Object o) -> KeyMap.lookup inner o
    _ -> Nothing

-- | A rewrite hook that stamps a boolean marker onto an object, witnessing the hook ran.
markWith :: Key -> Value -> Value
markWith k = \case
    Object o -> Object (KeyMap.insert k (Bool True) o)
    v -> v
