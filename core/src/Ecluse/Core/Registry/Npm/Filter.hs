-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Assemble admitted npm versions from their source snapshots and rebase artifact URLs.
Unknown wire fields remain on the exact selected entries.
-}
module Ecluse.Core.Registry.Npm.Filter (
    -- * URL rewriting
    rewriteVersion,

    -- * Assembling the served document
    assembleMergedPackument,
    npmDocumentName,

    -- * The served-document boundary (npm's 'CachedDoc' capabilities)
    assembleMergedDocument,
    serialiseMergedDocument,
) where

import Data.Aeson (Value (Object, String), encode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Package.Entry (EntryKey (ObjectEntry))
import Ecluse.Core.Package.Merge (MergePlan (mpDistTags, mpTime), SourceId)
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached)
import Ecluse.Core.Registry.Npm.Project (projectName)
import Ecluse.Core.Registry.Npm.Route (tarballPath)
import Ecluse.Core.Registry.ServedDocument (overlaySurvivors, rebaseArtifactUrl, safeDocumentName, stringField)
import Ecluse.Core.Snapshot (Snapshot)
import Ecluse.Core.Text (joinUrlPath, renderIso8601Utc)
import Ecluse.Core.Version (renderVersion)

-- | The packument's own @name@, safety-gated before it is interpolated into a rewritten path.
npmDocumentName :: KeyMap Value -> Maybe PackageName
npmDocumentName = safeDocumentName (rightToMaybe . projectName)

{- | Rewrite one version object's @dist.tarball@ to @{prefix}\/-\/{file}@, keeping the file name
verbatim. @prefix@ is upstream-controlled, so the caller gates it through 'npmDocumentName'.
-}
rewriteVersion :: (Text -> Maybe Text) -> Value -> Value
rewriteVersion servedUrl = \case
    Object vo -> Object (adjustObject "dist" (rewriteDist servedUrl) vo)
    other -> other

-- | Rewrite a @dist@ object's @tarball@, leaving one with no readable file name unchanged.
rewriteDist :: (Text -> Maybe Text) -> Value -> Value
rewriteDist servedUrl = \case
    Object dist
        | Just url <- stringField "tarball" dist
        , Just rebased <- rebaseArtifactUrl servedUrl url ->
            Object (KeyMap.insert "tarball" (String rebased) dist)
    other -> other

{- | Assemble the served packument for @mountBase@. The plan owns @versions@, @dist-tags@, and
@time@, every other top-level key comes from the base document, and the result is an object.
-}
assembleMergedPackument :: Text -> Map SourceId (Snapshot Value) -> MergePlan -> Value -> Value
assembleMergedPackument mountBase bySource plan base =
    Object rebuilt
  where
    rebuilt :: KeyMap Value
    rebuilt =
        baseObject
            & KeyMap.insert "versions" (Object survivingVersions)
            & KeyMap.insert "dist-tags" (Object distTags)
            & KeyMap.insert "time" (Object reconciledTime)

    baseObject :: KeyMap Value
    baseObject = case base of
        Object o -> o
        _ -> mempty

    -- The shared gate reads the document's own upstream-controlled @name@ before it reaches
    -- the URL, and a document with no usable name has no version rewritten.
    rewriteSurvivor :: Value -> Value
    rewriteSurvivor = maybe id (rewriteVersion . servedTarballUrl mountBase) (npmDocumentName baseObject)

    -- Each survivor's object is the raw @Value@ of the source that won the key, unmodelled
    -- keys and all. A survivor whose source object is missing drops out, never fabricated.
    survivingVersions :: KeyMap Value
    survivingVersions =
        KeyMap.fromList
            [ (Key.fromText version, rewriteSurvivor object)
            | (version, object) <- overlaySurvivors versionEntries bySource plan
            ]

    -- The plan has already resolved @latest@ and dropped absent-target tags over the union.
    distTags :: KeyMap Value
    distTags =
        KeyMap.fromList
            [ (Key.fromText tag, String (renderVersion v))
            | (tag, v) <- Map.toList (mpDistTags plan)
            ]

    -- @time@ rebuilt from the plan's surviving-version times, with the base
    -- document's non-version bookkeeping keys (@created@\/@modified@) retained.
    reconciledTime :: KeyMap Value
    reconciledTime =
        bookkeepingTime
            <> KeyMap.fromList
                [ (Key.fromText version, String (renderIso8601Utc t))
                | (version, t) <- Map.toList (mpTime plan)
                ]

    -- Two direct lookups, not a traversal: the base @time@ map carries one entry per published
    -- version alongside the @created@\/@modified@ bookkeeping keys.
    bookkeepingTime :: KeyMap Value
    bookkeepingTime = case KeyMap.lookup "time" baseObject of
        Just (Object timeObject) ->
            KeyMap.fromList
                [ (k, value)
                | name <- timeBookkeepingKeys
                , let k = Key.fromText name
                , Just value <- [KeyMap.lookup k timeObject]
                ]
        _ -> mempty

{- | npm's served-document __assemble__ capability
('Ecluse.Core.Registry.Adapter.Types.metadataAssemble'), across npm's own 'CachedDoc' boundary.
-}
assembleMergedDocument :: Text -> Map SourceId (Snapshot CachedDoc) -> MergePlan -> Maybe CachedDoc -> CachedDoc
assembleMergedDocument mountBase bySource plan base =
    fst npmCached (assembleMergedPackument mountBase (Map.mapMaybe (traverse npmValue) bySource) plan (fromMaybe (Object mempty) (npmValue =<< base)))

{- | npm's served-document __serialise__ capability
('Ecluse.Core.Registry.Adapter.Types.metadataSerialise'), to the compact wire bytes.
-}
serialiseMergedDocument :: CachedDoc -> LByteString
serialiseMergedDocument = encode . fromMaybe (Object mempty) . npmValue

-- A document another ecosystem injected projects as 'Nothing' and contributes nothing, rather
-- than reading as an empty one.
npmValue :: CachedDoc -> Maybe Value
npmValue = snd npmCached

versionEntries :: Value -> [(EntryKey, Value)]
versionEntries = \case
    Object o
        | Just (Object versions) <- KeyMap.lookup "versions" o ->
            [(ObjectEntry (Key.toText key), entry) | (key, entry) <- KeyMap.toList versions]
    _ -> []

-- The non-version keys an npm @time@ object carries, which the assembly relays
-- unchanged.
timeBookkeepingKeys :: [Text]
timeBookkeepingKeys = ["created", "modified"]

{- | Apply a function to the value at @key@, only when the object already carries that key.
A missing key stays absent, never fabricated, so passthrough stays lossless.
-}
adjustObject :: Key.Key -> (Value -> Value) -> KeyMap Value -> KeyMap Value
adjustObject key f o = case KeyMap.lookup key o of
    Just v -> KeyMap.insert key (f v) o
    Nothing -> o

{- The mount-local URL a served artifact resolves to, rendered from the artifact route that
must claim it. -}
servedTarballUrl :: Text -> PackageName -> Text -> Maybe Text
servedTarballUrl mountBase name file = joinUrlPath mountBase <$> tarballPath name file
