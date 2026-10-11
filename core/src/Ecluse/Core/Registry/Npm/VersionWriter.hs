-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TypeFamilies #-}

{- | npm policy facts collected beside the existing packed writer.
The walk still owns shapes, depth checks and first-member handling.
Only the three install hooks retain script names, and no command survives in typed facts.
-}
module Ecluse.Core.Registry.Npm.VersionWriter (
    -- * Packing
    VersionWriter,
    newVersionWriter,
    packedWriter,
    sealVersion,
    discardVersion,

    -- * Typed facts
    VersionValue,
    versionFacts,
) where

import Control.Monad.ST (ST)
import Data.Aeson (FromJSON (parseJSON), Object, Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import Data.Primitive.MutVar (MutVar, newMutVar, readMutVar, writeMutVar)
import Data.Set qualified as Set

import Ecluse.Core.Package (Availability (Available))
import Ecluse.Core.Registry.Json.Intern (Entry, entryString, nameText)
import Ecluse.Core.Registry.Json.Packed (Packed)
import Ecluse.Core.Registry.Json.Shape (Build (..), MemberKey (..), memberText)
import Ecluse.Core.Registry.Json.Walk (Steps)
import Ecluse.Core.Registry.Json.Writer (Frame, Writer, discard, newWriter, sealValue)
import Ecluse.Core.Registry.Npm.Project (installHooks)
import Ecluse.Core.Registry.Npm.Wire (Dist, License, Person, VersionManifest (..), availability, distFields)

-- | One read's packed storage and the role of the next value.
data VersionWriter st = VersionWriter
    { packedWriter :: Writer st
    -- ^ The existing diagnostic decoder reads this writer's sealed values.
    , nextRole :: MutVar st Role
    }

-- | A completed value. Invalid facts require the existing whole-release diagnostic decoder.
data VersionValue
    = ScalarValue Value
    | ScriptValue (Maybe (Map Text Text))
    | ReleaseValue (Maybe VersionManifest)
    | Unobserved

data Role = Release | ScalarField | DistObject | LicenseObject | PersonObject | Scripts | Command | Ignore

data VersionFrame = VersionFrame Frame ObservedFields (Set Text)

data ObservedFields
    = ReleaseFields Manifest
    | ObjectFields Role Object
    | ScriptFields (Maybe (Map Text Text))
    | IgnoredFields

data Manifest = Manifest
    { manifestName :: Maybe Text
    , manifestVersion :: Maybe Text
    , manifestDist :: Maybe Dist
    , manifestAvailability :: Availability
    , manifestInstallScript :: Maybe (Maybe Bool)
    , manifestScripts :: Maybe (Map Text Text)
    , manifestLicense :: Maybe (Maybe License)
    , manifestPublisher :: Bool
    }

-- | The optional replacement member follows 'newWriter', including original-author diagnostics.
newVersionWriter :: Maybe (Entry, Entry) -> ST st (VersionWriter st)
newVersionWriter added = VersionWriter <$> newWriter added <*> newMutVar Release

-- | Seal with the existing codec and reset the observer for the next release.
sealVersion :: VersionWriter st -> [Text] -> ST st Packed
sealVersion writer path = do
    sealed <- sealValue (packedWriter writer) path
    writeMutVar (nextRole writer) Release
    pure sealed

-- | Discard both the packed scratch and the next value's observed role.
discardVersion :: VersionWriter st -> ST st ()
discardVersion writer = discard (packedWriter writer) >> writeMutVar (nextRole writer) Release

-- | Successful facts retain only hook presence in 'vmScripts', represented by empty commands.
versionFacts :: VersionValue -> Maybe VersionManifest
versionFacts = \case
    ReleaseValue facts -> facts
    _ -> Nothing

instance Build (VersionWriter st) (ST st (Steps (ST st) s)) where
    type Built (VersionWriter st) = VersionValue
    type Fields (VersionWriter st) = VersionFrame
    type Items (VersionWriter st) = Int
    sharedString writer entry next =
        sharedString (packedWriter writer) entry $ \() ->
            observeString writer (entryString entry) next
    ownString writer name next =
        ownString (packedWriter writer) name $ \() ->
            observeString writer (String (nameText name)) next
    integer writer number next =
        integer (packedWriter writer) number (\() -> next (ScalarValue (Number (fromIntegral number))))
    whole writer value next = whole (packedWriter writer) value (\() -> next (ScalarValue value))
    emptyContainer writer next = emptyContainer (packedWriter writer) (\() -> next (ScalarValue (Array mempty)))
    openObject writer next = openObject (packedWriter writer) $ \frame -> do
        role <- readMutVar (nextRole writer)
        writeMutVar (nextRole writer) Ignore
        next (VersionFrame frame (fieldsFor role) mempty)
    beginMember writer key (VersionFrame frame fields seen) next =
        beginMember (packedWriter writer) key frame $ \repeated -> do
            writeMutVar (nextRole writer) (memberRole fields (memberText key))
            next (repeated || Set.member (memberText key) seen)
    addMember writer key value (VersionFrame frame fields seen) next =
        addMember (packedWriter writer) key () frame $ \frame' ->
            next (VersionFrame frame' (addField (memberText key) value fields) (rememberOwn fields key seen))
    dropValue writer _ = dropValue (packedWriter writer) ()
    closeObject writer (VersionFrame frame fields _) next =
        closeObject (packedWriter writer) frame (\() -> next (finishFields fields))
    openArray writer next = do
        writeMutVar (nextRole writer) Ignore
        openArray (packedWriter writer) next
    addItem writer _ = addItem (packedWriter writer) ()
    closeArray writer count items next = closeArray (packedWriter writer) count items (\() -> next (ScalarValue (Array mempty)))

observeString :: VersionWriter st -> Value -> (VersionValue -> ST st r) -> ST st r
observeString writer value next = do
    role <- readMutVar (nextRole writer)
    next $! case role of
        Command -> ScriptValue (Just mempty)
        Ignore -> Unobserved
        _ -> ScalarValue value

rememberOwn :: ObservedFields -> MemberKey -> Set Text -> Set Text
rememberOwn fields key seen = case key of
    OwnKey _ -> case memberRole fields (memberText key) of
        Ignore -> seen
        _ -> Set.insert (memberText key) seen
    SharedKey _ -> seen

fieldsFor :: Role -> ObservedFields
fieldsFor = \case
    Release -> ReleaseFields (Manifest Nothing Nothing Nothing Available (Just Nothing) (Just mempty) (Just Nothing) True)
    Scripts -> ScriptFields (Just mempty)
    role@DistObject -> ObjectFields role mempty
    role@LicenseObject -> ObjectFields role mempty
    role@PersonObject -> ObjectFields role mempty
    _ -> IgnoredFields

memberRole :: ObservedFields -> Text -> Role
memberRole fields key = case fields of
    ReleaseFields _ -> case key of
        "dist" -> DistObject
        "license" -> LicenseObject
        "_npmUser" -> PersonObject
        "scripts" -> Scripts
        "name" -> ScalarField
        "version" -> ScalarField
        "deprecated" -> ScalarField
        "hasInstallScript" -> ScalarField
        _ -> Ignore
    ObjectFields role _
        | key `elem` objectMembers role -> ScalarField
        | otherwise -> Ignore
    ScriptFields _ -> Command
    IgnoredFields -> Ignore

objectMembers :: Role -> [Text]
objectMembers = \case
    DistObject -> distFields
    LicenseObject -> ["type", "url"]
    PersonObject -> ["name", "email", "url"]
    _ -> []

addField :: Text -> VersionValue -> ObservedFields -> ObservedFields
addField key value = \case
    ReleaseFields manifest -> ReleaseFields (addManifest key value manifest)
    ObjectFields role fields -> case value of
        ScalarValue scalar | key `elem` objectMembers role -> ObjectFields role (KeyMap.insert (Key.fromText key) scalar fields)
        _ -> ObjectFields role fields
    ScriptFields scripts -> ScriptFields $ do
        held <- scripts
        case value of
            ScriptValue (Just _) -> Just (keepHook key held)
            ScalarValue scalar -> parseMaybe (parseJSON @Text) scalar $> keepHook key held
            _ -> Nothing
    IgnoredFields -> IgnoredFields

keepHook :: Text -> Map Text Text -> Map Text Text
keepHook key scripts
    | key `elem` installHooks = Map.insert key "" scripts
    | otherwise = scripts

addManifest :: Text -> VersionValue -> Manifest -> Manifest
addManifest key value manifest = case (key, value) of
    ("scripts", ScriptValue scripts) -> manifest{manifestScripts = scripts}
    (_, ScalarValue scalar) -> case key of
        "name" -> manifest{manifestName = parseMaybe parseJSON scalar}
        "version" -> manifest{manifestVersion = parseMaybe parseJSON scalar}
        "dist" -> manifest{manifestDist = parseMaybe parseJSON scalar}
        "deprecated" -> manifest{manifestAvailability = availability (Just scalar)}
        "hasInstallScript" -> manifest{manifestInstallScript = parseMaybe parseJSON scalar}
        "scripts" -> manifest{manifestScripts = fromMaybe mempty <$> parseMaybe (parseJSON @(Maybe (Map Text Text))) scalar}
        "license" -> manifest{manifestLicense = parseMaybe parseJSON scalar}
        "_npmUser" -> manifest{manifestPublisher = isJust (parseMaybe (parseJSON @(Maybe Person)) scalar)}
        _ -> manifest
    _ -> manifest

finishFields :: ObservedFields -> VersionValue
finishFields = \case
    ReleaseFields manifest -> ReleaseValue $ do
        guard (manifestPublisher manifest)
        VersionManifest
            <$> manifestName manifest
            <*> manifestVersion manifest
            <*> manifestDist manifest
            <*> pure (manifestAvailability manifest)
            <*> manifestInstallScript manifest
            <*> manifestScripts manifest
            <*> manifestLicense manifest
    ObjectFields _ fields -> ScalarValue (Object fields)
    ScriptFields scripts -> ScriptValue scripts
    -- Wire validation needs the object's kind, without retaining its children.
    IgnoredFields -> ScalarValue (Object mempty)
