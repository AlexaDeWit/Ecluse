-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TypeFamilies #-}

{- | npm policy facts collected beside the existing packed writer.
The walk still owns shapes, depth checks and first-member handling.
Only the three install hooks retain script names, and no command survives in typed facts.
A dependency spec is classified where it is read, and only an external one is kept.
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
import Data.Aeson (FromJSON (parseJSON), Value (..))
import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import Data.Primitive.MutVar (MutVar, newMutVar, readMutVar, writeMutVar)
import Data.Set qualified as Set

import Ecluse.Core.Package (Availability (Available))
import Ecluse.Core.Registry.Json.Intern (Entry, entryString, entryText, nameText)
import Ecluse.Core.Registry.Json.Packed (Packed)
import Ecluse.Core.Registry.Json.Shape (Build (..), MemberKey (..), memberText)
import Ecluse.Core.Registry.Json.Walk (Steps)
import Ecluse.Core.Registry.Json.Writer (Frame, Writer, discard, newWriter, sealValue)
import Ecluse.Core.Registry.Npm.DependencySource (ExternalDependency (ExternalDependency), ExternalSource, InstallableField (..), earlierDependency, externalSource)
import Ecluse.Core.Registry.Npm.Project (installHooks)
import Ecluse.Core.Registry.Npm.Wire (Dist, License, Person, VersionManifest (..), availability, distFields, distFromFields, licenseFromFields, personFromFields)

-- | One read's packed storage and the role of the next value.
data VersionWriter st = VersionWriter
    { packedWriter :: Writer st
    -- ^ The existing diagnostic decoder reads this writer's sealed values.
    , nextRole :: MutVar st Role
    }

-- | A completed value. Invalid facts require the existing whole-release diagnostic decoder.
data VersionValue
    = ScalarValue Value
    | IntegerValue Int
    | DistValue (Maybe Dist)
    | LicenseValue (Maybe License)
    | PersonValue Bool
    | StringCommand
    | ScriptValue (Maybe (Map Text Text))
    | ReleaseValue (Maybe VersionManifest)
    | ExternalSpec ExternalSource
    | -- A dependency spec, or a whole dependency map, that fetches nothing from outside the registry.
      NoExternalSpec
    | ExternalMap ExternalDependency
    | Unobserved

data Role = Release | ScalarField | DistObject | LicenseObject | PersonObject | Scripts | Command | DependencyMap InstallableField | DependencySpec | Ignore

data VersionFrame
    = VersionFrame Frame ObservedFields (Set Text)
    | IgnoredFrame Frame VersionValue

data ObservedFields
    = ReleaseFields Manifest
    | DistFields (Maybe Text) (Maybe (Maybe Text)) (Maybe (Maybe Text)) (Maybe Int)
    | LicenseFields (Maybe (Maybe Text)) (Maybe (Maybe Text))
    | PersonFields (Maybe (Maybe Text)) (Maybe (Maybe Text)) (Maybe (Maybe Text))
    | ScriptFields (Maybe (Map Text Text))
    | DependencyFields InstallableField (Maybe ExternalDependency)

data Manifest = Manifest
    { manifestName :: Maybe Text
    , manifestVersion :: Maybe Text
    , manifestDist :: Maybe Dist
    , manifestAvailability :: Availability
    , manifestInstallScript :: Maybe (Maybe Bool)
    , manifestScripts :: Maybe (Map Text Text)
    , manifestLicense :: Maybe (Maybe License)
    , manifestPublisher :: Bool
    , manifestExternal :: Maybe ExternalDependency
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
            observeString writer (entryText entry) (entryString entry) >>= next
    ownString writer name next =
        ownString (packedWriter writer) name $ \() ->
            observeString writer (nameText name) (String (nameText name)) >>= next
    integer writer number next =
        integer (packedWriter writer) number (\() -> observeScalar writer (IntegerValue number) >>= next)
    whole writer value next = whole (packedWriter writer) value $ \() -> case value of
        String text -> observeString writer text value >>= next
        _ -> observeScalar writer (ScalarValue value) >>= next
    emptyContainer writer next = emptyContainer (packedWriter writer) (\() -> observeScalar writer (ScalarValue (Array mempty)) >>= next)
    openObject writer next = openObject (packedWriter writer) $ \frame -> do
        role <- readMutVar (nextRole writer)
        case role of
            Ignore -> pure ()
            _ -> writeMutVar (nextRole writer) Ignore
        next $! case fieldsFor role of
            Just fields -> VersionFrame frame fields mempty
            Nothing -> IgnoredFrame frame $ case role of
                Ignore -> Unobserved
                _ -> ScalarValue (Object mempty)
    beginMember writer key frame next = case frame of
        IgnoredFrame packed _ -> beginMember (packedWriter writer) key packed next
        VersionFrame packed fields seen ->
            beginMember (packedWriter writer) key packed $ \repeated -> do
                let duplicate =
                        repeated || case key of
                            OwnKey _ -> Set.member (memberText key) seen
                            SharedKey _ -> False
                -- A role written lazily would allocate a thunk for every member, every dependency spec included.
                writeMutVar (nextRole writer) $! if duplicate then Ignore else memberRole fields (memberText key)
                next duplicate
    addMember writer key value frame next = case frame of
        -- The packed writer returns the same frame after each member.
        IgnoredFrame packed _ -> addMember (packedWriter writer) key () packed (const (next frame))
        VersionFrame packed fields seen ->
            addMember (packedWriter writer) key () packed $ \packed' ->
                case (fields, value, key) of
                    (ReleaseFields{}, Unobserved, _) -> next frame
                    (DistFields{}, Unobserved, _) -> next frame
                    (LicenseFields{}, Unobserved, _) -> next frame
                    (PersonFields{}, Unobserved, _) -> next frame
                    -- A spec under a table key changes nothing, and the packed writer already knows the key.
                    (_, NoExternalSpec, SharedKey _) -> next frame
                    _ -> next (VersionFrame packed' (addField (memberText key) value fields) (rememberOwn key value seen))
    dropValue writer _ = dropValue (packedWriter writer) ()
    closeObject writer frame next = case frame of
        IgnoredFrame packed value -> closeObject (packedWriter writer) packed (\() -> next value)
        VersionFrame packed fields _ -> closeObject (packedWriter writer) packed (\() -> next (finishFields fields))
    openArray writer next = do
        writeMutVar (nextRole writer) Ignore
        openArray (packedWriter writer) next
    addItem writer _ = addItem (packedWriter writer) ()
    closeArray writer count items next = closeArray (packedWriter writer) count items (\() -> next (ScalarValue (Array mempty)))
    {-# INLINE sharedString #-}
    {-# INLINE ownString #-}
    {-# INLINE integer #-}
    {-# INLINE whole #-}
    {-# INLINE emptyContainer #-}
    {-# INLINE openObject #-}
    {-# INLINE beginMember #-}
    {-# INLINE addMember #-}
    {-# INLINE dropValue #-}
    {-# INLINE closeObject #-}
    {-# INLINE openArray #-}
    {-# INLINE addItem #-}
    {-# INLINE closeArray #-}

-- A registry spec must classify without allocating: the npm full reads' allocation budgets hold a 1% margin.
observeString :: VersionWriter st -> Text -> Value -> ST st VersionValue
observeString writer text value = do
    role <- readMutVar (nextRole writer)
    pure $! case role of
        Command -> StringCommand
        DependencySpec -> maybe NoExternalSpec ExternalSpec (externalSource text)
        Ignore -> Unobserved
        _ -> ScalarValue value
{-# INLINE observeString #-}

observeScalar :: VersionWriter st -> VersionValue -> ST st VersionValue
observeScalar writer value = do
    role <- readMutVar (nextRole writer)
    pure $! case role of
        DependencySpec -> NoExternalSpec
        Ignore -> Unobserved
        _ -> value
{-# INLINE observeScalar #-}

rememberOwn :: MemberKey -> VersionValue -> Set Text -> Set Text
rememberOwn key value seen = case (key, value) of
    (OwnKey _, Unobserved) -> seen
    (OwnKey _, _) -> Set.insert (memberText key) seen
    (SharedKey _, _) -> seen

fieldsFor :: Role -> Maybe ObservedFields
fieldsFor = \case
    Release -> Just (ReleaseFields (Manifest Nothing Nothing Nothing Available (Just Nothing) (Just mempty) (Just Nothing) True Nothing))
    DependencyMap field -> Just (DependencyFields field Nothing)
    Scripts -> Just (ScriptFields (Just mempty))
    DistObject -> Just (DistFields Nothing (Just Nothing) (Just Nothing) Nothing)
    LicenseObject -> Just (LicenseFields (Just Nothing) (Just Nothing))
    PersonObject -> Just (PersonFields (Just Nothing) (Just Nothing) (Just Nothing))
    _ -> Nothing

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
        "dependencies" -> DependencyMap Dependencies
        "optionalDependencies" -> DependencyMap OptionalDependencies
        "peerDependencies" -> DependencyMap PeerDependencies
        _ -> Ignore
    DistFields{} -> if key `elem` distFields then ScalarField else Ignore
    LicenseFields{} -> if key == "type" || key == "url" then ScalarField else Ignore
    PersonFields{} -> if key == "name" || key == "email" || key == "url" then ScalarField else Ignore
    ScriptFields _ -> Command
    DependencyFields{} -> DependencySpec

addField :: Text -> VersionValue -> ObservedFields -> ObservedFields
addField "unpackedSize" (IntegerValue number) (DistFields tarball shasum integrity _) =
    DistFields tarball shasum integrity (force (Just number))
addField key (IntegerValue number) fields = addField key (ScalarValue (Number (fromIntegral number))) fields
addField key value observed = case observed of
    ReleaseFields manifest -> ReleaseFields (addManifest key value manifest)
    fields@(DistFields tarball shasum integrity size) -> case (key, value) of
        ("tarball", ScalarValue scalar) -> DistFields (parseMaybe parseJSON scalar) shasum integrity size
        ("shasum", ScalarValue scalar) -> DistFields tarball (parseMaybe parseJSON scalar) integrity size
        ("integrity", ScalarValue scalar) -> DistFields tarball shasum (parseMaybe parseJSON scalar) size
        ("unpackedSize", ScalarValue scalar) -> DistFields tarball shasum integrity (force (parseMaybe parseJSON scalar))
        _ -> fields
    fields@(LicenseFields kind url) -> case (key, value) of
        ("type", ScalarValue scalar) -> LicenseFields (parseMaybe parseJSON scalar) url
        ("url", ScalarValue scalar) -> LicenseFields kind (parseMaybe parseJSON scalar)
        _ -> fields
    fields@(PersonFields name email url) -> case (key, value) of
        ("name", ScalarValue scalar) -> PersonFields (parseMaybe parseJSON scalar) email url
        ("email", ScalarValue scalar) -> PersonFields name (parseMaybe parseJSON scalar) url
        ("url", ScalarValue scalar) -> PersonFields name email (parseMaybe parseJSON scalar)
        _ -> fields
    ScriptFields scripts -> ScriptFields $ do
        held <- scripts
        case value of
            StringCommand -> Just (keepHook key held)
            ScalarValue scalar -> parseMaybe (parseJSON @Text) scalar $> keepHook key held
            _ -> Nothing
    fields@(DependencyFields field held) -> case value of
        -- Chosen before it is wrapped, so a map of external specs holds no chain of thunks.
        ExternalSpec source ->
            let found = ExternalDependency field key source
                !chosen = maybe found (`earlierDependency` found) held
             in DependencyFields field (Just chosen)
        _ -> fields

keepHook :: Text -> Map Text Text -> Map Text Text
keepHook key scripts
    | key `elem` installHooks = Map.insert key "" scripts
    | otherwise = scripts

addManifest :: Text -> VersionValue -> Manifest -> Manifest
addManifest key value manifest = case (key, value) of
    ("dist", DistValue dist) -> manifest{manifestDist = dist}
    ("license", LicenseValue licence) -> manifest{manifestLicense = Just <$> licence}
    ("_npmUser", PersonValue valid) -> manifest{manifestPublisher = valid}
    ("scripts", ScriptValue scripts) -> manifest{manifestScripts = scripts}
    (_, ExternalMap found) ->
        let !chosen = maybe found (`earlierDependency` found) (manifestExternal manifest)
         in manifest{manifestExternal = Just chosen}
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
            <*> pure (manifestExternal manifest)
    DistFields tarball shasum integrity size -> DistValue (distFromFields tarball shasum integrity (Just size))
    LicenseFields kind url -> LicenseValue (licenseFromFields kind url)
    PersonFields name email url -> PersonValue (isJust (personFromFields name email url))
    ScriptFields scripts -> ScriptValue scripts
    DependencyFields _ held -> maybe NoExternalSpec ExternalMap held
