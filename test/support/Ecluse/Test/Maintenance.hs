-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | An in-memory 'Ecluse.Core.Registry.Maintenance.StoreMaintenance', the second
implementation of the handle. It answers from a seeded map that its own deletes mutate,
so a sweep driven against it observes the store changing. Its defaults take the opposite
arm of every backend-varying fact from the CodeArtifact leaf, which is what shows that a
fact is a value the handle supplies rather than a branch a caller takes.
-}
module Ecluse.Test.Maintenance (
    FakeStore (..),
    FakeStoreConfig (..),
    defaultFakeStoreConfig,
    newFakeStore,
) where

import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Maintenance (
    CompletionNotion (CompletesLater),
    ConsentVerdict (ConsentGranted),
    DeleteCeiling (AtMost),
    RefillPosture (RefillRefused),
    StoreClass (StoreDestroyable),
    StoreFacts (..),
    StoreFault,
    StoreMaintenance (..),
    StoredVersion (..),
    VersionOutcome (VersionRefused, VersionRemoving),
    storeRefusal,
    unreachedBatch,
 )
import Ecluse.Core.Version (Version)

-- | What a fake store holds and answers with.
data FakeStoreConfig = FakeStoreConfig
    { fakeContents :: Map PackageName [StoredVersion]
    -- ^ The packages and versions the store starts with.
    , fakeConsent :: ConsentVerdict
    , fakeClass :: StoreClass
    , fakeFacts :: StoreFacts
    , fakeFault :: Maybe StoreFault
    -- ^ When set, every call faults with it, so a caller's fault path is drivable.
    }

{- | A consenting, destroyable, empty store whose facts take the arm CodeArtifact does not:
a small ceiling, no re-publication, and a delete that finishes after the call.
-}
defaultFakeStoreConfig :: FakeStoreConfig
defaultFakeStoreConfig =
    FakeStoreConfig
        { fakeContents = Map.empty
        , fakeConsent = ConsentGranted
        , fakeClass = StoreDestroyable
        , fakeFacts =
            StoreFacts
                { factBackend = "fake"
                , factDeleteCeiling = AtMost 2
                , factRefill = RefillRefused
                , factCompletion = CompletesLater
                }
        , fakeFault = Nothing
        }

-- | A fake store: the handle a caller drives, and the contents a test asserts against.
data FakeStore = FakeStore
    { fakeMaintenance :: StoreMaintenance
    , readFakeContents :: IO (Map PackageName [StoredVersion])
    }

-- | Build a fake store over its seeded contents.
newFakeStore :: FakeStoreConfig -> IO FakeStore
newFakeStore config = do
    contents <- newIORef (fakeContents config)
    pure
        FakeStore
            { fakeMaintenance =
                StoreMaintenance
                    { storeFacts = fakeFacts config
                    , enumeratePackages = orFault config (Map.keys <$> readIORef contents)
                    , enumerateVersions = \name ->
                        orFault config (Map.findWithDefault [] name <$> readIORef contents)
                    , deleteVersions = \name versions -> case fakeFault config of
                        Just fault -> pure (unreachedBatch fault versions)
                        Nothing -> atomicModifyIORef' contents (removeVersions name versions)
                    , rehearseDelete = Just $ \name versions ->
                        snd . removeVersions name versions <$> readIORef contents
                    , verifyConsent = orFault config (pure (fakeConsent config))
                    , classifyStore = orFault config (pure (fakeClass config))
                    }
            , readFakeContents = readIORef contents
            }

-- Every read answers the configured fault instead, when there is one.
orFault :: FakeStoreConfig -> IO a -> IO (Either StoreFault a)
orFault config action = maybe (Right <$> action) (pure . Left) (fakeFault config)

{- Drop the named versions and report one outcome each. A version the store does not hold
is refused rather than reported gone, so a caller cannot mistake a miss for a delete. -}
removeVersions ::
    PackageName ->
    [Version] ->
    Map PackageName [StoredVersion] ->
    (Map PackageName [StoredVersion], [(Version, VersionOutcome)])
removeVersions name versions contents =
    (Map.adjust (filter kept) name contents, map outcome versions)
  where
    held = map storedVersion (Map.findWithDefault [] name contents)
    kept stored = storedVersion stored `notElem` versions
    outcome version
        | version `elem` held = (version, VersionRemoving "fake-operation")
        | otherwise =
            (version, VersionRefused (storeRefusal "NOT_FOUND" "the store holds no such version"))
