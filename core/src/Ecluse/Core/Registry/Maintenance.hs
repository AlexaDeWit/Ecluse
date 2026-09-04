-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The store maintenance handle: enumerate what a mirror store holds, and delete
versions from it. Enumeration and deletion are backend operations rather than ecosystem
ones, because the npm wire protocol has no enumeration at all and a managed registry
deletes through its own control plane. So the handle sits beside
"Ecluse.Core.Registry.Adapter" instead of inside it, one value per backend, resolved at
the Dredger's composition root. Every backend-varying fact is a value the handle
supplies rather than a branch a sweep takes, so a new backend is one more handle and no
change here.
-}
module Ecluse.Core.Registry.Maintenance (
    -- * The handle
    StoreMaintenance (..),

    -- * What the backend does
    StoreFacts (..),
    DeleteCeiling (..),
    RefillPosture (..),
    CompletionNotion (..),

    -- * Enumeration
    StoredVersion (..),
    VersionPresence (..),

    -- * Deletion
    VersionOutcome (..),
    StoreRefusal,
    storeRefusal,
    refusalCode,
    refusalDetail,
    unreachedBatch,

    -- * Backend-neutral drives
    pageAll,
    chunksOfCeiling,
    deleteAll,

    -- * Verdicts
    ConsentVerdict (..),
    StoreClass (..),

    -- * Faults
    StoreFault (..),
    RetryAdvice (..),
) where

import Data.Set qualified as Set

import Ecluse.Core.Fault (
    RetryAfter,
    TransportCause (TransportProtocol),
    TransportFault,
    boundedDetail,
    transportFault,
 )
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Version (Version)

{- | The maintenance capabilities of one mirror store. Like the other handles, the
effectful fields return __'IO', not @App@__, so an adapter never imports the proxy's @Env@.
-}
data StoreMaintenance = StoreMaintenance
    { storeFacts :: StoreFacts
    -- ^ What the backend does, readable without a call.
    , enumeratePackages :: IO (Either StoreFault [PackageName])
    {- ^ Every package the store holds. The adapter pages to exhaustion, so a caller
    sees a complete listing or a fault, never a page token.
    -}
    , enumerateVersions :: PackageName -> IO (Either StoreFault [StoredVersion])
    -- ^ Every version the store holds for one package, paged the same way.
    , deleteVersions :: PackageName -> [Version] -> IO [(Version, VersionOutcome)]
    {- ^ Delete versions of one package. The adapter splits the batch to its own ceiling,
    so any size is accepted and every version handed over gets exactly one outcome back.
    -}
    , rehearseDelete :: Maybe (PackageName -> [Version] -> IO [(Version, VersionOutcome)])
    {- ^ The backend's own dry run, where it has one: the outcomes a delete would report,
    with nothing deleted. 'Nothing' where a rehearsal has to stop short of the call.
    -}
    , verifyConsent :: IO (Either StoreFault ConsentVerdict)
    -- ^ Whether the operator has marked this store for deletion.
    , classifyStore :: IO (Either StoreFault StoreClass)
    -- ^ Whether deleting from this store destroys anything.
    }

{- | The backend's standing behaviour, fixed for the life of the handle. A sweep reads
these rather than asking which backend it is talking to.
-}
data StoreFacts = StoreFacts
    { factBackend :: Text
    -- ^ The backend's name, for the boot line that puts the Dredger's blast radius on record.
    , factDeleteCeiling :: DeleteCeiling
    -- ^ How many versions one destructive call accepts.
    , factRefill :: RefillPosture
    -- ^ What the backend does with a re-publication of a deleted version.
    , factCompletion :: CompletionNotion
    -- ^ When a delete is finished relative to the call that asked for it.
    }
    deriving stock (Eq, Show)

{- | Whether a version can come back under the same name after a delete. Recorded from
the backend's own documentation, never enforced here, so a sweep warns and never promises.
-}
data RefillPosture
    = -- | The backend accepts a re-publication of a version it deleted (CodeArtifact).
      RefillPermitted
    | {- | The backend refuses one, so a delete also retires the name for good (GCP
      Artifact Registry, for the npm format).
      -}
      RefillRefused
    deriving stock (Eq, Show)

{- | How many versions one destructive call accepts. A store with no control plane, an
object store walked by prefix for one, deletes an object at a time or a listing at once.
-}
data DeleteCeiling
    = -- | The backend takes a batch of any size, so a caller never splits one.
      NoCeiling
    | -- | The backend refuses a call carrying more than this many versions.
      AtMost Int
    deriving stock (Eq, Show)

-- | When a delete is finished, relative to the call that asked for it.
data CompletionNotion
    = -- | The delete is done by the time the call answers.
      CompletesOnCall
    | -- | The call starts a long-running operation, and the outcome names it.
      CompletesLater
    deriving stock (Eq, Show)

-- | One version an enumeration found, with what the store does with it now.
data StoredVersion = StoredVersion
    { storedVersion :: Version
    , storedPresence :: VersionPresence
    }
    deriving stock (Eq, Show)

{- | Whether the store still serves a version it holds. A backend lists a deleted version
too, so a sweep blind to this would re-issue a destructive call for it on every cycle.
-}
data VersionPresence
    = -- | The store serves the version, so deleting it removes something.
      VersionServed
    | -- | The store lists the version but no longer serves it.
      VersionWithdrawn
    deriving stock (Eq, Show)

-- | What became of one version a caller asked to delete.
data VersionOutcome
    = -- | The backend removed it before answering.
      VersionRemoved
    | {- | The backend accepted the removal and carries on, named by the reference an
      operator follows the work with.
      -}
      VersionRemoving Text
    | -- | The backend refused this one version and said why.
      VersionRefused StoreRefusal
    | -- | The call carrying this version did not reach the backend.
      VersionUnreached StoreFault
    deriving stock (Eq, Show)

{- | A backend's refusal of one version. Build it with 'storeRefusal' so the detail
stays bounded.
-}
data StoreRefusal = StoreRefusal
    { refusalCode :: Text
    -- ^ The backend's own code, which an operator looks up in its documentation.
    , refusalDetail :: Text
    -- ^ The backend's message, bounded to the shared log-line budget and never parsed.
    }
    deriving stock (Eq, Show)

-- | Build a 'StoreRefusal', truncating the detail to the log-line budget.
storeRefusal :: Text -> Text -> StoreRefusal
storeRefusal code detail = StoreRefusal code (boundedDetail detail)

{- | Mark a whole batch unreached, for when the call carrying it faulted. An adapter uses
this so a caller reads one outcome per version whether the call landed or not.
-}
unreachedBatch :: StoreFault -> [Version] -> [(Version, VersionOutcome)]
unreachedBatch fault versions = [(version, VersionUnreached fault) | version <- versions]

-- | Whether the operator has consented to deletion from this store.
data ConsentVerdict
    = -- | The store carries the consent marker.
      ConsentGranted
    | {- | It does not. The text is the backend's own how-to-attach descriptor, logged
      verbatim, because the marker is a tag on one backend and an object on another.
      -}
      ConsentWithheld Text
    deriving stock (Eq, Show)

-- | Whether deleting from this store destroys anything.
data StoreClass
    = -- | A private store that holds only what was published to it, so a delete is final.
      StoreDestroyable
    | {- | A store that refills itself from somewhere else, carrying why. A pull-through
      cache serves a deleted version again, so sweeping one changes nothing.
      -}
      StorePreserved Text
    deriving stock (Eq, Show)

{- | A maintenance call that produced no answer, classified once at the adapter edge. The
transport half is "Ecluse.Core.Fault"'s vocabulary, and the advice half is what to do next.
-}
data StoreFault = StoreFault
    { faultTransport :: TransportFault
    , faultRetry :: RetryAdvice
    }
    deriving stock (Eq, Show)

-- | What a caller does after a fault.
data RetryAdvice
    = -- | Another attempt fails the same way, so the caller stops.
      RetryFutile
    | -- | Worth another attempt, with no delay the backend asked for.
      RetryWorthwhile
    | -- | Worth another attempt, no sooner than the delay the backend itself asked for.
      RetryDelayed RetryAfter
    deriving stock (Eq, Show)

{- | Walk a paged listing to exhaustion. A store that returns a page token it has already
handed out would page forever, so that reads as a fault rather than a short listing.
-}
pageAll ::
    (Monad m) =>
    (Maybe Text -> m (Either StoreFault (Maybe Text, [a]))) ->
    m (Either StoreFault [a])
pageAll fetch = go Set.empty Nothing []
  where
    go seen token pages =
        fetch token >>= \case
            Left fault -> pure (Left fault)
            Right (next, page) -> case next of
                Nothing -> pure (Right (concat (reverse (page : pages))))
                Just following
                    | Set.member following seen -> pure (Left (repeatedTokenFault following))
                    | otherwise -> go (Set.insert following seen) (Just following) (page : pages)

-- A cycle in the store's own paging, which the next attempt reproduces.
repeatedTokenFault :: Text -> StoreFault
repeatedTokenFault token =
    StoreFault
        { faultTransport =
            transportFault TransportProtocol ("the store handed back a page token it had already given: " <> token)
        , faultRetry = RetryFutile
        }

{- | Split a batch into chunks the backend's destructive call accepts. A ceiling below one
would divide the batch forever, so it takes one item at a time instead.
-}
chunksOfCeiling :: DeleteCeiling -> [a] -> [[a]]
chunksOfCeiling ceiling' items = case ceiling' of
    NoCeiling -> [items | not (null items)]
    AtMost limit -> go (max 1 limit) items
  where
    go _ [] = []
    go size batch = let (chunk, rest) = splitAt size batch in chunk : go size rest

{- | Send each chunk in turn and collect one outcome per version. A faulted chunk stops the run,
because the fault carries the backend's own retry advice.
-}
deleteAll ::
    (Monad m) =>
    ([Version] -> m (Either StoreFault [(Version, VersionOutcome)])) ->
    [[Version]] ->
    m [(Version, VersionOutcome)]
deleteAll send = go []
  where
    go sent [] = pure (concat (reverse sent))
    go sent (chunk : rest) =
        send chunk >>= \case
            Left fault -> pure (concat (reverse sent) <> concatMap (unreachedBatch fault) (chunk : rest))
            Right outcomes -> go (outcomes : sent) rest
