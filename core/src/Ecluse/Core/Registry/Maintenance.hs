-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Backend maintenance capabilities for a mirror store.
Enumeration and deletion may require a backend control plane beyond its package protocol.
The sweep reads backend limits and policies from the handle.
-}
module Ecluse.Core.Registry.Maintenance (
    -- * The handle
    StoreMaintenance (..),

    -- * Its two halves, held apart
    StoreObservation (..),
    StoreDeletion (..),
    DeleteGuard (..),
    DeletePhase (..),
    observationOf,
    deletionOf,

    -- * What the backend does
    StoreFacts (..),
    DeleteCeiling (..),
    RefillPosture (..),
    CompletionNotion (..),

    -- * Counting and pacing what a handle asks of the backend
    meteredObservation,
    meteredMaintenance,

    -- * Enumeration
    StoredVersion (..),
    VersionPresence (..),

    -- * The name space, walked in buckets
    NameAlphabet,
    mkNameAlphabet,
    noNameAlphabet,
    NamePrefix,
    wholeNameSpace,
    renderNamePrefix,
    parseNamePrefix,
    initialBuckets,
    extendBucket,
    inBucket,

    -- * Walk resumption
    StoreCursor (..),

    -- * Reading a package's metadata from the store
    StoreManifestRead,
    storeFaultOfFetch,
    storeFaultOfMetadata,
    protocolFault,
    unformableFault,

    -- * Deletion
    VersionOutcome (..),
    StoreRefusal,
    storeRefusal,
    refusalCode,
    refusalDetail,
    unreachedBatch,

    -- * Backend-neutral drives
    pageSource,
    collectPages,
    collectPagesBounded,
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

import Data.Conduit (ConduitT, await, fuseBoth, fuseBothMaybe, fuseUpstream, runConduit, yield)
import Data.Conduit.List qualified as CL
import Data.Set qualified as Set
import Data.Text qualified as T

import Ecluse.Core.Fault (
    RetryAfter,
    TransportCause (TransportProtocol),
    TransportFault,
    boundedDetail,
    tfCause,
    transportFault,
    transportRetryable,
 )
import Ecluse.Core.Fault.Http (isRetryableStatusCode)
import Ecluse.Core.Package (PackageName, unscopedName)
import Ecluse.Core.Registry (
    FetchFault (FetchBoundExceeded, FetchTransport, FetchUrlUnformable),
    UrlFormationError,
    renderUrlFormationError,
 )
import Ecluse.Core.Registry.Maintenance.Budget (
    RequestGate (gateSpend),
    RequestKind (CursorRead, CursorWrite, DeleteBatch, ListingPage, ManifestRead, PermissionRead, VersionPage),
    StoreBudget,
 )
import Ecluse.Core.Registry.Metadata (
    Manifest,
    MetadataError (MetadataAbsent, MetadataAuthorisationFailure, MetadataBoundExceeded, MetadataFetch, MetadataHttpFailure, MetadataNameMismatch, MetadataUndecodable),
 )
import Ecluse.Core.Version (Version)

-- | Backend operations for one store, independent of the application's runtime.
data StoreMaintenance = StoreMaintenance
    { storeFacts :: StoreFacts
    -- ^ What the backend does, readable without a call.
    , listPackagesIn :: NamePrefix -> ConduitT () [PackageName] IO (Maybe StoreFault)
    -- ^ Stream a bucket's pages, ending with its failure or 'Nothing' on completion.
    , enumerateVersions :: PackageName -> IO (Either StoreFault [StoredVersion])
    -- ^ Every version the store holds for one package, paged to exhaustion.
    , readStoreManifest :: StoreManifestRead
    -- ^ Read through the store's credential and ecosystem codec, including every stored version.
    , deleteVersions :: DeleteGuard -> PackageName -> [Version] -> IO [(Version, VersionOutcome)]
    -- ^ Accept any batch size and return exactly one outcome per supplied version.
    , verifyConsent :: IO (Either StoreFault ConsentVerdict)
    -- ^ Whether the operator has marked this store for deletion.
    , classifyStore :: IO (Either StoreFault StoreClass)
    -- ^ Whether deleting from this store destroys anything.
    , storeCursor :: Maybe StoreCursor
    -- ^ Optional persisted progress. Without it, every walk starts at the first bucket.
    }

{- | The calls that only observe a store. A caller handed one can enumerate it, read a package's
metadata, and read the two standing permissions, and can change nothing.
-}
data StoreObservation = StoreObservation
    { obFacts :: StoreFacts
    -- ^ What the backend does, readable without a call.
    , obListPackagesIn :: NamePrefix -> ConduitT () [PackageName] IO (Maybe StoreFault)
    -- ^ Stream a bucket's pages, ending with its failure or 'Nothing' on completion.
    , obEnumerateVersions :: PackageName -> IO (Either StoreFault [StoredVersion])
    -- ^ Every version the store holds for one package, paged to exhaustion.
    , obReadManifest :: StoreManifestRead
    -- ^ Read through the store's credential and ecosystem codec, including every stored version.
    , obVerifyConsent :: IO (Either StoreFault ConsentVerdict)
    -- ^ Whether the operator has marked this store for deletion.
    , obClassifyStore :: IO (Either StoreFault StoreClass)
    -- ^ Whether deleting from this store destroys anything.
    }

-- | The calls that change a store, which only a role authorised to delete from it holds.
data StoreDeletion = StoreDeletion
    { dlDeleteVersions :: DeleteGuard -> PackageName -> [Version] -> IO [(Version, VersionOutcome)]
    -- ^ Accept any batch size and return exactly one outcome per supplied version.
    , dlCursor :: Maybe StoreCursor
    -- ^ Optional persisted progress. Without it, every walk starts at the first bucket.
    }

-- | Observation after uncertainty must not reserve or announce another destructive attempt.
data DeletePhase
    = -- | Recheck and reserve immediately before a destructive attempt.
      BeforeDelete
    | -- | Inspect an uncertain result without reserving or announcing another attempt.
      AfterUncertain
    deriving stock (Eq, Show)

-- | The sweep rechecks authority inside backend-owned batches and bounds each uncertain retry.
data DeleteGuard = DeleteGuard
    { dgCheck :: DeletePhase -> [Version] -> IO (Either StoreFault [Version])
    , dgRetry :: StoreFault -> IO Bool
    }

-- | The observing half of a whole handle.
observationOf :: StoreMaintenance -> StoreObservation
observationOf store =
    StoreObservation
        { obFacts = storeFacts store
        , obListPackagesIn = listPackagesIn store
        , obEnumerateVersions = enumerateVersions store
        , obReadManifest = readStoreManifest store
        , obVerifyConsent = verifyConsent store
        , obClassifyStore = classifyStore store
        }

-- | The changing half of a whole handle.
deletionOf :: StoreMaintenance -> StoreDeletion
deletionOf store = StoreDeletion{dlDeleteVersions = deleteVersions store, dlCursor = storeCursor store}

{- | Count and pace every request the observing calls make. A version enumeration counts as one
request however many pages it takes, so a large package costs the backend more than was counted.
-}
meteredObservation :: RequestGate -> StoreObservation -> StoreObservation
meteredObservation gate observed =
    observed
        { obListPackagesIn = \prefix -> obListPackagesIn observed prefix `fuseUpstream` CL.mapM counted
        , obEnumerateVersions = \name -> spend VersionPage >> obEnumerateVersions observed name
        , obReadManifest = \name -> spend ManifestRead >> obReadManifest observed name
        , obVerifyConsent = spend PermissionRead >> obVerifyConsent observed
        , obClassifyStore = spend PermissionRead >> obClassifyStore observed
        }
  where
    spend = gateSpend gate
    -- The page is counted once it arrives, so the wait falls between it and the next request.
    counted page = spend ListingPage $> page

{- | The same metering over a whole handle. A delete counts one request per batch the backend's own
ceiling divides the versions into, whatever the batch then reports per version.
-}
meteredMaintenance :: RequestGate -> StoreMaintenance -> StoreMaintenance
meteredMaintenance gate handle =
    handle
        { listPackagesIn = obListPackagesIn observed
        , enumerateVersions = obEnumerateVersions observed
        , readStoreManifest = obReadManifest observed
        , verifyConsent = obVerifyConsent observed
        , classifyStore = obClassifyStore observed
        , deleteVersions = \checks name versions -> do
            traverse_ (const (gateSpend gate DeleteBatch)) (chunksOfCeiling (factDeleteCeiling (storeFacts handle)) versions)
            deleteVersions handle checks name versions
        , storeCursor = meteredCursor gate <$> storeCursor handle
        }
  where
    observed = meteredObservation gate (observationOf handle)

-- The marker reads and writes a full walk makes, each counted as its own request.
meteredCursor :: RequestGate -> StoreCursor -> StoreCursor
meteredCursor gate cursor =
    StoreCursor
        { readCursor = gateSpend gate CursorRead >> readCursor cursor
        , writeCursor = \prefix -> gateSpend gate CursorWrite >> writeCursor cursor prefix
        , clearCursor = gateSpend gate CursorWrite >> clearCursor cursor
        }

-- | Backend capabilities and limits fixed for this handle's lifetime.
data StoreFacts = StoreFacts
    { factBackend :: Text
    -- ^ The backend's name, for the boot line that puts the Dredger's blast radius on record.
    , factDeleteCeiling :: DeleteCeiling
    -- ^ How many versions one destructive call accepts.
    , factRefill :: RefillPosture
    -- ^ What the backend does with a re-publication of a deleted version.
    , factCompletion :: CompletionNotion
    -- ^ When a delete is finished relative to the call that asked for it.
    , factNameAlphabet :: NameAlphabet
    -- ^ The characters this store's name space is partitioned into buckets by.
    , factBudget :: StoreBudget
    -- ^ The request capacity this store runs under, which the sweep paces its next cycle by.
    }
    deriving stock (Eq, Show)

-- | The backend's documented re-publication policy after deletion, without an enforcement guarantee.
data RefillPosture
    = -- | The backend accepts a re-publication of a version it deleted (CodeArtifact).
      RefillPermitted
    | -- | Deletion permanently prevents re-publication under the same version name.
      RefillRefused
    deriving stock (Eq, Show)

-- | The maximum batch size supported by one backend deletion call.
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
    , storedRevision :: Maybe Text
    -- ^ Opaque backend revision, absent where the backend supplies none.
    }
    deriving stock (Eq, Show)

-- | Distinguish served versions from retained deletion records to avoid repeated deletion.
data VersionPresence
    = -- | The store serves the version, so deleting it removes something.
      VersionServed
    | -- | The store lists the version but no longer serves it.
      VersionWithdrawn
    deriving stock (Eq, Show)

-- | Permitted leading characters of ecosystem package names.
newtype NameAlphabet = NameAlphabet [Char]
    deriving stock (Eq, Show)

-- | Build an alphabet, dropping repeats and keeping the order given.
mkNameAlphabet :: [Char] -> NameAlphabet
mkNameAlphabet = NameAlphabet . ordNub

-- | Use a single whole-store bucket when the backend cannot filter its listing.
noNameAlphabet :: NameAlphabet
noNameAlphabet = NameAlphabet []

-- | A bucket prefix addresses the package's base name, excluding its namespace.
newtype NamePrefix = NamePrefix Text
    deriving stock (Eq, Ord, Show)

-- | The unfiltered whole-store bucket.
wholeNameSpace :: NamePrefix
wholeNameSpace = NamePrefix ""

-- | The prefix as a store filter and a walk cursor spell it. Empty stands for no filter at all.
renderNamePrefix :: NamePrefix -> Text
renderNamePrefix (NamePrefix raw) = raw

-- | Reject prefixes outside the current alphabet so an incompatible cursor restarts the walk.
parseNamePrefix :: NameAlphabet -> Text -> Maybe NamePrefix
parseNamePrefix (NameAlphabet chars) raw
    | T.all (`elem` chars) raw = Just (NamePrefix raw)
    | otherwise = Nothing

-- | Partition the store into disjoint buckets that cover every permitted name.
initialBuckets :: NameAlphabet -> NonEmpty NamePrefix
initialBuckets (NameAlphabet chars) =
    maybe (wholeNameSpace :| []) (fmap (NamePrefix . T.singleton)) (nonEmpty chars)

-- | Subdivide an oversized bucket. An empty alphabet permits no subdivision.
extendBucket :: NameAlphabet -> NamePrefix -> [NamePrefix]
extendBucket (NameAlphabet chars) (NamePrefix raw) =
    [NamePrefix (raw <> T.singleton ch) | ch <- chars]

-- | Whether a name falls in a bucket, for a store whose listing has no prefix filter of its own.
inBucket :: NamePrefix -> PackageName -> Bool
inBucket (NamePrefix raw) name = raw `T.isPrefixOf` unscopedName name

-- | Persist the last completed bucket so a restart repeats only unfinished work.
data StoreCursor = StoreCursor
    { readCursor :: IO (Either StoreFault (Maybe NamePrefix))
    -- ^ The bucket the last run completed, 'Nothing' when no walk is under way.
    , writeCursor :: NamePrefix -> IO (Either StoreFault ())
    -- ^ Record a completed bucket, replacing whatever was recorded before.
    , clearCursor :: IO (Either StoreFault ())
    -- ^ Forget the walk, which a completed one does so the next starts from the first bucket.
    }

-- | What became of one version a caller asked to delete.
data VersionOutcome
    = -- | The backend removed it before answering.
      VersionRemoved
    | -- | The backend accepted the removal and carries on, named by the reference an operator follows the work with.
      VersionRemoving Text
    | -- | The backend refused this one version and said why.
      VersionRefused StoreRefusal
    | -- | The call carrying this version did not reach the backend.
      VersionUnreached StoreFault
    | -- | A destructive call faulted after issue, so its effects need a fresh observation.
      VersionUncertain StoreFault
    deriving stock (Eq, Show)

-- | A backend's refusal of one version. Build it with 'storeRefusal' so the detail stays bounded.
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

-- | Give every version an unreached outcome when its batch call faults.
unreachedBatch :: StoreFault -> [Version] -> [(Version, VersionOutcome)]
unreachedBatch fault versions = [(version, VersionUnreached fault) | version <- versions]

-- | Whether the operator has consented to deletion from this store.
data ConsentVerdict
    = -- | The store carries the consent marker.
      ConsentGranted
    | -- | The required consent marker is absent. Carries the backend's instructions for adding it.
      ConsentWithheld Text
    deriving stock (Eq, Show)

-- | Whether deleting from this store destroys anything.
data StoreClass
    = -- | A private store that holds only what was published to it, so a delete is final.
      StoreDestroyable
    | -- | The store can refill deleted versions. Carries the reason deletion must be withheld.
      StorePreserved Text
    deriving stock (Eq, Show)

-- | An adapter-classified failure with transport details and retry advice.
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

-- | Stream pages until completion, a fault, or a repeated continuation token.
pageSource ::
    (Monad m) =>
    (Maybe Text -> m (Either StoreFault (Maybe Text, [a]))) ->
    ConduitT i [a] m (Maybe StoreFault)
pageSource fetch = go Set.empty Nothing
  where
    go seen token =
        lift (fetch token) >>= \case
            Left fault -> pure (Just fault)
            Right (next, page) -> do
                yield page
                case next of
                    Nothing -> pure Nothing
                    Just following
                        | Set.member following seen -> pure (Just (repeatedTokenFault following))
                        | otherwise -> go (Set.insert following seen) (Just following)

-- | Buffer a bounded listing, discarding collected pages if the stream faults.
collectPages :: (Monad m) => ConduitT () [a] m (Maybe StoreFault) -> m (Either StoreFault [a])
collectPages source = outcome <$> runConduit (fuseBoth source CL.consume)
  where
    outcome (mFault, pages) = maybe (Right (concat pages)) Left mFault

-- | Stop consuming pages at the item bound and return no partial inventory.
collectPagesBounded :: (Monad m) => Int -> ConduitT () [a] m (Maybe StoreFault) -> m (Either StoreFault [a])
collectPagesBounded limit source = outcome <$> runConduit (fuseBothMaybe source (consume 0 []))
  where
    consume held pages =
        await >>= \case
            Nothing -> pure (Just (concat (reverse pages)))
            Just page
                | length page > max 0 limit - held -> pure Nothing
                | otherwise -> consume (held + length page) (page : pages)
    outcome = \case
        (_, Nothing) -> Left (protocolFault "the store inventory crossed limits.maxVersionCount")
        (Just (Just fault), _) -> Left fault
        (_, Just values) -> Right values

-- | Collect one package's versions. Return a fault without partial results.
pageAll ::
    (Monad m) =>
    (Maybe Text -> m (Either StoreFault (Maybe Text, [a]))) ->
    m (Either StoreFault [a])
pageAll = collectPages . pageSource

-- A cycle in the store's own paging, which the next attempt reproduces.
repeatedTokenFault :: Text -> StoreFault
repeatedTokenFault token =
    StoreFault
        { faultTransport =
            transportFault TransportProtocol ("the store handed back a page token it had already given: " <> token)
        , faultRetry = RetryFutile
        }

-- | Read a package manifest using the store's credential and ecosystem codec.
type StoreManifestRead = PackageName -> IO (Either StoreFault Manifest)

-- | Only retryable transport faults warrant another attempt within the same cycle.
storeFaultOfFetch :: FetchFault -> StoreFault
storeFaultOfFetch = \case
    FetchTransport fault ->
        StoreFault
            { faultTransport = fault
            , faultRetry = if transportRetryable (tfCause fault) then RetryWorthwhile else RetryFutile
            }
    FetchBoundExceeded _ -> protocolFault "the store's answer crossed the response-size bound"
    FetchUrlUnformable err -> unformableFault err

-- | Preserve HTTP and transport retry advice. Absence and other terminal refusals advise no retry.
storeFaultOfMetadata :: MetadataError -> StoreFault
storeFaultOfMetadata = \case
    MetadataAbsent -> protocolFault "the store has no metadata for the requested package (HTTP 404)"
    MetadataHttpFailure code ->
        StoreFault
            { faultTransport = transportFault TransportProtocol ("the store refused the metadata read with HTTP " <> show code)
            , faultRetry = if isRetryableStatusCode code then RetryWorthwhile else RetryFutile
            }
    MetadataAuthorisationFailure _ -> protocolFault "the store refused metadata access"
    MetadataFetch fault -> storeFaultOfFetch fault
    MetadataBoundExceeded _ -> protocolFault "the store's metadata crossed a structural bound"
    MetadataUndecodable -> protocolFault "the store's metadata did not decode into a manifest"
    MetadataNameMismatch reported ->
        protocolFault ("the store's metadata reported another package's name: " <> reported)

-- | A URL the store's own coordinates could not form, reduced to its authority.
unformableFault :: UrlFormationError -> StoreFault
unformableFault err =
    protocolFault ("the store's request could not be formed: " <> renderUrlFormationError err)

-- | A fault in the store's own answer, which the next attempt reproduces.
protocolFault :: Text -> StoreFault
protocolFault detail =
    StoreFault{faultTransport = transportFault TransportProtocol detail, faultRetry = RetryFutile}

-- | Apply the backend batch limit, treating a non-positive limit as one.
chunksOfCeiling :: DeleteCeiling -> [a] -> [[a]]
chunksOfCeiling ceiling' items = case ceiling' of
    NoCeiling -> [items | not (null items)]
    AtMost limit -> go (max 1 limit) items
  where
    go _ [] = []
    go size batch = let (chunk, rest) = splitAt size batch in chunk : go size rest

-- | The backend owns chunks. A request fault stops later chunks, including after a guarded retry.
deleteAll ::
    DeleteGuard ->
    ([Version] -> IO (Either StoreFault [(Version, VersionOutcome)])) ->
    [[Version]] ->
    IO [(Version, VersionOutcome)]
deleteAll checks send = go []
  where
    go sent [] = pure (concat (reverse sent))
    go sent (chunk : rest) =
        dgCheck checks BeforeDelete chunk >>= \case
            Left fault -> pure (concat (reverse sent) <> concatMap (unreachedBatch fault) (chunk : rest))
            Right current -> do
                let permitted = filter (`elem` current) chunk
                    withheld = skipped (filter (`notElem` current) chunk)
                answer <- if null permitted then pure (Right []) else send permitted
                case answer of
                    Right outcomes -> go ((withheld <> outcomes) : sent) rest
                    Left fault -> do
                        void (dgCheck checks AfterUncertain permitted)
                        retry <- dgRetry checks fault
                        fresh <- if retry then dgCheck checks BeforeDelete permitted else pure (Right [])
                        outcomes <- retryBatch retry fault permitted fresh
                        pure (concat (reverse sent) <> withheld <> outcomes <> concatMap (unreachedBatch fault) rest)
    retryBatch retry fault issued fresh = case fresh of
        Right current | retry && not (null current) -> do
            let permitted = filter (`elem` current) issued
                unchanged = uncertain fault (filter (`notElem` current) issued)
            answer <- if null permitted then pure (Right []) else send permitted
            case answer of
                Right outcomes -> pure (unchanged <> outcomes)
                Left again -> do
                    void (dgCheck checks AfterUncertain permitted)
                    pure (unchanged <> uncertain again permitted)
        _ -> pure (uncertain fault issued)
    skipped = map (,VersionRefused (storeRefusal "REASSESSED" "current evidence does not authorise this delete"))
    uncertain fault = map (,VersionUncertain fault)
