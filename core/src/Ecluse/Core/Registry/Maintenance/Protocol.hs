-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Maintenance through registry protocol endpoints.
Deletion needs the raw document revision, with listing bounds separate from serve-path bounds.
Consent and refill classification rely on operator configuration.
-}
module Ecluse.Core.Registry.Maintenance.Protocol (
    ProtocolRead (..),
    ProtocolStore (..),
    newProtocolObservation,
    newProtocolMaintenance,
) where

import Data.Conduit (ConduitT, yield)
import Network.HTTP.Client (Request)

import Ecluse.Core.Credential (ClientCredential (credSecret), Secret)
import Ecluse.Core.Fault (
    TransportCause (TransportProtocol),
    transportFault,
 )
import Ecluse.Core.Fault.Http (isRetryableStatusCode)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (
    ParseError (parseErrorMessage),
    RegistryResponse (RegistryResponse),
    UrlFormationError,
    isSuccessStatus,
 )
import Ecluse.Core.Registry.Adapter.Capability (
    StoreListing (listingParse, listingRequest),
    VersionDelete (deleteDocumentRequest, deleteRequests),
 )
import Ecluse.Core.Registry.Exchange (boundedExchange, formThen)
import Ecluse.Core.Registry.Maintenance (
    CompletionNotion (CompletesOnCall),
    ConsentVerdict (ConsentGranted, ConsentWithheld),
    DeleteCeiling (AtMost),
    NamePrefix,
    RefillPosture (RefillPermitted),
    RetryAdvice (RetryFutile, RetryWorthwhile),
    StoreClass (StoreDestroyable, StorePreserved),
    StoreFacts (..),
    StoreFault (..),
    StoreMaintenance (..),
    StoreManifestRead,
    StoreObservation (..),
    StoreRefusal,
    StoredVersion (StoredVersion, storedPresence, storedVersion),
    VersionOutcome (VersionRefused, VersionRemoved),
    VersionPresence (VersionServed),
    chunksOfCeiling,
    deleteAll,
    inBucket,
    noNameAlphabet,
    protocolFault,
    storeFaultOfFetch,
    storeRefusal,
    unformableFault,
 )
import Ecluse.Core.Registry.Origin (OriginClient (ocBaseUrl, ocLimits, ocManager, ocToken))
import Ecluse.Core.Registry.Publish (PublishCodec (pcParseVersionList, pcProbeRequest))
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Version (Version)

{- | One protocol-only store as a reader reaches it: where it is, how its protocol enumerates it,
and the consent an operator declared for it. Nothing here changes the store.
-}
data ProtocolRead = ProtocolRead
    { prOrigin :: OriginClient
    -- ^ The store's coordinates, its credential, and the bound every read is held to.
    , prListing :: StoreListing
    -- ^ The ecosystem's package listing verb.
    , prCodec :: PublishCodec
    -- ^ The ecosystem's publish codec, whose presence probe already reads a store's version list for the mirror worker.
    , prReadManifest :: StoreManifestRead
    -- ^ One package's metadata as this store serves it, assembled at the composition root.
    , prBackendName :: Text
    -- ^ The store backend's name, which the boot line records the Dredger's blast radius as.
    , prPermitDeletion :: Bool
    -- ^ Whether the operator marked this store for deletion.
    , prConsentDescriptor :: Text
    -- ^ How an operator marks it, logged verbatim when consent is withheld.
    }

-- | One protocol-only store a caller may delete from: its reads, beside the verb that removes a version.
data ProtocolStore = ProtocolStore
    { psRead :: ProtocolRead
    , psDelete :: VersionDelete
    }

-- | The calls that only enumerate and read, built without the delete verb.
newProtocolObservation :: ProtocolRead -> StoreObservation
newProtocolObservation store =
    StoreObservation
        { obFacts = protocolFacts (prBackendName store)
        , obListPackagesIn = listBucket store
        , obEnumerateVersions = listVersions store
        , obReadManifest = prReadManifest store
        , obVerifyConsent = pure (Right (consentVerdict store))
        , obClassifyStore = pure (Right (storeClass store))
        }

-- | Delete versions individually because each edit changes the document revision needed by the next.
newProtocolMaintenance :: ProtocolStore -> StoreMaintenance
newProtocolMaintenance store =
    StoreMaintenance
        { storeFacts = obFacts observed
        , listPackagesIn = obListPackagesIn observed
        , enumerateVersions = obEnumerateVersions observed
        , readStoreManifest = obReadManifest observed
        , deleteVersions = deleteStoredVersions store
        , verifyConsent = obVerifyConsent observed
        , classifyStore = obClassifyStore observed
        , -- The protocol writes nothing but a publish, so a walk over this store keeps no cursor.
          storeCursor = Nothing
        }
  where
    observed = newProtocolObservation (psRead store)

{- The store re-admits a version published again after a delete, and has applied it by the time it
answers. It reports no alphabet: the listing below reads one document whole, bucket or no bucket. -}
protocolFacts :: Text -> StoreFacts
protocolFacts backend =
    StoreFacts
        { factBackend = backend
        , factDeleteCeiling = deleteCeiling
        , factRefill = RefillPermitted
        , factCompletion = CompletesOnCall
        , factNameAlphabet = noNameAlphabet
        }

{- The delete edit addresses the document revision it was formed from, and applying one changes
that revision, so a batch of two would send the second against a revision that no longer exists. -}
deleteCeiling :: DeleteCeiling
deleteCeiling = AtMost 1

consentVerdict :: ProtocolRead -> ConsentVerdict
consentVerdict store
    | prPermitDeletion store = ConsentGranted
    | otherwise = ConsentWithheld (prConsentDescriptor store)

{- No protocol read can see whether this store refills itself from an uplink, so the operator's
own key is the only evidence either way. -}
storeClass :: ProtocolRead -> StoreClass
storeClass store
    | prPermitDeletion store = StoreDestroyable
    | otherwise = StorePreserved (prConsentDescriptor store)

{- One bucket of the store's names, as the single page its one listing document holds. The
protocol spells no prefix filter, so the bucket is applied to what came back. -}
listBucket :: ProtocolRead -> NamePrefix -> ConduitT () [PackageName] IO (Maybe StoreFault)
listBucket store prefix =
    lift (listPackages store) >>= \case
        Left fault -> pure (Just fault)
        Right names -> Nothing <$ yield (filter (inBucket prefix) names)

listPackages :: ProtocolRead -> IO (Either StoreFault [PackageName])
listPackages store =
    sendFormed store (listingRequest (prListing store) (prOrigin store)) <&> \case
        Left fault -> Left fault
        Right (status, body)
            | status == 200 -> first (parseFault "package listing") (listingParse (prListing store) body)
            | otherwise -> Left (listingUnavailable status)

listingUnavailable :: Int -> StoreFault
listingUnavailable status =
    StoreFault
        { faultTransport =
            transportFault
                TransportProtocol
                ( "the store answered the package listing with HTTP "
                    <> show status
                    <> if status == 404 then ": it serves no enumeration this sweep can walk" else ""
                )
        , faultRetry = if isRetryableStatusCode status then RetryWorthwhile else RetryFutile
        }

{- The presence probe's read, which already projects a store's version list for the mirror
worker. A store that holds no document for a package holds no versions of it either. -}
listVersions :: ProtocolRead -> PackageName -> IO (Either StoreFault [StoredVersion])
listVersions store name =
    sendFormed store (pcProbeRequest (prCodec store) (originBase store) (originToken store) name) <&> \case
        Left fault -> Left fault
        Right (status, body)
            | status == 404 -> Right []
            | isSuccessStatus status -> first (parseFault "version list") (served status body)
            | otherwise -> Left (readFault "version list" status)
  where
    served status body = map stored <$> pcParseVersionList (prCodec store) (RegistryResponse status body)
    stored version = StoredVersion{storedVersion = version, storedPresence = VersionServed}

deleteStoredVersions :: ProtocolStore -> PackageName -> [Version] -> IO [(Version, VersionOutcome)]
deleteStoredVersions store name versions =
    deleteAll (deleteChunk store name) (chunksOfCeiling deleteCeiling versions)

{- One version at a time: re-read the document, form the protocol's request sequence over it,
and send each in turn. A refusal is this version's alone, and a fault ends the whole run. -}
deleteChunk :: ProtocolStore -> PackageName -> [Version] -> IO (Either StoreFault [(Version, VersionOutcome)])
deleteChunk store name = \case
    [version] ->
        sendFormed (psRead store) (deleteDocumentRequest (psDelete store) (prOrigin (psRead store)) name) >>= \case
            Left fault -> pure (Left fault)
            Right (status, body)
                | status == 404 -> pure (refused version absentDocument)
                | not (isSuccessStatus status) -> pure (Left (readFault "document" status))
                | otherwise -> applyDelete store name version status body
    -- 'deleteCeiling' splits to one, so a wider chunk refuses whole rather than losing its tail.
    chunk -> pure (Right [(version, VersionRefused oversizedChunk) | version <- chunk])
  where
    absentDocument = storeRefusal "NOT_FOUND" "the store holds no document for this package"
    oversizedChunk = storeRefusal "CEILING_EXCEEDED" "this protocol deletes one version per call"

applyDelete :: ProtocolStore -> PackageName -> Version -> Int -> ByteString -> IO (Either StoreFault [(Version, VersionOutcome)])
applyDelete store name version status body =
    case deleteRequests (psDelete store) (prOrigin (psRead store)) name version (RegistryResponse status body) of
        Left refusal -> pure (refused version refusal)
        Right requests ->
            sendSequence (psRead store) (toList requests) <&> fmap outcomeOf
  where
    outcomeOf = \case
        Nothing -> [(version, VersionRemoved)]
        Just refusal -> [(version, VersionRefused refusal)]

{- Send each request in order, stopping at the first refusal. That can leave the version
half-removed, so the code an operator looks up names which call stopped. -}
sendSequence :: ProtocolRead -> [Request] -> IO (Either StoreFault (Maybe StoreRefusal))
sendSequence store = go (1 :: Int)
  where
    go _ [] = pure (Right Nothing)
    go position (request : rest) =
        send store request >>= \case
            Left fault -> pure (Left fault)
            Right (status, _)
                | isSuccessStatus status -> go (position + 1) rest
                | otherwise -> pure (Right (Just (refusedAt position status)))

    refusedAt position status =
        storeRefusal
            ("HTTP " <> show status)
            ("the store refused request " <> show position <> " of the delete sequence")

refused :: Version -> StoreRefusal -> Either StoreFault [(Version, VersionOutcome)]
refused version refusal = Right [(version, VersionRefused refusal)]

send :: ProtocolRead -> Request -> IO (Either StoreFault (Int, ByteString))
send store request =
    first storeFaultOfFetch
        <$> boundedExchange (,) (ocManager origin) (ocLimits origin) request
  where
    origin = prOrigin store

sendFormed :: ProtocolRead -> Either UrlFormationError Request -> IO (Either StoreFault (Int, ByteString))
sendFormed store = formThen unformableFault (send store)

originBase :: ProtocolRead -> Text
originBase = registryUrlText . ocBaseUrl . prOrigin

originToken :: ProtocolRead -> Maybe Secret
originToken = fmap credSecret . ocToken . prOrigin

parseFault :: Text -> ParseError -> StoreFault
parseFault subject err =
    protocolFault ("the store's " <> subject <> " did not parse: " <> parseErrorMessage err)

-- Version and document reads retain their server-error-only retry policy.
readFault :: Text -> Int -> StoreFault
readFault subject status =
    StoreFault
        { faultTransport =
            transportFault TransportProtocol ("the store answered the " <> subject <> " read with HTTP " <> show status)
        , faultRetry = if status >= 500 then RetryWorthwhile else RetryFutile
        }
