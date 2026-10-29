-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Mirror-store probes, settle windows, and diagnostics for the local Verdaccio topology.
module Ecluse.E2E.Harness.Verdaccio (
    awaitMirrorWindow,
    verdaccioHasVersion,
    verdaccioHasVersionNow,
    verdaccioMirroredWithinWindow,
    verdaccioListing,
    verdaccioAwaitListed,
    verdaccioNamesUnder,
    verdaccioVersions,
    verdaccioVersionObject,
    verdaccioDocumentFacts,
    verdaccioAwaitVersions,
    verdaccioArtifact,
    verdaccioArtifactBytes,
    verdaccioLatest,
    verdaccioSnapshot,
) where

import Data.Aeson (Object, Value (Object, String), decodeStrict)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Client (
    Response,
    httpLbs,
    parseRequest,
    responseBody,
    responseStatus,
 )
import Network.HTTP.Types (statusCode)
import UnliftIO (handleAny)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Registry (ParseError (parseErrorMessage))
import Ecluse.Core.Registry.Adapter.Capability (AdapterMaintenance (maintenanceListing))
import Ecluse.Core.Registry.Maintenance (StoreObservation (obListPackagesIn), collectPages, storeFaultOfMetadata)
import Ecluse.Core.Registry.Maintenance.Protocol (ProtocolRead (..), newProtocolObservation)
import Ecluse.Core.Registry.Metadata.Fetch (fetchManifest)
import Ecluse.Core.Registry.Npm.Maintenance (npmMaintenance)
import Ecluse.Core.Registry.Npm.Metadata (npmRead)
import Ecluse.Core.Registry.Npm.Publish (npmPublishCodec)
import Ecluse.Core.Registry.Origin (originClient)
import Ecluse.Core.Rules.Install (unavailableInstallArtifactRead)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.E2E.Harness.Npm (npmArtifactPath)
import Ecluse.E2E.Harness.Types
import Ecluse.Test.Deadline (withExchangeClient)
import Ecluse.Test.Maintenance (withBucket)
import Ecluse.Test.Poll (pollUntil)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Registry.Npm.Maintenance (parsePackageListing)
import Ecluse.Test.Registry.Npm.StoredVersions (storedDocumentFacts, storedObjectResponse, storedVersionKeys)
import Ecluse.Test.Support (expectRightText)

-- | Wait out a worker's mirror window, so an absence read after it is not a race.
awaitMirrorWindow :: IO ()
awaitMirrorWindow = threadDelay 1500000

-- | Poll for a version, returning 'False' if the mirror does not serve it before the timeout.
verdaccioHasVersion :: E2E -> Text -> Text -> IO Bool
verdaccioHasVersion e2e pkg version =
    pollUntil 40 500000 id (verdaccioHasVersionNow e2e pkg version)

-- | Whether the mirror took a version, read once after 'awaitMirrorWindow' instead of polling until a timeout.
verdaccioMirroredWithinWindow :: E2E -> Text -> Text -> IO Bool
verdaccioMirroredWithinWindow e2e pkg version = do
    awaitMirrorWindow
    verdaccioHasVersionNow e2e pkg version

-- | Probe the exact version key. Only a readable document or HTTP 404 proves absence.
verdaccioHasVersionNow :: E2E -> Text -> Text -> IO Bool
verdaccioHasVersionNow e2e pkg version = elem version <$> verdaccioVersions e2e pkg

-- | Project package names from the store's @\/-\/all@ document, failing on an unreadable listing.
verdaccioListing :: E2E -> IO [Text]
verdaccioListing e2e = map renderPackageName <$> verdaccioPackages e2e

-- | Wait for a package to enter the listing, returning a diagnostic only when it never appears.
verdaccioAwaitListed :: E2E -> Text -> IO (Maybe Text)
verdaccioAwaitListed e2e pkg = do
    listed <- pollUntil 40 500000 id (handleAny (\_ -> pure False) (elem pkg <$> verdaccioListing e2e))
    if listed then pure Nothing else Just <$> unlistedReport e2e pkg

unlistedReport :: E2E -> Text -> IO Text
unlistedReport e2e pkg =
    handleAny (\err -> pure (preamble <> "the listing could not be read: " <> show err)) $ do
        body <- verdaccioListingBody e2e
        served <- packumentReport e2e pkg
        pure
            ( preamble
                <> "the projector read: "
                <> either parseErrorMessage (T.intercalate ", " . map renderPackageName) (parsePackageListing body)
                <> "\nthe same handle's packument for it: "
                <> served
                <> "\nthe store served (first 2048 characters): "
                <> T.take 2048 (decodeUtf8 body)
            )
  where
    preamble = "the store never listed " <> pkg <> ". "

packumentReport :: E2E -> Text -> IO Text
packumentReport e2e pkg =
    handleAny (\err -> pure ("unreadable: " <> show err)) $ do
        resp <- fetchPackument e2e pkg
        let body = LBS.toStrict (responseBody resp)
        pure
            ( "status "
                <> show (statusCode (responseStatus resp))
                <> ", time key "
                <> (if hasTimeKey body then "present" else "absent")
                <> ", first 512 characters: "
                <> T.take 512 (decodeUtf8 body)
            )

fetchPackument :: E2E -> Text -> IO (Response LBS.ByteString)
fetchPackument e2e pkg = do
    store <- serviceUrl <$> packageStore e2e
    req <- parseRequest (toString (store <> "/" <> pkg))
    httpLbs req (e2eManager e2e)

-- Verdaccio's listing entries all carry @time@, so its absence is worth reporting.
hasTimeKey :: ByteString -> Bool
hasTimeKey body = maybe False (KeyMap.member "time") (decodeStrict body :: Maybe Object)

-- | Walk a prefix through the store's own listing, including the empty prefix for the whole store.
verdaccioNamesUnder :: E2E -> Text -> IO [Text]
verdaccioNamesUnder e2e raw = do
    result <- withVerdaccioObservation e2e (\store -> withBucket raw (collectPages . obListPackagesIn store))
    names <- expectRightText (first (\fault -> "Verdaccio bucket " <> raw <> ": " <> show fault) result)
    pure (sort (map renderPackageName names))

-- | Every raw version key, failing on an unreadable packument. Only HTTP 404 means the package is absent.
verdaccioVersions :: E2E -> Text -> IO [Text]
verdaccioVersions = readStored storedVersionKeys

-- | The stored document's versions, tags, and deprecation markers, read past the proxy.
verdaccioDocumentFacts :: E2E -> Text -> IO [Text]
verdaccioDocumentFacts = readStored storedDocumentFacts

-- | One stored version's object as the store serves it. 'Nothing' for an absent package or version.
verdaccioVersionObject :: E2E -> Text -> Text -> IO (Maybe Object)
verdaccioVersionObject e2e name version = do
    versions <- storedObject "versions" e2e name
    pure (versions >>= KeyMap.lookup (Key.fromText version) >>= objectOf)
  where
    objectOf = \case
        Object o -> Just o
        _ -> Nothing

{- Read one top-level object field of the stored packument, rejecting an unreadable or malformed
document. Only HTTP 404 means the package is absent. -}
storedObject :: Key.Key -> E2E -> Text -> IO (Maybe Object)
storedObject field = readStored (storedObjectResponse field)

readStored :: (Int -> ByteString -> Either Text a) -> E2E -> Text -> IO a
readStored parse e2e name = do
    resp <- fetchPackument e2e name
    let status = statusCode (responseStatus resp)
        body = LBS.toStrict (responseBody resp)
    expectRightText (first (\err -> name <> ": " <> err) (parse status body))

-- | Poll until the store serves exactly the wanted sorted versions, yielding the last read so a failure names it.
verdaccioAwaitVersions :: E2E -> Text -> [Text] -> IO [Text]
verdaccioAwaitVersions e2e name wanted =
    pollUntil 40 500000 (== wanted) (verdaccioVersions e2e name)

-- | 'verdaccioArtifactBytes' reduced to the status and the body size.
verdaccioArtifact :: E2E -> Text -> Text -> IO (Int, Int64)
verdaccioArtifact e2e name version =
    second LBS.length <$> verdaccioArtifactBytes e2e name version

-- | One version's artifact status and bytes, read past the proxy to tell a stored artifact from a public fallback.
verdaccioArtifactBytes :: E2E -> Text -> Text -> IO (Int, LByteString)
verdaccioArtifactBytes e2e name version = do
    store <- serviceUrl <$> packageStore e2e
    req <- parseRequest (toString (store <> npmArtifactPath name version))
    resp <- httpLbs req (e2eManager e2e)
    pure (statusCode (responseStatus resp), responseBody resp)

-- | The store's own @dist-tags.latest@. 'Nothing' for an absent package or tag, a failure for an unreadable packument.
verdaccioLatest :: E2E -> Text -> IO (Maybe Text)
verdaccioLatest e2e name = do
    tags <- storedObject "dist-tags" e2e name
    pure (tags >>= KeyMap.lookup "latest" >>= textOf)
  where
    textOf = \case
        String raw -> Just raw
        _ -> Nothing

-- | Snapshot every listed package and its versions, failing if a packument cannot be read.
verdaccioSnapshot :: E2E -> IO (Map Text [Text])
verdaccioSnapshot e2e = do
    names <- verdaccioListing e2e
    Map.fromList <$> traverse (\name -> (name,) <$> verdaccioVersions e2e name) names

-- The store observed over the harness's manager, with its reads swept for the length of the action.
withVerdaccioObservation :: E2E -> (StoreObservation -> IO a) -> IO a
withVerdaccioObservation e2e action = do
    listing <- expectRightText (maybeToRight "npm has no listing capability" (maintenanceListing npmMaintenance))
    store <- serviceUrl <$> packageStore e2e
    withExchangeClient (pure (e2eManager e2e)) $ \client -> do
        let origin = originClient defaultLimits client (loopbackRegistryUrl store) Nothing
        action . newProtocolObservation $
            ProtocolRead
                { prOrigin = origin
                , prListing = listing
                , prCodec = npmPublishCodec
                , prReadManifest = fmap (first storeFaultOfMetadata) . fetchManifest npmRead passthroughTracingPort origin
                , prReadArtifact = unavailableInstallArtifactRead
                , prBackendName = "verdaccio"
                , prPermitDeletion = False
                , prConsentDescriptor = "these observing calls carry no deletion consent"
                }

verdaccioPackages :: E2E -> IO [PackageName]
verdaccioPackages e2e = do
    body <- verdaccioListingBody e2e
    either (fail . toString . parseErrorMessage) pure (parsePackageListing body)

verdaccioListingBody :: E2E -> IO ByteString
verdaccioListingBody e2e = do
    store <- serviceUrl <$> packageStore e2e
    req <- parseRequest (toString (store <> "/-/all"))
    resp <- httpLbs req (e2eManager e2e)
    when (statusCode (responseStatus resp) /= 200) (fail "the store served no package listing")
    pure (LBS.toStrict (responseBody resp))
