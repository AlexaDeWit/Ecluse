-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Mirror-store probes and diagnostics for the local Verdaccio end-to-end topology.
module Ecluse.E2E.Harness.Verdaccio (
    verdaccioHasVersion,
    verdaccioHasVersionNow,
    verdaccioListing,
    verdaccioAwaitListed,
    verdaccioNamesUnder,
    verdaccioVersions,
    verdaccioAwaitVersions,
    verdaccioArtifact,
    verdaccioLatest,
    verdaccioSnapshot,
) where

import Data.Aeson (Object, Value (String), decodeStrict, eitherDecodeStrict, (.:))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
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

import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Registry (ParseError (parseErrorMessage))
import Ecluse.Core.Registry.Adapter.Capability (AdapterMaintenance (maintenanceListing))
import Ecluse.Core.Registry.Maintenance (StoreObservation (obListPackagesIn), collectPages, storeFaultOfMetadata)
import Ecluse.Core.Registry.Maintenance.Protocol (ProtocolRead (..), newProtocolObservation)
import Ecluse.Core.Registry.Npm.Maintenance (npmMaintenance, parsePackageListing)
import Ecluse.Core.Registry.Npm.Metadata (fetchNpmManifest)
import Ecluse.Core.Registry.Npm.Publish (npmPublishCodec)
import Ecluse.Core.Registry.Origin (originClient)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.E2E.Harness.Types
import Ecluse.Test.Maintenance (withBucket)
import Ecluse.Test.Poll (pollUntil)
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Support (expectRight)

-- | Poll for a version, returning 'False' if the mirror does not serve it before the timeout.
verdaccioHasVersion :: E2E -> Text -> Text -> IO Bool
verdaccioHasVersion e2e pkg version =
    pollUntil 40 500000 id (verdaccioHasVersionNow e2e pkg version)

-- | Probe once for a version, returning 'False' on HTTP or transport failure.
verdaccioHasVersionNow :: E2E -> Text -> Text -> IO Bool
verdaccioHasVersionNow e2e pkg version =
    handleAny (\_ -> pure False) $ do
        resp <- fetchPackument e2e pkg
        pure
            ( statusCode (responseStatus resp) == 200
                && version `T.isInfixOf` decodeUtf8 (LBS.toStrict (responseBody resp))
            )

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
    req <- parseRequest (toString (e2eVerdaccio e2e <> "/" <> pkg))
    httpLbs req (e2eManager e2e)

-- Verdaccio's listing entries all carry @time@, so its absence is worth reporting.
hasTimeKey :: ByteString -> Bool
hasTimeKey body = maybe False (KeyMap.member "time") (decodeStrict body :: Maybe Object)

-- | Walk a prefix through the store's own listing, including the empty prefix for the whole store.
verdaccioNamesUnder :: E2E -> Text -> IO [Text]
verdaccioNamesUnder e2e raw = do
    store <- verdaccioObservation e2e
    result <- withBucket raw (collectPages . obListPackagesIn store)
    names <- expectRight (first (\fault -> "Verdaccio bucket " <> raw <> ": " <> show fault) result)
    pure (sort (map renderPackageName names))

{- | Preserve every raw version key, rejecting unreadable or malformed packuments.
Only HTTP 404 means the package is absent.
-}
verdaccioVersions :: E2E -> Text -> IO [Text]
verdaccioVersions e2e name = do
    resp <- fetchPackument e2e name
    let status = statusCode (responseStatus resp)
        body = LBS.toStrict (responseBody resp)
        versions = eitherDecodeStrict body >>= parseEither (.: "versions") :: Either String Object
    case status of
        404 -> pure []
        200 -> sort . map Key.toText . KeyMap.keys <$> expectRight (first (\err -> name <> ": " <> toText err) versions)
        _ -> expectRight (Left (name <> ": packument returned HTTP " <> show status) :: Either Text [Text])

{- | Poll until the store serves exactly the wanted versions, sorted as 'verdaccioVersions' returns
them. It yields what it last read, so a failure names the versions the store actually held.
-}
verdaccioAwaitVersions :: E2E -> Text -> [Text] -> IO [Text]
verdaccioAwaitVersions e2e name wanted =
    pollUntil 40 500000 (== wanted) (handleAny (\_ -> pure []) (verdaccioVersions e2e name))

{- | Read one version's artifact straight from the store, returning the status and the body size.
It bypasses the proxy, so a case can tell a stored artifact from a public-leg fallback.
-}
verdaccioArtifact :: E2E -> Text -> Text -> IO (Int, Int64)
verdaccioArtifact e2e name version = do
    req <- parseRequest (toString (e2eVerdaccio e2e <> "/" <> name <> "/-/" <> name <> "-" <> version <> ".tgz"))
    resp <- httpLbs req (e2eManager e2e)
    pure (statusCode (responseStatus resp), LBS.length (responseBody resp))

{- | The store's own @dist-tags.latest@ target. 'Nothing' for an absent package or an absent tag,
and a failure for an unreadable packument.
-}
verdaccioLatest :: E2E -> Text -> IO (Maybe Text)
verdaccioLatest e2e name = do
    resp <- fetchPackument e2e name
    let status = statusCode (responseStatus resp)
        body = LBS.toStrict (responseBody resp)
        tags = eitherDecodeStrict body >>= parseEither (.: "dist-tags") :: Either String Object
    case status of
        404 -> pure Nothing
        200 -> do
            decoded <- expectRight (first (\err -> name <> ": " <> toText err) tags)
            pure (KeyMap.lookup "latest" decoded >>= textOf)
        _ -> expectRight (Left (name <> ": packument returned HTTP " <> show status) :: Either Text (Maybe Text))
  where
    textOf = \case
        String raw -> Just raw
        _ -> Nothing

-- | Snapshot every listed package and its versions, failing if a packument cannot be read.
verdaccioSnapshot :: E2E -> IO (Map Text [Text])
verdaccioSnapshot e2e = do
    names <- verdaccioListing e2e
    Map.fromList <$> traverse (\name -> (name,) <$> verdaccioVersions e2e name) names

verdaccioObservation :: E2E -> IO StoreObservation
verdaccioObservation e2e = do
    listing <- expectRight (maybeToRight ("npm has no listing capability" :: Text) (maintenanceListing npmMaintenance))
    let origin = originClient defaultLimits (e2eManager e2e) (loopbackRegistryUrl (e2eVerdaccio e2e)) Nothing
    pure . newProtocolObservation $
        ProtocolRead
            { prOrigin = origin
            , prListing = listing
            , prCodec = npmPublishCodec
            , prReadManifest = fmap (first storeFaultOfMetadata) . fetchNpmManifest passthroughTracingPort origin
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
    req <- parseRequest (toString (e2eVerdaccio e2e <> "/-/all"))
    resp <- httpLbs req (e2eManager e2e)
    when (statusCode (responseStatus resp) /= 200) (fail "the store served no package listing")
    pure (LBS.toStrict (responseBody resp))
