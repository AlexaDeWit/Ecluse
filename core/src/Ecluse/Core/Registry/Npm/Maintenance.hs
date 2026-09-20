-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | npm listing and unpublish requests for the protocol maintenance backend.
The final version requires whole-package deletion, because an empty Verdaccio
packument truncates subsequent store listings.
-}
module Ecluse.Core.Registry.Npm.Maintenance (
    npmMaintenance,

    -- * The listing
    listingRequestFor,
    parsePackageListing,
    packageListingParser,

    -- * The unpublish
    packumentRequestFor,
    versionDeleteRequestsFor,
) where

import Data.Aeson (Object, Value (Object, String), decodeStrict, encode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.JsonStream.Parser qualified as J
import Data.Map.Strict qualified as Map
import Network.HTTP.Client (Request (method, requestHeaders))
import Network.HTTP.Types.Header (hAccept)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, unscopedName)
import Ecluse.Core.Registry (
    ParseError (ParseError),
    RegistryResponse (responseBody),
    UrlFormationError,
    renderUrlFormationError,
 )
import Ecluse.Core.Registry.Adapter.Capability (
    AdapterMaintenance (..),
    StoreListing (..),
    VersionDelete (..),
 )
import Ecluse.Core.Registry.JsonStream (StreamResult (streamValue), parseJsonChunks)
import Ecluse.Core.Registry.Maintenance (StoreRefusal, storeRefusal)
import Ecluse.Core.Registry.Maintenance.NameSpace (mkNameAlphabet)
import Ecluse.Core.Registry.Npm.Project (npmNameLeadChars, projectName)
import Ecluse.Core.Registry.Npm.Request (
    MetadataForm (Full),
    artifactFileUrl,
    jsonPutRequest,
    metadataRequest,
    packageUrl,
    withToken,
 )
import Ecluse.Core.Registry.Origin (OriginClient (ocToken), originBaseUrl)
import Ecluse.Core.Registry.Request (joinPath, parseRequestEither)
import Ecluse.Core.Registry.ServedDocument (adjustField, stringField)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit))
import Ecluse.Core.Server.Path (encodeComponent, isSafeComponent)
import Ecluse.Core.Text (nonBlank, urlFilenameComponent)
import Ecluse.Core.Version (Version, compareVersions, mkVersion, renderVersion)

-- | npm's maintenance slice. It fills both verbs, so an npm mount is sweepable.
npmMaintenance :: AdapterMaintenance
npmMaintenance =
    AdapterMaintenance
        { maintenanceListing =
            Just
                StoreListing
                    { listingRequest = listingRequestFor
                    , listingParser = packageListingParser
                    }
        , maintenanceVersionDelete =
            Just
                VersionDelete
                    { deleteDocumentRequest = packumentRequestFor
                    , deleteRequests = versionDeleteRequestsFor
                    }
        , maintenanceAlphabet = mkNameAlphabet npmNameLeadChars
        }

-- | Read the store listing. The caller classifies any response other than @200@.
listingRequestFor :: OriginClient -> Either UrlFormationError Request
listingRequestFor origin = do
    url <- joinPath (originBaseUrl origin) "-/all"
    base <- parseRequestEither url
    pure . withToken (ocToken origin) $
        base{requestHeaders = (hAccept, "application/json") : requestHeaders base}

-- | Ignore the @_updated@ bookkeeping key and keys that are not npm package names.
parsePackageListing :: ByteString -> Either ParseError [PackageName]
parsePackageListing body = do
    streamed <- first (ParseError . show) (parseJsonChunks (MetadataBodyLimit (BS.length body)) packageListingParser (\_ names -> Right names) [] [body])
    streamValue streamed

-- | Read only package-name keys. Values are skipped without constructing package objects.
packageListingParser :: J.Parser [PackageName]
packageListingParser = J.mapWithFailure finish (J.foldI collect Nothing events)
  where
    events = J.objectFound Nothing Nothing (Just . fst <$> J.objectItems (pure ()))
    collect found Nothing = Just (fromMaybe mempty found)
    collect found (Just raw) = Just $ case projectName raw of
        Right name | raw /= "_updated" -> Map.insert raw name (fromMaybe mempty found)
        _ -> fromMaybe mempty found
    finish Nothing = Left "the store's package listing is not a JSON object"
    finish (Just names) = Right (Map.elems names)

-- | Read the full packument, because the install view omits @_rev@ and @time@.
packumentRequestFor :: OriginClient -> PackageName -> Either UrlFormationError Request
packumentRequestFor origin =
    metadataRequest (originBaseUrl origin) (ocToken origin) Full

-- | Refuse absent versions and unreadable revisions. Delete the whole package only for its last version.
versionDeleteRequestsFor ::
    OriginClient ->
    PackageName ->
    Version ->
    RegistryResponse ->
    Either StoreRefusal (NonEmpty Request)
versionDeleteRequestsFor origin name version response = do
    packument <- decodePackument (responseBody response)
    revision <- revisionOf packument
    versions <- versionsOf packument
    manifest <- manifestOf raw versions
    if KeyMap.size versions == 1
        then deleteWholePackage origin revision name
        else
            deleteOneVersion
                origin
                revision
                name
                (tarballFilename name version manifest)
                (removeVersion raw versions packument)
  where
    raw = renderVersion version

deleteWholePackage :: OriginClient -> Text -> PackageName -> Either StoreRefusal (NonEmpty Request)
deleteWholePackage origin revision name = do
    request <- unformable (packageUrl (originBaseUrl origin) name >>= deleteAtRevision origin revision)
    pure (request :| [])

-- The requests run in order, so the packument edit goes first and a refused tarball delete
-- cannot leave the version still served.
deleteOneVersion :: OriginClient -> Text -> PackageName -> Text -> Object -> Either StoreRefusal (NonEmpty Request)
deleteOneVersion origin revision name filename edited = do
    editRequest <- unformable (packumentPutRequest origin name revision edited)
    tarballRequest <- unformable (artifactFileUrl (originBaseUrl origin) name filename >>= deleteAtRevision origin revision)
    pure (editRequest :| [tarballRequest])

-- A URL that will not form is this one version's refusal, with the URL reduced to its authority.
unformable :: Either UrlFormationError a -> Either StoreRefusal a
unformable = first (storeRefusal "UNFORMABLE_URL" . renderUrlFormationError)

decodePackument :: ByteString -> Either StoreRefusal Object
decodePackument body =
    maybeToRight
        (storeRefusal "UNREADABLE_DOCUMENT" "the store's packument is not a JSON object")
        (decodeStrict body)

-- Verdaccio does not enforce revision matching, so concurrent publishes can be lost.
revisionOf :: Object -> Either StoreRefusal Text
revisionOf packument = case KeyMap.lookup "_rev" packument of
    Just (String revision) | isSafeComponent revision -> Right revision
    _ ->
        Left (storeRefusal "NO_REVISION" "the store's packument carries no _rev an edit can address")

versionsOf :: Object -> Either StoreRefusal Object
versionsOf packument = case KeyMap.lookup "versions" packument of
    Just (Object versions) -> Right versions
    _ ->
        Left (storeRefusal "UNREADABLE_DOCUMENT" "the store's packument carries no versions object")

manifestOf :: Text -> Object -> Either StoreRefusal Value
manifestOf raw versions =
    maybeToRight
        (storeRefusal "VERSION_ABSENT" "the store's packument holds no such version")
        (KeyMap.lookup (Key.fromText raw) versions)

packumentPutRequest :: OriginClient -> PackageName -> Text -> Object -> Either UrlFormationError Request
packumentPutRequest origin name revision packument = do
    url <- atRevision revision <$> packageUrl (originBaseUrl origin) name
    jsonPutRequest (ocToken origin) url (toStrict (encode packument))

deleteAtRevision :: OriginClient -> Text -> Text -> Either UrlFormationError Request
deleteAtRevision origin revision url = do
    base <- parseRequestEither (atRevision revision url)
    pure
        . withToken (ocToken origin)
        $ base
            { method = "DELETE"
            , requestHeaders = (hAccept, "application/json") : requestHeaders base
            }

atRevision :: Text -> Text -> Text
atRevision revision url = url <> "/-rev/" <> encodeComponent revision

{- A @latest@ that pointed at the removed version moves to the greatest survivor, because a
packument without one leaves an unqualified install with no version to resolve. -}
removeVersion :: Text -> Object -> Object -> Object
removeVersion raw versions packument =
    KeyMap.insert "versions" (Object remaining) (adjustField "dist-tags" (withinObject retag) prunedTime)
  where
    key = Key.fromText raw
    remaining = KeyMap.delete key versions
    prunedTime = adjustField "time" (withinObject (KeyMap.delete key)) packument
    retag tags = maybe kept (\latest -> KeyMap.insert "latest" (String latest) kept) restoredLatest
      where
        kept = KeyMap.filter (/= String raw) tags
        restoredLatest = do
            guard (KeyMap.lookup "latest" tags == Just (String raw))
            greatestVersion (map Key.toText (KeyMap.keys remaining))

-- A slot holding anything but an object is left as the store sent it.
withinObject :: (Object -> Object) -> Value -> Value
withinObject edit = \case
    Object inner -> Object (edit inner)
    other -> other

greatestVersion :: [Text] -> Maybe Text
greatestVersion = foldl' keepGreater Nothing
  where
    keepGreater held candidate = Just (maybe candidate (greaterOf candidate) held)

greaterOf :: Text -> Text -> Text
greaterOf a b = if npmVersionOrdering a b == GT then a else b

-- Non-semver pairs fall back to text ordering, so the choice stays deterministic.
npmVersionOrdering :: Text -> Text -> Ordering
npmVersionOrdering a b = fromMaybe (compare a b) (compareVersions (mkVersion Npm a) (mkVersion Npm b))

tarballFilename :: PackageName -> Version -> Value -> Text
tarballFilename name version manifest =
    fromMaybe conventional (mfilter isSafeComponent (nonBlank =<< distTarballSegment manifest))
  where
    conventional = unscopedName name <> "-" <> renderVersion version <> ".tgz"

distTarballSegment :: Value -> Maybe Text
distTarballSegment manifest = urlFilenameComponent <$> tarballUrl manifest

tarballUrl :: Value -> Maybe Text
tarballUrl = \case
    Object manifest
        | Just (Object dist) <- KeyMap.lookup "dist" manifest ->
            stringField "tarball" dist
    _ -> Nothing
