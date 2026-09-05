-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | npm's store maintenance verbs: the package listing @GET \/-\/all@, and the unpublish
sequence that removes one version. Pure request formation and projection, in the shape
"Ecluse.Core.Registry.Adapter.Capability" declares, so the backend leaf that drives them speaks
no npm. The unpublish is two requests, both required: a @PUT@ of the packument with the version
edited out, then a @DELETE@ of its tarball. Both address the document revision the edit was
formed from, which is why the sequence is re-read per version rather than batched.
-}
module Ecluse.Core.Registry.Npm.Maintenance (
    npmMaintenance,

    -- * The listing
    listingRequestFor,
    parsePackageListing,

    -- * The unpublish
    packumentRequestFor,
    versionDeleteRequestsFor,
) where

import Data.Aeson (Object, Value (Object, String), decodeStrict, encode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T
import Network.HTTP.Client (Request (method, requestBody, requestHeaders), RequestBody (RequestBodyBS))
import Network.HTTP.Types.Header (hAccept, hContentType)

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
import Ecluse.Core.Registry.Maintenance (StoreRefusal, storeRefusal)
import Ecluse.Core.Registry.Npm.Project (projectName)
import Ecluse.Core.Registry.Npm.Request (
    MetadataForm (Full),
    artifactFileUrl,
    metadataRequest,
    packageUrl,
    parseRequestEither,
    withToken,
 )
import Ecluse.Core.Registry.Origin (OriginClient (ocBaseUrl, ocToken))
import Ecluse.Core.Registry.Request (joinPath, noValidators)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Server.Path (encodeComponent, isSafeComponent)
import Ecluse.Core.Text (nonBlank)
import Ecluse.Core.Version (Version, compareVersions, mkVersion, renderVersion)

-- | npm's maintenance slice. It fills both verbs, so an npm mount is sweepable.
npmMaintenance :: AdapterMaintenance
npmMaintenance =
    AdapterMaintenance
        { maintenanceListing =
            Just
                StoreListing
                    { listingRequest = listingRequestFor
                    , listingParse = parsePackageListing
                    }
        , maintenanceVersionDelete =
            Just
                VersionDelete
                    { deleteDocumentRequest = packumentRequestFor
                    , deleteRequests = versionDeleteRequestsFor
                    }
        }

{- | Form the listing read @GET {base}\/-\/all@. A store that does not implement it answers
something other than @200@, which is the caller's to classify.
-}
listingRequestFor :: OriginClient -> Either UrlFormationError Request
listingRequestFor origin = do
    url <- joinPath (originBase origin) "-/all"
    base <- parseRequestEither url
    pure . withToken (ocToken origin) $
        base{requestHeaders = (hAccept, "application/json") : requestHeaders base}

{- | Project a listing body onto the names it holds. @\/-\/all@ carries an @_updated@
bookkeeping key beside the packages, and a key that is no npm name is dropped.
-}
parsePackageListing :: ByteString -> Either ParseError [PackageName]
parsePackageListing body = case decodeStrict body :: Maybe Object of
    Nothing -> Left (ParseError "the store's package listing is not a JSON object")
    Just listing ->
        Right
            [ name
            | key <- KeyMap.keys listing
            , let raw = Key.toText key
            , raw /= "_updated"
            , Right name <- [projectName raw]
            ]

{- | Form the packument read the unpublish edit is built from. It asks for the full form,
because the abbreviated install view carries neither @_rev@ nor @time@.
-}
packumentRequestFor :: OriginClient -> PackageName -> Either UrlFormationError Request
packumentRequestFor origin =
    metadataRequest (originBase origin) (ocToken origin) Full noValidators

{- | Form the two requests that remove one version: the edited packument, then its tarball.
Both carry the revision the fetched packument reported.
-}
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
    manifest <-
        maybeToRight
            (storeRefusal "VERSION_ABSENT" "the store's packument holds no such version")
            (KeyMap.lookup (Key.fromText raw) versions)
    let filename = tarballFilename name version manifest
        edited = removeVersion raw versions packument
    editRequest <- unformable (packumentPutRequest origin name revision edited)
    tarballRequest <- unformable (tarballDeleteRequest origin name filename revision)
    pure (editRequest :| [tarballRequest])
  where
    raw = renderVersion version

-- The base URL as characters, read once per formation rather than at every join.
originBase :: OriginClient -> Text
originBase = registryUrlText . ocBaseUrl

-- A URL that will not form is this one version's refusal, with the URL reduced to its authority.
unformable :: Either UrlFormationError a -> Either StoreRefusal a
unformable = first (storeRefusal "UNFORMABLE_URL" . renderUrlFormationError)

decodePackument :: ByteString -> Either StoreRefusal Object
decodePackument body =
    maybeToRight
        (storeRefusal "UNREADABLE_DOCUMENT" "the store's packument is not a JSON object")
        (decodeStrict body)

{- The revision marker addresses the edit at the document it was formed from. Verdaccio does
not check it, and a registry that does would apply the edit to a document nobody read. -}
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

-- A spec-compliant registry answers 415 unless the edited body is declared application/json.
packumentPutRequest :: OriginClient -> PackageName -> Text -> Object -> Either UrlFormationError Request
packumentPutRequest origin name revision packument = do
    url <- atRevision revision <$> packageUrl (originBase origin) name
    base <- parseRequestEither url
    pure
        . withToken (ocToken origin)
        $ base
            { method = "PUT"
            , requestBody = RequestBodyBS (toStrict (encode packument))
            , requestHeaders =
                (hContentType, "application/json")
                    : (hAccept, "application/json")
                    : requestHeaders base
            }

tarballDeleteRequest :: OriginClient -> PackageName -> Text -> Text -> Either UrlFormationError Request
tarballDeleteRequest origin name filename revision = do
    url <- atRevision revision <$> artifactFileUrl (originBase origin) name filename
    base <- parseRequestEither url
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
    KeyMap.insert "versions" (Object remaining) (adjustObject "dist-tags" retag prunedTime)
  where
    key = Key.fromText raw
    remaining = KeyMap.delete key versions
    prunedTime = adjustObject "time" (KeyMap.delete key) packument
    retag tags = maybe kept (\latest -> KeyMap.insert "latest" (String latest) kept) restoredLatest
      where
        kept = KeyMap.filter (/= String raw) tags
        restoredLatest = do
            guard (KeyMap.lookup "latest" tags == Just (String raw))
            greatestVersion (map Key.toText (KeyMap.keys remaining))

-- Apply an edit to an object-valued key, leaving a key that is absent or not an object alone.
adjustObject :: Key.Key -> (Object -> Object) -> Object -> Object
adjustObject key edit document = case KeyMap.lookup key document of
    Just (Object inner) -> KeyMap.insert key (Object (edit inner)) document
    _ -> document

{- The greatest of the surviving versions by npm's own ordering. A pair neither of which
parses falls back to comparing the raw text, so the choice is total and deterministic. -}
greatestVersion :: [Text] -> Maybe Text
greatestVersion = foldl' keepGreater Nothing
  where
    keepGreater held candidate = Just (maybe candidate (greater candidate) held)
    greater a b = if ordering a b == GT then a else b
    ordering a b = fromMaybe (compare a b) (compareVersions (mkVersion Npm a) (mkVersion Npm b))

{- The tarball's on-the-wire filename. @dist.tarball@ is what this store itself serves the
version from, and npm's conventional name stands in when the manifest carries none. -}
tarballFilename :: PackageName -> Version -> Value -> Text
tarballFilename name version manifest =
    fromMaybe conventional (mfilter isSafeComponent (nonBlank =<< distTarballSegment manifest))
  where
    conventional = unscopedName name <> "-" <> renderVersion version <> ".tgz"

{- The last path segment of @dist.tarball@, which the store chose and Écluse never validated.
A query or fragment is no part of the filename, so both are cut before the segment is taken. -}
distTarballSegment :: Value -> Maybe Text
distTarballSegment manifest = lastSegment . T.takeWhile inPath <$> tarballUrl manifest
  where
    inPath ch = ch /= '?' && ch /= '#'
    lastSegment = T.takeWhileEnd (/= '/')

tarballUrl :: Value -> Maybe Text
tarballUrl = \case
    Object manifest -> case KeyMap.lookup "dist" manifest of
        Just (Object dist) -> case KeyMap.lookup "tarball" dist of
            Just (String url) -> Just url
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing
