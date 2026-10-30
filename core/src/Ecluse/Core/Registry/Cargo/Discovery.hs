-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Origin-scoped sparse discovery and checksum-bound native download templates.
module Ecluse.Core.Registry.Cargo.Discovery (
    CargoDiscovery,
    cargoDiscovery,
    parseDiscoveryWithDepth,
    discoveryDl,
    discoveryApi,
    discoveryIndex,
    renderCargoDiscovery,
    CargoArtifactCoordinate,
    cargoArtifactCoordinate,
    coordinateName,
    coordinateVersion,
    coordinateFilename,
    coordinateChecksum,
    bindCargoDownload,
) where

import Data.Aeson (Value (..), encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Char (isSpace)
import Data.Text qualified as T

import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Package (Hash, HashAlg (SHA256), hashAlg, hashValue, mkHash)
import Ecluse.Core.Package.Filter.Internal (ArtifactOrigin, artifactOrigin, checkArtifactAddress)
import Ecluse.Core.Registry (FetchFault (FetchUrlUnformable))
import Ecluse.Core.Registry.Cargo.Project (cargoArchiveFilename, projectName)
import Ecluse.Core.Registry.Cargo.Request (cargoArtifactHosts, cargoConfigRequest, cargoPrefix)
import Ecluse.Core.Registry.Cargo.Wire (CargoRecordError (..), decodeCargoRecord)
import Ecluse.Core.Registry.Exchange (boundedIncrementalRead, formThen, withSuccessBodyUsing)
import Ecluse.Core.Registry.Metadata (MetadataError (MetadataBoundExceeded, MetadataUndecodable), metadataResponse)
import Ecluse.Core.Registry.Origin (OriginClient (..), originBaseUrl)
import Ecluse.Core.Registry.Public (ReadControl (..))
import Ecluse.Core.Security (AllowedHostPorts, BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge), Limits (maxMetadataBytes, maxNestingDepth, progressFloor), ecosystemArtifactAuthorities)
import Ecluse.Core.Server.Path (Filename, encodeComponent, mkFilename)
import Ecluse.Core.Telemetry.Upstream (PublicReadKind (Listing))
import Ecluse.Core.Text (httpPrefix, httpsPrefix, isPrefixOfLowered)
import Ecluse.Core.Version (Version, mkVersion, renderVersion)
import Ecluse.Core.Version.Cargo (parseCargo)

-- | A checked response from one authorised index origin. Its URLs grant no new authority.
data CargoDiscovery = CargoDiscovery !Text !(Maybe Text) !Text !ArtifactOrigin

-- | The checked native template, with markers still unexpanded.
discoveryDl :: CargoDiscovery -> Text
discoveryDl (CargoDiscovery template _ _ _) = template

-- | The advertised API remains a location hint, never an upload credential grant.
discoveryApi :: CargoDiscovery -> Maybe Text
discoveryApi (CargoDiscovery _ api _ _) = api

-- | The complete selected index root, including any configured base path.
discoveryIndex :: CargoDiscovery -> Text
discoveryIndex (CargoDiscovery _ _ index _) = index

discoveryOrigin :: CargoDiscovery -> ArtifactOrigin
discoveryOrigin (CargoDiscovery _ _ _ origin) = origin

-- | Read bounded discovery through the same transport and public control as index metadata.
cargoDiscovery :: OriginClient -> IO (Either MetadataError CargoDiscovery)
cargoDiscovery origin = do
    response <- formThen FetchUrlUnformable exchange (cargoConfigRequest (ocBaseRequest origin) (ocToken origin))
    pure $ do
        (_, bytes) <- metadataResponse response
        parseDiscoveryWithDepth (min 32 (maxNestingDepth limits)) (ecosystemArtifactAuthorities cargoArtifactHosts) (originBaseUrl origin) bytes
  where
    limits = ocLimits origin
    bound = MetadataBodyLimit (min 65536 (maxMetadataBytes limits))
    control = maybe Uncontrolled (\handle -> Controlled handle (fromMaybe Listing (ocReadKind origin))) (ocPublicControl origin)
    exchange = withSuccessBodyUsing control (ocClient origin) (progressFloor limits) (boundedIncrementalRead (ocChargeFullRead origin) bound)

-- | Validate bounded discovery against predeclared authority and the selected origin.
parseDiscoveryWithDepth :: Int -> AllowedHostPorts -> Text -> ByteString -> Either MetadataError CargoDiscovery
parseDiscoveryWithDepth depth hosts origin bytes = do
    unless (BS.length bytes <= 65536) (Left (MetadataBoundExceeded (BodyTooLarge (MetadataBodyLimit 65536))))
    value <- first recordError (decodeCargoRecord depth bytes)
    first (const MetadataUndecodable) (finishDiscovery value)
  where
    recordError = \case
        CargoRecordBound fault -> MetadataBoundExceeded fault
        CargoRecordMalformed _ -> MetadataUndecodable
    finishDiscovery value = do
        fields <- case value of
            Object members -> Right members
            _ -> Left "discovery is not an object"
        dl <- requiredText "dl" fields
        unless (validTemplate dl) (Left "invalid Cargo download template")
        let authority = artifactOrigin hosts origin
        _ <- checkedAddress authority (templateExample dl)
        api <- case KeyMap.lookup "api" fields of
            Nothing -> Right Nothing
            Just Null -> Right Nothing
            Just (String url) -> Just url <$ checkedAbsolute url
            _ -> Left "invalid Cargo API URL"
        case KeyMap.lookup "auth-required" fields of
            Nothing -> Right ()
            Just (Bool _) -> Right ()
            _ -> Left "invalid Cargo authentication declaration"
        pure (CargoDiscovery dl api origin authority)

-- | Local discovery uses only the supplied local routes and authentication requirement.
renderCargoDiscovery :: Text -> Maybe Text -> Bool -> ByteString
renderCargoDiscovery dl api auth = toStrict (encode (object (["dl" .= dl, "auth-required" .= auth] <> maybe [] (\url -> ["api" .= url]) api)))

-- | Exact native claims for one archive, independent of its download URL's final component.
data CargoArtifactCoordinate = CargoArtifactCoordinate !Text !Version !Filename !Hash
    deriving stock (Eq, Show)

-- | Exact published name, including native case and punctuation.
coordinateName :: CargoArtifactCoordinate -> Text
coordinateName (CargoArtifactCoordinate name _ _ _) = name

-- | Native raw version, including build metadata.
coordinateVersion :: CargoArtifactCoordinate -> Version
coordinateVersion (CargoArtifactCoordinate _ version _ _) = version

-- | The checked logical archive name does not depend on the URL's terminal component.
coordinateFilename :: CargoArtifactCoordinate -> Filename
coordinateFilename (CargoArtifactCoordinate _ _ filename _) = filename

-- | The validated SHA-256 from the same native row as this coordinate.
coordinateChecksum :: CargoArtifactCoordinate -> Hash
coordinateChecksum (CargoArtifactCoordinate _ _ _ checksum) = checksum

-- | Bind published spelling, raw version and SHA-256 before any template interpolation.
cargoArtifactCoordinate :: Text -> Version -> Hash -> Either Text CargoArtifactCoordinate
cargoArtifactCoordinate name version checksum = do
    _ <- first (const "invalid Cargo package name") (projectName name)
    _ <- maybeToRight "invalid Cargo version" (parseCargo (renderVersion version))
    unless (hashAlg checksum == SHA256) (Left "Cargo requires SHA-256")
    _ <- mkHash SHA256 (hashValue checksum)
    filename <- maybeToRight "invalid Cargo archive filename" (mkFilename (cargoArchiveFilename name version))
    pure (CargoArtifactCoordinate name (mkVersion Cargo (renderVersion version)) filename checksum)

-- | Expand only native markers and recheck the address against the captured origin.
bindCargoDownload :: CargoDiscovery -> CargoArtifactCoordinate -> Either Text Text
bindCargoDownload discovery coordinate = checkedAddress (discoveryOrigin discovery) expanded
  where
    template = discoveryDl discovery
    name = coordinateName coordinate
    prefix = cargoPrefix name
    replacements =
        [ ("{crate}", encodeComponent name)
        , ("{version}", encodeComponent (renderVersion (coordinateVersion coordinate)))
        , ("{prefix}", prefix)
        , ("{lowerprefix}", T.toLower prefix)
        , ("{sha256-checksum}", hashValue (coordinateChecksum coordinate))
        ]
    expanded
        | "{" `T.isInfixOf` template = foldl' (\text (marker, value) -> T.replace marker value text) template replacements
        | otherwise = T.dropWhileEnd (== '/') template <> "/" <> encodeComponent name <> "/" <> encodeComponent (renderVersion (coordinateVersion coordinate)) <> "/download"

requiredText :: Key.Key -> KeyMap.KeyMap Value -> Either Text Text
requiredText key fields = case KeyMap.lookup key fields of
    Just (String text) -> Right text
    _ -> Left "missing Cargo download template"

validTemplate :: Text -> Bool
validTemplate template =
    not (T.null template)
        && not (T.any isSpace template)
        && not (T.any (`elem` ['{', '}']) withoutMarkers)
        && ("{" `T.isInfixOf` template || not (T.any (`elem` ['?', '#']) template))
  where
    withoutMarkers = foldl' (\text marker -> T.replace marker "" text) template markers

markers :: [Text]
markers = ["{crate}", "{version}", "{prefix}", "{lowerprefix}", "{sha256-checksum}"]

templateExample :: Text -> Text
templateExample template = foldl' (\text marker -> T.replace marker "cargo" text) template markers

checkedAbsolute :: Text -> Either Text ()
checkedAbsolute url = do
    unless ((isPrefixOfLowered httpsPrefix url || isPrefixOfLowered httpPrefix url) && not (T.any isSpace url)) (Left "invalid absolute Cargo URL")
    let authority = T.takeWhile (`notElem` ['/', '?', '#']) (T.drop 3 (snd (T.breakOn "://" url)))
    unless (not (T.null authority) && not (T.any (`elem` ['@', '{', '}']) authority) && not ("#" `T.isInfixOf` url)) (Left "invalid Cargo authority")

checkedAddress :: ArtifactOrigin -> Text -> Either Text Text
checkedAddress origin url = checkedAbsolute url >> first (const "unapproved Cargo URL authority or scheme") (checkArtifactAddress origin url)
