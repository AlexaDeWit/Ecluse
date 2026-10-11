-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Prepare an owner-run index read and redact its evidence without applying artifact admission.
module Ecluse.Test.Registry.PyPI.Observation (
    SmokeInput (..),
    smokeInputName,
    ObservationConfig (..),
    observationConfig,
    ObservationError (..),
    renderObservationError,
    ObservationTarget,
    observationTarget,
    observationRequest,
    observeResponse,
) where

import Data.Aeson (eitherDecodeStrict)
import Data.Set qualified as Set
import Data.Text qualified as T
import Network.HTTP.Client (Request)
import Network.URI qualified as URI

import Ecluse.Core.Credential (ClientCredential (..), Secret)
import Ecluse.Core.Fault (TransportCause, tfCause)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (FetchFault (..), RegistryResponse (..))
import Ecluse.Core.Registry.PyPI.Project (canonicalName, projectName)
import Ecluse.Core.Registry.PyPI.Request (simpleIndexRequest, simpleIndexUrl)
import Ecluse.Core.Registry.PyPI.Wire (IndexFile (ifUrl), SimpleIndex (..))
import Ecluse.Core.Security.Authority (dialledAuthorityLabel, hostPortAddressWithDefault)
import Ecluse.Core.Security.Egress (mkConfiguredRegistryUrl, registryUrlText)

-- | Nonsecret opt-in inputs. AWS identity stays with the standard credential discovery chain.
data SmokeInput = RegionInput | DomainInput | OwnerInput | RepositoryInput | PackageInput
    deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | The environment key, never its configured value.
smokeInputName :: SmokeInput -> Text
smokeInputName input =
    "ECLTEST_SMOKE_CODEARTIFACT_" <> case input of
        RegionInput -> "REGION"
        DomainInput -> "DOMAIN"
        OwnerInput -> "DOMAIN_OWNER"
        RepositoryInput -> "REPOSITORY"
        PackageInput -> "PACKAGE"

-- | Parsed inputs for a single read. No registry URL override is accepted.
data ObservationConfig = ObservationConfig
    { ocRegion :: Text
    , ocDomain :: Text
    , ocOwner :: Maybe Text
    , ocRepository :: Text
    , ocPackage :: PackageName
    }

-- | Absent repository and package inputs preserve token-only use. Partial configuration fails.
observationConfig :: (SmokeInput -> Maybe Text) -> Either ObservationError (Maybe ObservationConfig)
observationConfig input
    | isNothing (input RepositoryInput) && isNothing (input PackageInput) = Right Nothing
    | otherwise = do
        region <- required RegionInput
        domain <- required DomainInput
        owner <- traverse (nonemptyInput OwnerInput) (input OwnerInput)
        repository <- required RepositoryInput
        package <- required PackageInput >>= first (const (InvalidInput PackageInput)) . projectName
        pure (Just (ObservationConfig region domain owner repository package))
  where
    required key = maybe (Left (InvalidInput key)) (nonemptyInput key) (input key)

nonemptyInput :: SmokeInput -> Text -> Either ObservationError Text
nonemptyInput key value
    | T.null (T.strip value) = Left (InvalidInput key)
    | otherwise = Right value

-- | Closed diagnostics exclude decoder messages, URLs, requests and exception details.
data ObservationError
    = InvalidInput SmokeInput
    | AwsDiscoveryFailed
    | EndpointDiscoveryFailed
    | EndpointMissing
    | EndpointInvalid
    | TokenMintFailed
    | IndexRequestInvalid
    | IndexExchangeFailed
    | IndexTransportFailed TransportCause
    | IndexBoundExceeded
    | IndexStatus Int
    | IndexDecodeFailed
    | IndexProjectMismatch
    | IndexMalformedEntries
    | IndexEmpty
    | FileReferenceInvalid
    deriving stock (Eq, Show)

-- | Only fixed labels and typed status or cause values reach the smoke report.
renderObservationError :: ObservationError -> Text
renderObservationError = \case
    InvalidInput key -> "invalid or missing " <> smokeInputName key
    other -> "CodeArtifact observation failed: " <> show other

-- | A discovered HTTPS endpoint and the exact index URL used to resolve file references.
data ObservationTarget = ObservationTarget
    { otEndpoint :: Text
    , otIndex :: URI.URI
    , otPackage :: PackageName
    }

-- | Refuse credential carriers and malformed authorities before a token can enter the request.
observationTarget :: Text -> PackageName -> Either ObservationError ObservationTarget
observationTarget raw package = do
    endpoint <- registryUrlText <$> first (const EndpointInvalid) (mkConfiguredRegistryUrl raw)
    endpointUri <- maybe (Left EndpointInvalid) Right (URI.parseURI (toString endpoint))
    _ <- first (const EndpointInvalid) (httpAuthority endpointUri)
    index <- first (const IndexRequestInvalid) (simpleIndexUrl endpoint package)
    indexUri <- maybe (Left IndexRequestInvalid) Right (URI.parseURI (toString index))
    pure (ObservationTarget endpoint indexUri package)

-- | CodeArtifact requires Basic username @aws@, rather than PyPI's default @__token__@.
observationRequest :: ObservationTarget -> Secret -> Either ObservationError Request
observationRequest target token =
    first (const IndexRequestInvalid) $
        simpleIndexRequest (otEndpoint target) (Just (ClientCredential (Just "aws") token)) (otPackage target)

-- | Refuse incomplete observations. Successful rows are sorted and distinct, with no raw paths.
observeResponse :: ObservationTarget -> Either FetchFault RegistryResponse -> Either ObservationError (NonEmpty Text)
observeResponse target response = do
    body <- first observationFetchError response
    unless (responseStatusCode body == 200) (Left (IndexStatus (responseStatusCode body)))
    index <- first (const IndexDecodeFailed) (eitherDecodeStrict (responseBody body))
    reported <- first (const IndexProjectMismatch) (projectName (siName index))
    unless (canonicalName reported == canonicalName (otPackage target)) (Left IndexProjectMismatch)
    unless (null (siInvalidEntries index)) (Left IndexMalformedEntries)
    files <- maybe (Left IndexEmpty) Right (nonEmpty (siFiles index))
    rows <- traverse (observeFile target . ifUrl) files
    maybe (Left IndexEmpty) Right (nonEmpty (Set.toAscList (Set.fromList (toList rows))))

observationFetchError :: FetchFault -> ObservationError
observationFetchError = \case
    FetchUrlUnformable _ -> IndexRequestInvalid
    FetchBoundExceeded _ -> IndexBoundExceeded
    FetchTransport fault -> IndexTransportFailed (tfCause fault)

observeFile :: ObservationTarget -> Text -> Either ObservationError Text
observeFile target raw = do
    when (T.null raw) (Left FileReferenceInvalid)
    reference <- maybe (Left FileReferenceInvalid) Right (URI.parseURIReference (toString raw))
    let resolved = reference `URI.relativeTo` otIndex target
    authority <- httpAuthority resolved
    let endpointAuthority = dialledAuthorityLabel (otEndpoint target)
    pure $
        T.intercalate
            " "
            [ "endpoint=" <> endpointAuthority
            , "file=" <> authority
            , "scheme=" <> T.dropEnd 1 (T.toLower (toText (URI.uriScheme resolved)))
            , "reference=" <> referenceKind reference
            , "same-authority=" <> boolean (authority == endpointAuthority)
            , "path=" <> redactedPath (toText (URI.uriPath resolved))
            , "query=" <> boolean (not (null (URI.uriQuery resolved)))
            , "fragment=" <> boolean (not (null (URI.uriFragment resolved)))
            ]

httpAuthority :: URI.URI -> Either ObservationError Text
httpAuthority uri = do
    let scheme = T.toLower (toText (URI.uriScheme uri))
        raw = toText (show uri :: String)
        port = if scheme == "http:" then 80 else 443
    unless (scheme == "https:" || scheme == "http:") (Left FileReferenceInvalid)
    authority <- maybe (Left FileReferenceInvalid) Right (URI.uriAuthority uri)
    when (null (URI.uriRegName authority)) (Left FileReferenceInvalid)
    _ <- maybe (Left FileReferenceInvalid) Right (hostPortAddressWithDefault port raw)
    pure (dialledAuthorityLabel raw)

referenceKind :: URI.URI -> Text
referenceKind uri
    | not (null (URI.uriScheme uri)) = "absolute"
    | isJust (URI.uriAuthority uri) = "scheme-relative"
    | "/" `isPrefixOf` URI.uriPath uri = "root-relative"
    | otherwise = "path-relative"

redactedPath :: Text -> Text
redactedPath = T.intercalate "/" . map redactedComponent . T.splitOn "/"

redactedComponent :: Text -> Text
redactedComponent component
    | component `elem` ["", "pypi", "simple"] = component
    | otherwise = "<segment>" <> fromMaybe "" (find (`T.isSuffixOf` T.toLower component) artifactExtensions)

artifactExtensions :: [Text]
artifactExtensions = [".tar.gz", ".whl", ".zip"]

boolean :: Bool -> Text
boolean value = if value then "true" else "false"
