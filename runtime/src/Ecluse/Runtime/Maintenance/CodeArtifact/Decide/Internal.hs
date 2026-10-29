-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | CodeArtifact request construction, inventory parsing and maintenance decisions.
module Ecluse.Runtime.Maintenance.CodeArtifact.Decide.Internal (
    -- * Coordinates
    CodeArtifactStore (..),
    CodeArtifactFormat,
    codeArtifactFormat,
    formatEcosystem,
    formatToken,

    -- * What the backend does
    codeArtifactFacts,
    deleteCeiling,
    listPageLimit,

    -- * Native coordinates
    packageCoordinates,
    packageNameFrom,

    -- * Requests
    listPackagesRequest,
    listVersionsRequest,
    listVersionsResult,
    deleteRequest,
    describeRepositoryRequest,
    describeUpstreamRequest,
    listTagsRequest,

    -- * The walk cursor
    cursorTagKey,
    cursorTagRequest,
    cursorUntagRequest,
    cursorOfTags,

    -- * Responses
    packagesOfPage,
    presenceOf,
    foldDeleteResponse,
    classifyRepository,
    repositoryOfStore,
    upstreamLinksOf,
    consentOfTags,
    repositoryOfResponse,
    arnOfDescription,

    -- * Consent marker
    consentTagKey,
    consentTagValue,
    consentDescriptor,

    -- * Faults
    classifyStoreFault,
    describeUpstreamRefusal,
    describeRepositoryGrant,
) where

import Amazonka qualified as AWS
import Amazonka.CodeArtifact qualified as CA
import Amazonka.CodeArtifact.Lens qualified as CAL
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Lens.Micro ((.~), (?~), (^.))
import Network.HTTP.Types (Header, statusCode)
import Network.HTTP.Types.Header (hRetryAfter)

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems), ecosystemName)
import Ecluse.Core.Fault (
    RetryAfter (RetryAfter),
    TransportCause (TransportProtocol),
    tfCause,
    transportFault,
    transportRetryable,
 )
import Ecluse.Core.Package (PackageName, mkPackageName, mkScope, pkgNamespace, unScope, unscopedName)
import Ecluse.Core.Registry (ParseError)
import Ecluse.Core.Registry.Cargo.Project qualified as Cargo
import Ecluse.Core.Registry.Maintenance (
    CompletionNotion (CompletesOnCall),
    ConsentVerdict (ConsentGranted, ConsentWithheld),
    DeleteCeiling (AtMost),
    RefillPosture (RefillPermitted),
    RetryAdvice (RetryDelayed, RetryFutile, RetryWorthwhile),
    StoreClass (StoreDestroyable, StorePreserved),
    StoreFacts (..),
    StoreFault (..),
    StoreRefusal,
    VersionOutcome (VersionRefused, VersionRemoved),
    VersionPresence (VersionServed, VersionWithdrawn),
    protocolFault,
    storeRefusal,
 )
import Ecluse.Core.Registry.Maintenance.Budget (
    QuotaDimension (AccountReads, AccountWrites, NameListing, TokenReads, VersionListing),
    QuotaOrigin (QuotaDocumented),
    QuotaScope,
    RequestKind (CursorRead, CursorWrite, DeleteBatch, ListingPage, ManifestRead, PermissionRead, VersionPage),
    StoreBudget (StoreBudget, bgCosts, bgOrigin, bgQuotas, bgScope),
    mkQuotaScope,
 )
import Ecluse.Core.Registry.Maintenance.NameSpace (
    NameAlphabet,
    NamePrefix,
    parseNamePrefix,
    renderNamePrefix,
 )
import Ecluse.Core.Registry.Maintenance.Upstream (
    ExternalConnection (ExternalConnection, externalConnectionText),
    PermissionName (PermissionName),
    RepositoryLinks (RepositoryLinks, rlConnections, rlUpstreams),
    RepositoryName (RepositoryName, repositoryNameText),
    UndecidabilityReason (NetworkFailure),
    UnsafeReason (InsufficientPermissions),
    UpstreamSafety (Undecidable, Unsafe),
 )
import Ecluse.Core.Registry.RubyGems.Project qualified as RubyGems
import Ecluse.Core.Text (nonBlank, readDecimalText)
import Ecluse.Core.Version (Version, renderVersion)
import Ecluse.Runtime.Aws.Fault (classifyAwsTransport, errorCodeText)

-- | Vetted repository coordinates and native format from the composition root.
data CodeArtifactStore = CodeArtifactStore
    { casDomain :: Text
    , casDomainOwner :: Text
    -- ^ The 12-digit account number that owns the domain.
    , casRegion :: Text
    , casRepository :: Text
    , casFormat :: CodeArtifactFormat
    }
    deriving stock (Eq, Show)

-- | A native format token bound to its ecosystem.
data CodeArtifactFormat = CodeArtifactFormat Ecosystem CA.PackageFormat
    deriving stock (Eq, Show)

-- | The format CodeArtifact addresses an ecosystem by, 'Nothing' where it has none.
codeArtifactFormat :: Ecosystem -> Maybe CodeArtifactFormat
codeArtifactFormat = \case
    Npm -> Just (CodeArtifactFormat Npm CA.PackageFormat_Npm)
    PyPI -> Just (CodeArtifactFormat PyPI CA.PackageFormat_Pypi)
    Cargo -> Just (CodeArtifactFormat Cargo (CA.PackageFormat' "cargo"))
    RubyGems -> Just (CodeArtifactFormat RubyGems (CA.PackageFormat' "ruby"))

-- | The ecosystem a format was built from.
formatEcosystem :: CodeArtifactFormat -> Ecosystem
formatEcosystem (CodeArtifactFormat eco _) = eco

-- | Native format token and first segment of the repository endpoint.
formatToken :: CodeArtifactFormat -> Text
formatToken (CodeArtifactFormat _ token) = CA.fromPackageFormat token

-- The format CodeArtifact addresses a store's own ecosystem by.
storePackageFormat :: CodeArtifactStore -> CA.PackageFormat
storePackageFormat store = case casFormat store of
    CodeArtifactFormat _ token -> token

-- | Deletion completes on response and permits later refill, under the ecosystem's name alphabet.
codeArtifactFacts :: NameAlphabet -> CodeArtifactStore -> StoreFacts
codeArtifactFacts alphabet store =
    StoreFacts
        { factBackend = "codeArtifact"
        , factDeleteCeiling = deleteCeiling
        , factRefill = RefillPermitted
        , factCompletion = CompletesOnCall
        , factNameAlphabet = alphabet
        , factBudget = codeArtifactBudget{bgScope = codeArtifactScope store}
        }

codeArtifactScope :: CodeArtifactStore -> QuotaScope
codeArtifactScope store = mkQuotaScope (casDomainOwner store <> "." <> casRegion store)

codeArtifactBudget :: StoreBudget
codeArtifactBudget =
    StoreBudget
        { bgScope = mkQuotaScope ""
        , bgQuotas =
            Map.fromList
                [ (NameListing, 200)
                , (VersionListing, 200)
                , (AccountReads, 800)
                , (AccountWrites, 100)
                , (TokenReads, 1200)
                ]
        , bgOrigin = QuotaDocumented
        , bgCosts = Map.fromList codeArtifactCosts
        }

{- The pools each call is charged to. Assigning the account read and write dimensions to the IAM
access level is a conservative inference: AWS publishes no exhaustive operation-to-quota map. -}
codeArtifactCosts :: [(RequestKind, Map QuotaDimension Rational)]
codeArtifactCosts =
    [ (ListingPage, Map.fromList [(NameListing, 1), (AccountReads, 1)])
    , (VersionPage, Map.fromList [(VersionListing, 1), (AccountReads, 1)])
    , (ManifestRead, Map.fromList [(AccountReads, 1), (TokenReads, 1)])
    , (PermissionRead, Map.fromList [(AccountReads, 1)])
    , (CursorRead, Map.fromList [(AccountReads, 1)])
    , (CursorWrite, Map.fromList [(AccountWrites, 1)])
    , (DeleteBatch, Map.fromList [(AccountWrites, 1)])
    ]

-- | The most versions one @DeletePackageVersions@ call accepts.
deleteCeiling :: DeleteCeiling
deleteCeiling = AtMost 100

-- | The largest @maxResults@ the list calls accept. AWS documents no default page size.
listPageLimit :: Natural
listPageLimit = 1000

-- | Native coordinates, with an npm scope represented as a namespace without its sigil.
packageCoordinates :: PackageName -> (Maybe Text, Text)
packageCoordinates name = (unScope <$> pkgNamespace name, unscopedName name)

-- | Parse native listing identities and refuse namespaces for formats that do not have them.
packageNameFrom :: Ecosystem -> Maybe Text -> Text -> Either StoreFault PackageName
packageNameFrom eco namespace raw = case nativeGrammar eco of
    Just parse -> nativePackageName eco parse namespace raw
    Nothing -> Right (mkPackageName eco (mkScope <$> (nonBlank =<< namespace)) raw)

-- The ecosystems whose listings CodeArtifact must return as checked, unnamespaced native names.
nativeGrammar :: Ecosystem -> Maybe (Text -> Either ParseError PackageName)
nativeGrammar = \case
    Cargo -> Just Cargo.projectName
    RubyGems -> Just RubyGems.projectName
    Npm -> Nothing
    PyPI -> Nothing

nativePackageName :: Ecosystem -> (Text -> Either ParseError PackageName) -> Maybe Text -> Text -> Either StoreFault PackageName
nativePackageName eco parse namespace raw = do
    unless (maybe True T.null namespace) (Left (protocolFault (show eco <> " packages cannot have a namespace")))
    first (const (protocolFault ("CodeArtifact returned an invalid " <> show eco <> " package name"))) (parse raw)

-- | List one bucket page. The prefix filters the package component, never its namespace.
listPackagesRequest :: CodeArtifactStore -> NamePrefix -> Maybe Text -> CA.ListPackages
listPackagesRequest store prefix token =
    CA.newListPackages (casDomain store) (casRepository store)
        & (CAL.listPackages_domainOwner ?~ casDomainOwner store)
        & (CAL.listPackages_format ?~ storePackageFormat store)
        & (CAL.listPackages_packagePrefix .~ nonBlank (renderNamePrefix prefix))
        & (CAL.listPackages_maxResults ?~ listPageLimit)
        & (CAL.listPackages_nextToken .~ token)

-- | List one page of a package's versions, continuing from a page token when there is one.
listVersionsRequest :: CodeArtifactStore -> PackageName -> Maybe Text -> CA.ListPackageVersions
listVersionsRequest store name token =
    CA.newListPackageVersions (casDomain store) (casRepository store) (storePackageFormat store) package
        & (CAL.listPackageVersions_domainOwner ?~ casDomainOwner store)
        & (CAL.listPackageVersions_namespace .~ namespace)
        & (CAL.listPackageVersions_maxResults ?~ listPageLimit)
        & (CAL.listPackageVersions_nextToken .~ token)
  where
    (namespace, package) = packageCoordinates name

-- | A missing package has an empty local inventory. Other service faults retain their retry advice.
listVersionsResult :: Either AWS.Error CA.ListPackageVersionsResponse -> Either StoreFault CA.ListPackageVersionsResponse
listVersionsResult = \case
    Left (AWS.ServiceError service)
        | service ^. AWS.serviceError_code == AWS.newErrorCode "ResourceNotFoundException" ->
            Right (CA.newListPackageVersionsResponse 200)
    result -> first classifyStoreFault result

-- | Physically delete a batch already bounded by 'deleteCeiling', requiring Published status.
deleteRequest :: CodeArtifactStore -> PackageName -> [Version] -> CA.DeletePackageVersions
deleteRequest store name versions =
    CA.newDeletePackageVersions (casDomain store) (casRepository store) (storePackageFormat store) package
        & (CAL.deletePackageVersions_domainOwner ?~ casDomainOwner store)
        & (CAL.deletePackageVersions_namespace .~ namespace)
        & (CAL.deletePackageVersions_versions .~ map renderVersion versions)
        & (CAL.deletePackageVersions_expectedStatus ?~ CA.PackageVersionStatus_Published)
  where
    (namespace, package) = packageCoordinates name

-- | Describe the repository, which carries both its ARN and its refill posture.
describeRepositoryRequest :: CodeArtifactStore -> CA.DescribeRepository
describeRepositoryRequest store =
    CA.newDescribeRepository (casDomain store) (casRepository store)
        & (CAL.describeRepository_domainOwner ?~ casDomainOwner store)

-- | Describe another repository in the same domain, which is how the probe walks a chain.
describeUpstreamRequest :: CodeArtifactStore -> RepositoryName -> CA.DescribeRepository
describeUpstreamRequest store repository =
    describeRepositoryRequest store{casRepository = repositoryNameText repository}

-- | The repository a store's coordinates name, where a probe of its chain starts.
repositoryOfStore :: CodeArtifactStore -> RepositoryName
repositoryOfStore = RepositoryName . casRepository

-- | Read a repository's tags, which is where CodeArtifact carries the consent marker.
listTagsRequest :: Text -> CA.ListTagsForResource
listTagsRequest = CA.newListTagsForResource

-- | An ecosystem-specific cursor tag lets mounts share a repository without sharing their walk.
cursorTagKey :: Ecosystem -> Text
cursorTagKey eco = "ecluse-dredger-cursor-" <> ecosystemName eco

-- | Record one completed bucket without replacing other repository tags.
cursorTagRequest :: Ecosystem -> Text -> NamePrefix -> CA.TagResource
cursorTagRequest eco arn prefix =
    CA.newTagResource arn
        & (CAL.tagResource_tags .~ [CA.newTag (cursorTagKey eco) (renderNamePrefix prefix)])

-- | Forget the walk, which a completed one does.
cursorUntagRequest :: Ecosystem -> Text -> CA.UntagResource
cursorUntagRequest eco arn =
    CA.newUntagResource arn & (CAL.untagResource_tagKeys .~ [cursorTagKey eco])

-- | Parse a cursor under the current alphabet, restarting when the stored prefix no longer fits.
cursorOfTags :: NameAlphabet -> Ecosystem -> [CA.Tag] -> Maybe NamePrefix
cursorOfTags alphabet eco tags = do
    tag <- find ((== cursorTagKey eco) . (^. CAL.tag_key)) tags
    parseNamePrefix alphabet =<< nonBlank (tag ^. CAL.tag_value)

-- | Require the optional repository description before classifying the store.
repositoryOfResponse :: CA.DescribeRepositoryResponse -> Either StoreFault CA.RepositoryDescription
repositoryOfResponse response =
    maybeToRight
        (descriptionFault "the store described no repository")
        (response ^. CAL.describeRepositoryResponse_repository)

-- | Require the repository ARN before reading or writing its tags.
arnOfDescription :: CA.RepositoryDescription -> Either StoreFault Text
arnOfDescription description =
    maybeToRight
        (descriptionFault "the store described the repository without an ARN")
        (nonBlank =<< description ^. CAL.repositoryDescription_arn)

-- A description that answers nothing is a protocol fault, and a second call reads the same.
descriptionFault :: Text -> StoreFault
descriptionFault detail =
    StoreFault
        { faultTransport = transportFault TransportProtocol detail
        , faultRetry = RetryFutile
        }

-- | Reject malformed native-grammar inventory. Other formats retain their absent-name filtering.
packagesOfPage :: Ecosystem -> [CA.PackageSummary] -> Either StoreFault [PackageName]
packagesOfPage eco = fmap catMaybes . traverse named
  where
    named summary = case (nativeGrammar eco, summary ^. CAL.packageSummary_package) of
        (Just parse, Just raw) -> Just <$> nativePackageName eco parse (namespaceOf summary) raw
        (Just _, Nothing) -> Left (protocolFault ("CodeArtifact returned a " <> show eco <> " package without a name"))
        (Nothing, raw) -> traverse (packageNameFrom eco (namespaceOf summary)) (nonBlank =<< raw)
    namespaceOf summary = summary ^. CAL.packageSummary_namespace

-- | Published and Unlisted versions remain installable. Other statuses are withdrawn.
presenceOf :: CA.PackageVersionStatus -> VersionPresence
presenceOf status
    | status == CA.PackageVersionStatus_Published = VersionServed
    | status == CA.PackageVersionStatus_Unlisted = VersionServed
    | otherwise = VersionWithdrawn

-- | Give each submitted version one outcome, refusing absent or conflicting success evidence.
foldDeleteResponse :: [Version] -> CA.DeletePackageVersionsResponse -> [(Version, VersionOutcome)]
foldDeleteResponse submitted response =
    [(version, outcomeOf (renderVersion version)) | version <- submitted]
  where
    failures = fromMaybe HM.empty (response ^. CAL.deletePackageVersionsResponse_failedVersions)
    successes = fromMaybe HM.empty (response ^. CAL.deletePackageVersionsResponse_successfulVersions)

    outcomeOf raw = case HM.lookup raw failures of
        Just failure -> VersionRefused (refusalOf failure)
        Nothing
            | HM.member raw successes -> VersionRemoved
            | otherwise ->
                VersionRefused
                    (storeRefusal "UNREPORTED" "the store answered with no outcome for this version")

-- The error code an operator looks up, with CodeArtifact's own message beside it.
refusalOf :: CA.PackageVersionError -> StoreRefusal
refusalOf failure =
    storeRefusal
        (maybe "UNKNOWN" CA.fromPackageVersionErrorCode (failure ^. CAL.packageVersionError_errorCode))
        (fromMaybe "" (failure ^. CAL.packageVersionError_errorMessage))

-- | Preserve repositories whose upstream or external connection can refill deleted versions.
classifyRepository :: CA.RepositoryDescription -> StoreClass
classifyRepository description
    | not (null connections) =
        StorePreserved
            ( "the repository has an external connection ("
                <> T.intercalate ", " connections
                <> "), which serves a deleted version again on the next request"
            )
    | not (null upstreams) =
        StorePreserved
            ( "the repository has an upstream ("
                <> T.intercalate ", " upstreams
                <> "), which serves a deleted version from another repository"
            )
    | otherwise = StoreDestroyable
  where
    links = upstreamLinksOf description
    connections = map externalConnectionText (rlConnections links)
    upstreams = map repositoryNameText (rlUpstreams links)

-- | Read external connections and upstream repositories, excluding unnamed entries.
upstreamLinksOf :: CA.RepositoryDescription -> RepositoryLinks
upstreamLinksOf description =
    RepositoryLinks
        { rlConnections =
            map ExternalConnection . mapMaybe (^. CAL.repositoryExternalConnectionInfo_externalConnectionName) $
                fromMaybe [] (description ^. CAL.repositoryDescription_externalConnections)
        , rlUpstreams =
            map RepositoryName . mapMaybe (^. CAL.upstreamRepositoryInfo_repositoryName) $
                fromMaybe [] (description ^. CAL.repositoryDescription_upstreams)
        }

-- | The tag key that carries the operator's consent on CodeArtifact.
consentTagKey :: Text
consentTagKey = "ecluse-dredger-consent"

-- | The value that key must hold for consent to count.
consentTagValue :: Text
consentTagValue = "true"

-- | How an operator attaches the marker, logged verbatim when consent is withheld.
consentDescriptor :: Text
consentDescriptor =
    "attach the tag "
        <> consentTagKey
        <> "="
        <> consentTagValue
        <> " to the mirror repository in CodeArtifact: the Dredger deletes nothing from a store that does not carry it"

-- | Whether a repository's tags carry the consent marker.
consentOfTags :: [CA.Tag] -> ConsentVerdict
consentOfTags tags
    | any marker tags = ConsentGranted
    | otherwise = ConsentWithheld consentDescriptor
  where
    marker tag =
        tag ^. CAL.tag_key == consentTagKey && tag ^. CAL.tag_value == consentTagValue

-- | Classify an @amazonka@ error for a maintenance call, refining the shared transport classification, which reports a throttle and a denied permission as the same fault.
classifyStoreFault :: AWS.Error -> StoreFault
classifyStoreFault err =
    StoreFault{faultTransport = fault, faultRetry = advice}
  where
    fault = classifyAwsTransport err
    advice = case err of
        AWS.ServiceError service -> serviceRetryAdvice service
        _ | transportRetryable (tfCause fault) -> RetryWorthwhile
        _ -> RetryFutile

-- | Read a failed probe read. An identity that may not ask cannot clear the repository, so its refusal is an unsafe answer rather than a fault, and every other failure leaves the question open.
describeUpstreamRefusal :: AWS.Error -> UpstreamSafety
describeUpstreamRefusal = \case
    AWS.ServiceError service
        | isAccessDeniedCode (service ^. AWS.serviceError_code) ->
            Unsafe (InsufficientPermissions describeRepositoryGrant)
    _ -> Undecidable NetworkFailure

-- | The grant the probe needs on the private upstream and on every repository in its chain.
describeRepositoryGrant :: PermissionName
describeRepositoryGrant = PermissionName "codeartifact:DescribeRepository"

-- A refusal of the identity itself, rather than of the request it carried.
isAccessDeniedCode :: AWS.ErrorCode -> Bool
isAccessDeniedCode code = errorCodeText code `elem` accessDeniedCodes

accessDeniedCodes :: [Text]
accessDeniedCodes =
    [ "accessdenied"
    , "unauthorized"
    , "unrecognizedclient"
    , "invalidclienttokenid"
    , "missingauthenticationtoken"
    ]

{- A throttle and a server-side failure clear on their own. Every other refusal (a denied
permission, a missing repository, a malformed request) fails the same way next time. -}
serviceRetryAdvice :: AWS.ServiceError -> RetryAdvice
serviceRetryAdvice service
    | throttled || serverSide = maybe RetryWorthwhile RetryDelayed (retryAfterSeconds headers)
    | otherwise = RetryFutile
  where
    headers = service ^. AWS.serviceError_headers
    status = statusCode (service ^. AWS.serviceError_status)
    serverSide = status >= 500
    throttled = status == 429 || isThrottlingCode (service ^. AWS.serviceError_code)

{- @amazonka@ strips the @Exception@ suffix from a service's error code, so
@ThrottlingException@ arrives as @Throttling@. -}
isThrottlingCode :: AWS.ErrorCode -> Bool
isThrottlingCode code = errorCodeText code `elem` throttlingCodes

throttlingCodes :: [Text]
throttlingCodes =
    [ "throttling"
    , "throttled"
    , "toomanyrequests"
    , "requestthrottled"
    , "requestlimitexceeded"
    , "slowdown"
    , "provisionedthroughputexceeded"
    ]

{- Only the delta-seconds form of @Retry-After@ is read, because turning its HTTP-date form
into a delay needs the current time and this stays pure. -}
retryAfterSeconds :: [Header] -> Maybe RetryAfter
retryAfterSeconds headers = do
    raw <- decodeUtf8 . snd <$> find ((== hRetryAfter) . fst) headers
    RetryAfter <$> readDecimalText raw
