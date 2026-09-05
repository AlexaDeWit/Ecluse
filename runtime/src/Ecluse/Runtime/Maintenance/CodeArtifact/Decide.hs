-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Every decision the CodeArtifact store-maintenance leaf makes, as pure functions over
@amazonka@'s own request and response types. @amazonka@ is trusted for serialisation,
signing, and decoding against the service model, so what stays ours is which call to
build and how to read what comes back. Keeping those here leaves the effectful half in
"Ecluse.Runtime.Maintenance.CodeArtifact" thin enough to hold no logic worth testing.
-}
module Ecluse.Runtime.Maintenance.CodeArtifact.Decide (
    -- * Coordinates
    CodeArtifactStore (..),
    CodeArtifactFormat,
    codeArtifactFormat,
    formatEcosystem,
    formatToken,

    -- * What the backend does
    codeArtifactFacts,
    deleteCeiling,

    -- * The npm codec
    packageCoordinates,
    packageNameFrom,

    -- * Requests
    listPackagesRequest,
    listVersionsRequest,
    deleteRequest,
    describeRepositoryRequest,
    listTagsRequest,

    -- * The walk cursor
    cursorTagKey,
    cursorTagRequest,
    cursorUntagRequest,
    cursorOfTags,

    -- * Responses
    packagesOfPage,
    versionsOfPage,
    foldDeleteResponse,
    classifyRepository,
    consentOfTags,
    repositoryOfResponse,
    arnOfDescription,

    -- * Consent marker
    consentTagKey,
    consentTagValue,
    consentDescriptor,

    -- * Faults
    classifyStoreFault,
) where

import Amazonka qualified as AWS
import Amazonka.CodeArtifact qualified as CA
import Amazonka.CodeArtifact.Lens qualified as CAL
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Lens.Micro ((.~), (?~), (^.))
import Network.HTTP.Types (Header, statusCode)
import Network.HTTP.Types.Header (hRetryAfter)

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems), ecosystemName)
import Ecluse.Core.Fault (
    RetryAfter (RetryAfter),
    TransportCause (TransportProtocol),
    tfCause,
    transportFault,
    transportRetryable,
 )
import Ecluse.Core.Package (PackageName, mkPackageName, mkScope, pkgNamespace, unScope, unscopedName)
import Ecluse.Core.Registry.Maintenance (
    CompletionNotion (CompletesOnCall),
    ConsentVerdict (ConsentGranted, ConsentWithheld),
    DeleteCeiling (AtMost),
    NameAlphabet,
    NamePrefix,
    RefillPosture (RefillPermitted),
    RetryAdvice (RetryDelayed, RetryFutile, RetryWorthwhile),
    StoreClass (StoreDestroyable, StorePreserved),
    StoreFacts (..),
    StoreFault (..),
    StoreRefusal,
    StoredVersion (..),
    VersionOutcome (VersionRefused, VersionRemoved),
    VersionPresence (VersionServed, VersionWithdrawn),
    parseNamePrefix,
    renderNamePrefix,
    storeRefusal,
 )
import Ecluse.Core.Text (nonBlank, readDecimalText)
import Ecluse.Core.Version (Version, mkVersion, renderVersion)
import Ecluse.Runtime.Aws.Fault (classifyAwsTransport)

{- | Where one CodeArtifact repository lives. The composition root parses these from the
vetted mirror-store URL, so the leaf never sees a URL.
-}
data CodeArtifactStore = CodeArtifactStore
    { casDomain :: Text
    , casDomainOwner :: Text
    -- ^ The 12-digit account number that owns the domain.
    , casRegion :: Text
    , casRepository :: Text
    , casFormat :: CodeArtifactFormat
    }
    deriving stock (Eq, Show)

{- | The format token CodeArtifact addresses an ecosystem by, held with the ecosystem it
came from, so a coordinate cannot name one format and mean another ecosystem.
-}
data CodeArtifactFormat = CodeArtifactFormat Ecosystem CA.PackageFormat
    deriving stock (Eq, Show)

-- | The format CodeArtifact addresses an ecosystem by, 'Nothing' where it has none.
codeArtifactFormat :: Ecosystem -> Maybe CodeArtifactFormat
codeArtifactFormat = \case
    Npm -> Just (CodeArtifactFormat Npm CA.PackageFormat_Npm)
    PyPI -> Just (CodeArtifactFormat PyPI CA.PackageFormat_Pypi)
    RubyGems -> Nothing

-- | The ecosystem a format was built from.
formatEcosystem :: CodeArtifactFormat -> Ecosystem
formatEcosystem (CodeArtifactFormat eco _) = eco

{- | The format as CodeArtifact spells it, which is also the first path segment of a
repository's per-format endpoint.
-}
formatToken :: CodeArtifactFormat -> Text
formatToken (CodeArtifactFormat _ token) = CA.fromPackageFormat token

{- | What CodeArtifact does: it re-admits a version published again after a delete, and has applied
it by the time it answers. The alphabet is the mount ecosystem's, whose grammar spells the names.
-}
codeArtifactFacts :: NameAlphabet -> StoreFacts
codeArtifactFacts alphabet =
    StoreFacts
        { factBackend = "codeArtifact"
        , factDeleteCeiling = deleteCeiling
        , factRefill = RefillPermitted
        , factCompletion = CompletesOnCall
        , factNameAlphabet = alphabet
        }

-- | The most versions one @DeletePackageVersions@ call accepts.
deleteCeiling :: DeleteCeiling
deleteCeiling = AtMost 100

{- | The namespace and package CodeArtifact addresses a name by. An npm scope is the
namespace, without its sigil, and the unscoped name is the package.
-}
packageCoordinates :: PackageName -> (Maybe Text, Text)
packageCoordinates name = (unScope <$> pkgNamespace name, unscopedName name)

{- | Rebuild a name from the namespace and package a listing returned. A blank namespace
reads as none, so an empty string cannot become an empty scope.
-}
packageNameFrom :: Ecosystem -> Maybe Text -> Text -> PackageName
packageNameFrom eco namespace = mkPackageName eco (mkScope <$> (nonBlank =<< namespace))

{- | One page of one bucket, continuing from a page token when there is one. @packagePrefix@
matches the package component alone and never a namespace, and the empty bucket filters nothing.
-}
listPackagesRequest :: CodeArtifactStore -> NamePrefix -> Maybe Text -> CA.ListPackages
listPackagesRequest store prefix token =
    CA.newListPackages (casDomain store) (casRepository store)
        & (CAL.listPackages_domainOwner ?~ casDomainOwner store)
        & (CAL.listPackages_format ?~ formatTokenOf store)
        & (CAL.listPackages_packagePrefix .~ nonBlank (renderNamePrefix prefix))
        & (CAL.listPackages_nextToken .~ token)

-- | List one page of a package's versions, continuing from a page token when there is one.
listVersionsRequest :: CodeArtifactStore -> PackageName -> Maybe Text -> CA.ListPackageVersions
listVersionsRequest store name token =
    CA.newListPackageVersions (casDomain store) (casRepository store) (formatTokenOf store) package
        & (CAL.listPackageVersions_domainOwner ?~ casDomainOwner store)
        & (CAL.listPackageVersions_namespace .~ namespace)
        & (CAL.listPackageVersions_nextToken .~ token)
  where
    (namespace, package) = packageCoordinates name

{- | Delete one chunk of a package's versions. The caller has already split the batch to
'deleteCeiling', because CodeArtifact refuses a larger one outright.
-}
deleteRequest :: CodeArtifactStore -> PackageName -> [Version] -> CA.DeletePackageVersions
deleteRequest store name versions =
    CA.newDeletePackageVersions (casDomain store) (casRepository store) (formatTokenOf store) package
        & (CAL.deletePackageVersions_domainOwner ?~ casDomainOwner store)
        & (CAL.deletePackageVersions_namespace .~ namespace)
        & (CAL.deletePackageVersions_versions .~ map renderVersion versions)
  where
    (namespace, package) = packageCoordinates name

-- | Describe the repository, which carries both its ARN and its refill posture.
describeRepositoryRequest :: CodeArtifactStore -> CA.DescribeRepository
describeRepositoryRequest store =
    CA.newDescribeRepository (casDomain store) (casRepository store)
        & (CAL.describeRepository_domainOwner ?~ casDomainOwner store)

-- | Read a repository's tags, which is where CodeArtifact carries the consent marker.
listTagsRequest :: Text -> CA.ListTagsForResource
listTagsRequest = CA.newListTagsForResource

{- | The one tag key the Dredger writes, per ecosystem, so two mounts sharing one repository as
their mirror target keep their own walk. The grant admits this prefix and nothing else.
-}
cursorTagKey :: Ecosystem -> Text
cursorTagKey eco = "ecluse-dredger-cursor-" <> ecosystemName eco

{- | Record a completed bucket. @TagResource@ adds and updates the keys it names and replaces no
others, so this cannot disturb the consent tag beside it.
-}
cursorTagRequest :: Ecosystem -> Text -> NamePrefix -> CA.TagResource
cursorTagRequest eco arn prefix =
    CA.newTagResource arn
        & (CAL.tagResource_tags .~ [CA.newTag (cursorTagKey eco) (renderNamePrefix prefix)])

-- | Forget the walk, which a completed one does.
cursorUntagRequest :: Ecosystem -> Text -> CA.UntagResource
cursorUntagRequest eco arn =
    CA.newUntagResource arn & (CAL.untagResource_tagKeys .~ [cursorTagKey eco])

{- | The bucket a repository's tags record, 'Nothing' when none is recorded or when the recorded
one is no prefix this alphabet spells, which is how an alphabet change restarts the walk.
-}
cursorOfTags :: NameAlphabet -> Ecosystem -> [CA.Tag] -> Maybe NamePrefix
cursorOfTags alphabet eco tags = do
    tag <- find ((== cursorTagKey eco) . (^. CAL.tag_key)) tags
    parseNamePrefix alphabet =<< nonBlank (tag ^. CAL.tag_value)

{- | The description @DescribeRepository@ carried. It is optional on the wire, and a verdict
on a repository the store did not describe would be invented rather than read.
-}
repositoryOfResponse :: CA.DescribeRepositoryResponse -> Either StoreFault CA.RepositoryDescription
repositoryOfResponse response =
    maybeToRight
        (descriptionFault "the store described no repository")
        (response ^. CAL.describeRepositoryResponse_repository)

{- | The repository ARN, which is how CodeArtifact addresses a tag read. It is optional on
the wire, and without it there is no marker to read and so no verdict to give.
-}
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

{- | The names in one listing page. CodeArtifact types a summary's package name optional,
and an entry without one names nothing to sweep, so it is dropped rather than guessed at.
-}
packagesOfPage :: Ecosystem -> [CA.PackageSummary] -> [PackageName]
packagesOfPage eco = mapMaybe named
  where
    named summary =
        packageNameFrom eco (summary ^. CAL.packageSummary_namespace)
            <$> (nonBlank =<< summary ^. CAL.packageSummary_package)

-- | The versions in one listing page, each with what the store still does with it.
versionsOfPage :: Ecosystem -> [CA.PackageVersionSummary] -> [StoredVersion]
versionsOfPage eco = map stored
  where
    stored summary =
        StoredVersion
            { storedVersion = mkVersion eco (summary ^. CAL.packageVersionSummary_version)
            , storedPresence = presenceOf (summary ^. CAL.packageVersionSummary_status)
            }

-- @Published@ and @Unlisted@ are the two statuses CodeArtifact still serves an install from.
presenceOf :: CA.PackageVersionStatus -> VersionPresence
presenceOf status
    | status == CA.PackageVersionStatus_Published = VersionServed
    | status == CA.PackageVersionStatus_Unlisted = VersionServed
    | otherwise = VersionWithdrawn

{- | Read a delete response back against the versions submitted, so each gets one outcome.
CodeArtifact may report a version neither way, which is a refusal, never a silent success.
-}
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

{- | Whether deleting from this repository destroys anything. An external connection or an
upstream serves a deleted version again from elsewhere, so neither is worth sweeping.
-}
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
    connections =
        mapMaybe
            (^. CAL.repositoryExternalConnectionInfo_externalConnectionName)
            (fromMaybe [] (description ^. CAL.repositoryDescription_externalConnections))
    upstreams =
        mapMaybe
            (^. CAL.upstreamRepositoryInfo_repositoryName)
            (fromMaybe [] (description ^. CAL.repositoryDescription_upstreams))

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

{- | Classify an @amazonka@ error for a maintenance call, refining the shared transport
classification, which reports a throttle and a denied permission as the same fault.
-}
classifyStoreFault :: AWS.Error -> StoreFault
classifyStoreFault err =
    StoreFault{faultTransport = fault, faultRetry = advice}
  where
    fault = classifyAwsTransport err
    advice = case err of
        AWS.ServiceError service -> serviceRetryAdvice service
        _ | transportRetryable (tfCause fault) -> RetryWorthwhile
        _ -> RetryFutile

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
isThrottlingCode (AWS.ErrorCode code) = T.toLower code `elem` throttlingCodes

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

-- The format token for a store's own ecosystem.
formatTokenOf :: CodeArtifactStore -> CA.PackageFormat
formatTokenOf store = case casFormat store of
    CodeArtifactFormat _ token -> token
