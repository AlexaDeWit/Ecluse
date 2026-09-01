-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Maintenance.CodeArtifact.DecideSpec (spec) where

import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Lens.Micro ((?~), (^.))
import Network.HTTP.Client (
    HttpException (HttpExceptionRequest, InvalidUrlException),
    HttpExceptionContent (ConnectionTimeout),
    defaultRequest,
 )
import Network.HTTP.Types (Header, Status, status403, status429, status503)
import Network.HTTP.Types.Header (hRetryAfter)
import Test.Hspec

import Amazonka qualified as AWS
import Amazonka.CodeArtifact qualified as CA
import Amazonka.CodeArtifact.Lens qualified as CAL

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Fault (RetryAfter (RetryAfter), TransportCause (TransportTimeout), tfCause)
import Ecluse.Core.Package (PackageName, mkPackageName, mkScope, renderPackageName)
import Ecluse.Core.Registry.Maintenance (
    ConsentVerdict (ConsentGranted, ConsentWithheld),
    DeleteCeiling (AtMost),
    RetryAdvice (RetryDelayed, RetryFutile, RetryWorthwhile),
    StoreClass (StoreDestroyable, StorePreserved),
    StoreFault (..),
    StoredVersion (..),
    VersionOutcome (VersionRefused, VersionRemoved),
    VersionPresence (VersionServed, VersionWithdrawn),
    refusalCode,
    refusalDetail,
 )
import Ecluse.Core.Version (Version, mkVersion, renderVersion)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (
    CodeArtifactStore (..),
    arnOfDescription,
    classifyRepository,
    classifyStoreFault,
    codeArtifactFormat,
    consentOfTags,
    consentTagKey,
    consentTagValue,
    deleteCeiling,
    deleteRequest,
    describeRepositoryRequest,
    foldDeleteResponse,
    formatEcosystem,
    formatToken,
    listPackagesRequest,
    listTagsRequest,
    listVersionsRequest,
    packageCoordinates,
    packageNameFrom,
    packagesOfPage,
    repositoryOfResponse,
    versionsOfPage,
 )

spec :: Spec
spec = do
    formatSpec
    codecSpec
    requestSpec
    ceilingSpec
    deleteFoldSpec
    faultSpec
    verdictSpec

formatSpec :: Spec
formatSpec = describe "codeArtifactFormat" $ do
    it "names npm and pypi by CodeArtifact's own format tokens" $ do
        fmap formatToken (codeArtifactFormat Npm) `shouldBe` Just "npm"
        fmap formatToken (codeArtifactFormat PyPI) `shouldBe` Just "pypi"

    it "keeps the ecosystem a format was built from" $
        fmap formatEcosystem (codeArtifactFormat PyPI) `shouldBe` Just PyPI

    it "has no format for an ecosystem CodeArtifact does not serve" $
        codeArtifactFormat RubyGems `shouldBe` Nothing

codecSpec :: Spec
codecSpec = describe "the npm codec" $ do
    it "splits a scoped name into CodeArtifact's namespace and package" $
        packageCoordinates scopedName `shouldBe` (Just "babel", "core")

    it "gives an unscoped name no namespace" $
        packageCoordinates plainName `shouldBe` (Nothing, "lodash")

    it "rebuilds the name a listing returned" $ do
        renderPackageName (packageNameFrom Npm (Just "babel") "core") `shouldBe` "@babel/core"
        renderPackageName (packageNameFrom Npm Nothing "lodash") `shouldBe` "lodash"

    it "reads a blank namespace as no namespace, never as an empty scope" $
        packageNameFrom Npm (Just "") "lodash" `shouldBe` plainName

    it "drops a listing entry CodeArtifact returned with no package name" $ do
        let named = CA.newPackageSummary & CAL.packageSummary_package ?~ "lodash"
        packagesOfPage Npm [named, CA.newPackageSummary] `shouldBe` [plainName]

    it "reads a scoped listing entry back into a scoped name" $ do
        let scoped =
                CA.newPackageSummary
                    & CAL.packageSummary_package
                    ?~ "core"
                        & CAL.packageSummary_namespace
                    ?~ "babel"
        packagesOfPage Npm [scoped] `shouldBe` [scopedName]

    it "serves only a published or unlisted version, and withdraws the rest" $ do
        let summary = CA.newPackageVersionSummary
            page =
                versionsOfPage
                    Npm
                    [ summary "1.0.0" CA.PackageVersionStatus_Published
                    , summary "1.1.0" CA.PackageVersionStatus_Unlisted
                    , summary "1.2.0" CA.PackageVersionStatus_Archived
                    , summary "1.3.0" CA.PackageVersionStatus_Deleted
                    , summary "1.4.0" CA.PackageVersionStatus_Disposed
                    , summary "1.5.0" CA.PackageVersionStatus_Unfinished
                    ]
        map storedPresence page
            `shouldBe` [ VersionServed
                       , VersionServed
                       , VersionWithdrawn
                       , VersionWithdrawn
                       , VersionWithdrawn
                       , VersionWithdrawn
                       ]
        map (renderVersion . storedVersion) page
            `shouldBe` ["1.0.0", "1.1.0", "1.2.0", "1.3.0", "1.4.0", "1.5.0"]

requestSpec :: Spec
requestSpec = describe "the requests the leaf builds" $ maybe noNpmFormat requestCases npmStore

-- The store's coordinates carry a parsed format, so a spec over them starts from one.
noNpmFormat :: Spec
noNpmFormat = it "has a CodeArtifact format for npm" $ expectationFailure "npm resolved to no CodeArtifact format"

requestCases :: CodeArtifactStore -> Spec
requestCases store = do
    it "addresses a package listing by domain, owner, repository, and format" $ do
        let request = listPackagesRequest store Nothing
        request ^. CAL.listPackages_domain `shouldBe` "acme"
        request ^. CAL.listPackages_repository `shouldBe` "mirror"
        request ^. CAL.listPackages_domainOwner `shouldBe` Just "111122223333"
        request ^. CAL.listPackages_format `shouldBe` Just CA.PackageFormat_Npm
        request ^. CAL.listPackages_nextToken `shouldBe` Nothing

    it "carries the page token onto the next listing call" $
        listPackagesRequest store (Just "page-2") ^. CAL.listPackages_nextToken `shouldBe` Just "page-2"

    it "addresses a version listing by the package's namespace and base name" $ do
        let request = listVersionsRequest store scopedName (Just "page-2")
        request ^. CAL.listPackageVersions_package `shouldBe` "core"
        request ^. CAL.listPackageVersions_namespace `shouldBe` Just "babel"
        request ^. CAL.listPackageVersions_format `shouldBe` CA.PackageFormat_Npm
        request ^. CAL.listPackageVersions_domainOwner `shouldBe` Just "111122223333"
        request ^. CAL.listPackageVersions_nextToken `shouldBe` Just "page-2"

    it "sends an unscoped package with no namespace" $
        listVersionsRequest store plainName Nothing ^. CAL.listPackageVersions_namespace `shouldBe` Nothing

    it "sends the versions to delete in their published spelling" $ do
        let request = deleteRequest store scopedName [version "7.0.0", version "7.1.0"]
        request ^. CAL.deletePackageVersions_versions `shouldBe` ["7.0.0", "7.1.0"]
        request ^. CAL.deletePackageVersions_package `shouldBe` "core"
        request ^. CAL.deletePackageVersions_namespace `shouldBe` Just "babel"

    it "describes and tags the repository the coordinates name" $ do
        describeRepositoryRequest store ^. CAL.describeRepository_repository `shouldBe` "mirror"
        describeRepositoryRequest store ^. CAL.describeRepository_domainOwner `shouldBe` Just "111122223333"
        listTagsRequest "arn:aws:codeartifact:eu-west-1:111122223333:repository/acme/mirror"
            ^. CAL.listTagsForResource_resourceArn
            `shouldBe` "arn:aws:codeartifact:eu-west-1:111122223333:repository/acme/mirror"

ceilingSpec :: Spec
ceilingSpec =
    describe "deleteCeiling" $
        it "accepts 100 versions per destructive CodeArtifact call" $
            deleteCeiling `shouldBe` AtMost 100

deleteFoldSpec :: Spec
deleteFoldSpec = describe "foldDeleteResponse" $ do
    it "reports one outcome for every version submitted" $ do
        let outcomes = foldDeleteResponse submitted (deleteResponse ["1.0.0"] [("1.1.0", "NOT_FOUND", "no such version")])
        map (renderVersion . fst) outcomes `shouldBe` ["1.0.0", "1.1.0", "1.2.0"]

    it "reads a reported success as removed" $
        outcomeFor "1.0.0" (deleteResponse ["1.0.0"] []) `shouldBe` Just VersionRemoved

    it "carries the backend's own code and message on a refusal" $ do
        let refusal = outcomeFor "1.1.0" (deleteResponse [] [("1.1.0", "NOT_ALLOWED", "origin control")])
        fmap describeOutcome refusal `shouldBe` Just (Just ("NOT_ALLOWED", "origin control"))

    it "refuses a version the response reported neither way, never counts it removed" $ do
        let unreported = outcomeFor "1.2.0" (deleteResponse ["1.0.0"] [])
        fmap describeOutcome unreported `shouldBe` Just (Just ("UNREPORTED", "the store answered with no outcome for this version"))

    it "names an unspecified error code rather than guessing one" $ do
        let response =
                CA.newDeletePackageVersionsResponse 200
                    & CAL.deletePackageVersionsResponse_failedVersions
                    ?~ HM.singleton "1.0.0" CA.newPackageVersionError
        fmap describeOutcome (outcomeFor "1.0.0" response) `shouldBe` Just (Just ("UNKNOWN", ""))
  where
    outcomeFor raw response =
        snd <$> find ((== raw) . fst) [(renderVersion v, outcome) | (v, outcome) <- foldDeleteResponse submitted response]

    describeOutcome = \case
        VersionRefused refusal -> Just (refusalCode refusal, refusalDetail refusal)
        _ -> Nothing

    submitted = [version "1.0.0", version "1.1.0", version "1.2.0"]

faultSpec :: Spec
faultSpec = describe "classifyStoreFault" $ do
    it "reads a throttle as worth another attempt" $
        adviceFor (serviceError status429 "ThrottlingException" []) `shouldBe` RetryWorthwhile

    it "reads a throttling code at any status the same way" $
        adviceFor (serviceError status403 "TooManyRequestsException" []) `shouldBe` RetryWorthwhile

    it "reads a server-side failure as worth another attempt" $
        adviceFor (serviceError status503 "ServiceUnavailable" []) `shouldBe` RetryWorthwhile

    it "honours a server-supplied retry-after in seconds" $
        adviceFor (serviceError status429 "ThrottlingException" [(hRetryAfter, "30")])
            `shouldBe` RetryDelayed (RetryAfter 30)

    it "ignores an http-date retry-after rather than guess a delay" $
        adviceFor (serviceError status429 "ThrottlingException" [(hRetryAfter, "Wed, 21 Oct 2026 07:28:00 GMT")])
            `shouldBe` RetryWorthwhile

    it "reads a refused permission as futile, because the next call is refused too" $
        adviceFor (serviceError status403 "AccessDeniedException" []) `shouldBe` RetryFutile

    it "reads a timeout as worth another attempt and keeps the transport cause" $ do
        let err = AWS.TransportError (HttpExceptionRequest defaultRequest ConnectionTimeout)
        adviceFor err `shouldBe` RetryWorthwhile
        tfCause (faultTransport (classifyStoreFault err)) `shouldBe` TransportTimeout

    it "reads an unusable URL as futile" $
        adviceFor (AWS.TransportError (InvalidUrlException "u" "unusable")) `shouldBe` RetryFutile
  where
    adviceFor = faultRetry . classifyStoreFault

verdictSpec :: Spec
verdictSpec = describe "the verdicts a sweep reads before it deletes" $ do
    it "refuses a repository with an external connection, which refills itself" $ do
        let connected =
                CA.newRepositoryDescription
                    & CAL.repositoryDescription_externalConnections
                    ?~ [ CA.newRepositoryExternalConnectionInfo
                            & CAL.repositoryExternalConnectionInfo_externalConnectionName
                            ?~ "public:npmjs"
                       ]
        classifyRepository connected `shouldSatisfy` preservedFor "public:npmjs"

    it "refuses a repository fed by an upstream" $ do
        let routed =
                CA.newRepositoryDescription
                    & CAL.repositoryDescription_upstreams
                    ?~ [CA.newUpstreamRepositoryInfo & CAL.upstreamRepositoryInfo_repositoryName ?~ "shared"]
        classifyRepository routed `shouldSatisfy` preservedFor "shared"

    it "accepts a repository that holds only what was published to it" $
        classifyRepository CA.newRepositoryDescription `shouldBe` StoreDestroyable

    it "grants consent on the marker tag" $
        consentOfTags [CA.newTag consentTagKey consentTagValue] `shouldBe` ConsentGranted

    it "withholds consent when the marker carries another value" $
        consentOfTags [CA.newTag consentTagKey "false"] `shouldSatisfy` withheld

    it "withholds consent on an untagged repository, and says how to attach the marker" $
        consentOfTags [] `shouldSatisfy` describesAttachment

    it "reads the described repository, and faults when the store described none" $ do
        let described = CA.newDescribeRepositoryResponse 200 & CAL.describeRepositoryResponse_repository ?~ CA.newRepositoryDescription
        repositoryOfResponse described `shouldSatisfy` isRight
        repositoryOfResponse (CA.newDescribeRepositoryResponse 200) `shouldSatisfy` isLeft

    it "reads the ARN a tag call needs, and faults when the description carries none" $ do
        let arned = CA.newRepositoryDescription & CAL.repositoryDescription_arn ?~ "arn:aws:codeartifact:::repository/acme/mirror"
        arnOfDescription arned `shouldBe` Right "arn:aws:codeartifact:::repository/acme/mirror"
        arnOfDescription CA.newRepositoryDescription `shouldSatisfy` isLeft
  where
    preservedFor named = \case
        StorePreserved reason -> named `T.isInfixOf` reason
        StoreDestroyable -> False

    withheld = \case
        ConsentWithheld _ -> True
        ConsentGranted -> False

    describesAttachment = \case
        ConsentWithheld descriptor ->
            consentTagKey `T.isInfixOf` descriptor && consentTagValue `T.isInfixOf` descriptor
        ConsentGranted -> False

-- The CodeArtifact npm repository the request cases address.
npmStore :: Maybe CodeArtifactStore
npmStore = coordinates <$> codeArtifactFormat Npm
  where
    coordinates format =
        CodeArtifactStore
            { casDomain = "acme"
            , casDomainOwner = "111122223333"
            , casRegion = "eu-west-1"
            , casRepository = "mirror"
            , casFormat = format
            }

scopedName :: PackageName
scopedName = mkPackageName Npm (Just (mkScope "babel")) "core"

plainName :: PackageName
plainName = mkPackageName Npm Nothing "lodash"

version :: Text -> Version
version = mkVersion Npm

-- A delete response reporting the named successes and failures, the way CodeArtifact does.
deleteResponse :: [Text] -> [(Text, Text, Text)] -> CA.DeletePackageVersionsResponse
deleteResponse successes failures =
    CA.newDeletePackageVersionsResponse 200
        & CAL.deletePackageVersionsResponse_successfulVersions
        ?~ HM.fromList [(raw, CA.newSuccessfulPackageVersionInfo) | raw <- successes]
            & CAL.deletePackageVersionsResponse_failedVersions
        ?~ HM.fromList
            [ (raw, CA.newPackageVersionError & CAL.packageVersionError_errorCode ?~ CA.PackageVersionErrorCode' code & CAL.packageVersionError_errorMessage ?~ message)
            | (raw, code, message) <- failures
            ]

serviceError :: Status -> Text -> [Header] -> AWS.Error
serviceError status code headers =
    AWS.ServiceError (AWS.ServiceError' "CodeArtifact" status headers (AWS.newErrorCode code) Nothing Nothing)
