-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Runtime.Maintenance.CodeArtifact.ReadSpec (spec) where

import Lens.Micro ((.~), (?~), (^.))
import Network.HTTP.Types (Status, status403, status404, status429)
import Test.Hspec

import Amazonka qualified as AWS
import Amazonka.CodeArtifact qualified as CA
import Amazonka.CodeArtifact.Lens qualified as CAL

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Fault (TransportCause (TransportProtocol), tfDetail, transportFault)
import Ecluse.Core.Package (PackageName, mkPackageName, mkScope)
import Ecluse.Core.Registry.Maintenance (
    RetryAdvice (RetryFutile, RetryWorthwhile),
    StoreFault (..),
    StoredVersion (..),
    VersionPresence (VersionServed, VersionWithdrawn),
 )
import Ecluse.Core.Version (Version, mkVersion, renderVersion)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (
    CodeArtifactStore (..),
    codeArtifactFormat,
 )
import Ecluse.Runtime.Maintenance.CodeArtifact.Read (
    LocalVersionRead (VersionAbsentLocally, VersionEvidenceIncomplete, VersionObserved),
    RepositoryIdentity (..),
    VersionObservation (..),
    VersionOrigin (..),
    VersionReadFault (VersionNotHeld, VersionUnread),
    classifyVersionRead,
    describeVersionRequest,
    identityOfStore,
    observationsOfPage,
    readOfAnswer,
    storedOfObservation,
    versionsOfPage,
 )

{- | The read-only CodeArtifact layer: what one observation preserves, and what a direct version
read establishes. A 'ReadPlane' has no deletion, cursor, publication, or tag-writing field, so
every case here drives evidence alone. The coordinates and verdicts this builds on are covered in
"Ecluse.Runtime.Maintenance.CodeArtifact.DecideSpec".
-}
spec :: Spec
spec = maybe noNpmFormat readCases npmStore

-- The store's coordinates carry a parsed format, so a spec over them starts from one.
noNpmFormat :: Spec
noNpmFormat = it "has a CodeArtifact format for npm" $ expectationFailure "npm resolved to no CodeArtifact format"

readCases :: CodeArtifactStore -> Spec
readCases store = do
    identitySpec store
    pageSpec store
    projectionSpec store
    requestSpec store
    faultSpec
    answerSpec store

identitySpec :: CodeArtifactStore -> Spec
identitySpec store =
    describe "identityOfStore" $
        it "names the account, region, domain, repository, and ecosystem the handle is bound to" $
            identityOfStore store
                `shouldBe` RepositoryIdentity
                    { ridDomainOwner = "111122223333"
                    , ridRegion = "eu-west-1"
                    , ridDomain = "acme"
                    , ridRepository = "mirror"
                    , ridEcosystem = Npm
                    }

pageSpec :: CodeArtifactStore -> Spec
pageSpec store = describe "observationsOfPage" $ do
    it "keeps CodeArtifact's own status beside the served or withdrawn projection" $ do
        let page = observed store [summaryOf raw status | (raw, status) <- statusRun]
        map obsStatus page `shouldBe` map snd statusRun
        map obsPresence page
            `shouldBe` [VersionServed, VersionServed] <> replicate 5 VersionWithdrawn

    it "keeps the raw version as published, and the package the listing was read for" $ do
        let page = observed store [published "1.0.0-rc.1+build"]
        map (renderVersion . obsVersion) page `shouldBe` ["1.0.0-rc.1+build"]
        map obsPackage page `shouldBe` [scopedName]

    it "reports no revision where the store reported none" $
        map obsRevision (observed store [published "1.0.0"]) `shouldBe` [Nothing]

    it "reads a blank revision as none, never as a revision named by the empty string" $
        map obsRevision (observed store [revised "  " (published "1.0.0")]) `shouldBe` [Nothing]

    it "keeps a reported revision verbatim" $
        map obsRevision (observed store [revised "rev-1" (published "1.0.0")]) `shouldBe` [Just "rev-1"]

    it "reports no origin at all where the store reported none" $
        map obsOrigin (observed store [published "1.0.0"]) `shouldBe` [Nothing]

    it "keeps an origin whose every field the store omitted, distinct from no origin" $
        map obsOrigin (observed store [originating CA.newPackageVersionOrigin (published "1.0.0")])
            `shouldBe` [Just (VersionOrigin Nothing Nothing Nothing)]

    it "keeps an origin type this build does not know, rather than drop it" $
        map (obsOrigin >=> vorType) (observed store [originating (originTyped laterOriginType) (published "1.0.0")])
            `shouldBe` [Just laterOriginType]

    it "keeps the domain entry point's repository and external connection apart" $
        map obsOrigin (observed store [originating ingested (published "1.0.0")])
            `shouldBe` [ Just
                            VersionOrigin
                                { vorType = Just CA.PackageVersionOriginType_EXTERNAL
                                , vorEntryRepository = Just "shared"
                                , vorEntryConnection = Just "public:npmjs"
                                }
                       ]

    it "reads a blank entry-point name as absent, never as a repository called the empty string" $
        map (obsOrigin >=> vorEntryRepository) (observed store [originating (enteredAt "") (published "1.0.0")])
            `shouldBe` [Nothing]

    it "keeps one observation per listing entry, never merging two by version" $
        length (observed store [published "1.0.0", published "1.0.0"]) `shouldBe` 2

    it "keeps the same package and version at two repositories as two observations" $ do
        let here = observed store [originating (originTyped CA.PackageVersionOriginType_INTERNAL) (published "1.0.0")]
            there = observed store{casRepository = "retainer"} [originating (originTyped CA.PackageVersionOriginType_EXTERNAL) (published "1.0.0")]
        map obsIdentity here `shouldNotBe` map obsIdentity there
        here `shouldNotBe` there

projectionSpec :: CodeArtifactStore -> Spec
projectionSpec store = describe "versionsOfPage" $ do
    it "projects exactly the observations of the same page" $ do
        let page = [summaryOf raw status | (raw, status) <- statusRun]
        stored store page `shouldBe` map storedOfObservation (observed store page)

    it "carries nothing but the version and its presence, so no origin or revision reaches a delete" $ do
        let evidenced = revised "rev-1" (originating (originTyped CA.PackageVersionOriginType_INTERNAL) (published "1.0.0"))
        stored store [published "1.0.0"] `shouldBe` stored store [evidenced]

    it "reads a served version as one the store still holds" $
        stored store [published "1.0.0"]
            `shouldBe` [StoredVersion{storedVersion = version "1.0.0", storedPresence = VersionServed}]

requestSpec :: CodeArtifactStore -> Spec
requestSpec store = describe "describeVersionRequest" $ do
    it "addresses the bound domain, owner, repository, format, namespace, package, and version" $ do
        let request = describeVersionRequest store scopedName (version "7.0.0")
        request ^. CAL.describePackageVersion_domain `shouldBe` "acme"
        request ^. CAL.describePackageVersion_domainOwner `shouldBe` Just "111122223333"
        request ^. CAL.describePackageVersion_repository `shouldBe` "mirror"
        request ^. CAL.describePackageVersion_format `shouldBe` CA.PackageFormat_Npm
        request ^. CAL.describePackageVersion_namespace `shouldBe` Just "babel"
        request ^. CAL.describePackageVersion_package `shouldBe` "core"
        request ^. CAL.describePackageVersion_packageVersion `shouldBe` "7.0.0"

    it "sends an unscoped package with no namespace" $
        describeVersionRequest store plainName (version "1.0.0")
            ^. CAL.describePackageVersion_namespace
            `shouldBe` Nothing

    it "sends the version in its published spelling" $
        describeVersionRequest store plainName (version "1.0.0-rc.1+build")
            ^. CAL.describePackageVersion_packageVersion
            `shouldBe` "1.0.0-rc.1+build"

faultSpec :: Spec
faultSpec = describe "classifyVersionRead" $ do
    it "reads CodeArtifact's own missing resource as a version the repository does not hold" $
        classifyVersionRead (serviceError status404 "ResourceNotFoundException") `shouldBe` VersionNotHeld

    it "keeps a refused permission a fault, never absence" $
        classifyVersionRead (serviceError status403 "AccessDeniedException")
            `shouldSatisfy` unreadAdvising RetryFutile

    it "keeps a throttle a fault worth another attempt, never absence" $
        classifyVersionRead (serviceError status429 "ThrottlingException")
            `shouldSatisfy` unreadAdvising RetryWorthwhile

answerSpec :: CodeArtifactStore -> Spec
answerSpec store = describe "readOfAnswer" $ do
    it "reads a version the repository does not hold as local absence" $
        answerFor store (Left VersionNotHeld) `shouldBe` VersionAbsentLocally

    it "keeps a failed read apart from absence, so no fault becomes an empty repository" $
        answerFor store (Left (VersionUnread refusedRead)) `shouldBe` VersionEvidenceIncomplete refusedRead

    it "observes a description that names the version asked for" $
        answerFor store (Right (responseOf (revisedDescription "rev-1" (describedInternally describedVersion))))
            `shouldBe` VersionObserved
                VersionObservation
                    { obsIdentity = identityOfStore store
                    , obsPackage = scopedName
                    , obsVersion = version "7.0.0"
                    , obsStatus = CA.PackageVersionStatus_Published
                    , obsPresence = VersionServed
                    , obsRevision = Just "rev-1"
                    , obsOrigin = Just (VersionOrigin (Just CA.PackageVersionOriginType_INTERNAL) Nothing Nothing)
                    }

    it "observes a description that supplies no identity of its own, having nothing to disagree with" $
        answerFor store (Right (responseOf statusAlone)) `shouldSatisfy` observing

    it "refuses a description naming another version" $
        refusalFor store (describedVersion & (CAL.packageVersionDescription_version ?~ "7.0.1"))
            `shouldBe` Just "the store described version 7.0.1, not the one asked for"

    it "refuses a description naming another package" $
        refusalFor store (describedVersion & (CAL.packageVersionDescription_packageName ?~ "runtime"))
            `shouldBe` Just "the store described package runtime, not the one asked for"

    it "refuses a description naming another namespace" $
        refusalFor store (describedVersion & (CAL.packageVersionDescription_namespace ?~ "vue"))
            `shouldBe` Just "the store described namespace vue, not the one asked for"

    it "refuses a description naming another format" $
        refusalFor store (describedVersion & (CAL.packageVersionDescription_format ?~ CA.PackageFormat_Pypi))
            `shouldBe` Just "the store described format pypi, not the one asked for"

    it "refuses a description carrying no status, which says nothing about what the store holds" $
        refusalFor store (describedVersion & (CAL.packageVersionDescription_status .~ Nothing))
            `shouldBe` Just "the store described the version without a status"

-- The description a store answers with for the version every case here asks about.
describedVersion :: CA.PackageVersionDescription
describedVersion =
    CA.newPackageVersionDescription
        & (CAL.packageVersionDescription_format ?~ CA.PackageFormat_Npm)
        & (CAL.packageVersionDescription_namespace ?~ "babel")
        & (CAL.packageVersionDescription_packageName ?~ "core")
        & (CAL.packageVersionDescription_version ?~ "7.0.0")
        & (CAL.packageVersionDescription_status ?~ CA.PackageVersionStatus_Published)

statusAlone :: CA.PackageVersionDescription
statusAlone =
    CA.newPackageVersionDescription
        & (CAL.packageVersionDescription_status ?~ CA.PackageVersionStatus_Published)

revisedDescription :: Text -> CA.PackageVersionDescription -> CA.PackageVersionDescription
revisedDescription raw = CAL.packageVersionDescription_revision ?~ raw

describedInternally :: CA.PackageVersionDescription -> CA.PackageVersionDescription
describedInternally =
    CAL.packageVersionDescription_origin ?~ originTyped CA.PackageVersionOriginType_INTERNAL

responseOf :: CA.PackageVersionDescription -> CA.DescribePackageVersionResponse
responseOf = CA.newDescribePackageVersionResponse 200

answerFor :: CodeArtifactStore -> Either VersionReadFault CA.DescribePackageVersionResponse -> LocalVersionRead
answerFor store = readOfAnswer store scopedName (version "7.0.0")

-- What the read refused, so a case reads the refusal rather than only that one happened.
refusalFor :: CodeArtifactStore -> CA.PackageVersionDescription -> Maybe Text
refusalFor store described = case answerFor store (Right (responseOf described)) of
    VersionEvidenceIncomplete fault -> Just (tfDetail (faultTransport fault))
    _ -> Nothing

observing :: LocalVersionRead -> Bool
observing = \case
    VersionObserved _ -> True
    _ -> False

unreadAdvising :: RetryAdvice -> VersionReadFault -> Bool
unreadAdvising advice = \case
    VersionUnread fault -> faultRetry fault == advice
    VersionNotHeld -> False

refusedRead :: StoreFault
refusedRead =
    StoreFault
        { faultTransport = transportFault TransportProtocol "the store refused the read"
        , faultRetry = RetryFutile
        }

observed :: CodeArtifactStore -> [CA.PackageVersionSummary] -> [VersionObservation]
observed store = observationsOfPage (identityOfStore store) scopedName

stored :: CodeArtifactStore -> [CA.PackageVersionSummary] -> [StoredVersion]
stored store = versionsOfPage (identityOfStore store) scopedName

-- Every status CodeArtifact names, plus one it has not named yet.
statusRun :: [(Text, CA.PackageVersionStatus)]
statusRun =
    [ ("1.0.0", CA.PackageVersionStatus_Published)
    , ("1.1.0", CA.PackageVersionStatus_Unlisted)
    , ("1.2.0", CA.PackageVersionStatus_Archived)
    , ("1.3.0", CA.PackageVersionStatus_Deleted)
    , ("1.4.0", CA.PackageVersionStatus_Disposed)
    , ("1.5.0", CA.PackageVersionStatus_Unfinished)
    , ("1.6.0", CA.PackageVersionStatus' "SOME_LATER_STATUS")
    ]

laterOriginType :: CA.PackageVersionOriginType
laterOriginType = CA.PackageVersionOriginType' "SOME_LATER_ORIGIN"

summaryOf :: Text -> CA.PackageVersionStatus -> CA.PackageVersionSummary
summaryOf = CA.newPackageVersionSummary

published :: Text -> CA.PackageVersionSummary
published raw = summaryOf raw CA.PackageVersionStatus_Published

revised :: Text -> CA.PackageVersionSummary -> CA.PackageVersionSummary
revised raw = CAL.packageVersionSummary_revision ?~ raw

originating :: CA.PackageVersionOrigin -> CA.PackageVersionSummary -> CA.PackageVersionSummary
originating origin = CAL.packageVersionSummary_origin ?~ origin

originTyped :: CA.PackageVersionOriginType -> CA.PackageVersionOrigin
originTyped kind = CA.newPackageVersionOrigin & (CAL.packageVersionOrigin_originType ?~ kind)

-- An origin whose entry point names a repository, for the blank and named cases alike.
enteredAt :: Text -> CA.PackageVersionOrigin
enteredAt named =
    CA.newPackageVersionOrigin
        & (CAL.packageVersionOrigin_domainEntryPoint ?~ (CA.newDomainEntryPoint & (CAL.domainEntryPoint_repositoryName ?~ named)))

ingested :: CA.PackageVersionOrigin
ingested =
    CA.newPackageVersionOrigin
        & (CAL.packageVersionOrigin_originType ?~ CA.PackageVersionOriginType_EXTERNAL)
        & (CAL.packageVersionOrigin_domainEntryPoint ?~ entryPoint)
  where
    entryPoint =
        CA.newDomainEntryPoint
            & (CAL.domainEntryPoint_repositoryName ?~ "shared")
            & (CAL.domainEntryPoint_externalConnectionName ?~ "public:npmjs")

scopedName :: PackageName
scopedName = mkPackageName Npm (Just (mkScope "babel")) "core"

plainName :: PackageName
plainName = mkPackageName Npm Nothing "lodash"

version :: Text -> Version
version = mkVersion Npm

serviceError :: Status -> Text -> AWS.Error
serviceError status code =
    AWS.ServiceError (AWS.ServiceError' "CodeArtifact" status [] (AWS.newErrorCode code) Nothing Nothing)

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
