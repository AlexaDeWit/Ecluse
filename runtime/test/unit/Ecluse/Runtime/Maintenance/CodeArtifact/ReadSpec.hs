-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Runtime.Maintenance.CodeArtifact.ReadSpec (spec) where

import Lens.Micro ((?~))
import Test.Hspec

import Amazonka.CodeArtifact qualified as CA
import Amazonka.CodeArtifact.Lens qualified as CAL

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName, mkScope)
import Ecluse.Core.Registry.Maintenance (
    StoredVersion (..),
    VersionPresence (VersionServed, VersionWithdrawn),
 )
import Ecluse.Core.Version (Version, mkVersion, renderVersion)
import Ecluse.Runtime.Maintenance.CodeArtifact.Decide (
    CodeArtifactStore (..),
    codeArtifactFormat,
 )
import Ecluse.Runtime.Maintenance.CodeArtifact.Read (
    RepositoryIdentity (..),
    VersionObservation (..),
    VersionOrigin (..),
    identityOfStore,
    observationsOfPage,
    storedOfObservation,
    versionsOfPage,
 )

{- | The read-only CodeArtifact layer: what one observation of a listing preserves. The
coordinates and verdicts it builds on are covered in
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

version :: Text -> Version
version = mkVersion Npm

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
