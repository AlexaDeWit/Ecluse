-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Fixed persistence laws over production adapters and independent protocol observations.
module Ecluse.Test.Contract.PackageStore (
    PackageStoreArtifact (..),
    PackageStoreRelease (..),
    PackageStoreOperations (..),
    PackageStoreSession (..),
    FullPackageStoreDriver (..),
    fullPackageStoreContract,
    packageStoreSpec,
    packageStoreCatalogue,
    packageStoreCatalogueFor,
    packageStoreCompletion,
    validatePackageStore,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Completion (Completion (..), CompletionScope (FullPackageStore), LawEvidence (..), validateCompletion)
import Ecluse.Test.Contract.Manifest (failRunner)
import Ecluse.Test.Contract.Runner (DeclaredGroup (..))
import Ecluse.Test.Contract.Types

-- | Every file in a version participates in equality, including its verified digest.
data PackageStoreArtifact = PackageStoreArtifact
    { psaFilename :: Text
    , psaBytes :: ByteString
    , psaDigest :: Text
    }
    deriving stock (Eq, Show)

-- | Consumer metadata and the complete artifact inventory expected after publication.
data PackageStoreRelease = PackageStoreRelease
    { psrVersion :: Text
    , psrMetadata :: Map Text Text
    , psrArtifacts :: NonEmpty PackageStoreArtifact
    }
    deriving stock (Eq, Show)

-- | Operations delegate to production handles. Drivers return evidence without assertions.
data PackageStoreOperations = PackageStoreOperations
    { psoRead :: Text -> IO (Either Text (Maybe PackageStoreRelease))
    , psoArtifact :: Text -> Text -> IO (Either Text (Maybe ByteString))
    -- ^ The bytes at a version's native artifact location, read apart from its metadata. 'Nothing' on a 404.
    , psoWrite :: PackageStoreRelease -> IO (Either Text ())
    , psoInventory :: IO (Either Text [(Text, [Text])])
    , psoDelete :: [Text] -> IO [(Text, Either Text ())]
    }

-- | Fault controls act at the protocol boundary. Reopening retains only the remote fixture state.
data PackageStoreSession = PackageStoreSession
    { pssOperations :: PackageStoreOperations
    , pssObservedWrites :: IO [PackageStoreRelease]
    , pssFailNextRead :: IO ()
    , pssFailNextWrite :: IO ()
    , pssFailNextDelete :: IO ()
    , pssReopen :: IO PackageStoreOperations
    }

-- | Two distinct release fixtures exercise preservation of an unrelated version during deletion.
data FullPackageStoreDriver = FullPackageStoreDriver
    { psdSubject :: Text
    , psdPackage :: Text
    , psdPrimary :: PackageStoreRelease
    , psdSibling :: PackageStoreRelease
    , psdWithSession :: (PackageStoreSession -> IO ()) -> IO ()
    }

-- | Every provider executes the same six families without filtering by advertised capabilities.
fullPackageStoreContract :: FullPackageStoreDriver -> [DeclaredGroup]
fullPackageStoreContract driver =
    [DeclaredGroup (packageStoreGroup (psdSubject driver) family) (const spec) | (family, spec) <- packageStoreLaws driver]

-- | The ordinary unit tier executes the same canonical laws under its existing coverage run.
packageStoreSpec :: FullPackageStoreDriver -> Spec
packageStoreSpec = mapM_ snd . packageStoreLaws

packageStoreLaws :: FullPackageStoreDriver -> [(Text, Spec)]
packageStoreLaws driver =
    [ declare "reads" $ do
        contractCase "reads" "present" "reads every stored file and supported metadata" $ \session -> do
            publishBoth session
            psoRead (pssOperations session) primaryVersion `shouldReturn` Right (Just primary)
            psoRead (pssOperations session) siblingVersion `shouldReturn` Right (Just sibling)
        contractCase "reads" "absent" "distinguishes a missing release from a failed read" $ \session -> do
            psoRead (pssOperations session) primaryVersion `shouldReturn` Right Nothing
            psoWrite (pssOperations session) primary `shouldReturn` Right ()
            pssFailNextRead session
            psoRead (pssOperations session) primaryVersion >>= (`shouldSatisfy` isLeft)
            psoRead (pssOperations session) primaryVersion `shouldReturn` Right (Just primary)
    , declare "writes" $ do
        contractCase "writes" "complete" "publishes the complete release and preserves its sibling" $ \session -> do
            publishBoth session
            pssObservedWrites session `shouldReturn` [primary, sibling]
            psoRead (pssOperations session) primaryVersion `shouldReturn` Right (Just primary)
            psoRead (pssOperations session) siblingVersion `shouldReturn` Right (Just sibling)
        contractCase "writes" "idempotent" "repeated publication keeps the same complete inventory" $ \session -> do
            publishBoth session
            psoWrite (pssOperations session) primary `shouldReturn` Right ()
            psoInventory (pssOperations session) `shouldReturn` Right expectedInventory
            psoRead (pssOperations session) primaryVersion `shouldReturn` Right (Just primary)
    , declare "inventory" $
        contractCase "inventory" "exact" "enumerates precisely the published packages and versions" $ \session -> do
            psoInventory (pssOperations session) `shouldReturn` Right []
            publishBoth session
            psoInventory (pssOperations session) `shouldReturn` Right expectedInventory
    , declare "deletion" $
        contractCase "deletion" "selected" "removes the selected version and preserves its sibling" $ \session -> do
            publishBoth session
            psoDelete (pssOperations session) [primaryVersion] `shouldReturn` [(primaryVersion, Right ())]
            psoRead (pssOperations session) primaryVersion `shouldReturn` Right Nothing
            psoRead (pssOperations session) siblingVersion `shouldReturn` Right (Just sibling)
            psoInventory (pssOperations session) `shouldReturn` Right [(psdPackage driver, [siblingVersion])]
            for_ (psrArtifacts primary) $ \artifact ->
                psoArtifact (pssOperations session) primaryVersion (psaFilename artifact) `shouldReturn` Right Nothing
            for_ (psrArtifacts sibling) $ \artifact ->
                psoArtifact (pssOperations session) siblingVersion (psaFilename artifact) `shouldReturn` Right (Just (psaBytes artifact))
    , declare "integrity" $
        contractCase "integrity" "bytes-and-digests" "preserves every artifact byte and declared verified digest" (packageStoreIntegrityLaw driver)
    , declare "recovery" $ do
        contractCase "recovery" "publish-retry" "reports a failed write before a later attempt completes" $ \session -> do
            pssFailNextWrite session
            psoWrite (pssOperations session) primary >>= (`shouldSatisfy` isLeft)
            psoWrite (pssOperations session) primary `shouldReturn` Right ()
            psoRead (pssOperations session) primaryVersion `shouldReturn` Right (Just primary)
        contractCase "recovery" "reopen" "a fresh handle reads the completed release without its source" $ \session -> do
            publishBoth session
            reopened <- pssReopen session
            psoRead reopened primaryVersion `shouldReturn` Right (Just primary)
            psoRead reopened siblingVersion `shouldReturn` Right (Just sibling)
            psoInventory reopened `shouldReturn` Right expectedInventory
        contractCase "recovery" "delete-retry" "reports incomplete deletion and completes a later attempt" $ \session -> do
            publishBoth session
            pssFailNextDelete session
            failed <- psoDelete (pssOperations session) [primaryVersion]
            map fst failed `shouldBe` [primaryVersion]
            map snd failed `shouldSatisfy` all isLeft
            psoRead (pssOperations session) siblingVersion `shouldReturn` Right (Just sibling)
            psoDelete (pssOperations session) [primaryVersion] `shouldReturn` [(primaryVersion, Right ())]
            psoRead (pssOperations session) primaryVersion `shouldReturn` Right Nothing
            psoRead (pssOperations session) siblingVersion `shouldReturn` Right (Just sibling)
    ]
  where
    primary = psdPrimary driver
    sibling = psdSibling driver
    primaryVersion = psrVersion primary
    siblingVersion = psrVersion sibling
    expectedInventory = [(psdPackage driver, sort [primaryVersion, siblingVersion])]
    publishBoth session = do
        primaryVersion `shouldNotBe` siblingVersion
        psoWrite (pssOperations session) primary `shouldReturn` Right ()
        psoWrite (pssOperations session) sibling `shouldReturn` Right ()
    contractCase family suffix description = it (caseLabel (identifier driver family <> "/" <> suffix) description)
    declare family spec = (family, around (psdWithSession driver) spec)

-- Every artifact byte and verified digest survives a write and a read.
packageStoreIntegrityLaw :: FullPackageStoreDriver -> PackageStoreSession -> Expectation
packageStoreIntegrityLaw driver session = do
    psoWrite (pssOperations session) primary `shouldReturn` Right ()
    pssObservedWrites session `shouldReturn` [primary]
    psoRead (pssOperations session) primaryVersion `shouldReturn` Right (Just primary)
  where
    primary = psdPrimary driver
    primaryVersion = psrVersion primary

-- | The required cases are declared independently of the Hspec examples above.
packageStoreCatalogue :: Text -> Catalogue
packageStoreCatalogue subject = packageStoreCatalogueFor "npm" subject ("package-store-" <> subject) subject

-- | Ecosystem, backend and suite remain explicit when one backend hosts several profiles.
packageStoreCatalogueFor :: Text -> Text -> Text -> Text -> Catalogue
packageStoreCatalogueFor ecosystem backend partition subject =
    Catalogue
        { catalogueProductionEcosystems = [ecosystem]
        , catalogueProfiles = [Profile profile [ecosystem] [backend] families]
        , catalogueObligations =
            [ Obligation storeGroupId profile family Supported [family] (caseIdentifiers subject family)
            | family <- families
            , let storeGroupId = storeIdentifier subject family
            ]
        , catalogueGroups = [(packageStoreGroup subject family){groupPartition = partition} | family <- families]
        }
  where
    profile = "package-store/" <> subject

-- | Evidence names concrete cases while the independent completion module owns mandatory laws.
packageStoreCompletion :: Text -> Completion
packageStoreCompletion subject =
    Completion
        subject
        FullPackageStore
        [ LawEvidence family suffix Unit (storeIdentifier subject family) (storeIdentifier subject family <> "/" <> suffix)
        | (family, suffixes) <- Map.toList familyCases
        , suffix <- suffixes
        ]

-- | Omitted families or laws fail before fixture acquisition, even if the catalogue omits them too.
validatePackageStore :: Text -> IO ()
validatePackageStore subject =
    either
        (failRunner . T.unlines)
        (const (pure ()))
        (validateCompletion (packageStoreCompletion subject) [packageStoreCatalogue subject])

packageStoreGroup :: Text -> Text -> Group
packageStoreGroup subject family =
    Group
        { groupId = storeGroupId
        , groupContractId = family
        , groupProfileId = "package-store/" <> subject
        , groupTier = Unit
        , groupPartition = "package-store-" <> subject
        , groupCases = [Case name [storeGroupId] | name <- caseIdentifiers subject family]
        , groupCapabilities = [family]
        , groupPrerequisites = []
        , groupResources = ["loopback-registry", "deterministic-control-plane"]
        , groupIsolation = ["fresh-store-per-example"]
        }
  where
    storeGroupId = storeIdentifier subject family

families :: [Text]
families = ["reads", "writes", "inventory", "deletion", "integrity", "recovery"]

caseIdentifiers :: Text -> Text -> [Text]
caseIdentifiers subject family = map ((storeIdentifier subject family <> "/") <>) suffixes
  where
    suffixes = Map.findWithDefault [] family familyCases

familyCases :: Map Text [Text]
familyCases =
    Map.fromList
        [ ("reads", ["present", "absent"])
        , ("writes", ["complete", "idempotent"])
        , ("inventory", ["exact"])
        , ("deletion", ["selected"])
        , ("integrity", ["bytes-and-digests"])
        , ("recovery", ["publish-retry", "reopen", "delete-retry"])
        ]

identifier :: FullPackageStoreDriver -> Text -> Text
identifier = storeIdentifier . psdSubject

storeIdentifier :: Text -> Text -> Text
storeIdentifier subject family = "package-store/" <> subject <> "/" <> family
