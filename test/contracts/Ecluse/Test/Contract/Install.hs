-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Real-client installation outcomes and native installation observations.
module Ecluse.Test.Contract.Install (
    ClientResult (..),
    InstallDriver (..),
    TargetPortability (..),
    TargetObservation (..),
    TargetExpectation (..),
    TargetRefusal (..),
    crossTargetInstallCase,
    targetInstallCase,
    InstallationDriver (..),
    UpstreamRead (..),
    CodedArtifact (..),
    ArtifactProbe (..),
    installationContract,
    codedArtifactCase,
    RefusalDriver (..),
    refusesWithoutEffects,
    installsObserved,
    refusesObserved,
    shouldSucceed,
    shouldFail,
    shouldWarn,
    clientReport,
) where

import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Ecluse.Test.InstalledTree (InstalledTree, TreeEntry (TreeFile))
import System.Exit (ExitCode (ExitSuccess))
import Test.Hspec (Expectation, SpecWith, expectationFailure, it, shouldBe, shouldMatchList, shouldNotBe, shouldSatisfy)

import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Serving (ServingResponse (responseBody, responseHeaders), artifactDigestCase, artifactHeadLengthCase, artifactHeadUpstreamCase)

-- | The outcome of one client invocation: what ran, its exit code, and its captured output.
data ClientResult = ClientResult
    { crCommand :: Text
    , crExit :: ExitCode
    , crStdout :: Text
    , crStderr :: Text
    }
    deriving stock (Show)

-- | A fresh client invocation with its independent observation of the installed result.
data InstallDriver observation = InstallDriver
    { installRun :: IO (ClientResult, observation)
    , installExpected :: observation
    }

-- | Native operations for the same admission and artifact scenarios on each ecosystem.
data InstallationDriver observation = InstallationDriver
    { installationAccepted :: InstallDriver observation
    , installationDenied :: InstallDriver (observation, InstalledTree)
    , installationArtifact :: IO (Text, ServingResponse)
    , installationDigest :: ByteString -> Text
    , installationHead :: IO ArtifactProbe
    , installationCodedArtifact :: IO CodedArtifact
    }

-- | The content codings of one read a public upstream answered, as its access log records them.
data UpstreamRead = UpstreamRead
    { upstreamAccepted :: Maybe Text
    -- ^ The request's @Accept-Encoding@.
    , upstreamCoding :: Maybe Text
    -- ^ The response's @Content-Encoding@.
    }
    deriving stock (Eq, Show)

-- | One artifact HEAD through the proxy, with the methods of the public upstream's reads of that artifact meanwhile.
data ArtifactProbe = ArtifactProbe
    { probeResponse :: ServingResponse
    , probeUpstreamMethods :: [Text]
    }

{- | One artifact read through the proxy by a client that accepts gzip, from a public upstream that
gzips any response a request permits.
-}
data CodedArtifact = CodedArtifact
    { codedDigest :: Text
    -- ^ The digest the served metadata advertises for the artifact.
    , codedResponse :: ServingResponse
    -- ^ The proxy's response on the wire, undecoded.
    , codedArtifactReads :: [UpstreamRead]
    -- ^ The upstream's reads of this artifact during the fetch.
    , codedRouteReads :: [UpstreamRead]
    -- ^ Every read the same upstream answered so far, the control that it gzips on request.
    }

-- | Require a fresh installation, refusal without files, and intact artifact representations.
installationContract :: (Eq observation, Show observation) => Text -> SpecWith (InstallationDriver observation)
installationContract prefix = do
    it (label "accepted-install" "installs the admitted release into a fresh client directory") $ \driver ->
        installsObserved (installationAccepted driver)
    it (label "default-denial" "refuses an unadmitted release without installing files") $ \driver ->
        refusesObserved (installationDenied driver)
    it (label "artifact-integrity" "serves bytes matching the digest advertised in metadata") $ \driver -> do
        (digest, response) <- installationArtifact driver
        artifactDigestCase (installationDigest driver) digest response
    it (label "artifact-head" "advertises an admitted artifact without transferring its body") $ \driver -> do
        probe <- installationHead driver
        (_, artifact) <- installationArtifact driver
        artifactHeadLengthCase (toInteger (LBS.length (responseBody artifact))) (probeResponse probe)
        artifactHeadUpstreamCase (map encodeUtf8 (probeUpstreamMethods probe))
    it (label "artifact-identity-coding" "fetches the artifact in identity coding from an upstream that gzips on request") $ \driver ->
        installationCodedArtifact driver >>= codedArtifactCase (installationDigest driver)
  where
    label name = caseLabel (prefix <> "/" <> name)

{- | Require an identity-coded upstream fetch and the advertised bytes, uncoded, for a client that
accepts gzip. The upstream must have gzipped some read, or the fetch proves nothing.
-}
codedArtifactCase :: (ByteString -> Text) -> CodedArtifact -> Expectation
codedArtifactCase digest coded = do
    codedRouteReads coded `shouldSatisfy` any ((== Just "gzip") . upstreamCoding)
    codedArtifactReads coded `shouldSatisfy` (not . null)
    for_ (codedArtifactReads coded) (`shouldBe` UpstreamRead (Just "identity") Nothing)
    lookup "Content-Encoding" (responseHeaders (codedResponse coded)) `shouldBe` Nothing
    artifactDigestCase digest (codedDigest coded) (codedResponse coded)

-- | An attempted install and an independent destination observation after its write window.
data RefusalDriver evidence = RefusalDriver
    { refusalInstall :: IO ClientResult
    , refusalObserve :: IO evidence
    , refusalSettle :: IO ()
    }

-- | Refusal must leave the required destination state intact after asynchronous work settles.
refusesWithoutEffects :: (Eq evidence, Show evidence) => evidence -> RefusalDriver evidence -> Expectation
refusesWithoutEffects expected driver = do
    void (refusalInstall driver >>= shouldFail)
    refusalSettle driver
    observed <- refusalObserve driver
    observed `shouldBe` expected

-- | Require client success and the expected native installation result.
installsObserved :: (Eq observation, Show observation) => InstallDriver observation -> Expectation
installsObserved driver = do
    (result, observed) <- installRun driver
    void (shouldSucceed result)
    observed `shouldBe` installExpected driver

-- | Require client refusal and the expected unchanged installation result.
refusesObserved :: (Eq observation, Show observation) => InstallDriver observation -> Expectation
refusesObserved driver = do
    (result, observed) <- installRun driver
    void (shouldFail result)
    observed `shouldBe` installExpected driver

-- | Fail the assertion with the client's output when the command failed.
shouldSucceed :: (MonadIO m) => ClientResult -> m ClientResult
shouldSucceed res = liftIO $ case crExit res of
    ExitSuccess -> pure res
    _ -> expectationFailure (toString (clientReport res "failed")) >> pure res

-- | Fail the assertion with the client's output when the command unexpectedly succeeded.
shouldFail :: (MonadIO m) => ClientResult -> m ClientResult
shouldFail res = liftIO $ case crExit res of
    ExitSuccess -> expectationFailure (toString (clientReport res "incorrectly succeeded")) >> pure res
    _ -> pure res

-- | Fail the assertion with the client's output unless one line of its stderr is exactly this one.
shouldWarn :: (MonadIO m) => Text -> ClientResult -> m ClientResult
shouldWarn line res = liftIO $ do
    unless (line `elem` lines (crStderr res)) $
        expectationFailure (toString (clientReport res ("wrote no stderr line " <> show line)))
    pure res

-- | The command, outcome, and captured output for a failed client assertion.
clientReport :: ClientResult -> Text -> Text
clientReport res outcome =
    crCommand res
        <> " "
        <> outcome
        <> "!\nSTDOUT:\n"
        <> crStdout res
        <> "\nSTDERR:\n"
        <> crStderr res

-- | Portable artifacts make no foreign-architecture claim. Architecture-specific installs name their target.
data TargetPortability = ArchitectureIndependent | ArchitectureSpecific Text
    deriving stock (Eq, Show)

-- | Independent expected selections and installed marker bytes for one target.
data TargetExpectation = TargetExpectation
    { targetExpectedPortability :: TargetPortability
    , targetExpectedArtifacts :: [Text]
    , targetExpectedMarkers :: [(FilePath, ByteString)]
    }

-- | Evidence from a real isolated installation, including its native source report and installed graph.
data TargetObservation graph = TargetObservation
    { targetPortability :: TargetPortability
    , targetClient :: ClientResult
    , targetGraph :: Either Text graph
    , targetArtifacts :: Either Text [Text]
    , targetTree :: InstalledTree
    }

-- | A real unsupported-target attempt and every file it left in its new destination.
data TargetRefusal = TargetRefusal
    { targetRefusedClient :: ClientResult
    , targetRefusedTree :: InstalledTree
    }

-- | Require actual target selection, installed contents, and the applicable refusal control.
crossTargetInstallCase :: (Eq graph, Show graph) => graph -> (TargetExpectation, TargetExpectation) -> (TargetObservation graph, TargetObservation graph) -> Maybe TargetRefusal -> Expectation
crossTargetInstallCase graph (sourceExpected, mirrorExpected) (source, mirrored) refused = do
    targetInstallCase graph sourceExpected source
    targetInstallCase graph mirrorExpected mirrored
    case (targetExpectedPortability sourceExpected, targetExpectedPortability mirrorExpected) of
        (ArchitectureIndependent, ArchitectureIndependent) -> do
            targetExpectedArtifacts sourceExpected `shouldMatchList` targetExpectedArtifacts mirrorExpected
            isNothing refused `shouldBe` True
        (ArchitectureSpecific sourceArchitecture, ArchitectureSpecific mirrorArchitecture) -> do
            sourceArchitecture `shouldNotBe` ""
            mirrorArchitecture `shouldNotBe` sourceArchitecture
            mirrorArchitecture `shouldNotBe` ""
            targetExpectedMarkers sourceExpected `shouldSatisfy` (not . null)
            targetExpectedMarkers mirrorExpected `shouldSatisfy` (not . null)
            case refused of
                Just control -> do
                    void (shouldFail (targetRefusedClient control))
                    targetRefusedTree control `shouldBe` Map.empty
                Nothing -> expectationFailure "architecture-specific installation lacks its unsupported-target control"
        _ -> expectationFailure "source and mirror disagree about artifact portability"

-- | Require the declared target, graph, artifact selection, and installed marker bytes.
targetInstallCase :: (Eq graph, Show graph) => graph -> TargetExpectation -> TargetObservation graph -> Expectation
targetInstallCase graph expected observed = do
    void (shouldSucceed (targetClient observed))
    targetPortability observed `shouldBe` targetExpectedPortability expected
    targetGraph observed `shouldBe` Right graph
    targetExpectedArtifacts expected `shouldSatisfy` (not . null)
    actual <- either (fail . toString) pure (targetArtifacts observed)
    actual `shouldMatchList` targetExpectedArtifacts expected
    for_ (targetExpectedMarkers expected) $ \(path, bytes) ->
        Map.lookup path (targetTree observed) `shouldBe` Just (TreeFile False bytes)
