-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native relay controls separate client checksum checks from source authority and transport framing.
module Ecluse.E2E.RubyGems.RelayE2ESpec (rubyRelayContract) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Types (RequestHeaders)
import System.Exit (ExitCode (ExitFailure))
import Test.Hspec (around, it, shouldBe, shouldReturn, shouldSatisfy)

import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (..))
import Ecluse.E2E.Fixtures.RubyGems
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.NativeEdge
import Ecluse.E2E.Harness.RubyGems
import Ecluse.E2E.Harness.RubyGemsRegistry
import Ecluse.E2E.RubyGems.InstallE2ESpec (rubyConsumer)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Ecosystem (FamilyMetadata (..))
import Ecluse.Test.Contract.Phase (timedFixture)
import Ecluse.Test.Contract.Runner (DeclaredGroup (..))
import Ecluse.Test.Contract.Serving (ServingResponse (..))
import Ecluse.Test.Contract.Types
import Ecluse.Test.Contract.Types qualified as Contract
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Registry.RubyGems.Fixtures

-- | Every failure owns a source, a fresh image and independently observed native request headers.
rubyRelayContract :: DeclaredGroup
rubyRelayContract = DeclaredGroup metadata $ \recorder ->
    around (timedFixture recorder (withFixture fixture)) $ do
        it (label "initial-digest" "refuses mismatched bytes during an initial install without a lockfile or mirror") initialDigest
        for_ [("private-401", 401), ("private-403", 403), ("private-digest", 200)] $ \(name, status) ->
            it (label name "preserves a private refusal without reading the public artifact") (privateFailure status)
        it (label "private-miss" "uses the public source facts after a genuine private artifact miss") privateMiss
        it (label "known-length" "serves an exact-bound native archive with its declared length") (exactBound False)
        it (label "chunked" "serves the same exact-bound archive from independently flushed chunks") (exactBound True)
        it (label "bound-known" "streams declared bytes beyond the archive inspection bound") (overBound False)
        it (label "bound-chunked" "streams chunked bytes beyond the archive inspection bound") (overBound True)
        for_ [("origin-304", 304), ("origin-206", 206)] $ \(name, status) ->
            it (label name "preserves the native origin representation status") (unexpectedStatus status)
        it (label "conditional-fetch" "forwards conditional validators and preserves the origin 304") (conditional False)
        it (label "conditional-mismatch" "relays changed bytes without claiming server checksum verification") (conditional True)
        it (label "conditional-revision" "preserves origin validators when admitted bytes change") conditionalRevision
        it (label "range" "fetches the complete representation when a client requests a range") rangeRequest
        it (label "head" "uses an actual upstream HEAD without fetching an archive body") headRequest
        it (label "head-conditional" "preserves an origin 304 through actual HEAD") headConditional
        it (label "head-private-auth" "checks current private authorisation before a matching HEAD validator") headPrivateAuth
  where
    fixture = rubyReadFixture "relay" ["read", "client-install"]
    schedule = fixtureMetadata fixture
    metadata =
        Group
            "rubygems/relay"
            "rubygems/relay"
            "rubygems/read"
            Contract.E2E
            "rubygems"
            [Case ("rubygems/relay/" <> name) ["rubygems/relay"] | name <- ["initial-digest", "private-401", "private-403", "private-digest", "private-miss", "known-length", "chunked", "bound-known", "bound-chunked", "origin-304", "origin-206", "conditional-fetch", "conditional-mismatch", "conditional-revision", "range", "head", "head-conditional", "head-private-auth"]]
            (familyCapabilities schedule)
            (familyPrerequisites schedule)
            (familyResources schedule)
            (familyIsolation schedule)
    label name = caseLabel ("rubygems/relay/" <> name)

withRelay :: GlobalDataPlane -> (RubyRegistry -> RubyRelease -> IO ()) -> IO ()
withRelay plane action = withRubyRegistry $ \registry -> do
    releases <- loadRubyReleases
    release <- rubyRelease releases rubyLeaf rubyVersion "ruby"
    withStubFault plane (RubyGemsBackend (rubyRegistryPort registry) False) (action registry release)

serveOnly :: E2EConfig
serveOnly = defaultE2EConfig{ecArgs = ["proxy", "--no-worker"]}

bounded :: Int -> E2EConfig
bounded bytes = serveOnly{ecExtraEnv = [("ECLUSE_LIMITS__MAX_ARTIFACT_BYTES", show bytes)]}

nativeReply :: Int -> Bool -> ByteString -> RubyArtifactResponse
nativeReply status chunked bytes = RubyArtifactResponse status [] [BS.take 7 bytes, BS.drop 7 bytes] chunked

setReply :: RubyRegistry -> Text -> RubyRelease -> RubyArtifactResponse -> IO ()
setReply registry host release = setRubyArtifactResponse registry host (releaseFilename release) . Just

requests :: RubyRegistry -> IO [(Text, ByteString, Text, RequestHeaders)]
requests = readTVarIO . rubyRegistryHeaders

artifactRequests :: Int -> RubyRegistry -> RubyRelease -> IO [(Text, ByteString, RequestHeaders)]
artifactRequests before registry release = do
    observed <- drop before <$> requests registry
    pure [(host, method, headers) | (host, method, path, headers) <- observed, path == "/gems/" <> releaseFilename release]

rejected :: Int -> ServingResponse -> IO ()
rejected status response = do
    responseStatus response `shouldBe` status
    let bytes = LBS.toStrict (responseBody response)
    decodeUtf8' bytes `shouldSatisfy` isRight
    BS.elem 0 bytes `shouldBe` False
    marker `BS.isInfixOf` bytes `shouldBe` False

marker :: ByteString
marker = BS.pack [0 .. 255]

initialDigest :: GlobalDataPlane -> IO ()
initialDigest plane = for_ [minimumBundler, currentBundler] $ \client -> withRelay plane $ \registry release -> do
    rubyRegistryFiles registry "rubygems-mirror" `shouldReturn` []
    withObservedNativeProxy serveOnly plane $ \observed -> do
        rubyConsumer (observedProxy observed) client $ \control -> do
            rubyDependency control rubyLeaf ("= " <> rubyVersion)
            rubyLockfile control `shouldReturn` Nothing
            void (rubyInstall control False >>= shouldSucceed)
        let changed = releaseArchive release <> "changed"
        setReply registry "rubygems-upstream" release (nativeReply 200 True changed)
        before <- length <$> requests registry
        rubyConsumer (observedProxy observed) client $ \project -> do
            rubyDependency project rubyLeaf ("= " <> rubyVersion)
            rubyLockfile project `shouldReturn` Nothing
            result <- runBundler project ["install", "--verbose"] >>= shouldFail
            crExit result `shouldBe` ExitFailure 37
            (crStdout result <> crStderr result) `shouldSatisfy` T.isInfixOf "ChecksumMismatchError"
            tree <- rubyInstalledTree project
            Map.keys tree `shouldSatisfy` (not . any (T.isSuffixOf "ecluse_fixture_leaf.rb" . toText))
        attempted <- artifactRequests before registry release
        attempted `shouldSatisfy` (not . null)
        proxyGet (observedProxy observed) (rubyArchiveRoute release) `shouldReturn` (200, LBS.fromStrict changed)
    rubyRegistryFiles registry "rubygems-mirror" `shouldReturn` []

privateFailure :: Int -> GlobalDataPlane -> IO ()
privateFailure expected plane = withRelay plane $ \registry release -> do
    withObservedNativeProxy serveOnly plane $ \observed -> do
        good <- proxyGet (observedProxy observed) (rubyArchiveRoute release)
        good `shouldBe` (200, LBS.fromStrict (releaseArchive release))
    seedRubyRegistry registry "rubygems-private" [release]
    let config = serveOnly{ecExtraEnv = [("ECLUSE_MOUNTS__RUBYGEMS__PRIVATE_UPSTREAM__REGISTRY__URL", stubUrl RubyGemsPrivate)]}
        changed = releaseArchive release <> "changed"
    setReply registry "rubygems-private" release (nativeReply expected False changed)
    withObservedNativeProxy config plane $ \observed -> do
        before <- length <$> requests registry
        if expected == 200
            then for_ [minimumBundler, currentBundler] $ \client -> rubyConsumer (observedProxy observed) client $ \project -> do
                rubyDependency project rubyLeaf ("= " <> rubyVersion)
                rubyLockfile project `shouldReturn` Nothing
                result <- runBundler project ["install", "--verbose"] >>= shouldFail
                crExit result `shouldBe` ExitFailure 37
                (crStdout result <> crStderr result) `shouldSatisfy` T.isInfixOf "ChecksumMismatchError"
                tree <- rubyInstalledTree project
                Map.keys tree `shouldSatisfy` (not . any (T.isSuffixOf "ecluse_fixture_leaf.rb" . toText))
            else proxyRequest "GET" [] "" (observedProxy observed) (rubyArchiveRoute release) >>= rejected expected
        actual <- artifactRequests before registry release
        map (\(host, method, _) -> (host, method)) actual `shouldBe` replicate (if expected == 200 then 2 else 1) ("rubygems-private", "GET")

exactBound :: Bool -> GlobalDataPlane -> IO ()
exactBound chunked plane = withRelay plane $ \registry release -> do
    let bytes = releaseArchive release
    setReply registry "rubygems-upstream" release (nativeReply 200 chunked bytes)
    withObservedNativeProxy (bounded (BS.length bytes)) plane $ \observed -> do
        proxyGet (observedProxy observed) (rubyArchiveRoute release) `shouldReturn` (200, LBS.fromStrict bytes)
        seen <- artifactRequests 0 registry release
        seen `shouldSatisfy` (not . null)
        for_ seen $ \(_, method, headers) -> do
            method `shouldBe` "GET"
            lookup "Accept-Encoding" headers `shouldBe` Just "identity"

overBound :: Bool -> GlobalDataPlane -> IO ()
overBound chunked plane = withRelay plane $ \registry release -> do
    let cap = BS.length (releaseArchive release)
        bytes = releaseArchive release <> "x"
        digest = hexSha256Of bytes
        advertised = release{releaseChecksum = digest, releaseInfoRow = T.replace (releaseChecksum release) digest (releaseInfoRow release)}
    seedRubyRegistry registry "rubygems-upstream" [advertised]
    setReply registry "rubygems-upstream" release (nativeReply 200 chunked bytes)
    withObservedNativeProxy (bounded (cap + 1)) plane $ \observed ->
        proxyGet (observedProxy observed) (rubyArchiveRoute release) `shouldReturn` (200, LBS.fromStrict bytes)
    withObservedNativeProxy (bounded cap) plane $ \observed ->
        proxyGet (observedProxy observed) (rubyArchiveRoute release) `shouldReturn` (200, LBS.fromStrict bytes)

unexpectedStatus :: Int -> GlobalDataPlane -> IO ()
unexpectedStatus status plane = withRelay plane $ \registry release -> do
    let bytes = releaseArchive release
        reply = (nativeReply status False (if status == 304 then "" else bytes)){artifactReplyHeaders = [("Content-Range", "bytes 0-" <> show (BS.length bytes - 1) <> "/" <> show (BS.length bytes)) | status == 206]}
    setReply registry "rubygems-upstream" release reply
    withObservedNativeProxy serveOnly plane $ \observed -> do
        response <- proxyRequest "GET" [] "" (observedProxy observed) (rubyArchiveRoute release)
        responseStatus response `shouldBe` status
        responseBody response `shouldBe` (if status == 304 then "" else LBS.fromStrict bytes)
        actual <- artifactRequests 0 registry release
        actual `shouldSatisfy` (not . null)
        for_ actual $ \(_, method, headers) -> do
            method `shouldBe` "GET"
            lookup "If-None-Match" headers `shouldBe` Nothing
            lookup "Range" headers `shouldBe` Nothing

conditional :: Bool -> GlobalDataPlane -> IO ()
conditional tamper plane = withRelay plane $ \registry release -> do
    let reply bytes = (nativeReply 200 False bytes){artifactReplyHeaders = [("ETag", "\"fixture-tag\""), ("Last-Modified", "Wed, 21 Oct 2015 07:28:00 GMT")]}
    setReply registry "rubygems-upstream" release (reply (releaseArchive release))
    withObservedNativeProxy serveOnly plane $ \observed -> do
        initial <- proxyRequest "GET" [] "" (observedProxy observed) (rubyArchiveRoute release)
        responseStatus initial `shouldBe` 200
        tag <- maybe (fail "native response omitted its origin validator") pure (lookup "ETag" (responseHeaders initial))
        setReply registry "rubygems-upstream" release $
            if tamper then reply marker else (reply ""){artifactReplyStatus = 304}
        before <- length <$> requests registry
        response <- proxyRequest "GET" [("If-None-Match", tag), ("If-Modified-Since", "Wed, 21 Oct 2015 07:28:00 GMT")] "" (observedProxy observed) (rubyArchiveRoute release)
        if tamper
            then do
                responseStatus response `shouldBe` 200
                responseBody response `shouldBe` LBS.fromStrict marker
                lookup "ETag" (responseHeaders response) `shouldBe` Just tag
            else do
                responseStatus response `shouldBe` 304
                responseBody response `shouldBe` ""
                lookup "ETag" (responseHeaders response) `shouldBe` Just tag
        actual <- artifactRequests before registry release
        length actual `shouldBe` 1
        for_ actual $ \(_, method, headers) -> do
            method `shouldBe` "GET"
            lookup "If-None-Match" headers `shouldBe` Just tag
            lookup "If-Modified-Since" headers `shouldBe` Just "Wed, 21 Oct 2015 07:28:00 GMT"

rangeRequest :: GlobalDataPlane -> IO ()
rangeRequest plane = withRelay plane $ \registry release ->
    withObservedNativeProxy serveOnly plane $ \observed -> do
        response <- proxyRequest "GET" [("Range", "bytes=0-7"), ("If-Range", "\"fixture-tag\"")] "" (observedProxy observed) (rubyArchiveRoute release)
        responseStatus response `shouldBe` 200
        responseBody response `shouldBe` LBS.fromStrict (releaseArchive release)
        actual <- artifactRequests 0 registry release
        length actual `shouldBe` 1
        for_ actual $ \(_, _, headers) -> do
            lookup "Range" headers `shouldBe` Nothing
            lookup "If-Range" headers `shouldBe` Nothing

headRequest :: GlobalDataPlane -> IO ()
headRequest plane = withRelay plane $ \registry release ->
    withObservedNativeProxy serveOnly plane $ \observed -> do
        response <- proxyHead (observedProxy observed) (rubyArchiveRoute release)
        responseStatus response `shouldBe` 200
        responseBody response `shouldBe` ""
        lookup "Content-Length" (responseHeaders response) `shouldBe` Just (show (BS.length (releaseArchive release)))
        actual <- artifactRequests 0 registry release
        map (\(_, method, _) -> method) actual `shouldBe` ["HEAD"]

headConditional :: GlobalDataPlane -> IO ()
headConditional plane = withRelay plane $ \registry release -> do
    setReply registry "rubygems-upstream" release ((nativeReply 200 False (releaseArchive release)){artifactReplyHeaders = [("ETag", "\"fixture-tag\"")]})
    withObservedNativeProxy serveOnly plane $ \observed -> do
        initial <- proxyRequest "GET" [] "" (observedProxy observed) (rubyArchiveRoute release)
        responseStatus initial `shouldBe` 200
        tag <- maybe (fail "GET has no origin validator") pure (lookup "ETag" (responseHeaders initial))
        setReply registry "rubygems-upstream" release ((nativeReply 304 False ""){artifactReplyHeaders = [("ETag", tag)]})
        before <- length <$> requests registry
        response <- proxyRequest "HEAD" [("If-None-Match", tag)] "" (observedProxy observed) (rubyArchiveRoute release)
        responseStatus response `shouldBe` 304
        responseBody response `shouldBe` ""
        lookup "ETag" (responseHeaders response) `shouldBe` Just tag
        actual <- artifactRequests before registry release
        map (\(_, method, _) -> method) actual `shouldBe` ["HEAD"]
        for_ actual $ \(_, _, headers) -> lookup "If-None-Match" headers `shouldBe` Just tag

headPrivateAuth :: GlobalDataPlane -> IO ()
headPrivateAuth plane = withRelay plane $ \registry release -> do
    seedRubyRegistry registry "rubygems-private" [release]
    setReply registry "rubygems-private" release ((nativeReply 200 False (releaseArchive release)){artifactReplyHeaders = [("ETag", "\"private-tag\"")]})
    let config = serveOnly{ecExtraEnv = [("ECLUSE_MOUNTS__RUBYGEMS__PRIVATE_UPSTREAM__REGISTRY__URL", stubUrl RubyGemsPrivate)]}
    withObservedNativeProxy config plane $ \observed -> do
        initial <- proxyRequest "GET" [] "" (observedProxy observed) (rubyArchiveRoute release)
        responseStatus initial `shouldBe` 200
        tag <- maybe (fail "private GET has no origin validator") pure (lookup "ETag" (responseHeaders initial))
        setReply registry "rubygems-private" release (nativeReply 401 False marker)
        before <- length <$> requests registry
        response <- proxyRequest "HEAD" [("If-None-Match", tag)] "" (observedProxy observed) (rubyArchiveRoute release)
        responseStatus response `shouldBe` 401
        responseBody response `shouldBe` ""
        actual <- artifactRequests before registry release
        map (\(host, method, _) -> (host, method)) actual `shouldBe` [("rubygems-private", "HEAD")]
        for_ actual $ \(_, _, headers) -> lookup "If-None-Match" headers `shouldBe` Just tag

conditionalRevision :: GlobalDataPlane -> IO ()
conditionalRevision plane = withRelay plane $ \registry release -> do
    let reply bytes = (nativeReply 200 False bytes){artifactReplyHeaders = [("ETag", "\"unchanged-origin-tag\"")]}
    setReply registry "rubygems-upstream" release (reply (releaseArchive release))
    oldTag <- withObservedNativeProxy serveOnly plane $ \observed -> do
        initial <- proxyRequest "GET" [] "" (observedProxy observed) (rubyArchiveRoute release)
        responseStatus initial `shouldBe` 200
        maybe (fail "initial response has no origin validator") pure (lookup "ETag" (responseHeaders initial))
    let changed = releaseArchive release <> "x"
        digest = hexSha256Of changed
    seedRubyRegistry registry "rubygems-upstream" [release{releaseChecksum = digest, releaseInfoRow = T.replace (releaseChecksum release) digest (releaseInfoRow release)}]
    setReply registry "rubygems-upstream" release (reply changed)
    withObservedNativeProxy serveOnly plane $ \observed -> do
        response <- proxyRequest "GET" [("If-None-Match", oldTag)] "" (observedProxy observed) (rubyArchiveRoute release)
        responseStatus response `shouldBe` 200
        responseBody response `shouldBe` LBS.fromStrict changed
        lookup "ETag" (responseHeaders response) `shouldBe` Just oldTag

privateMiss :: GlobalDataPlane -> IO ()
privateMiss plane = withRelay plane $ \registry release -> do
    let privateDigest = T.replicate 64 "0"
        private = release{releaseChecksum = privateDigest, releaseInfoRow = T.replace (releaseChecksum release) privateDigest (releaseInfoRow release)}
        config = serveOnly{ecExtraEnv = [("ECLUSE_MOUNTS__RUBYGEMS__PRIVATE_UPSTREAM__REGISTRY__URL", stubUrl RubyGemsPrivate)]}
    seedRubyRegistry registry "rubygems-private" [private]
    setReply registry "rubygems-private" release (nativeReply 404 False "")
    withObservedNativeProxy config plane $ \observed -> do
        response <- proxyRequest "GET" [] "" (observedProxy observed) (rubyArchiveRoute release)
        responseStatus response `shouldBe` 200
        responseBody response `shouldBe` LBS.fromStrict (releaseArchive release)
        actual <- artifactRequests 0 registry release
        map (\(host, method, _) -> (host, method)) actual `shouldBe` [("rubygems-private", "GET"), ("rubygems-upstream", "GET")]
