-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Offline contracts for the owner-run observation, including redaction of untrusted evidence.
module Ecluse.Test.Registry.PyPI.ObservationSpec (spec) where

import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Client (Request (..))
import Network.HTTP.Types.Header (hAccept, hAcceptEncoding, hAuthorization)
import Test.Hspec

import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Fault (TransportCause (..), transportFault)
import Ecluse.Core.Registry (FetchFault (..), RegistryResponse (..), UrlFormationError (UnparseableUrl))
import Ecluse.Core.Registry.PyPI.Project (canonicalName, projectName)
import Ecluse.Core.Registry.PyPI.Wire (simpleIndexMediaType)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge))
import Ecluse.Test.Registry.PyPI.Observation
import Ecluse.Test.Support (expectRight)

-- | These checks never discover AWS identity or open a connection.
spec :: Spec
spec = describe "CodeArtifact observation preparation" $ do
    describe "configuration" $ do
        it "leaves absent observation inputs pending for token-only use" $
            (isNothing <$> observationConfig (`Map.lookup` tokenInputs)) `shouldBe` Right True
        it "preserves the owner and parses the canonical project" $ do
            config <- expectRight (observationConfig (`Map.lookup` configuredInputs))
            (ocOwner <$> config) `shouldBe` Just (Just "123456789012")
            (canonicalName . ocPackage <$> config) `shouldBe` Just "sample-project"
        forM_ [RegionInput, DomainInput, RepositoryInput, PackageInput] $ \key ->
            it ("refuses a missing " <> toString (smokeInputName key)) $
                configFailure (Map.delete key configuredInputs) `shouldBe` Just (InvalidInput key)
        forM_ [minBound .. maxBound] $ \key ->
            it ("refuses an empty " <> toString (smokeInputName key)) $
                configFailure (Map.insert key " " configuredInputs) `shouldBe` Just (InvalidInput key)
        it "allows the owner to be absent" $ do
            config <- expectRight (observationConfig (`Map.lookup` Map.delete OwnerInput configuredInputs))
            (ocOwner <$> config) `shouldBe` Just Nothing
        it "refuses invalid package syntax without exposing it" $ do
            let failure = configFailure (Map.insert PackageInput "private-canary/package" configuredInputs)
            failure `shouldBe` Just (InvalidInput PackageInput)
            (renderObservationError <$> failure) `shouldBe` Just "invalid or missing ECLTEST_SMOKE_CODEARTIFACT_PACKAGE"
    describe "discovered endpoint and request" $ do
        it "keeps the repository path and uses aws Basic authentication without redirects" $ do
            target <- fixtureTarget endpoint
            request <- expectRight (observationRequest target (mkSecret "dummy"))
            secure request `shouldBe` True
            method request `shouldBe` "GET"
            host request `shouldBe` "repo.example"
            path request `shouldBe` "/pypi/repository/simple/sample-project/"
            queryString request `shouldBe` ""
            redirectCount request `shouldBe` 0
            lookup hAuthorization (requestHeaders request) `shouldBe` Just "Basic YXdzOmR1bW15"
            lookup hAccept (requestHeaders request) `shouldBe` Just simpleIndexMediaType
            lookup hAcceptEncoding (requestHeaders request) `shouldBe` Just "gzip"
        forM_ invalidEndpoints $ \raw ->
            it ("refuses endpoint fixture " <> show (fst raw)) $ do
                package <- expectRight (projectName "sample-project")
                leftToMaybe (observationTarget (snd raw) package) `shouldBe` Just EndpointInvalid
    describe "raw index evidence" $ do
        it "keeps unfamiliar authorities, sorts rows and removes duplicate observations" $ do
            target <- fixtureTarget endpoint
            rows <- expectRight (observeResponse target (indexResponse (map file ["https://z.example/a.whl", "https://repo.example/b.whl", "https://z.example/c.whl"])))
            length rows `shouldBe` 2
            NE.toList rows `shouldSatisfy` all (T.isInfixOf "endpoint=repo.example:443")
            NE.head rows `shouldSatisfy` T.isInfixOf "file=repo.example:443 scheme=https reference=absolute same-authority=true"
            NE.last rows `shouldSatisfy` T.isInfixOf "file=z.example:443 scheme=https reference=absolute same-authority=false"
        forM_ referenceCases $ \(url, expected) ->
            it ("resolves " <> toString expected) $ do
                target <- fixtureTarget endpoint
                rows <- expectRight (observeResponse target (indexResponse [file url]))
                NE.toList rows `shouldBe` ["endpoint=repo.example:443 " <> expected <> " query=false fragment=false"]
        forM_ ["ftp://files.example/file", "https:///file", "http://files.example:0/file", "https://[broken/file", "", "bad reference"] $ \url ->
            it "refuses an unsupported or unresolvable file reference" $ do
                target <- fixtureTarget endpoint
                observeResponse target (indexResponse [file url]) `shouldBe` Left FileReferenceInvalid
        it "refuses an empty index or a dropped file" $ do
            target <- fixtureTarget endpoint
            observeResponse target (indexResponse []) `shouldBe` Left IndexEmpty
            observeResponse target (indexResponse [file "file.whl", object ["filename" .= ("dropped-canary" :: Text)]]) `shouldBe` Left IndexMalformedEntries
        forM_ invalidBodies $ \(body, expected) ->
            it ("refuses " <> show expected) $ do
                target <- fixtureTarget endpoint
                observeResponse target (response 200 body) `shouldBe` Left expected
        forM_ [301, 302, 401, 403, 404, 500] $ \status ->
            it ("refuses status " <> show status <> " before decoding its body") $ do
                target <- fixtureTarget endpoint
                observeResponse target (response status "body-canary") `shouldBe` Left (IndexStatus status)
    describe "redaction" $ do
        it "removes userinfo, arbitrary path components, query and fragment values" $ do
            target <- fixtureTarget endpoint
            rows <- expectRight (observeResponse target (indexResponse [file "https://user-canary:password-canary@FILES.example:8443/pypi/path-canary/simple/name-canary/file-canary.whl?query-canary#fragment-canary"]))
            NE.toList rows
                `shouldBe` ["endpoint=repo.example:443 file=files.example:8443 scheme=https reference=absolute same-authority=false path=/pypi/<segment>/simple/<segment>/<segment>.whl query=true fragment=true"]
            T.intercalate "\n" (NE.toList rows) `shouldSatisfy` (not . T.isInfixOf "canary")
        it "drops raw transport detail and malformed request URLs" $ do
            target <- fixtureTarget endpoint
            forM_ [TransportTimeout, TransportUnreachable, TransportTls, TransportProtocol] $ \cause -> do
                let result = observeResponse target (Left (FetchTransport (transportFault cause "transport-canary")))
                leftToMaybe result `shouldBe` Just (IndexTransportFailed cause)
                renderObservationError <$> leftToMaybe result `shouldSatisfy` maybe False (not . T.isInfixOf "canary")
            observeResponse target (Left (FetchUrlUnformable (UnparseableUrl "url-canary"))) `shouldBe` Left IndexRequestInvalid
            observeResponse target (Left (FetchBoundExceeded (BodyTooLarge (MetadataBodyLimit 1)))) `shouldBe` Left IndexBoundExceeded

endpoint :: Text
endpoint = "https://repo.example/pypi/repository/"

tokenInputs :: Map SmokeInput Text
tokenInputs = Map.fromList [(RegionInput, "us-east-1"), (DomainInput, "domain")]

configuredInputs :: Map SmokeInput Text
configuredInputs = tokenInputs <> Map.fromList [(OwnerInput, "123456789012"), (RepositoryInput, "repository"), (PackageInput, "Sample_Project")]

configFailure :: Map SmokeInput Text -> Maybe ObservationError
configFailure inputs = leftToMaybe (observationConfig (`Map.lookup` inputs))

fixtureTarget :: Text -> IO ObservationTarget
fixtureTarget url = do
    package <- expectRight (projectName "Sample_Project")
    expectRight (observationTarget url package)

file :: Text -> Value
file url = object ["filename" .= ("sample_project-1.0-py3-none-any.whl" :: Text), "url" .= url]

indexResponse :: [Value] -> Either FetchFault RegistryResponse
indexResponse files = response 200 . LBS.toStrict . encode $ object ["name" .= ("sample-project" :: Text), "files" .= files]

response :: Int -> ByteString -> Either FetchFault RegistryResponse
response status body = Right (RegistryResponse status (BS.length body) body)

invalidEndpoints :: [(Int, Text)]
invalidEndpoints =
    zip
        [1 ..]
        [ "http://repo.example/pypi/repository/"
        , "https://userinfo-canary@repo.example/"
        , "https://repo.example/?query-canary"
        , "https://repo.example/#fragment-canary"
        , "https:///missing-host"
        , "https://repo.example:0/path"
        , "https://repo.example/invalid path"
        ]

invalidBodies :: [(ByteString, ObservationError)]
invalidBodies =
    [ ("decoder-canary", IndexDecodeFailed)
    , ("{\"meta\":{\"api-version\":\"2.decoder-canary\"}}", IndexDecodeFailed)
    , ("{\"name\":\"mismatch-canary\",\"files\":[]}", IndexProjectMismatch)
    ]

referenceCases :: [(Text, Text)]
referenceCases =
    [ ("https://FILES.example:8443/a.whl", "file=files.example:8443 scheme=https reference=absolute same-authority=false path=/<segment>.whl")
    , ("//repo.EXAMPLE:443/a.zip", "file=repo.example:443 scheme=https reference=scheme-relative same-authority=true path=/<segment>.zip")
    , ("/pypi/repo/a.tar.gz", "file=repo.example:443 scheme=https reference=root-relative same-authority=true path=/pypi/<segment>/<segment>.tar.gz")
    , ("a.whl", "file=repo.example:443 scheme=https reference=path-relative same-authority=true path=/pypi/<segment>/simple/<segment>/<segment>.whl")
    , ("../a.whl", "file=repo.example:443 scheme=https reference=path-relative same-authority=true path=/pypi/<segment>/simple/<segment>.whl")
    , ("http://repo.example/a.whl", "file=repo.example:80 scheme=http reference=absolute same-authority=false path=/<segment>.whl")
    , ("https://[2001:DB8::1]:8443/a.whl", "file=[2001:db8::1]:8443 scheme=https reference=absolute same-authority=false path=/<segment>.whl")
    ]
