-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Registry.PyPI.RequestSpec (spec) where

import Data.List (lookup)
import Network.HTTP.Client qualified as Client
import Test.Hspec

import Ecluse.Core.BuildIdentity (userAgent)
import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Registry (UrlFormationError (EmptyBaseUrl))
import Ecluse.Core.Registry.PyPI.Request (
    artifactFileUrl,
    artifactRequestByFile,
    artifactRequestByUrl,
    pypiArtifactHosts,
    simpleIndexRequest,
    simpleIndexUrl,
 )
import Ecluse.Test.Package (requestsName)
import Ecluse.Test.Registry.PyPI (alicePair)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    indexSpec
    artifactSpec
    filesHostSpec
    sealSpec

indexSpec :: Spec
indexSpec = describe "the Simple-index read" $ do
    it "addresses the project under /simple/ with its trailing slash, which the index would redirect to" $
        simpleIndexUrl "https://pypi.org" requestsName `shouldBe` Right "https://pypi.org/simple/requests/"

    it "normalises the project name to its PEP 503 canonical spelling, which needs no redirect" $
        simpleIndexUrl "https://pypi.org" zopeInterface `shouldBe` Right "https://pypi.org/simple/zope-interface/"

    it "joins onto a base URL that writes its own trailing slash" $
        simpleIndexUrl "https://index.test/pypi/" requestsName `shouldBe` Right "https://index.test/pypi/simple/requests/"

    it "refuses an empty base URL rather than forming a relative request" $
        simpleIndexUrl "" requestsName `shouldBe` Left EmptyBaseUrl

    it "asks for the PEP 691 JSON form and no HTML one" $ do
        req <- expectRight (simpleIndexRequest "https://pypi.org" Nothing requestsName)
        lookup "Accept" (Client.requestHeaders req) `shouldBe` Just "application/vnd.pypi.simple.v1+json"

    it "asks for gzip, because a project's index runs to megabytes" $ do
        req <- expectRight (simpleIndexRequest "https://pypi.org" Nothing requestsName)
        lookup "Accept-Encoding" (Client.requestHeaders req) `shouldBe` Just "gzip"

    it "attaches the caller's pair on the passthrough read" $ do
        req <- expectRight (simpleIndexRequest "https://index.test" alicePair requestsName)
        lookup "Authorization" (Client.requestHeaders req) `shouldBe` Just "Basic YWxpY2U6aHVudGVyMg=="

    it "sends no credential header on an anonymous read" $ do
        req <- expectRight (simpleIndexRequest "https://pypi.org" Nothing requestsName)
        lookup "Authorization" (Client.requestHeaders req) `shouldBe` Nothing

artifactSpec :: Spec
artifactSpec = describe "the artifact read" $ do
    it "addresses a file under the one spelling this mount serves" $
        artifactFileUrl "https://index.test" requestsName "requests-2.34.2-py3-none-any.whl"
            `shouldBe` Right "https://index.test/simple/requests/requests-2.34.2-py3-none-any.whl"

    it "percent-encodes the file name, so a decoded escape cannot reach the upstream raw" $
        artifactFileUrl "https://index.test" requestsName "a/../b.whl"
            `shouldBe` Right "https://index.test/simple/requests/a%2F..%2Fb.whl"

    it "advertises no encoding and does not decompress, so the served sha256 verifies" $ do
        req <- expectRight (artifactRequestByFile "https://index.test" Nothing requestsName "requests-2.34.2.tar.gz")
        Client.decompress req "application/gzip" `shouldBe` False
        Client.requestHeaders req `shouldNotSatisfy` any ((== "accept-encoding") . fst)

    it "fetches an absolute file location without naming a base URL" $ do
        req <- expectRight (artifactRequestByUrl Nothing "https://files.pythonhosted.org/packages/a0/requests-2.34.2.tar.gz")
        Client.host req `shouldBe` "files.pythonhosted.org"
        Client.path req `shouldBe` "/packages/a0/requests-2.34.2.tar.gz"
        Client.decompress req "application/gzip" `shouldBe` False

    it "carries the mount credential on a by-URL fetch that names one" $ do
        req <- expectRight (artifactRequestByUrl alicePair "https://index.test/packages/requests-2.34.2.tar.gz")
        lookup "Authorization" (Client.requestHeaders req) `shouldBe` Just "Basic YWxpY2U6aHVudGVyMg=="

    it "fetches anonymously when the caller names no credential" $ do
        req <- expectRight (artifactRequestByUrl Nothing "https://files.pythonhosted.org/packages/a0/requests-2.34.2.tar.gz")
        lookup "Authorization" (Client.requestHeaders req) `shouldBe` Nothing

filesHostSpec :: Spec
filesHostSpec =
    describe "the declared artifact host" $
        it "names the files host public PyPI serves distribution bytes from" $
            pypiArtifactHosts `shouldBe` ["https://files.pythonhosted.org"]

sealSpec :: Spec
sealSpec = describe "every request carries the shared outbound seal" $ do
    it "pins the redirect count on the index read, credentialed or not" $ do
        anonymous <- expectRight (simpleIndexRequest "https://pypi.org" Nothing requestsName)
        Client.redirectCount anonymous `shouldBe` 0
        credentialed <- expectRight (simpleIndexRequest "https://index.test" alicePair requestsName)
        Client.redirectCount credentialed `shouldBe` 0

    it "pins the redirect count on both artifact arms" $ do
        byFile <- expectRight (artifactRequestByFile "https://index.test" alicePair requestsName "requests-2.34.2.tar.gz")
        Client.redirectCount byFile `shouldBe` 0
        byUrl <- expectRight (artifactRequestByUrl alicePair "https://index.test/packages/requests-2.34.2.tar.gz")
        Client.redirectCount byUrl `shouldBe` 0

    it "identifies the proxy without spelling a User-Agent of its own" $ do
        req <- expectRight (simpleIndexRequest "https://pypi.org" Nothing requestsName)
        lookup "User-Agent" (Client.requestHeaders req) `shouldBe` Just userAgent

-- | A project whose published spelling is not its canonical one.
zopeInterface :: PackageName
zopeInterface = mkPackageName PyPI Nothing "Zope.Interface"
