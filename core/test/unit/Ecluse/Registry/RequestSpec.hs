-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Registry.RequestSpec (spec) where

import Data.List (lookup)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types.Header (hIfModifiedSince, hIfNoneMatch)
import Test.Hspec (
    Spec,
    describe,
    it,
    shouldBe,
    shouldNotSatisfy,
    shouldSatisfy,
 )

import Ecluse.Core.Registry (UrlFormationError (EmptyBaseUrl, UnparseableUrl))
import Ecluse.Core.Registry.Request (
    Validators (..),
    addValidators,
    artifactRequestByUrl,
    finaliseRequest,
    joinPath,
    noValidators,
    parseRequestEither,
 )

spec :: Spec
spec = do
    finaliseRequestSpec
    artifactByUrlSpec
    validatorsSpec
    joinPathSpec
    parseSpec

finaliseRequestSpec :: Spec
finaliseRequestSpec = describe "finaliseRequest pins the redirect count for every request" $ do
    it "disables redirect following (redirectCount 0) with an identity attach" $ do
        req <- parseOrFail "https://reg.test/x"
        Client.redirectCount (finaliseRequest id req) `shouldBe` 0

    it "disables redirect following even when the attach adds a credential header" $ do
        req <- parseOrFail "https://reg.test/x"
        Client.redirectCount (finaliseRequest (addAuth "Bearer tok") req) `shouldBe` 0

    it "applies the injected credential attach" $ do
        req <- parseOrFail "https://reg.test/x"
        lookup "Authorization" (Client.requestHeaders (finaliseRequest (addAuth "Bearer tok") req))
            `shouldBe` Just "Bearer tok"

    it "leaves the request unauthenticated when the attach is identity" $ do
        req <- parseOrFail "https://reg.test/x"
        lookup "Authorization" (Client.requestHeaders (finaliseRequest id req)) `shouldBe` Nothing

    it "pins the redirect count even when the injected attach itself sets one (un-bypassable)" $ do
        req <- parseOrFail "https://reg.test/x"
        Client.redirectCount (finaliseRequest overrideRedirects req) `shouldBe` 0

artifactByUrlSpec :: Spec
artifactByUrlSpec = describe "artifactRequestByUrl (opaque, non-decompressing, by url)" $ do
    it "addresses the authoritative URL verbatim (host, path, query) and never decompresses" $ do
        let url = "https://cdn.example.net/files/abc/code-frame-7.0.0.tgz?sig=deadbeef"
        case artifactRequestByUrl id url of
            Left err -> fail ("artifactRequestByUrl failed: " <> show err)
            Right req -> do
                Client.host req `shouldBe` "cdn.example.net"
                Client.path req `shouldBe` "/files/abc/code-frame-7.0.0.tgz"
                Client.queryString req `shouldBe` "?sig=deadbeef"
                Client.decompress req "application/gzip" `shouldBe` False
                Client.requestHeaders req `shouldNotSatisfy` any ((== "accept-encoding") . fst)
                Client.redirectCount req `shouldBe` 0

    it "applies the injected credential attach onto the finalised request" $ do
        case artifactRequestByUrl (addAuth "Bearer tok-xyz") "https://private.reg/files/thing.tgz" of
            Left err -> fail ("artifactRequestByUrl failed: " <> show err)
            Right req -> lookup "Authorization" (Client.requestHeaders req) `shouldBe` Just "Bearer tok-xyz"

    it "refuses an unparseable URL as a UrlFormationError" $ do
        artifactRequestByUrl id "not a url with spaces"
            `shouldSatisfy` urlErrorWas (UnparseableUrl "not a url with spaces")

validatorsSpec :: Spec
validatorsSpec = describe "conditional-GET validators" $ do
    it "adds both If-None-Match and If-Modified-Since when present" $ do
        req <- parseOrFail "https://reg.test/x"
        let validators = Validators (Just "\"etag-123\"") (Just "Wed, 21 Oct 2015 07:28:00 GMT")
        let hs = Client.requestHeaders (addValidators validators req)
        lookup hIfNoneMatch hs `shouldBe` Just "\"etag-123\""
        lookup hIfModifiedSince hs `shouldBe` Just "Wed, 21 Oct 2015 07:28:00 GMT"

    it "adds neither header for noValidators" $ do
        req <- parseOrFail "https://reg.test/x"
        let hs = Client.requestHeaders (addValidators noValidators req)
        lookup hIfNoneMatch hs `shouldBe` Nothing
        lookup hIfModifiedSince hs `shouldBe` Nothing

joinPathSpec :: Spec
joinPathSpec = describe "joinPath guards the empty base and joins one path" $ do
    it "refuses an empty base URL as EmptyBaseUrl" $ do
        joinPath "" "is-odd" `shouldBe` Left EmptyBaseUrl

    it "joins a base with a trailing slash without doubling it" $ do
        joinPath "https://reg.test/" "is-odd" `shouldBe` Right "https://reg.test/is-odd"

    it "joins a base with no trailing slash" $ do
        joinPath "https://reg.test" "is-odd" `shouldBe` Right "https://reg.test/is-odd"

parseSpec :: Spec
parseSpec = describe "parseRequestEither maps a parse failure to UrlFormationError" $ do
    it "parses a well-formed URL" $ do
        case parseRequestEither "https://reg.test/x" of
            Left err -> fail ("expected a parseable URL: " <> show err)
            Right req -> Client.host req `shouldBe` "reg.test"

    it "refuses an unparseable URL as UnparseableUrl" $ do
        parseRequestEither "not a url with spaces"
            `shouldSatisfy` urlErrorWas (UnparseableUrl "not a url with spaces")

parseOrFail :: Text -> IO Client.Request
parseOrFail = Client.parseRequest . toString

addAuth :: ByteString -> Client.Request -> Client.Request
addAuth value req = req{Client.requestHeaders = ("Authorization", value) : Client.requestHeaders req}

-- An attach that (wrongly) reopens redirect following; the finaliser's pin must still win.
overrideRedirects :: Client.Request -> Client.Request
overrideRedirects req = req{Client.redirectCount = 10}

urlErrorWas :: UrlFormationError -> Either UrlFormationError a -> Bool
urlErrorWas expected = either (== expected) (const False)
