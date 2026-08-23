-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Registry.RequestSpec (spec) where

import Data.List (lookup)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types.Header (RequestHeaders, hIfModifiedSince, hIfNoneMatch)
import Test.Hspec (
    Spec,
    describe,
    it,
    shouldBe,
    shouldNotSatisfy,
    shouldSatisfy,
 )

import Ecluse.Core.Credential (Secret, mkSecret, unSecret)
import Ecluse.Core.Registry (UrlFormationError (EmptyBaseUrl, UnparseableUrl))
import Ecluse.Core.Registry.Request (
    CredentialMapping,
    Validators (..),
    addValidators,
    artifactRequestByUrl,
    attachCredential,
    credentialMapping,
    credentialRecover,
    finaliseRequest,
    joinPath,
    noValidators,
    parseRequestEither,
 )
import Ecluse.Test.Support (parseRequestOrFail)

spec :: Spec
spec = do
    finaliseRequestSpec
    credentialMappingSpec
    artifactByUrlSpec
    validatorsSpec
    joinPathSpec
    parseSpec

credentialMappingSpec :: Spec
credentialMappingSpec = describe "a credential mapping carries one ecosystem's presentation both ways" $ do
    it "recovers the token text from the header the mapping names" $
        credentialRecover apiKeyMapping [("X-Api-Key", "tok-abc")] `shouldBe` Just (mkSecret "tok-abc")

    it "recovers nothing when the request presents no credential in that form" $ do
        credentialRecover apiKeyMapping [("Authorization", "Bearer tok-abc")] `shouldBe` Nothing
        credentialRecover apiKeyMapping [] `shouldBe` Nothing

    it "attaches the credential under the mapping's own header, not an assumed one" $ do
        req <- parseRequestOrFail "https://reg.test/x"
        let headers = Client.requestHeaders (attachCredential apiKeyMapping (Just (mkSecret "tok-abc")) req)
        lookup "X-Api-Key" headers `shouldBe` Just "tok-abc"
        lookup "Authorization" headers `shouldBe` Nothing

    it "renders the header value through the mapping's own scheme" $ do
        req <- parseRequestOrFail "https://reg.test/x"
        lookup "Authorization" (Client.requestHeaders (attachCredential schemedMapping (Just (mkSecret "tok-abc")) req))
            `shouldBe` Just "Token tok-abc"

    it "attaches no header when the request is anonymous" $ do
        req <- parseRequestOrFail "https://reg.test/x"
        Client.requestHeaders (attachCredential apiKeyMapping Nothing req)
            `shouldBe` Client.requestHeaders req

    it "pins the redirect count with the credential attached (the attach cannot bypass it)" $ do
        req <- parseRequestOrFail "https://reg.test/x"
        Client.redirectCount (attachCredential apiKeyMapping (Just (mkSecret "tok-abc")) req) `shouldBe` 0

    it "pins the redirect count on an anonymous request too" $ do
        req <- parseRequestOrFail "https://reg.test/x"
        Client.redirectCount (attachCredential apiKeyMapping Nothing req) `shouldBe` 0

finaliseRequestSpec :: Spec
finaliseRequestSpec = describe "finaliseRequest pins the redirect count for every request" $ do
    it "disables redirect following (redirectCount 0) with an identity attach" $ do
        req <- parseRequestOrFail "https://reg.test/x"
        Client.redirectCount (finaliseRequest id req) `shouldBe` 0

    it "disables redirect following even when the attach adds a credential header" $ do
        req <- parseRequestOrFail "https://reg.test/x"
        Client.redirectCount (finaliseRequest (addAuth "Bearer tok") req) `shouldBe` 0

    it "applies the injected credential attach" $ do
        req <- parseRequestOrFail "https://reg.test/x"
        lookup "Authorization" (Client.requestHeaders (finaliseRequest (addAuth "Bearer tok") req))
            `shouldBe` Just "Bearer tok"

    it "leaves the request unauthenticated when the attach is identity" $ do
        req <- parseRequestOrFail "https://reg.test/x"
        lookup "Authorization" (Client.requestHeaders (finaliseRequest id req)) `shouldBe` Nothing

    it "pins the redirect count even when the injected attach itself sets one (un-bypassable)" $ do
        req <- parseRequestOrFail "https://reg.test/x"
        Client.redirectCount (finaliseRequest overrideRedirects req) `shouldBe` 0

artifactByUrlSpec :: Spec
artifactByUrlSpec = describe "artifactRequestByUrl (opaque, non-decompressing, by url)" $ do
    it "addresses the authoritative URL verbatim (host, path, query) and never decompresses" $ do
        let url = "https://cdn.example.net/files/abc/code-frame-7.0.0.tgz?sig=deadbeef"
        case artifactRequestByUrl apiKeyMapping Nothing url of
            Left err -> fail ("artifactRequestByUrl failed: " <> show err)
            Right req -> do
                Client.host req `shouldBe` "cdn.example.net"
                Client.path req `shouldBe` "/files/abc/code-frame-7.0.0.tgz"
                Client.queryString req `shouldBe` "?sig=deadbeef"
                Client.decompress req "application/gzip" `shouldBe` False
                Client.requestHeaders req `shouldNotSatisfy` any ((== "accept-encoding") . fst)
                Client.redirectCount req `shouldBe` 0

    it "attaches the credential under the mapping's presentation onto the finalised request" $ do
        case artifactRequestByUrl schemedMapping (Just (mkSecret "tok-xyz")) "https://private.reg/files/thing.tgz" of
            Left err -> fail ("artifactRequestByUrl failed: " <> show err)
            Right req -> lookup "Authorization" (Client.requestHeaders req) `shouldBe` Just "Token tok-xyz"

    it "refuses an unparseable URL as a UrlFormationError" $ do
        artifactRequestByUrl apiKeyMapping Nothing "not a url with spaces"
            `shouldSatisfy` urlErrorWas (UnparseableUrl "not a url with spaces")

validatorsSpec :: Spec
validatorsSpec = describe "conditional-GET validators" $ do
    it "adds both If-None-Match and If-Modified-Since when present" $ do
        req <- parseRequestOrFail "https://reg.test/x"
        let validators = Validators (Just "\"etag-123\"") (Just "Wed, 21 Oct 2015 07:28:00 GMT")
        let hs = Client.requestHeaders (addValidators validators req)
        lookup hIfNoneMatch hs `shouldBe` Just "\"etag-123\""
        lookup hIfModifiedSince hs `shouldBe` Just "Wed, 21 Oct 2015 07:28:00 GMT"

    it "adds neither header for noValidators" $ do
        req <- parseRequestOrFail "https://reg.test/x"
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

{- | A presentation that carries a raw token on a header it names itself. These cases
therefore drive the mapping vocabulary, not any registered ecosystem's scheme.
-}
apiKeyMapping :: CredentialMapping
apiKeyMapping = credentialMapping recoverApiKey "X-Api-Key" (encodeUtf8 . unSecret)

recoverApiKey :: RequestHeaders -> Maybe Secret
recoverApiKey headers = mkSecret . decodeUtf8 <$> lookup "X-Api-Key" headers

{- | A presentation on @Authorization@ under a scheme of its own. The rendered value
therefore comes from the mapping, not from a scheme the code assumes.
-}
schemedMapping :: CredentialMapping
schemedMapping = credentialMapping recoverApiKey "Authorization" (\secret -> "Token " <> encodeUtf8 (unSecret secret))

addAuth :: ByteString -> Client.Request -> Client.Request
addAuth value req = req{Client.requestHeaders = ("Authorization", value) : Client.requestHeaders req}

-- An attach that (wrongly) reopens redirect following. The finaliser's pin must still win.
overrideRedirects :: Client.Request -> Client.Request
overrideRedirects req = req{Client.redirectCount = 10}

urlErrorWas :: UrlFormationError -> Either UrlFormationError a -> Bool
urlErrorWas expected = either (== expected) (const False)
