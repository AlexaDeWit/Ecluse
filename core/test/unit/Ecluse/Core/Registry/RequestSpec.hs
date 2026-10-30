-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Registry.RequestSpec (spec) where

import Data.List (lookup)
import Data.Text qualified as T
import Hedgehog (Gen, cover, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (HeaderName, RequestHeaders, hAcceptEncoding, hUserAgent)
import Test.Hspec (
    Spec,
    describe,
    it,
    shouldBe,
    shouldReturn,
    shouldSatisfy,
 )
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.BuildIdentity (userAgent)
import Ecluse.Core.Credential (ClientCredential (ClientCredential, credSecret, credUsername), bareCredential, mkSecret, unSecret)
import Ecluse.Core.Registry (UrlFormationError (EmptyBaseUrl, UnparseableUrl))
import Ecluse.Core.Registry.Request (
    AuthMechanism (AuthBasic, AuthBearer, AuthRawKey),
    CredentialMapping,
    ServicePresentation,
    StoreOperation (StoreRead, StoreWrite),
    artifactRequestByUrl,
    attachCredential,
    credentialMapping,
    credentialRecover,
    declaredPresentation,
    finaliseRequest,
    joinPath,
    mkBaseRequest,
    nativePresentation,
    parseRequestEither,
    presentedMapping,
    presentedRequest,
    renderAuthMechanism,
    requestAt,
    sealRequest,
 )
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.FieldValue (fieldValues)
import Ecluse.Core.Server.Path (encodeComponent)
import Ecluse.Registry.Request.Support (baseAt, genBaseUrl, genComponent, outcomeFacts)
import Ecluse.Test.Stub (headerValue, lastCaptured, stubBaseUrl, withStub)
import Ecluse.Test.Support (parseRequestOrFail)

spec :: Spec
spec = do
    sealRequestSpec
    finaliseRequestSpec
    credentialMappingSpec
    servicePresentationSpec
    artifactByUrlSpec
    joinPathSpec
    parseSpec
    underBaseSpec

credentialMappingSpec :: Spec
credentialMappingSpec = describe "a credential mapping carries one ecosystem's presentation both ways" $ do
    it "recovers the token text from the header the mapping names" $
        credentialRecover apiKeyMapping [("X-Api-Key", "tok-abc")] `shouldBe` Just (bareCredential (mkSecret "tok-abc"))

    it "recovers nothing when the request presents no credential in that form" $ do
        credentialRecover apiKeyMapping [("Authorization", "Bearer tok-abc")] `shouldBe` Nothing
        credentialRecover apiKeyMapping [] `shouldBe` Nothing

    it "attaches the credential under the mapping's own header, not an assumed one" $ do
        req <- parseRequestOrFail "https://reg.test/x"
        let headers = Client.requestHeaders (attachCredential apiKeyMapping (Just (bareCredential (mkSecret "tok-abc"))) req)
        lookup "X-Api-Key" headers `shouldBe` Just "tok-abc"
        lookup "Authorization" headers `shouldBe` Nothing

    it "renders the header value through the mapping's own scheme" $ do
        req <- parseRequestOrFail "https://reg.test/x"
        lookup "Authorization" (Client.requestHeaders (attachCredential schemedMapping (Just (bareCredential (mkSecret "tok-abc"))) req))
            `shouldBe` Just "Token tok-abc"

    it "carries a presentation's username half verbatim, never a rewritten one" $ do
        -- A private index has username conventions of its own, so a passthrough leg renders the
        -- pair a client sent rather than substituting a name Écluse chose.
        req <- parseRequestOrFail "https://reg.test/x"
        let presented = ClientCredential (Just "__token__") (mkSecret "tok-abc")
        lookup "Authorization" (Client.requestHeaders (attachCredential basicMapping (Just presented) req))
            `shouldBe` Just "Basic __token__:tok-abc"

    it "attaches no credential header when the request is anonymous" $ do
        req <- parseRequestOrFail "https://reg.test/x"
        lookup "X-Api-Key" (Client.requestHeaders (attachCredential apiKeyMapping Nothing req))
            `shouldBe` Nothing

    it "pins the redirect count with the credential attached (the attach cannot bypass it)" $ do
        req <- parseRequestOrFail "https://reg.test/x"
        Client.redirectCount (attachCredential apiKeyMapping (Just (bareCredential (mkSecret "tok-abc"))) req) `shouldBe` 0

    it "pins the redirect count on an anonymous request too" $ do
        req <- parseRequestOrFail "https://reg.test/x"
        Client.redirectCount (attachCredential apiKeyMapping Nothing req) `shouldBe` 0

servicePresentationSpec :: Spec
servicePresentationSpec = describe "a store-declared service presentation" $ do
    it "renders each mechanism, Basic over the credential's own username" $ do
        let named = ClientCredential (Just "aws") (mkSecret "tok-abc")
        renderAuthMechanism AuthBearer named `shouldBe` "Bearer tok-abc"
        renderAuthMechanism AuthBasic named `shouldBe` "Basic YXdzOnRvay1hYmM="
        renderAuthMechanism AuthBasic (bareCredential (mkSecret "tok-abc")) `shouldBe` "Basic OnRvay1hYmM="
        renderAuthMechanism AuthRawKey named `shouldBe` "tok-abc"

    it "keeps the ecosystem's own mapping for an operation the store declares nothing for" $ do
        req <- parseRequestOrFail "https://reg.test/x"
        for_ [StoreRead, StoreWrite] $ \operation -> do
            let headers = Client.requestHeaders (attachCredential (presentedMapping nativePresentation operation apiKeyMapping) (Just (bareCredential (mkSecret "tok-abc"))) req)
            lookup "X-Api-Key" headers `shouldBe` Just "tok-abc"
            lookup "Authorization" headers `shouldBe` Nothing

    it "renders each operation's declared mechanism on Authorization in place of the ecosystem's header" $ do
        req <- parseRequestOrFail "https://reg.test/x"
        let headersFor operation = Client.requestHeaders (attachCredential (presentedMapping basicReadsBearerWrites operation apiKeyMapping) (Just (ClientCredential (Just "aws") (mkSecret "tok-abc"))) req)
        lookup "Authorization" (headersFor StoreRead) `shouldBe` Just "Basic YXdzOnRvay1hYmM="
        lookup "Authorization" (headersFor StoreWrite) `shouldBe` Just "Bearer tok-abc"
        lookup "X-Api-Key" (headersFor StoreWrite) `shouldBe` Nothing

    it "keeps the ecosystem's own recovery under a declared mechanism" $
        credentialRecover (presentedMapping basicReadsBearerWrites StoreWrite apiKeyMapping) [("X-Api-Key", "tok-abc")]
            `shouldBe` Just (bareCredential (mkSecret "tok-abc"))

    it "forms a request anonymously, then attaches the credential once under the mapping and seals it" $ do
        let build credential = attachCredential schemedMapping credential <$> parseRequestEither "https://reg.test/x"
            mapping = presentedMapping (declaredPresentation (const AuthRawKey)) StoreRead schemedMapping
        case presentedRequest mapping (Just (bareCredential (mkSecret "tok-abc"))) build of
            Left err -> fail ("expected a formed request, got " <> show err)
            Right request -> do
                headerValues "Authorization" request `shouldBe` ["tok-abc"]
                Client.redirectCount request `shouldBe` 0
                userAgents request `shouldBe` [userAgent]

sealRequestSpec :: Spec
sealRequestSpec = describe "sealRequest carries the outbound invariants onto every request" $ do
    it "disables redirect following (redirectCount 0)" $ do
        req <- parseRequestOrFail "https://reg.test/x"
        Client.redirectCount (sealRequest req) `shouldBe` 0

    it "identifies the proxy with the shared User-Agent" $ do
        req <- parseRequestOrFail "https://reg.test/x"
        lookup hUserAgent (Client.requestHeaders (sealRequest req)) `shouldBe` Just userAgent

    it "sets the User-Agent once, however many formation steps re-apply the seal" $ do
        req <- parseRequestOrFail "https://reg.test/x"
        userAgents (sealRequest (sealRequest (sealRequest req))) `shouldBe` [userAgent]

    it "leaves a User-Agent the caller already set in place" $ do
        req <- parseRequestOrFail "https://reg.test/x"
        userAgents (sealRequest (withAgent "other/1.0" req)) `shouldBe` ["other/1.0"]

finaliseRequestSpec :: Spec
finaliseRequestSpec = describe "finaliseRequest pins the redirect count for every request" $ do
    it "disables redirect following (redirectCount 0) with an identity attach" $ do
        req <- parseRequestOrFail "https://reg.test/x"
        Client.redirectCount (finaliseRequest id req) `shouldBe` 0

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
    it "addresses the authoritative URL verbatim (host, path, query) in identity coding and never decompresses" $ do
        let url = "https://cdn.example.net/files/abc/code-frame-7.0.0.tgz?sig=deadbeef"
        case artifactRequestByUrl apiKeyMapping Nothing url of
            Left err -> fail ("artifactRequestByUrl failed: " <> show err)
            Right req -> do
                Client.host req `shouldBe` "cdn.example.net"
                Client.path req `shouldBe` "/files/abc/code-frame-7.0.0.tgz"
                Client.queryString req `shouldBe` "?sig=deadbeef"
                Client.decompress req "application/gzip" `shouldBe` False
                acceptEncodings req `shouldBe` ["identity"]
                Client.redirectCount req `shouldBe` 0

    it "transmits identity coding, where http-client's default request asks for gzip" $
        withStub status200 "artifact bytes" $ \stub -> do
            manager <- Client.newManager Client.defaultManagerSettings
            let url = stubBaseUrl stub <> "/files/thing.tgz"
                transmitted request = Client.httpLbs request manager >> headerValue "Accept-Encoding" <$> lastCaptured stub
            either (fail . show) transmitted (parseRequestEither url) `shouldReturn` Just "gzip"
            either (fail . show) transmitted (artifactRequestByUrl apiKeyMapping Nothing url) `shouldReturn` Just "identity"

    it "attaches the credential under the mapping's presentation onto the finalised request" $ do
        case artifactRequestByUrl schemedMapping (Just (bareCredential (mkSecret "tok-xyz"))) "https://private.reg/files/thing.tgz" of
            Left err -> fail ("artifactRequestByUrl failed: " <> show err)
            Right req -> lookup "Authorization" (Client.requestHeaders req) `shouldBe` Just "Token tok-xyz"

    it "refuses an unparseable URL as a UrlFormationError" $ do
        artifactRequestByUrl apiKeyMapping Nothing "not a url with spaces"
            `shouldSatisfy` urlErrorWas (UnparseableUrl "not a url with spaces")

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

    it "seals what it parses, so no adapter obtains an unpinned request from the shared entry" $
        case parseRequestEither "https://reg.test/x" of
            Left err -> fail ("expected a parseable URL: " <> show err)
            Right req -> do
                Client.redirectCount req `shouldBe` 0
                lookup hUserAgent (Client.requestHeaders req) `shouldBe` Just userAgent

    it "holds the pin and one User-Agent under a credential attach, with no adapter involved" $
        -- The whole credentialed request is built from the shared entry and a presentation no
        -- registered ecosystem owns, so nothing here rides on an adapter's own builder.
        case parseRequestEither "https://reg.test/x" of
            Left err -> fail ("expected a parseable URL: " <> show err)
            Right req -> do
                let credentialed = attachCredential apiKeyMapping (Just (bareCredential (mkSecret "tok-abc"))) req
                Client.redirectCount credentialed `shouldBe` 0
                userAgents credentialed `shouldBe` [userAgent]
                lookup "X-Api-Key" (Client.requestHeaders credentialed) `shouldBe` Just "tok-abc"

underBaseSpec :: Spec
underBaseSpec = describe "requestAt builds under a base parsed once" $ do
    it "builds under a base with a query or a fragment as the joined URL parses" $
        for_ ["https://reg.test/?x", "https://reg.test#x"] $ \text ->
            outcomeFacts (requestAt (baseAt text) "pkg") `shouldBe` outcomeFacts (joinPath text "pkg" >>= parseRequestEither)

    it "encodes a path that is not verbatim path text as the joined URL's parse does" $
        for_ ["a b", "a?b", "a#b", "%4", "%zz", "\xFC"] $ \suffix ->
            outcomeFacts (requestAt (baseAt "https://reg.test") suffix) `shouldBe` outcomeFacts (joinPath "https://reg.test" suffix >>= parseRequestEither)

    modifyMaxSuccess (const 5000) $
        it "builds what parsing the joined URL builds, field by field, for generated bases and paths" $
            hedgehog $ do
                url <- loopbackRegistryUrl <$> forAll genBaseUrl
                suffix <- forAll genSuffix
                let joined = joinPath (registryUrlText url) suffix >>= parseRequestEither
                cover 30 "the joined URL parses" (isRight joined)
                cover 5 "the joined URL is refused" (isLeft joined)
                outcomeFacts (requestAt (mkBaseRequest url) suffix) === outcomeFacts joined

-- An encoded path as the builders write one, path text dense in escapes whole and cut short, or raw text.
genSuffix :: Gen Text
genSuffix =
    Gen.frequency
        [ (3, T.intercalate "/" <$> Gen.list (Range.linear 0 4) (encodeComponent <$> genComponent))
        , (1, Gen.text (Range.linear 0 8) (Gen.element ("aFg09-~@/%%%" :: String)))
        , (1, Gen.text (Range.linear 0 12) (Gen.element ("aZ09-._~@/%:?#[] \xFC+!$&'()*,;=\\" :: String)))
        ]

{- | A presentation that carries a raw token on a header it names itself. These cases
therefore drive the mapping vocabulary, not any registered ecosystem's scheme.
-}
apiKeyMapping :: CredentialMapping
apiKeyMapping = credentialMapping recoverApiKey "X-Api-Key" (encodeUtf8 . unSecret . credSecret)

recoverApiKey :: RequestHeaders -> Maybe ClientCredential
recoverApiKey headers = bareCredential . mkSecret . decodeUtf8 <$> lookup "X-Api-Key" headers

{- | A presentation on @Authorization@ under a scheme of its own. The rendered value
therefore comes from the mapping, not from a scheme the code assumes.
-}
schemedMapping :: CredentialMapping
schemedMapping = credentialMapping recoverApiKey "Authorization" (\credential -> "Token " <> encodeUtf8 (unSecret (credSecret credential)))

{- | A presentation that carries a username beside its secret, so a case can assert the pair
travels whole.
-}
-- A store that asks for Basic on reads and Bearer on writes.
basicReadsBearerWrites :: ServicePresentation
basicReadsBearerWrites = declaredPresentation $ \case
    StoreRead -> AuthBasic
    StoreWrite -> AuthBearer

basicMapping :: CredentialMapping
basicMapping = credentialMapping recoverApiKey "Authorization" renderPair
  where
    renderPair credential =
        "Basic " <> encodeUtf8 (fromMaybe "" (credUsername credential)) <> ":" <> encodeUtf8 (unSecret (credSecret credential))

-- Every value a request carries for one header, so a case can assert the header is set exactly once.
headerValues :: HeaderName -> Client.Request -> [ByteString]
headerValues header = fieldValues header . Client.requestHeaders

userAgents :: Client.Request -> [ByteString]
userAgents = headerValues hUserAgent

acceptEncodings :: Client.Request -> [ByteString]
acceptEncodings = headerValues hAcceptEncoding

withAgent :: ByteString -> Client.Request -> Client.Request
withAgent value req = req{Client.requestHeaders = (hUserAgent, value) : Client.requestHeaders req}

addAuth :: ByteString -> Client.Request -> Client.Request
addAuth value req = req{Client.requestHeaders = ("Authorization", value) : Client.requestHeaders req}

-- An attach that (wrongly) reopens redirect following. The finaliser's pin must still win.
overrideRedirects :: Client.Request -> Client.Request
overrideRedirects req = req{Client.redirectCount = 10}

urlErrorWas :: UrlFormationError -> Either UrlFormationError a -> Bool
urlErrorWas expected = either (== expected) (const False)
