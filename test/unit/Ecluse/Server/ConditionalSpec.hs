module Ecluse.Server.ConditionalSpec (spec) where

import Data.Text qualified as T
import Network.HTTP.Types (hIfModifiedSince, hIfNoneMatch, status200, status304, status404)
import Test.Hspec

import Ecluse.Server.Conditional (
    Conditional (Modified, NotModified),
    etagHeader,
    evaluateOwnETag,
    forwardValidators,
    isNotModified,
    ownETag,
    renderETag,
 )

-- | Two distinct served bodies, for asserting the ETag tracks the bytes.
bodyA, bodyB :: LByteString
bodyA = "{\"name\":\"is-odd\",\"versions\":{}}"
bodyB = "{\"name\":\"is-even\",\"versions\":{}}"

spec :: Spec
spec = do
    describe "ownETag — over the served bytes" $ do
        it "is stable for identical bytes" $
            ownETag bodyA `shouldBe` ownETag bodyA

        it "differs for different bytes" $
            ownETag bodyA `shouldNotBe` ownETag bodyB

        it "renders as a quoted strong validator" $ do
            let rendered = renderETag (ownETag bodyA)
            -- A strong validator is the opaque tag wrapped in double quotes, with
            -- no weakness (W/) prefix.
            rendered `shouldSatisfy` ("\"" `T.isPrefixOf`)
            rendered `shouldSatisfy` ("\"" `T.isSuffixOf`)
            rendered `shouldNotSatisfy` ("W/" `T.isPrefixOf`)

        it "renders the ETag header under the standard field name" $ do
            let (name, value) = etagHeader (ownETag bodyA)
            name `shouldBe` "ETag"
            value `shouldBe` encodeUtf8 (renderETag (ownETag bodyA))

    describe "evaluateOwnETag — transformed bodies (filtered packuments)" $ do
        it "is Modified when the request carries no validator" $
            case evaluateOwnETag [] bodyA of
                Modified e -> e `shouldBe` ownETag bodyA
                NotModified _ -> expectationFailure "no validator should not be a 304"

        it "is NotModified when If-None-Match matches our own ETag" $ do
            let etag = ownETag bodyA
                req = [(hIfNoneMatch, encodeUtf8 (renderETag etag))]
            case evaluateOwnETag req bodyA of
                NotModified e -> e `shouldBe` etag
                Modified _ -> expectationFailure "a matching validator should be a 304"

        it "is Modified when If-None-Match names a different ETag" $ do
            let req = [(hIfNoneMatch, encodeUtf8 (renderETag (ownETag bodyB)))]
            case evaluateOwnETag req bodyA of
                Modified e -> e `shouldBe` ownETag bodyA
                NotModified _ -> expectationFailure "a stale validator must re-serve"

        it "matches our ETag inside a comma-separated If-None-Match list" $ do
            let etag = ownETag bodyA
                req = [(hIfNoneMatch, "\"deadbeef\", " <> encodeUtf8 (renderETag etag))]
            case evaluateOwnETag req bodyA of
                NotModified e -> e `shouldBe` etag
                Modified _ -> expectationFailure "a match anywhere in the list is a 304"

        it "matches our ETag across two separate If-None-Match header lines" $ do
            -- A client may legally repeat If-None-Match as distinct header lines
            -- rather than one comma-joined value; the match must scan every line
            -- (the lookupAll/any path), not just the first.
            let etag = ownETag bodyA
                req =
                    [ (hIfNoneMatch, "\"deadbeef\"")
                    , (hIfNoneMatch, encodeUtf8 (renderETag etag))
                    ]
            case evaluateOwnETag req bodyA of
                NotModified e -> e `shouldBe` etag
                Modified _ -> expectationFailure "a match on any If-None-Match line is a 304"

        it "is Modified when no separate If-None-Match line carries our ETag" $ do
            -- The mirror of the above: multiple lines, none matching, must re-serve.
            let req =
                    [ (hIfNoneMatch, "\"deadbeef\"")
                    , (hIfNoneMatch, encodeUtf8 (renderETag (ownETag bodyB)))
                    ]
            case evaluateOwnETag req bodyA of
                Modified e -> e `shouldBe` ownETag bodyA
                NotModified _ -> expectationFailure "no matching line must re-serve"

        it "treats a wildcard If-None-Match as a match (304)" $
            case evaluateOwnETag [(hIfNoneMatch, "*")] bodyA of
                NotModified e -> e `shouldBe` ownETag bodyA
                Modified _ -> expectationFailure "a wildcard validator is a 304"

        it "matches a weakly-prefixed client validator against our strong ETag" $ do
            -- Clients may echo back our validator with a W/ weakness prefix; the
            -- comparison is on the opaque tag, so it still matches.
            let etag = ownETag bodyA
                req = [(hIfNoneMatch, "W/" <> encodeUtf8 (renderETag etag))]
            case evaluateOwnETag req bodyA of
                NotModified e -> e `shouldBe` etag
                Modified _ -> expectationFailure "a weak echo of our tag is still a match"

    describe "forwardValidators — pass-through bodies (artifacts, raw metadata)" $ do
        it "relays the client's If-None-Match upstream" $
            forwardValidators [(hIfNoneMatch, "\"abc\"")]
                `shouldBe` [(hIfNoneMatch, "\"abc\"")]

        it "relays the client's If-Modified-Since upstream" $
            forwardValidators [(hIfModifiedSince, "Wed, 21 Oct 2026 07:28:00 GMT")]
                `shouldBe` [(hIfModifiedSince, "Wed, 21 Oct 2026 07:28:00 GMT")]

        it "drops headers that are not conditional validators" $
            forwardValidators [("Accept", "application/json"), (hIfNoneMatch, "\"abc\"")]
                `shouldBe` [(hIfNoneMatch, "\"abc\"")]

        it "yields nothing when the client sends no validators" $
            forwardValidators [("Accept", "application/json")] `shouldBe` []

    describe "isNotModified — relaying upstream 304s" $ do
        it "recognises a 304 to pass straight back" $
            isNotModified status304 `shouldBe` True

        it "is not a 304 for an ordinary 200" $
            isNotModified status200 `shouldBe` False

        it "is not a 304 for a 404 miss" $
            isNotModified status404 `shouldBe` False
