-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Generated upstream base URLs and the facts a sent request carries, so a property can hold a
builder under a base parsed once to the builder that parses the joined URL.
-}
module Ecluse.Registry.Request.Support (
    -- * Request facts
    RequestFacts,
    outcomeFacts,

    -- * Generators
    forAllBase,
    genBaseUrl,
    genComponent,
    genCredential,
) where

import Data.Char (toUpper)
import Data.Text qualified as T
import Hedgehog (Gen, PropertyT, cover, forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Network.HTTP.Client (Request (cookieJar, decompress, host, method, path, port, queryString, redirectCount, requestHeaders, secure))
import Network.HTTP.Types (Method, RequestHeaders)

import Ecluse.Core.Credential (ClientCredential (ClientCredential), bareCredential, mkSecret)
import Ecluse.Core.Registry (UrlFormationError)
import Ecluse.Core.Registry.Request (BaseRequest, mkBaseRequest, requestUnder)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Package.Filter.Support (genHostileUrl)
import Ecluse.Test.Server.Route (genPathSegmentFrom)

-- | The fields a sent request carries, compared field by field and in header order.
data RequestFacts = RequestFacts
    { rfMethod :: Method
    , rfSecure :: Bool
    , rfHost :: ByteString
    , rfPort :: Int
    , rfPath :: ByteString
    , rfQueryString :: ByteString
    , rfHeaders :: RequestHeaders
    , rfDecompress :: [Bool]
    -- ^ The decision for each probed content type.
    , rfRedirectCount :: Int
    , rfCookieJar :: Maybe String
    }
    deriving stock (Eq, Show)

-- | A built request's facts, or the formation error itself.
outcomeFacts :: Either UrlFormationError Request -> Either UrlFormationError RequestFacts
outcomeFacts = fmap requestFacts

requestFacts :: Request -> RequestFacts
requestFacts request =
    RequestFacts
        { rfMethod = method request
        , rfSecure = secure request
        , rfHost = host request
        , rfPort = port request
        , rfPath = path request
        , rfQueryString = queryString request
        , rfHeaders = requestHeaders request
        , rfDecompress = map (decompress request) decompressProbes
        , rfRedirectCount = redirectCount request
        , rfCookieJar = show <$> cookieJar request
        }

-- The content types http-client's default decision and an identity read tell apart.
decompressProbes :: [ByteString]
decompressProbes = ["application/x-tar", "application/gzip", "application/json", ""]

{- | A generated base URL as the egress witness holds it, and the base parsed once from it. Most
generated bases must keep a parse, or the property would hold only the fallback.
-}
forAllBase :: PropertyT IO (Text, BaseRequest)
forAllBase = do
    url <- loopbackRegistryUrl <$> forAll genBaseUrl
    let base = mkBaseRequest url
    cover 40 "the base kept a parse" (isJust (requestUnder base ""))
    pure (registryUrlText url, base)

{- | A base URL as configuration might write one: respelled schemes, named, numeric, non-ASCII and
bracketed IPv6 hosts, written ports, directories with spaces and non-ASCII text, and zero or more
trailing slashes. Now and then it carries a query, a fragment, or a hostile URL's spelling.
-}
genBaseUrl :: Gen Text
genBaseUrl = Gen.frequency [(6, genConfiguredBase), (1, genHostileUrl ["registry.npmjs.org", "[::1]:8443", "pypi.org"])]

genConfiguredBase :: Gen Text
genConfiguredBase = do
    scheme <- Gen.element ["https://", "http://" :: Text] >>= respell
    userinfo <- Gen.frequency [(12, pure ""), (1, Gen.element ["user:pass@", "a b@", "%40@", "@"])]
    authority <- Gen.element hosts
    written <- Gen.frequency [(3, pure ""), (2, Gen.element ports)]
    directories <- Gen.list (Range.linear 0 3) genDirectory
    slashes <- Gen.int (Range.linear 0 3)
    ending <- Gen.frequency [(12, pure ""), (1, Gen.element ["?", "?q=1", "#", "#frag", "?a#b"])]
    pure (scheme <> userinfo <> authority <> written <> foldMap ("/" <>) directories <> T.replicate slashes "/" <> ending)
  where
    respell :: Text -> Gen Text
    respell = fmap toText . traverse (\c -> Gen.element (ordNub [c, toUpper c])) . toString
    hosts = ["registry.npmjs.org", "REGISTRY.NpmJS.org", "pypi.org", "localhost", "127.0.0.1", "[::1]", "[2606:4700::1111]", "[fe80::1%25eth0]", "[v1.x]", "b\xFC\&cher.example", "xn--bcher-kva.example", "my registry.example", "", "[::1", "host%", "a%41b", "999.1.1.1", "1.2.3.4.5"]
    ports = [":443", ":8443", ":8080", ":0", ":", ":65536", ":0443", ":44a", ":\xFF18"]

-- One directory of a base's path, from the plain names a registry mounts under to hostile text.
genDirectory :: Gen Text
genDirectory =
    Gen.frequency
        [ (4, Gen.element ["npm", "simple", "api/npm", "repository/npm-proxy", "pypi", "a b", "\xFC\&ber", "%41", "%2F", "~user", "@scope", "a:b", ".", ".."])
        , (1, Gen.text (Range.linear 0 4) Gen.unicode)
        , (1, Gen.text (Range.linear 1 6) (Gen.element ("%[]@:!$&'()*+,;= " :: String)))
        ]

-- | A name or filename component, including traversal, separator, escape and non-ASCII text.
genComponent :: Gen Text
genComponent =
    Gen.frequency
        [ (5, genPathSegmentFrom ["lodash", "code-frame", "requests", "Zope.Interface", "na\xEFve", "a b", "%41", "100%", "a?b", "a#b", "@x", "x-1.0.0.tgz"])
        , (1, Gen.text (Range.linear 0 8) Gen.unicode)
        ]

-- | No credential, a bare token, or a username and secret pair.
genCredential :: Gen (Maybe ClientCredential)
genCredential = Gen.maybe (Gen.element [bareCredential (mkSecret "tok-abc"), ClientCredential (Just "__token__") (mkSecret "pypi-AgEI")])
