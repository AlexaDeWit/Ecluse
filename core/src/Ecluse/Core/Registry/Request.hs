-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Ecosystem-agnostic request mechanics shared by every registry adapter's request layer:
the finaliser every outbound request passes through, the conditional-GET validators, the
opaque-artifact request core that streams a body byte-for-byte, URL parsing into a typed
'UrlFormationError', and the empty-base-guarded path join. An adapter supplies only its
ecosystem's protocol facts: its media types, its path encoding, its credential presentation.
'sealRequest' holds the outbound invariants and 'parseRequestEither' applies it, so an
adapter cannot obtain an unsealed 'Request' from this module at all.
-}
module Ecluse.Core.Registry.Request (
    -- * Request finalisation
    sealRequest,
    finaliseRequest,

    -- * Credential presentation
    CredentialMapping,
    credentialMapping,
    credentialRecover,
    attachCredential,

    -- * Conditional-GET validators
    Validators (..),
    noValidators,
    addValidators,

    -- * Request building
    artifactRequestByUrl,
    joinPath,
    parseRequestEither,
) where

import Data.Text qualified as T
import Network.HTTP.Client (Request (decompress, redirectCount, requestHeaders), parseRequest)
import Network.HTTP.Types.Header (HeaderName, RequestHeaders, hIfModifiedSince, hIfNoneMatch, hUserAgent)

import Ecluse.Core.BuildIdentity (userAgent)
import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Registry (UrlFormationError (EmptyBaseUrl, UnparseableUrl))
import Ecluse.Core.Text (joinUrlPath)

{- | Seal the outbound invariants onto a data-plane request: pin @redirectCount = 0@, and
identify the proxy with a @User-Agent@ unless one is already set. It is idempotent, so a
request that passes several formation steps carries one pin and one identity header.
'parseRequestEither' seals what it parses and "Ecluse.Core.Registry.Publish" re-seals whatever
a codec hands it, so __Écluse never follows an upstream redirect__, on the credentialed and
the anonymous plane alike.

Two dangers the pin forecloses:

\* __Credential leakage__ (credentialed plane). The http-client default @redirectCount = 10@
re-sends the @Authorization@ header to the redirect's @Location@, and
@shouldStripHeaderOnRedirect@ does not strip it cross-host. A hostile or misconfigured upstream
could @302@ a forwarded or minted credential to an attacker-chosen host, worst of all on the
trusted private manager.

\* __SSRF via redirect__ (anonymous plane). The proxy enforces the host allowlist when it builds
the URL, not per redirect hop. Following a @302@ would let an allowlisted upstream steer an
anonymous fetch to any host, an internal or cloud-metadata address included.

The accepted consequence: a read returns an upstream CDN's @3xx@ to the serve path rather than
chasing it. Écluse honours the packument's @dist.tarball@ location explicitly instead, gated by
the egress policy.
-}
sealRequest :: Request -> Request
sealRequest request =
    request
        { redirectCount = 0
        , requestHeaders = identify (requestHeaders request)
        }
  where
    identify headers
        | any ((== hUserAgent) . fst) headers = headers
        | otherwise = (hUserAgent, userAgent) : headers

{- | Apply the ecosystem's injected credential attach, then seal the result through
'sealRequest'. The attach runs first, so it cannot reopen redirect following.
-}
finaliseRequest :: (Request -> Request) -> Request -> Request
finaliseRequest attach = sealRequest . attach

{- | One ecosystem's credential presentation: how it recovers a client's credential from
presented headers, and how it carries one on an outbound request. The recovery yields the token
__text__, so an attach re-encodes rather than replaying a header verbatim.

The constructor is hidden and 'attachCredential' is the only way to run the encoding, so no
adapter spells its own attach point.
-}
data CredentialMapping = CredentialMapping
    { credentialRecover :: RequestHeaders -> Maybe Secret
    {- ^ Recover the token text a client presented, or 'Nothing' when the request carries no
    credential in this ecosystem's form. The edge gate denies a 'Nothing' on a mount with a
    configured inbound token, so it refuses a foreign presentation rather than half-reading one.
    -}
    , -- The header that carries an outbound credential: named per ecosystem, never assumed.
      credentialHeader :: HeaderName
    , -- How a token renders into that header's value (the ecosystem's own scheme).
      credentialRender :: Secret -> ByteString
    }

{- | Declare an ecosystem's credential presentation. The constructor is hidden, so this is the
only way to build a 'CredentialMapping'.
-}
credentialMapping ::
    (RequestHeaders -> Maybe Secret) ->
    HeaderName ->
    (Secret -> ByteString) ->
    CredentialMapping
credentialMapping recover header render =
    CredentialMapping
        { credentialRecover = recover
        , credentialHeader = header
        , credentialRender = render
        }

{- | Attach a credential to an outbound request under the mapping's own header, then finalise it
through 'finaliseRequest'. A 'Nothing' attaches no header, and the seal still applies.
-}
attachCredential :: CredentialMapping -> Maybe Secret -> Request -> Request
attachCredential mapping token = finaliseRequest $ case token of
    Nothing -> id
    Just secret -> \request ->
        request
            { requestHeaders =
                (credentialHeader mapping, credentialRender mapping secret) : requestHeaders request
            }

{- | The conditional-GET validators to relay on a metadata fetch. Replaying them lets the
upstream answer @304 Not Modified@ with no body on a cache revalidation.
-}
data Validators = Validators
    { validatorIfNoneMatch :: Maybe ByteString
    -- ^ An entity tag to send as @If-None-Match@ (an upstream @ETag@).
    , validatorIfModifiedSince :: Maybe ByteString
    {- ^ An RFC-1123 date to send as @If-Modified-Since@ (an upstream
    @Last-Modified@).
    -}
    }
    deriving stock (Eq, Show)

-- | No conditional-GET validators: an unconditional fetch.
noValidators :: Validators
noValidators = Validators{validatorIfNoneMatch = Nothing, validatorIfModifiedSince = Nothing}

-- Add the present conditional-GET validators as request headers.
addValidators :: Validators -> Request -> Request
addValidators validators request =
    request{requestHeaders = newHeaders <> requestHeaders request}
  where
    newHeaders =
        catMaybes
            [ (,) hIfNoneMatch <$> validatorIfNoneMatch validators
            , (,) hIfModifiedSince <$> validatorIfModifiedSince validators
            ]

{- | Build the artifact @GET@ addressing a tarball at the absolute @url@ a projection preserved
from the upstream's @dist.tarball@, never a rebuild from a @(base, package, file)@ coordinate.
That location is server-chosen data, and it is the one the served metadata pairs its integrity
digest with, so the bytes still verify.

The request is __non-decompressing__ ('decompress' returns 'False'), so nothing gunzips an
opaque tarball in flight and its integrity digest stays valid. It fails with a
'UrlFormationError' only when the @url@ cannot be parsed.
-}
artifactRequestByUrl :: CredentialMapping -> Maybe Secret -> Text -> Either UrlFormationError Request
artifactRequestByUrl mapping token url = do
    base <- parseRequestEither url
    pure . attachCredential mapping token $ base{decompress = const False}

{- Join a base URL and an already-encoded path with exactly one slash, whatever trailing
slashes the configured base writes.
-}
joinPath :: Text -> Text -> Either UrlFormationError Text
joinPath baseUrl path
    | T.null baseUrl = Left EmptyBaseUrl
    | otherwise = Right (joinUrlPath baseUrl path)

{- | Parse a URL into the sealed request every adapter builds from ('sealRequest'). The URL comes
from configuration and an already-safe name, so a parse failure here is a configuration fault.
-}
parseRequestEither :: Text -> Either UrlFormationError Request
parseRequestEither url =
    case parseRequest (toString url) of
        Just request -> Right (sealRequest request)
        Nothing -> Left (UnparseableUrl url)
