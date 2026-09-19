-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Ecosystem-agnostic request mechanics: the outbound finaliser, the credential presentation,
the conditional-GET validators, and URL parsing into a typed 'UrlFormationError'. An adapter
supplies only its own protocol facts.

'parseRequestEither' seals what it parses, so an adapter cannot obtain an unsealed 'Request'
from this module at all.
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
    authorizationUnder,

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
import Network.HTTP.Types.Header (
    HeaderName,
    RequestHeaders,
    hAuthorization,
    hIfModifiedSince,
    hIfNoneMatch,
    hUserAgent,
 )

import Ecluse.Core.BuildIdentity (userAgent)
import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Registry (UrlFormationError (EmptyBaseUrl, UnparseableUrl))
import Ecluse.Core.Text (joinUrlPath)

{- | Seal the outbound invariants onto a request, idempotently. A followed redirect could
re-send a credential cross-host or steer an anonymous fetch past the host allowlist.
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

{- | One ecosystem's credential presentation, recovered as a value so an attach re-encodes rather
than replaying a header. The constructor is hidden, so no adapter spells its own attach point.
-}
data CredentialMapping = CredentialMapping
    { credentialRecover :: RequestHeaders -> Maybe ClientCredential
    {- ^ 'Nothing' for a request carrying none in this ecosystem's form, which the edge gate
    denies rather than half-reading. The compare is over the secret half alone.
    -}
    , -- The header that carries an outbound credential: named per ecosystem, never assumed.
      credentialHeader :: HeaderName
    , -- How a credential renders into that header's value (the ecosystem's own scheme).
      credentialRender :: ClientCredential -> ByteString
    }

{- | Declare an ecosystem's credential presentation. The constructor is hidden, so this is the
only way to build a 'CredentialMapping'.
-}
credentialMapping ::
    (RequestHeaders -> Maybe ClientCredential) ->
    HeaderName ->
    (ClientCredential -> ByteString) ->
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
attachCredential :: CredentialMapping -> Maybe ClientCredential -> Request -> Request
attachCredential mapping credential = finaliseRequest $ case credential of
    Nothing -> id
    Just presented -> \request ->
        request
            { requestHeaders =
                (credentialHeader mapping, credentialRender mapping presented) : requestHeaders request
            }

{- | The first @Authorization@ header's remainder when it carries @scheme@ (compared without
case), with the separating spaces dropped. Another scheme or no header yields 'Nothing'.
-}
authorizationUnder :: Text -> RequestHeaders -> Maybe Text
authorizationUnder scheme headers = do
    (_, raw) <- find ((== hAuthorization) . fst) headers
    let (presented, rest) = T.break (== ' ') (decodeUtf8 raw)
    guard (T.toLower presented == T.toLower scheme)
    pure (T.dropWhile (== ' ') rest)

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

{- | Build the artifact @GET@ at the URL a projection preserved from upstream. Non-decompressing,
so the bytes the served integrity digest is paired with are never gunzipped.
-}
artifactRequestByUrl :: CredentialMapping -> Maybe ClientCredential -> Text -> Either UrlFormationError Request
artifactRequestByUrl mapping credential url = do
    base <- parseRequestEither url
    pure . attachCredential mapping credential $ base{decompress = const False}

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
