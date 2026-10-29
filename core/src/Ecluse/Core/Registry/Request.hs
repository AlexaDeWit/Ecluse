-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Shared HTTP requests with typed URL failures and ecosystem-specific credential presentation.
Requests parsed here refuse redirects, including after credentials attach.
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
    bareAuthorization,

    -- * Request building
    metadataGet,
    artifactRequestByUrl,
    artifactGet,
    joinPath,
    sidecarUrl,
    parseRequestEither,

    -- * Requests under a base parsed once
    BaseRequest,
    mkBaseRequest,
    baseRequestUrl,
    requestAt,
) where

import Data.ByteString qualified as BS
import Data.Char (isHexDigit)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client (Request (decompress, redirectCount, requestHeaders), parseRequest)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types.Header (
    HeaderName,
    RequestHeaders,
    hAccept,
    hAcceptEncoding,
    hAuthorization,
 )
import Text.Show (showsPrec)

import Ecluse.Core.BuildIdentity (identifyRequest)
import Ecluse.Core.Credential (ClientCredential, bareCredential, mkSecret)
import Ecluse.Core.Registry (UrlFormationError (EmptyBaseUrl, UnparseableUrl))
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Text (isAsciiAlphaNum, joinUrlPath)

{- | Seal the outbound invariants onto a request, idempotently. A followed redirect could
re-send a credential cross-host or steer an anonymous fetch past the host allowlist.
-}
sealRequest :: Request -> Request
sealRequest request = identifyRequest request{redirectCount = 0}

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

{- | A scheme-less credential from exactly one non-empty @Authorization@ line whose bytes are all
in RFC 9110's field-value set: visible ASCII, space and tab. Any other presentation yields 'Nothing'.
-}
bareAuthorization :: RequestHeaders -> Maybe ClientCredential
bareAuthorization headers = case [value | (name, value) <- headers, name == hAuthorization] of
    [value] | not (BS.null value), BS.all fieldByte value -> bareCredential . mkSecret <$> rightToMaybe (TE.decodeUtf8' value)
    _ -> Nothing
  where
    fieldByte byte = byte == 0x09 || (byte >= 0x20 && byte <= 0x7E)

-- | Negotiate compressed metadata under the ecosystem's media type and credential presentation.
metadataGet :: CredentialMapping -> Maybe ClientCredential -> ByteString -> Request -> Request
metadataGet mapping credential mediaType base =
    attachCredential mapping credential $
        base
            { requestHeaders =
                (hAccept, mediaType)
                    : (hAcceptEncoding, "gzip")
                    : requestHeaders base
            }

-- | Build the artifact @GET@ at the preserved upstream URL through 'artifactGet'.
artifactRequestByUrl :: CredentialMapping -> Maybe ClientCredential -> Text -> Either UrlFormationError Request
artifactRequestByUrl mapping credential url = artifactGet mapping credential <$> parseRequestEither url

{- | Make a parsed request the artifact @GET@, in identity coding and never decompressed.
Without the field, http-client asks for gzip, and gzip bytes miss the advertised digest.
-}
artifactGet :: CredentialMapping -> Maybe ClientCredential -> Request -> Request
artifactGet mapping credential base =
    attachCredential mapping credential $
        base
            { decompress = const False
            , requestHeaders = (hAcceptEncoding, "identity") : requestHeaders base
            }

{- | Join a base URL and an already-encoded path with exactly one slash, whatever trailing
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

-- | Append the companion suffix to the path while preserving query and fragment text.
sidecarUrl :: Text -> Text
sidecarUrl url = path <> ".metadata" <> suffix
  where
    (path, suffix) = T.break (\character -> character == '?' || character == '#') url

{- | An upstream base URL and its one parse, so a request under it appends its path rather than
parsing the joined URL again. 'mkBaseRequest' is the only builder.
-}
data BaseRequest = BaseRequest RegistryUrl (Maybe Request)

-- The parse is a function of the URL, so the URL alone decides equality and display.
instance Eq BaseRequest where
    left == right = baseRequestUrl left == baseRequestUrl right

instance Show BaseRequest where
    showsPrec precedence = showsPrec precedence . baseRequestUrl

{- | Parse @url@ joined to an empty path, which is the request every path under it extends. A URL
with a query or a fragment keeps no parse, because a path joined after either lands inside it.
-}
mkBaseRequest :: RegistryUrl -> BaseRequest
mkBaseRequest url = BaseRequest url directory
  where
    text = registryUrlText url
    directory
        | T.any (\c -> c == '?' || c == '#') text = Nothing
        | otherwise = rightToMaybe (joinPath text "" >>= parseRequestEither)

-- | The URL the base was parsed from.
baseRequestUrl :: BaseRequest -> RegistryUrl
baseRequestUrl (BaseRequest url _) = url

{- | The request for the base joined to an already-encoded @suffix@, equal to 'parseRequestEither' of
the joined URL. It parses that URL only when the base kept no parse or the suffix is not verbatim.
-}
requestAt :: BaseRequest -> Text -> Either UrlFormationError Request
requestAt base@(BaseRequest url _) suffix =
    maybe (joinPath (registryUrlText url) suffix >>= parseRequestEither) Right (requestUnder base suffix)

requestUnder :: BaseRequest -> Text -> Maybe Request
requestUnder (BaseRequest _ directory) suffix = do
    request <- directory
    guard (verbatimPath suffix)
    pure request{Client.path = Client.path request <> encodeUtf8 suffix}

-- How far a scan of path text has got: between characters, inside an escape, or past a refusal.
data PathScan = Between | AwaitingFirstHexDigit | AwaitingSecondHexDigit | Refused
    deriving stock (Eq)

{- Text that http-client's parse keeps as path byte for byte: unreserved characters, @\@@, @/@,
and complete escapes. An encoded component writes nothing else. -}
verbatimPath :: Text -> Bool
verbatimPath = (== Between) . T.foldl' step Between
  where
    step Between c
        | c == '%' = AwaitingFirstHexDigit
        | isAsciiAlphaNum c || c `elem` ("-._~@/" :: String) = Between
    step AwaitingFirstHexDigit c | isHexDigit c = AwaitingSecondHexDigit
    step AwaitingSecondHexDigit c | isHexDigit c = Between
    step _ _ = Refused
