-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Textual extraction of the @host[:port]@ authority an outbound request dials.

These are comparison extractors, not an RFC 3986 parser: a value with no recognisable
authority yields the empty string or 'Nothing', which every guard treats as not-allowed. The
SSRF gates in "Ecluse.Core.Security.Host" consume the extracted 'HostPort', so the parsing
here carries no policy of its own.
-}
module Ecluse.Core.Security.Authority (
    -- * The dialled authority
    HostPort (..),

    -- * Authority extraction
    hostAddress,
    hostPortAddress,
    hostPortAddressWithDefault,
    splitHostPort,

    -- * Comparing authorities
    AuthorityText,
    authorityText,
    authorityHostPort,

    -- * Configured-URL refusal
    refuseCredentialMaterial,

    -- * Log-safe rendering
    authorityLabel,
    dialledAuthorityLabel,
    credentialFreeUrl,
    credentialFreeOrigin,
    dialledOrigin,
    credentialFreePath,
) where

import Data.Text qualified as T

import Ecluse.Core.Text (afterFirst, httpPrefix, isPrefixOfLowered, readDecimalText, registryPath, toLowerUnlessAsciiUnchanged)

-- | The authority an outbound fetch dials: a bare host with its effective port. The gate authorises the pair, so an allowlisted host at an attacker-chosen port is not authorised.
data HostPort = HostPort
    { hpHost :: Text
    -- ^ The bare host: no brackets, no port, lower-cased by both extractors.
    , hpPort :: Word16
    -- ^ The effective port: the explicit @:port@, or the caller's portless default.
    }
    deriving stock (Eq, Ord, Show)

-- | The bare host of a URI or @host[:port]@ authority, lower-cased and unbracketed. A value with no recognisable host yields the empty string, which the guards treat as not-allowed.
hostAddress :: Text -> Text
hostAddress raw = toLowerUnlessAsciiUnchanged (maybe "" fst (splitHostPort (authorityOf raw)))

-- | Extract the host and effective port, defaulting to 443. Refuse absent hosts and malformed ports.
hostPortAddress :: Text -> Maybe HostPort
hostPortAddress = authorityHostPort . authorityText

-- | 'hostPortAddress' with the caller's own port for a URL that writes none, for a scheme whose portless default is not the gate's 443. The authority split and the port grammar do not change.
hostPortAddressWithDefault :: Word16 -> Text -> Maybe HostPort
hostPortAddressWithDefault portless = hostPortOf portless . authorityOf

-- | The only text of a URL that 'hostPortAddress' reads. Equal texts give equal results, so a caller may reuse one's result for the other.
newtype AuthorityText = AuthorityText Text
    deriving stock (Eq)

-- | The authority of a URI or bare @host[:port]@ value, without userinfo.
authorityText :: Text -> AuthorityText
authorityText = AuthorityText . authorityOf

-- | 'hostPortAddress' from the authority text alone: @hostPortAddress = authorityHostPort . authorityText@.
authorityHostPort :: AuthorityText -> Maybe HostPort
authorityHostPort (AuthorityText authority) = hostPortOf 443 authority

hostPortOf :: Word16 -> Text -> Maybe HostPort
hostPortOf portless authority = do
    (host, rest) <- splitHostPort authority
    guard (not (T.null host))
    port <- effectivePort portless authority rest
    pure (HostPort (toLowerUnlessAsciiUnchanged host) port)

-- | A host and effective port label without credential carriers, or @\<unresolved\>@ for an invalid authority.
authorityLabel :: Text -> Text
authorityLabel = authorityLabelWithDefault 443

-- | A credential-free authority label using port 80 for a portless @http://@ URL, otherwise 443.
dialledAuthorityLabel :: Text -> Text
dialledAuthorityLabel raw = authorityLabelWithDefault portless raw
  where
    portless = if isPrefixOfLowered httpPrefix raw then 80 else 443

authorityLabelWithDefault :: Word16 -> Text -> Text
authorityLabelWithDefault portless = maybe unresolvedAuthority renderHostPort . hostPortAddressWithDefault portless

-- | Remove userinfo, query and fragment while retaining the fetched URL's scheme, authority and path.
credentialFreeUrl :: Text -> Text
credentialFreeUrl raw = credentialFreeOrigin raw <> credentialFreePath raw

-- | The scheme and the authority as written, without userinfo.
credentialFreeOrigin :: Text -> Text
credentialFreeOrigin raw = schemePrefix raw <> authorityOf raw

-- | The scheme and the dialled @host:port@, the part of a URL that names where it was fetched from.
dialledOrigin :: Text -> Text
dialledOrigin raw = schemePrefix raw <> dialledAuthorityLabel raw

-- | The path a URL fetches. The query and fragment are cut first, so a slash inside them is never read as the path.
credentialFreePath :: Text -> Text
credentialFreePath = registryPath . T.takeWhile (`notElem` ['?', '#'])

-- The scheme with its separator, or empty for a URL that writes none.
schemePrefix :: Text -> Text
schemePrefix raw = case T.breakOn "://" raw of
    (written, separator) | not (T.null separator) -> written <> "://"
    _ -> ""

-- The angle brackets match what the resolved-configuration provenance lines use for a
-- withheld value.
unresolvedAuthority :: Text
unresolvedAuthority = "<unresolved>"

{- Render a 'HostPort' back to a @host:port@ authority. An IPv6 host must be re-bracketed, since
@2606:4700::1111:8443@ is neither the host nor a parseable authority. -}
renderHostPort :: HostPort -> Text
renderHostPort (HostPort host port)
    | ":" `T.isInfixOf` host = "[" <> host <> "]:" <> show port
    | otherwise = host <> ":" <> show port

{- The port an authority dials: an explicit @":port"@, or @portless@ when it writes none. A
written-but-empty port is malformed, and the authority's trailing colon is what catches it. -}
effectivePort :: Word16 -> Text -> Text -> Maybe Word16
effectivePort portless authority rest = case T.stripPrefix ":" rest of
    Just written -> parsePort written
    Nothing
        | not (T.null rest) -> Nothing
        | ":" `T.isSuffixOf` authority -> Nothing
        | otherwise -> Just portless

{- A dialled port in the one spelling the gate accepts: decimal digits, no leading zero, 1..65535.
No crafted spelling may alias a canonical port, so 'readDecimalText' bars the other spellings. -}
parsePort :: Text -> Maybe Word16
parsePort t = do
    guard (T.take 1 t /= "0")
    n <- readDecimalText t :: Maybe Integer
    guard (n >= 1 && n <= 65535)
    pure (fromInteger n)

-- Returns the answer alone, never the credential-bearing span it read.
carriesUserinfo :: Text -> Bool
carriesUserinfo = T.isInfixOf "@" . authoritySpan

-- | Refuse configured URLs with userinfo, query or fragment before any check quotes the URL. Reasons never quote credentials.
refuseCredentialMaterial :: Text -> Text -> Either Text ()
refuseCredentialMaterial subject url
    | carriesUserinfo url =
        Left (subject <> " must not carry userinfo (a credential belongs in its own configuration key)")
    | "?" `T.isInfixOf` url = Left (subject <> " must not carry a query string")
    | "#" `T.isInfixOf` url = Left (subject <> " must not carry a fragment")
    | otherwise = Right ()

{- The authority component, userinfo intact, truncated at the first path, query, or fragment
delimiter. Kept private: a caller holding the credential-bearing span is the exposure to prevent. -}
authoritySpan :: Text -> Text
authoritySpan raw = T.takeWhile inAuthority (afterFirst "://" raw)

inAuthority :: Char -> Bool
inAuthority c = c /= '/' && c /= '?' && c /= '#'

{- The authority of a URI or bare @host[:port]@ value, with any userinfo dropped. 'hostAddress' and
'hostPortAddress' share it, so the two extractions cannot drift on an authority edge case. -}
authorityOf :: Text -> Text
authorityOf = afterLast "@" . authoritySpan

-- The text after @needle@'s last occurrence, or all of @hay@ if absent. The last "@" is the
-- userinfo boundary, matching what URL parsers do.
afterLast :: Text -> Text -> Text
afterLast needle hay =
    let (pre, post) = T.breakOnEnd needle hay
     in if T.null pre then hay else post

-- | Split a @host[:port]@ authority into its bare host and the raw @":port"@ remainder, empty when no port is present. The split is bracket-aware, and an unclosed opening bracket yields 'Nothing'.
splitHostPort :: Text -> Maybe (Text, Text)
splitHostPort authority
    | T.null authority = Nothing
    | otherwise = case T.stripPrefix "[" authority of
        Just rest -> case T.breakOn "]" rest of
            (_, "") -> Nothing -- an opening bracket with no close: malformed
            (inner, afterBracket) -> Just (inner, T.drop 1 afterBracket)
        Nothing -> case T.breakOn ":" authority of
            ("", _) -> Nothing
            (h, "") -> Just (h, "")
            (h, p) -> if p == ":" then Just (h, "") else Just (h, p)
