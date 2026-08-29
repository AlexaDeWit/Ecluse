-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Textual extraction of the @host[:port]@ authority an outbound request dials.

These are comparison extractors, not an RFC 3986 parser: a value with no recognisable
authority yields the empty string or 'Nothing', which every guard treats as not-allowed.
The SSRF gates in "Ecluse.Core.Security.Host" consume the extracted 'HostPort', so the
parsing here carries no policy of its own. 'authorityLabel' renders the same extraction for
a log line, and 'refuseCredentialMaterial' refuses a configured URL carrying a credential.
-}
module Ecluse.Core.Security.Authority (
    -- * The dialled authority
    HostPort (..),

    -- * Authority extraction
    hostAddress,
    hostPortAddress,
    hostPortAddressWithDefault,
    splitHostPort,

    -- * Configured-URL refusal
    refuseCredentialMaterial,

    -- * Log-safe rendering
    authorityLabel,
) where

import Data.Text qualified as T

import Ecluse.Core.Text (readDecimalText)

{- | The authority an outbound fetch dials: a bare host with its effective port. The gate
authorises the pair, so an allowlisted host at an attacker-chosen port is not authorised.
-}
data HostPort = HostPort
    { hpHost :: Text
    -- ^ The bare host: no brackets, no port, lower-cased by both extractors.
    , hpPort :: Word16
    -- ^ The effective port: the explicit @:port@, or the caller's portless default.
    }
    deriving stock (Eq, Ord, Show)

{- | The bare host of a URI or @host[:port]@ authority, lower-cased and unbracketed. A value with no
recognisable host yields the empty string, which the guards treat as not-allowed.
-}
hostAddress :: Text -> Text
hostAddress raw = T.toLower (maybe "" fst (splitHostPort (authorityOf raw)))

{- | The host and the effective port a URI or @host[:port]@ authority dials, or 'Nothing' when it
holds none. A portless URL dials 443, and an out-of-grammar or written-but-empty port is refused.

>>> hostPortAddress "https://[2606:4700::1111]:8443/thing"
Just (HostPort {hpHost = "2606:4700::1111", hpPort = 8443})
-}
hostPortAddress :: Text -> Maybe HostPort
hostPortAddress = hostPortAddressWithDefault 443

{- | 'hostPortAddress' with the caller's own port for a URL that writes none, for a scheme whose
portless default is not the gate's 443. The authority split and the port grammar do not change.
-}
hostPortAddressWithDefault :: Word16 -> Text -> Maybe HostPort
hostPortAddressWithDefault portless raw = do
    let authority = authorityOf raw
    (host, rest) <- splitHostPort authority
    guard (not (T.null host))
    port <- effectivePort portless authority rest
    pure (HostPort (T.toLower host) port)

{- | The log-safe label for a URL: its validated host and effective port, or @\<unresolved\>@. An
attacker-influenced or credential-bearing URL must never reach a log line or a span as written.

>>> authorityLabel "https://deploy:hunter2@registry.npmjs.org/thing/-/thing-1.0.0.tgz?sig=abc"
"registry.npmjs.org:443"
-}
authorityLabel :: Text -> Text
authorityLabel = maybe unresolvedAuthority renderHostPort . hostPortAddress

-- What a value carrying no dialable authority renders as. The angle brackets match the
-- convention the resolved-configuration provenance lines use for a withheld value.
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

{- Whether a URI or bare @host[:port]@ value carries userinfo in its authority. It answers the
one question about the credential half, and returns no text. -}
carriesUserinfo :: Text -> Bool
carriesUserinfo = T.isInfixOf "@" . authoritySpan

{- | Refuse an operator-configured URL that carries credential material: userinfo, a query string,
or a fragment. Run it before any check that quotes the value, and the reason it returns never does.

>>> refuseCredentialMaterial "server.publicUrl" "https://deploy:hunter2@ecluse.example.test"
Left "server.publicUrl must not carry userinfo (a credential belongs in its own configuration key)"

>>> refuseCredentialMaterial "registry.url" "https://registry.npmjs.org/@acme/thing"
Right ()
-}
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
authoritySpan raw = T.takeWhile (`notElem` ['/', '?', '#']) (afterFirst "://" raw)

{- The authority of a URI or bare @host[:port]@ value, with any userinfo dropped. 'hostAddress' and
'hostPortAddress' share it, so the two extractions cannot drift on an authority edge case. -}
authorityOf :: Text -> Text
authorityOf = afterLast "@" . authoritySpan

-- The text after @needle@'s first occurrence, or all of @hay@ if absent. The scheme separator must
-- match first, so a crafted "https://169.254.169.254/x?u=https://ok" gates on the host dialled.
afterFirst :: Text -> Text -> Text
afterFirst needle hay = fromMaybe hay (T.stripPrefix needle (snd (T.breakOn needle hay)))

-- The text after @needle@'s last occurrence, or all of @hay@ if absent. The last "@" is the
-- userinfo boundary, matching what URL parsers do.
afterLast :: Text -> Text -> Text
afterLast needle hay =
    let (pre, post) = T.breakOnEnd needle hay
     in if T.null pre then hay else post

{- | Split a @host[:port]@ authority into its bare host and the raw @":port"@ remainder, empty when
no port is present. The split is bracket-aware, and an unclosed opening bracket yields 'Nothing'.
-}
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
