-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Textual extraction of the @host[:port]@ authority an outbound request dials.

Pragmatic, comparison-oriented extractors over a URI or bare @host[:port]@ value.
'hostAddress' recovers the bare host. 'hostPortAddress' recovers the host together
with its effective port as a 'HostPort' (443 when the URL writes none), and
'hostPortAddressWithDefault' takes that default from its caller. 'splitHostPort' is the
bracket-aware @host[:port]@ split they build on. These are not a full RFC 3986 parser. A
value with no recognisable authority yields the empty string or 'Nothing', which every
guard treats as not-allowed. The SSRF policy gates in "Ecluse.Core.Security.Host" consume
'HostPort', and the parsing here carries no policy of its own.

'authorityLabel' renders that same extraction back to text. Every log line and span
attribute reduces a URL through it before it names one.

'refuseCredentialMaterial' is the load-time counterpart. It refuses an
operator-configured URL that carries a credential. The boot-time configuration echo prints
every configured URL as written, so a credential must never reach it.
-}
module Ecluse.Core.Security.Authority (
    -- * The dialled authority
    HostPort (..),

    -- * Authority extraction
    hostAddress,
    hostPortAddress,
    hostPortAddressWithDefault,
    splitHostPort,
    carriesUserinfo,

    -- * Configured-URL refusal
    refuseCredentialMaterial,

    -- * Log-safe rendering
    authorityLabel,
) where

import Data.Text qualified as T

import Ecluse.Core.Security.IpLiteral (isDecimal)

{- | The authority an outbound fetch actually dials: a bare host with its effective port.

Registry egress is https-only ("Ecluse.Core.Security.Egress"), so a portless URL dials 443 and an
explicit @:443@ is the same authority. The gate authorises the pair, so a @dist.tarball@ on an
allowlisted host at an attacker-chosen port does not inherit that host's authorisation.
-}
data HostPort = HostPort
    { hpHost :: Text
    -- ^ The bare host: no brackets, no port, lower-cased by 'hostPortAddress'.
    , hpPort :: Word16
    -- ^ The effective port: the explicit @:port@, or the caller's portless default.
    }
    deriving stock (Eq, Ord, Show)

{- | Extract the bare host from a URI or @host[:port]@ authority, lower-cased and unbracketed. It is
a pragmatic extractor for comparison, not a full RFC 3986 parser.

A value with no recognisable host, a malformed authority included, yields the empty string, which
the guards treat as not-allowed. Authorisation compares host and port instead: use
'hostPortAddress'.
-}
hostAddress :: Text -> Text
hostAddress raw = T.toLower (maybe "" fst (splitHostPort (authorityOf raw)))

{- | Extract the host and the effective port a URI or @host[:port]@ authority dials, or 'Nothing'
when it holds no dialable authority.

A missing port defaults to 443, so an explicit @:443@ yields the same 'HostPort' as no port at all.
An out-of-grammar port, a written-but-empty port, or an unbracketed IPv6 literal yields 'Nothing',
which every authorisation clause treats as refused.

>>> hostPortAddress "https://registry.npmjs.org/thing/-/thing-1.0.0.tgz"
Just (HostPort {hpHost = "registry.npmjs.org", hpPort = 443})

>>> hostPortAddress "https://registry.npmjs.org:9443/thing"
Just (HostPort {hpHost = "registry.npmjs.org", hpPort = 9443})

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

{- | The log-safe label for a URL: its validated host and effective port.

An artifact URL is attacker-influenced and an advisory export URL is operator-supplied. Either can
carry a credential in its userinfo or a pre-signed query string, so every site that names a URL in
a log line or a span attribute names this instead. A URL with no dialable authority renders as
@\<unresolved\>@, never as a truncated piece of the input.

>>> authorityLabel "https://deploy:hunter2@registry.npmjs.org/thing/-/thing-1.0.0.tgz?sig=abc"
"registry.npmjs.org:443"

>>> authorityLabel "https://[2606:4700::1111]:8443/thing"
"[2606:4700::1111]:8443"

>>> authorityLabel "https://[::1/thing"
"<unresolved>"
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

{- A dialled port in the one canonical spelling the gate accepts: decimal digits, no leading zero,
value in 1..65535. A crafted spelling must not alias a canonical port, and an unparseable port must
yield no authority rather than fall back to the 443 default. The digit check is what stops
'readMaybe' accepting a sign or whitespace. -}
parsePort :: Text -> Maybe Word16
parsePort t = do
    guard (isDecimal t && T.take 1 t /= "0")
    n <- readMaybe (toString t) :: Maybe Integer
    guard (n >= 1 && n <= 65535)
    pure (fromInteger n)

{- | Whether a URI or bare @host[:port]@ value carries __userinfo__ in its authority
(@https:\/\/user:token\@host\/@).

A caller outside this module can ask only this about an authority's credential half,
and the answer hands back no credential-bearing text. 'refuseCredentialMaterial' asks
it, so that refusal and 'authorityOf' read the userinfo boundary the same way. A path
segment beginning with @\@@ (an npm scope) sits past the authority and is not userinfo.

>>> carriesUserinfo "https://deploy:hunter2@registry.npmjs.org/thing?sig=abc"
True

>>> carriesUserinfo "https://registry.npmjs.org/@acme/thing"
False
-}
carriesUserinfo :: Text -> Bool
carriesUserinfo = T.isInfixOf "@" . authoritySpan

{- | Refuse an operator-configured URL that carries credential material: userinfo
(@https:\/\/user:token\@host\/@), a query string, or a fragment.

@subject@ names the refused URL in the reason. It is the configuration key, or the kind
of URL where the caller holds no key. The reason reaches the boot log, so it states the
requirement and never the value. Run this before any check that quotes the value it
rejects. The rule covers a configured URL only. An upstream-supplied one may carry a
signed query, and 'authorityLabel' is what names it in a log line.

>>> refuseCredentialMaterial "server.publicUrl" "https://ecluse.example.test"
Right ()

>>> refuseCredentialMaterial "server.publicUrl" "https://deploy:hunter2@ecluse.example.test"
Left "server.publicUrl must not carry userinfo (a credential belongs in its own configuration key)"
-}
refuseCredentialMaterial :: Text -> Text -> Either Text ()
refuseCredentialMaterial subject url
    | carriesUserinfo url =
        Left (subject <> " must not carry userinfo (a credential belongs in its own configuration key)")
    | "?" `T.isInfixOf` url = Left (subject <> " must not carry a query string")
    | "#" `T.isInfixOf` url = Left (subject <> " must not carry a fragment")
    | otherwise = Right ()

{- The authority component of a URI or bare @host[:port]@ value, userinfo intact. It is
the text after the scheme separator, truncated at the first path\/query\/fragment
delimiter. Kept module-private, because a caller holding the credential-bearing span
is the exposure 'authorityLabel' exists to prevent.
'carriesUserinfo' answers the one question about it from outside.
-}
authoritySpan :: Text -> Text
authoritySpan raw = T.takeWhile (`notElem` ['/', '?', '#']) (afterFirst "://" raw)

{- The authority of a URI or bare @host[:port]@ value, with any userinfo dropped. 'hostAddress' and
'hostPortAddress' share it, so the two extractions cannot drift on an authority edge case. -}
authorityOf :: Text -> Text
authorityOf = afterLast "@" . authoritySpan

-- The text after @needle@'s first occurrence, or all of @hay@ if absent. The scheme
-- separator must match first: a crafted "https://169.254.169.254/x?u=https://ok.example"
-- gates on 169.254.169.254, the host dialled, never on the host after the last "://".
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
