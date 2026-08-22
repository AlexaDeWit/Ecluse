-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Textual extraction of the @host[:port]@ authority an outbound request dials.

Pragmatic, comparison-oriented extractors over a URI or bare @host[:port]@ value.
'hostAddress' recovers the bare host. 'hostPortAddress' recovers the host together
with its effective port as a 'HostPort' (443 when the URL writes none). 'splitHostPort'
is the bracket-aware @host[:port]@ split both build on, shared with the SQS endpoint
parser. These are not a full RFC 3986 parser. A value with no recognisable authority
yields the empty string or 'Nothing', which every guard treats as not-allowed. The
SSRF policy gates in "Ecluse.Core.Security.Host" consume 'HostPort', and the parsing
here carries no policy of its own.

'authorityLabel' renders that same extraction back to text. Every log line and span
attribute reduces a URL through it before it names one.
-}
module Ecluse.Core.Security.Authority (
    -- * The dialled authority
    HostPort (..),

    -- * Authority extraction
    hostAddress,
    hostPortAddress,
    splitHostPort,

    -- * Log-safe rendering
    authorityLabel,
) where

import Data.Text qualified as T

import Ecluse.Core.Security.IpLiteral (isDecimal)

{- | The authority an outbound fetch actually dials: a bare host together with its
effective port.

Registry egress is https-only ("Ecluse.Core.Security.Egress"), so a URL that writes no
port dials 443 by default. 'hostPortAddress' bakes that default in, and an explicit
@:443@ is therefore the same authority as no port at all. Carrying the port beside the
host lets the egress gate authorise the pair the dial targets rather than the host
alone. A @dist.tarball@ naming an allowlisted host on an attacker-chosen port must not
inherit that host's authorisation.
-}
data HostPort = HostPort
    { hpHost :: Text
    -- ^ The bare host: no brackets, no port, lower-cased by 'hostPortAddress'.
    , hpPort :: Word16
    -- ^ The effective port: the explicit @:port@, or 443 when the URL writes none.
    }
    deriving stock (Eq, Ord, Show)

{- | Extract the bare host from a URI or @host[:port]@ authority.

A convenience for the checks that classify the host alone.
'Ecluse.Core.Security.Host.isBlockedTarget' tests the bare literal, since an address
is internal regardless of port, and the same-host @http@-upgrade decision in
"Ecluse.Core.Security.Egress" compares bare hosts. This strips a @scheme:\/\/@
prefix, any @userinfo\@@, any @:port@ suffix, and any @\/path@\/@?query@\/@#fragment@
tail, lower-casing the result. It is a pragmatic extractor for comparison, not a full
RFC 3986 parser. A value with no recognisable host yields the empty string, which the
guards treat as not-allowed. An IPv6 literal in brackets (@[::1]:443@) comes back
without the brackets. The bracket-aware @host[:port]@ split is 'splitHostPort', shared
with the SQS endpoint parser so the two cannot drift on an authority edge case. A
malformed authority (an opening bracket with no close) yields the empty string, the
same fail-safe the guards apply to it. The authorisation clauses compare the host with
its effective port instead: extract those with 'hostPortAddress'.
-}
hostAddress :: Text -> Text
hostAddress raw = T.toLower (maybe "" fst (splitHostPort (authorityOf raw)))

{- | Extract the host and the effective port a URI or @host[:port]@ authority
dials, or 'Nothing' when it holds no dialable authority.

The authorisation-comparison companion to 'hostAddress'. It applies the same pragmatic
scheme\/userinfo\/path stripping, but parses the @:port@ suffix rather than discarding
it. The egress gate then compares the pair the fetch dials. A missing port defaults to
443, since registry egress is https-only, and an explicit @:443@ therefore yields the
same 'HostPort' as no port at all. The port is strict: a canonical run of decimal
digits, no leading zero, whose value fits @1..65535@. Anything else yields 'Nothing',
which every authorisation clause treats as refused:

* A non-numeric, signed, out-of-range, or leading-zero port ('parsePort').
* A written-but-empty port (@host:@ or @[::1]:@). The http-client library refuses any
  URL that writes a colon with no port digits. The gate refuses it too, rather than
  authorise an authority nothing can dial. The two spellings behave identically here.
  'splitHostPort' collapses the unbracketed @host:@ into an empty remainder,
  recognised by the authority's trailing colon. It carries the bracketed @[::1]:@
  through as a @":"@ remainder instead.
* Junk after a bracketed IPv6 literal, or an unbracketed IPv6 literal. An unbracketed
  literal's colons leave no unambiguous host\/port split, so the split refuses it whole
  rather than mangle it into a truncated host.

>>> hostPortAddress "https://registry.npmjs.org/thing/-/thing-1.0.0.tgz"
Just (HostPort {hpHost = "registry.npmjs.org", hpPort = 443})

>>> hostPortAddress "https://registry.npmjs.org:9443/thing"
Just (HostPort {hpHost = "registry.npmjs.org", hpPort = 9443})

>>> hostPortAddress "https://[2606:4700::1111]:8443/thing"
Just (HostPort {hpHost = "2606:4700::1111", hpPort = 8443})
-}
hostPortAddress :: Text -> Maybe HostPort
hostPortAddress raw = do
    let authority = authorityOf raw
    (host, rest) <- splitHostPort authority
    guard (not (T.null host))
    port <- effectivePort authority rest
    pure (HostPort (T.toLower host) port)

{- | The log-safe label for a URL: its validated host and effective port, with the
scheme, any userinfo, and the whole path\/query\/fragment tail dropped.

An artifact URL is attacker-influenced and an advisory export URL is operator-supplied.
Either can carry a credential in its userinfo or a pre-signed query string. No log line
and no span attribute may render one, so every site that wants to name a URL names this
instead. A value that 'hostPortAddress' cannot resolve to a dialable authority renders
as @\<unresolved\>@, never as a truncated piece of the input. The label re-brackets an
IPv6 literal, so it reads back as the same authority.

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

{- Render a 'HostPort' back to a @host:port@ authority. This re-brackets an IPv6
literal, since 'HostPort' holds the host unbracketed and @2606:4700::1111:8443@ would be
neither the host nor a parseable authority. -}
renderHostPort :: HostPort -> Text
renderHostPort (HostPort host port)
    | ":" `T.isInfixOf` host = "[" <> host <> "]:" <> show port
    | otherwise = host <> ":" <> show port

{- The effective port an authority dials, given the raw @rest@ 'splitHostPort' left
after the host. It is the parsed digits of an explicit @":port"@, or 443 for a
genuinely portless authority. It is 'Nothing' when nothing can dial the authority.
Three ways it is undialable, all fail closed.

\* A non-empty @rest@ that is not a @":port"@ is junk after a bracketed IPv6 literal
  (@[::1]x@ leaves @rest == "x"@). This refuses the whole authority.
\* A written-but-empty port is malformed, since http-client refuses a URL that writes
  a colon with no digits. The bracketed @[::1]:@ arrives as @rest == ":"@, so
  'parsePort' on the empty tail refuses it. 'splitHostPort' collapses the unbracketed
  @host:@ into an empty @rest@, recognised here by the authority's own trailing colon.
\* 'parsePort' refuses an out-of-grammar port digit sequence.

A genuinely portless authority has an empty @rest@ and no trailing colon.
-}
effectivePort :: Text -> Text -> Maybe Word16
effectivePort authority rest = case T.stripPrefix ":" rest of
    Just written -> parsePort written
    Nothing
        | not (T.null rest) -> Nothing
        | ":" `T.isSuffixOf` authority -> Nothing
        | otherwise -> Just 443

{- A dialled port under the strict, canonical spelling the gate accepts: a non-empty
run of decimal digits, with no leading zero, whose value fits 1..65535. One spelling
per port. This refuses a leading-zero form (@0443@, @080@) so a crafted spelling
cannot alias a canonical port, alongside the signed, out-of-range, and non-numeric
rejections. Strictness is load-bearing. An unparseable port must yield no authority at
all, and never fall back to the default. Otherwise a crafted suffix would alias the
default-port authority. The digit check keeps 'readMaybe' from accepting signs or
whitespace.
-}
parsePort :: Text -> Maybe Word16
parsePort t = do
    guard (isDecimal t && T.take 1 t /= "0")
    n <- readMaybe (toString t) :: Maybe Integer
    guard (n >= 1 && n <= 65535)
    pure (fromInteger n)

{- The authority component of a URI or bare @host[:port]@ value. It is the text after
the scheme separator, truncated at the first path\/query\/fragment delimiter, with any
userinfo dropped. 'hostAddress' and 'hostPortAddress' share it, so the two extractions
cannot drift on an authority edge case.
-}
authorityOf :: Text -> Text
authorityOf raw =
    let afterScheme = afterFirst "://" raw
        authority = T.takeWhile (`notElem` ['/', '?', '#']) afterScheme
     in afterLast "@" authority
  where
    -- The text after @needle@'s first occurrence, or all of @hay@ if absent. The
    -- scheme separator matches at its first occurrence, so the extracted authority is
    -- the one http-client actually dials. A later "://" inside a path or query is not
    -- that authority. A crafted dist.tarball like
    -- "https://169.254.169.254/x?u=https://ok.example" must gate on 169.254.169.254
    -- (the host connected to), never on the host after the last "://".
    afterFirst :: Text -> Text -> Text
    afterFirst needle hay = fromMaybe hay (T.stripPrefix needle (snd (T.breakOn needle hay)))

    -- The text after @needle@'s last occurrence, or all of @hay@ if absent. It marks
    -- the userinfo "@" boundary, where the last "@" in the authority separates
    -- userinfo from host (matching URL parsers).
    afterLast :: Text -> Text -> Text
    afterLast needle hay =
        let (pre, post) = T.breakOnEnd needle hay
         in if T.null pre then hay else post

{- | Split a @host[:port]@ authority into its bare host and the raw @":port"@
remainder, empty when no port is present. The split is bracket-aware, so an IPv6
literal's inner colons are never mistaken for the port separator.

The single canonical authority split. Both the data-plane host extractor
('hostAddress') and the SQS endpoint parser ('Ecluse.Composition.MirrorQueue.parseEndpointUrl')
use it, so the two cannot drift again on the @[::1]:port@ edge cases. A @[…]@ IPv6
literal splits on its closing bracket. The host comes back without the brackets, and
the remainder is whatever follows: a @":port"@, or empty. An inner @::@ is therefore
never read as the port separator. A bare authority splits on its first @':'@. An
opening bracket with no close is a malformed authority, and yields 'Nothing'.
'hostAddress' folds that to the empty (not-allowed) host, and the endpoint parser
surfaces it as a malformed-URL boot error.
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
