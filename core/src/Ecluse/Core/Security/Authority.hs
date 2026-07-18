-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Textual extraction of the @host[:port]@ authority an outbound request dials.

Pragmatic, comparison-oriented extractors over a URI or bare @host[:port]@ value:
'hostAddress' recovers the bare host, 'hostPortAddress' recovers the host together
with its effective port as a 'HostPort' (443 when none is written), and
'splitHostPort' is the bracket-aware @host[:port]@ split both build on (also shared
with the SQS endpoint parser). These are __not__ a full RFC 3986 parser: a value
with no recognisable authority yields the empty string or 'Nothing', which every
guard treats as not-allowed. The SSRF policy gates in "Ecluse.Core.Security.Host"
consume 'HostPort'; the parsing here carries no policy of its own.
-}
module Ecluse.Core.Security.Authority (
    -- * The dialled authority
    HostPort (..),

    -- * Authority extraction
    hostAddress,
    hostPortAddress,
    splitHostPort,
) where

import Data.Text qualified as T

import Ecluse.Core.Security.IpLiteral (isDecimal)

{- | The authority an outbound fetch actually dials: a bare host together with its
__effective__ port.

Registry egress is https-only ("Ecluse.Core.Security.Egress"), so a URL that writes
no port dials 443; 'hostPortAddress' bakes that default in, and an explicit @:443@
is therefore the same authority as no port at all. Carrying the port beside the
host is what lets the egress gate authorise the pair the dial targets rather than
the host alone: a @dist.tarball@ naming an allowlisted host on an attacker-chosen
port must not inherit that host's authorisation.
-}
data HostPort = HostPort
    { hpHost :: Text
    -- ^ The bare host: no brackets, no port, lower-cased by 'hostPortAddress'.
    , hpPort :: Word16
    -- ^ The effective port: the explicit @:port@, or 443 when none is written.
    }
    deriving stock (Eq, Ord, Show)

{- | Extract the bare host from a URI or @host[:port]@ authority.

A convenience for the checks that classify the host alone: 'Ecluse.Core.Security.Host.isBlockedTarget'
tests the bare literal (an address is internal regardless of port), and the
same-host @http@-upgrade decision in "Ecluse.Core.Security.Egress" compares bare
hosts. This strips a @scheme:\/\/@ prefix, any @userinfo\@@, any @:port@ suffix,
and any @\/path@\/@?query@\/@#fragment@ tail, lower-casing the result. It is a
pragmatic extractor for comparison, __not__ a full RFC 3986 parser; a value with
no recognisable host yields the empty string, which the guards treat as
not-allowed. IPv6 literals in brackets (@[::1]:443@) are returned without the
brackets -- the bracket-aware @host[:port]@ split is 'splitHostPort', shared with
the SQS endpoint parser so the two cannot drift on an authority edge case; a
malformed authority (an opening bracket with no close) yields the empty string,
the same fail-safe the guards apply to it. The authorisation clauses compare the
host __with__ its effective port instead: extract those with 'hostPortAddress'.
-}
hostAddress :: Text -> Text
hostAddress raw = T.toLower (maybe "" fst (splitHostPort (authorityOf raw)))

{- | Extract the host and the effective port a URI or @host[:port]@ authority
dials, or 'Nothing' when no dialable authority can be recovered.

The authorisation-comparison companion to 'hostAddress': the same pragmatic
scheme\/userinfo\/path stripping, but the @:port@ suffix is __parsed rather than
discarded__, so the egress gate compares the pair the fetch dials. A missing port
defaults to 443 (registry egress is https-only), and an explicit @:443@ therefore
yields the same 'HostPort' as no port at all. The port is strict: a canonical
run of decimal digits, no leading zero, whose value fits @1..65535@. Anything else
yields 'Nothing', which every authorisation clause treats as refused:

* a non-numeric, signed, out-of-range, or leading-zero port ('parsePort');
* a __written-but-empty__ port (@host:@ or @[::1]:@): http-client refuses any URL
  that writes a colon with no port digits, so the gate refuses it too rather than
  authorise an authority that can never be dialled. The two spellings are treated
  identically here even though 'splitHostPort' collapses the unbracketed @host:@
  into an empty remainder (recognised by the authority's trailing colon) while it
  carries the bracketed @[::1]:@ through as a @":"@ remainder;
* junk after a bracketed IPv6 literal, or an unbracketed IPv6 literal (whose colons
  leave no unambiguous host\/port split, so it is refused whole rather than mangled
  into a truncated host).

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

{- The effective port an authority dials, given the raw @rest@ 'splitHostPort' left
after the host: the parsed digits of an explicit @":port"@, 443 for a genuinely
portless authority, or 'Nothing' when the authority is undialable. Three ways it is
undialable, all fail closed:

\* a @rest@ that is non-empty but is not a @":port"@ is junk after a bracketed IPv6
  literal (@[::1]x@ leaves @rest == "x"@), so the whole authority is refused;
\* a written-but-empty port is malformed, since http-client refuses a URL that
  writes a colon with no digits: the bracketed @[::1]:@ arrives as @rest == ":"@ so
  'parsePort' on the empty tail refuses it, while the unbracketed @host:@ is
  collapsed by 'splitHostPort' into an empty @rest@, recognised here by the
  authority's own trailing colon;
\* an out-of-grammar port digit sequence is refused by 'parsePort'.

A genuinely portless authority has an empty @rest@ and no trailing colon.
-}
effectivePort :: Text -> Text -> Maybe Word16
effectivePort authority rest = case T.stripPrefix ":" rest of
    Just written -> parsePort written
    Nothing
        | not (T.null rest) -> Nothing
        | ":" `T.isSuffixOf` authority -> Nothing
        | otherwise -> Just 443

{- A dialled port under the strict, __canonical__ spelling the gate accepts: a
non-empty run of decimal digits, with __no leading zero__, whose value fits
1..65535. One spelling per port: a leading-zero form (@0443@, @080@) is refused so a
crafted spelling cannot alias a canonical port, alongside the signed, out-of-range,
and non-numeric rejections. Strictness is load-bearing: an unparseable port must
yield no authority at all, never fall back to the default, or a crafted suffix would
alias the default-port authority. The digit check keeps 'readMaybe' from accepting
signs or whitespace.
-}
parsePort :: Text -> Maybe Word16
parsePort t = do
    guard (isDecimal t && T.take 1 t /= "0")
    n <- readMaybe (toString t) :: Maybe Integer
    guard (n >= 1 && n <= 65535)
    pure (fromInteger n)

{- The authority component of a URI or bare @host[:port]@ value: the text after the
scheme separator, truncated at the first path\/query\/fragment delimiter, with any
userinfo dropped. Shared by 'hostAddress' and 'hostPortAddress' so the two
extractions cannot drift on an authority edge case.
-}
authorityOf :: Text -> Text
authorityOf raw =
    let afterScheme = afterFirst "://" raw
        authority = T.takeWhile (`notElem` ['/', '?', '#']) afterScheme
     in afterLast "@" authority
  where
    -- The text after @needle@'s __first__ occurrence, or all of @hay@ if absent.
    -- The scheme separator is matched at its first occurrence so the extracted
    -- authority is the one http-client actually dials, not a later "://" inside a
    -- path or query: a crafted dist.tarball like
    -- "https://169.254.169.254/x?u=https://ok.example" must gate on 169.254.169.254
    -- (the host connected to), never on the host after the last "://".
    afterFirst :: Text -> Text -> Text
    afterFirst needle hay = fromMaybe hay (T.stripPrefix needle (snd (T.breakOn needle hay)))

    -- The text after @needle@'s last occurrence, or all of @hay@ if absent. Used for
    -- the userinfo "@" boundary, where the last "@" in the authority separates
    -- userinfo from host (matching URL parsers).
    afterLast :: Text -> Text -> Text
    afterLast needle hay =
        let (pre, post) = T.breakOnEnd needle hay
         in if T.null pre then hay else post

{- | Split a @host[:port]@ authority into its bare host and the raw @":port"@
remainder (empty when no port is present), bracket-aware so an IPv6 literal's
inner colons are never mistaken for the port separator.

The single canonical authority split feeding both the data-plane host extractor
('hostAddress') and the SQS endpoint parser ('Ecluse.Composition.MirrorQueue.parseEndpointUrl'),
so the two re-implementations the @[::1]:port@ edge cases tripped on cannot drift
again. A @[…]@ IPv6 literal is split on its closing bracket -- the host is returned
without the brackets and the remainder is whatever follows (a @":port"@ or empty) --
so an inner @::@ is never read as the port separator; a bare authority is split on
its first @':'@. An opening bracket with __no__ close is a malformed authority and
yields 'Nothing', which 'hostAddress' folds to the empty (not-allowed) host and the
endpoint parser surfaces as a malformed-URL boot error.
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
