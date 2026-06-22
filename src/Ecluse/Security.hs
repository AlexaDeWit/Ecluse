{- | Outbound-request and response-bound guards for the proxy's data plane.

Écluse builds outbound HTTP requests from two untrusted sources — __client-supplied
package identifiers__ (the request path) and __upstream-supplied artifact
locations__ (a packument's @dist.tarball@) — and then parses whatever an upstream
returns. This module is the pure guard layer that keeps those steps from being
steered or exhausted by hostile input. It defends three boundaries:

* __Where the proxy fetches.__ 'isAllowedUpstreamHost' restricts outbound fetches
  to the configured upstream hosts, and 'isBlockedTarget' rejects internal address
  ranges (cloud instance metadata, loopback, RFC1918) that the proxy's network
  position can otherwise reach. Together they are the SSRF gate: a target must be
  both on the allowlist /and/ not an internal address.

* __How an upstream URL is derived.__ 'upstreamUrlFor' builds an artifact\/metadata
  URL from a configured base URL and an __already-parsed__ 'PackageName', never
  from raw client path segments, re-checking each name component with the router's
  own safety rule so traversal, encoded slashes, or an absolute URL cannot change
  the target.

* __How much an upstream may cost.__ A 'Limits' budget plus 'boundedRead' (abort a
  streamed body past 'maxBodyBytes') and 'checkVersionCount' \/ 'checkNestingDepth'
  (reject an oversized or deeply-nested parsed document) bound algorithmic-complexity
  DoS from a hostile or compromised upstream. Every limit __fails closed__: exceeding
  one yields 'Left', never a truncated or partial result.

The functions are pure and total; the streamed-body guard ('boundedRead') is
polymorphic over the producing monad so the streaming data plane can run it in
'IO' while tests drive it purely. They are __primitives__: the fetch and serve
layers compose them at the boundary (see @docs\/architecture\/registry-model.md@
→ "Registry Abstraction", @docs\/architecture\/web-layer.md@, and
@docs\/architecture\/hosting.md@ → "URL rewriting"). Path-component safety is
shared with the router's "Ecluse.Server.Route" ('isSafeComponent'); the threat
model these guards answer is recorded there too.
-}
module Ecluse.Security (
    -- * Outbound host allowlist
    LoweredHostSet,
    lowerCaseHosts,
    isAllowedUpstreamHost,

    -- * Internal-range block
    isBlockedTarget,
    hostAddress,

    -- * Identifier → URL safety
    upstreamUrlFor,
    UrlError (..),

    -- * Response bounds
    Limits (..),
    defaultLimits,
    LimitError (..),
    boundedRead,
    checkVersionCount,
    checkNestingDepth,
) where

import Data.Aeson (Value (Array, Bool, Null, Number, Object, String))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector qualified as V

import Ecluse.Package (PackageInfo, PackageName, infoVersions, renderPackageName)
import Ecluse.Server.Route (isSafeComponent)

-- ── outbound host allowlist ──────────────────────────────────────────────────

{- | A set of host strings normalised to lower case, the form the host guards
('isAllowedUpstreamHost' and 'isBlockedTarget') compare against.

The type is __opaque, and 'lowerCaseHosts' is its only constructor__: a value of
this type therefore carries the proof that every host in it is already
lower-cased, so the guards lower only the /incoming/ host and the case-insensitive
match cannot be bypassed by an un-normalised configuration set.
-}
newtype LoweredHostSet = LoweredHostSet (Set Text)
    deriving stock (Eq, Show)

{- | Normalise a set of configured host strings to lower case, yielding the
'LoweredHostSet' the host guards take. DNS hostnames are case-insensitive, so
folding case here lets the guards match an incoming host against the
configuration regardless of how either was spelled.
-}
lowerCaseHosts :: Set Text -> LoweredHostSet
lowerCaseHosts = LoweredHostSet . Set.map T.toLower

{- | Whether @host@ is one of the configured upstream hosts. __Pure and total.__

The first guard on every outbound fetch: the proxy talks to its configured
private\/public upstreams and mirror target, and __nothing else__ — so a target
host derived from a packument's @dist.tarball@ (or anywhere else) is fetched only
if it appears in @allowed@. The match is exact on the bare host (no port, no
scheme — extract it with 'hostAddress' first) and __case-insensitive__, since
DNS hostnames are; an empty @host@ is never allowed. This is the allowlist half
of the SSRF gate; pair it with 'isBlockedTarget' for the internal-range half.

The allowlist is a 'LoweredHostSet', so it is already lower-cased and only the
incoming @host@ is folded here.
-}
isAllowedUpstreamHost :: LoweredHostSet -> Text -> Bool
isAllowedUpstreamHost (LoweredHostSet allowed) host =
    not (T.null host) && T.toLower host `Set.member` allowed

-- ── internal-range block ─────────────────────────────────────────────────────

{- | Whether @host@ is an internal address the proxy must not fetch, /unless/ it
is explicitly opted in. __Pure and total.__

A proxy sits in a privileged network position, so an attacker who can steer a
fetch (see the module header) aims it at addresses only the proxy can reach: the
cloud instance-metadata endpoint (@169.254.169.254@), loopback, or the private
network (RFC1918). This blocks, by parsing @host@ as a literal IP and testing it
against:

* __link-local__ @169.254.0.0\/16@ (which contains the @169.254.169.254@ metadata
  address) and IPv6 @fe80::\/10@;
* __loopback__ @127.0.0.0\/8@ and IPv6 @::1@;
* __RFC1918 private__ @10.0.0.0\/8@, @172.16.0.0\/12@, and @192.168.0.0\/16@.

A host in @allowedInternal@ is __never__ blocked (matched case-insensitively, as
DNS and the host allowlist are) — the deliberate opt-in for a private upstream that
genuinely lives on an internal address. As a 'LoweredHostSet' it is already
lower-cased, so only the incoming @host@ is folded for the comparison. A @host@
that is not an IP literal (a DNS name) is __not__ blocked here: name-based targets
are constrained by the 'isAllowedUpstreamHost' allowlist instead, and
post-resolution IP filtering belongs to the resolving fetch layer, not this pure
check. Both guards apply — an allowlisted host that resolves to an internal literal
is still caught when its address is tested here.
-}
isBlockedTarget :: LoweredHostSet -> Text -> Bool
isBlockedTarget (LoweredHostSet allowedInternal) host =
    not (T.toLower host `Set.member` allowedInternal)
        && maybe False isInternalAddress (parseIpLiteral host)

{- | An IP literal, parsed from a host for internal-range testing. Internal to
this module; consumed only by 'isInternalAddress', so it carries no instances.
-}
data IpAddr
    = -- | An IPv4 address as its four octets.
      IPv4 Word8 Word8 Word8 Word8
    | -- | An IPv6 address, normalised to its eight 16-bit groups.
      IPv6 [Word16]

{- | Extract the bare host from a URI or @host[:port]@ authority. __Pure and
total.__

A convenience for the SSRF gate: an outbound target is usually a full URL or an
authority, but 'isAllowedUpstreamHost' and 'isBlockedTarget' compare the bare
host. This strips a @scheme:\/\/@ prefix, any @userinfo\@@, any @:port@ suffix,
and any @\/path@\/@?query@\/@#fragment@ tail, lower-casing the result. It is a
pragmatic extractor for comparison, __not__ a full RFC 3986 parser; a value with
no recognisable host yields the empty string, which both guards treat as
not-allowed. IPv6 literals in brackets (@[::1]:443@) are returned without the
brackets.
-}
hostAddress :: Text -> Text
hostAddress raw =
    let afterScheme = afterLast "://" raw
        authority = T.takeWhile (`notElem` ['/', '?', '#']) afterScheme
        afterUserinfo = afterLast "@" authority
     in T.toLower (stripPort afterUserinfo)
  where
    -- The text after @needle@'s last occurrence, or all of @hay@ if absent.
    -- ('T.breakOnEnd' yields @(hay, "")@ when the needle is absent — its prefix
    -- is non-empty exactly when the needle was found, since it includes it.)
    afterLast :: Text -> Text -> Text
    afterLast needle hay =
        let (pre, post) = T.breakOnEnd needle hay
         in if T.null pre then hay else post

    -- Drop a ':port' suffix. A bracketed IPv6 literal keeps its colons: strip
    -- the brackets and any trailing ':port', but never split on the inner ':'.
    stripPort :: Text -> Text
    stripPort h = case T.stripPrefix "[" h of
        Just rest -> T.takeWhile (/= ']') rest
        Nothing -> T.takeWhile (/= ':') h

-- | Whether a parsed IP literal falls in any blocked internal range.
isInternalAddress :: IpAddr -> Bool
isInternalAddress = \case
    IPv4 a b _ _ ->
        a == 127 -- loopback 127.0.0.0/8
            || (a == 169 && b == 254) -- link-local 169.254.0.0/16
            || a == 10 -- RFC1918 10.0.0.0/8
            || (a == 172 && b >= 16 && b <= 31) -- RFC1918 172.16.0.0/12
            || (a == 192 && b == 168) -- RFC1918 192.168.0.0/16
    IPv6 groups -> isInternalV6 groups

{- | Whether 16-bit IPv6 groups are loopback (@::1@), link-local
(@fe80::\/10@), or IPv4-mapped (@::ffff:0:0\/96@). The mapped range lets an
attacker embed an internal IPv4 literal (e.g. @::ffff:169.254.169.254@) in an
IPv6 form that the per-IPv4-range checks would otherwise miss; decoding the
embedded address and re-running 'isInternalAddress' on the IPv4 result closes
the gap. Both spellings of a mapped address reach here as the same eight groups
— the hex form (@::ffff:a9fe:a9fe@) and the canonical dotted form
(@::ffff:169.254.169.254@), which 'parseIPv6' expands. ULA (@fc00::\/7@) and
NAT64 (@64:ff9b::\/96@) are out of scope.
-}
isInternalV6 :: [Word16] -> Bool
isInternalV6 groups =
    groups == [0, 0, 0, 0, 0, 0, 0, 1] -- ::1 loopback
        || any linkLocal (take 1 groups) -- fe80::/10 link-local (first group)
        || isIpv4Mapped groups -- ::ffff:0:0/96 (IPv4-mapped)
  where
    linkLocal g0 = g0 >= 0xFE80 && g0 <= 0xFEBF
    -- Perform Word16 arithmetic before narrowing to Word8 so the high-byte
    -- extraction is not corrupted by premature truncation.
    isIpv4Mapped [0, 0, 0, 0, 0, 0xFFFF, hi, lo] =
        isInternalAddress
            ( IPv4
                (fromIntegral (hi `div` 256))
                (fromIntegral (hi `mod` 256))
                (fromIntegral (lo `div` 256))
                (fromIntegral (lo `mod` 256))
            )
    isIpv4Mapped _ = False

{- | Parse a host as an IP literal, or 'Nothing' for a DNS name. Handles dotted-
quad IPv4 and the IPv6 forms a host realistically carries — full eight-group form,
@::@-compressed forms (including @::1@), and a trailing embedded IPv4 (the
@a.b.c.d@ in @::ffff:a.b.c.d@) — which is enough to recognise the loopback,
link-local, and IPv4-mapped addresses 'isInternalAddress' blocks. It is
deliberately __not__ a complete IPv6 parser (no zone ids); an unrecognised
literal is treated as a name, which the host allowlist still constrains.
-}
parseIpLiteral :: Text -> Maybe IpAddr
parseIpLiteral host = case T.uncons host of
    Nothing -> Nothing -- empty host: not a literal
    Just _ -> if T.any (== ':') host then parseIPv6 host else parseIPv4 host

-- | Parse a strict dotted-quad @a.b.c.d@ with each octet in @0..255@.
parseIPv4 :: Text -> Maybe IpAddr
parseIPv4 host = case T.splitOn "." host of
    [a, b, c, d] -> IPv4 <$> octet a <*> octet b <*> octet c <*> octet d
    _ -> Nothing
  where
    -- An octet is a non-empty all-decimal run in 0..255. The digit check keeps
    -- 'readMaybe' from accepting signs/whitespace, so a parsed value is >= 0.
    octet :: Text -> Maybe Word8
    octet t = do
        n <- if isDecimal t then readMaybe (toString t) else Nothing :: Maybe Integer
        if n <= 255 then Just (fromInteger n) else Nothing

{- | Parse an IPv6 literal — either the full eight-group form or a @::@-compressed
form (at most one @::@), optionally ending in an embedded dotted-quad IPv4 — into
its eight 16-bit groups. Enough to recognise the @::1@, @fe80::\/10@, and
@::ffff:0:0\/96@ addresses we block; rejects anything malformed.
-}
parseIPv6 :: Text -> Maybe IpAddr
parseIPv6 host =
    case T.splitOn "::" host of
        [single] -> exactly8 =<< groupsOf single
        [before, after] -> do
            hd <- groupsOf before
            tl <- groupsOf after
            -- "::" stands for at least one all-zero group, so the explicit groups
            -- on either side must total at most 7 (leaving room to fill to 8).
            let present = length hd + length tl
            if present <= 7
                then Just (IPv6 (hd <> replicate (8 - present) 0 <> tl))
                else Nothing
        _ -> Nothing -- more than one "::" is illegal
  where
    -- The colon-separated groups of one side; "" → no groups. The final token
    -- may be a dotted-quad IPv4 (RFC 4291 §2.2.3, e.g. the @169.254.169.254@ in
    -- @::ffff:169.254.169.254@), which expands to its two 16-bit groups so an
    -- IPv4-mapped literal in its canonical dotted form is decoded rather than
    -- mistaken for a name. Only the last token may be dotted; an interior dotted
    -- token fails 'group16' (no hex '.') and the whole parse is rejected.
    groupsOf :: Text -> Maybe [Word16]
    groupsOf t
        | T.null t = Just []
        | otherwise = groups (T.splitOn ":" t)

    groups :: [Text] -> Maybe [Word16]
    groups [] = Just []
    groups [tok]
        | T.any (== '.') tok = embeddedV4 tok
        | otherwise = (: []) <$> group16 tok
    groups (tok : rest) = (:) <$> group16 tok <*> groups rest

    -- A trailing dotted-quad IPv4 as its two 16-bit groups (high pair, low pair).
    embeddedV4 :: Text -> Maybe [Word16]
    embeddedV4 t = case parseIPv4 t of
        Just (IPv4 a b c d) -> Just [pair a b, pair c d]
        _ -> Nothing
      where
        pair hi lo = fromIntegral hi * 256 + fromIntegral lo

    -- A group is a non-empty all-hex run that fits in 16 bits. The hex check
    -- keeps 'readMaybe' from accepting signs, so a parsed value is >= 0.
    group16 :: Text -> Maybe Word16
    group16 t = do
        n <- if isHex t then readMaybe ("0x" <> toString t) else Nothing :: Maybe Integer
        if n <= 0xFFFF then Just (fromInteger n) else Nothing

    exactly8 :: [Word16] -> Maybe IpAddr
    exactly8 gs = if length gs == 8 then Just (IPv6 gs) else Nothing

-- | Whether @t@ is a non-empty run of decimal digits (no sign or whitespace).
isDecimal :: Text -> Bool
isDecimal t = not (T.null t) && T.all (`elem` ['0' .. '9']) t

-- | Whether @t@ is a non-empty run of hexadecimal digits.
isHex :: Text -> Bool
isHex t = not (T.null t) && T.all isHexDigit t
  where
    isHexDigit c = c `elem` (['0' .. '9'] <> ['a' .. 'f'] <> ['A' .. 'F'])

-- ── identifier → URL safety ──────────────────────────────────────────────────

-- | Why building an upstream URL from an identifier was refused.
data UrlError
    = -- | A name component (scope or base name) is unsafe to interpolate — see
      -- 'Ecluse.Server.Route.isSafeComponent'. Carries the offending component.
      UnsafeComponent Text
    | -- | The configured base URL is empty, so no URL can be formed.
      EmptyBaseUrl
    deriving stock (Eq, Show)

{- | Build an upstream URL for a package from a configured base URL and an
__already-parsed__ 'PackageName'. __Pure and total.__

This is the only sanctioned way to derive an upstream URL for a package: the
target is @{baseUrl}\/{name}@, where @name@ is the package's rendered identifier
and @baseUrl@ is __configuration__, never a client-supplied path. The client
never chooses the host or the path prefix — only which (validated) package — so
@..\/@ traversal, an encoded slash, an absolute URL, or a CRLF in the original
request cannot steer the fetch elsewhere (see the module header).

Although a 'PackageName' is normally produced by the router's already-safe parse,
its smart constructor does no validation, so this __re-checks every structural
component__ (scope and base name) with the router's own
'Ecluse.Server.Route.isSafeComponent' as defence in depth — a name carrying a
@\'\/\'@, @\'\\\\\'@, control character, or a @"."@\/@".."@ component is refused
with 'UnsafeComponent' rather than interpolated. An empty @baseUrl@ is refused
with 'EmptyBaseUrl'. A single trailing slash on @baseUrl@ is tolerated so the
join never doubles it.
-}
upstreamUrlFor :: Text -> PackageName -> Either UrlError Text
upstreamUrlFor baseUrl name =
    if T.null baseUrl
        then Left EmptyBaseUrl
        else case filter (not . isSafeComponent) (nameComponents name) of
            (bad : _) -> Left (UnsafeComponent bad)
            [] -> Right (joinUrl baseUrl (renderPackageName name))
  where
    joinUrl :: Text -> Text -> Text
    joinUrl b path = fromMaybe b (T.stripSuffix "/" b) <> "/" <> path

{- | The structural components of a package name — its scope (if any) and base
name — each of which must independently pass 'isSafeComponent'. Recovered by
splitting the rendered name on the @\'\/\'@ scope separator, so a legitimate
scoped name's own separator is not itself judged as unsafe content.
-}
nameComponents :: PackageName -> [Text]
nameComponents name =
    let rendered = renderPackageName name
     in case T.stripPrefix "@" rendered of
            -- Scoped "@scope/base": split on the separator into [scope, base].
            -- A leading '@' with no '/' (or an empty base) falls through to the
            -- single-component case below.
            Just scopeAndBase ->
                let (scope, base) = T.breakOn "/" scopeAndBase
                 in if T.null base
                        then [rendered]
                        else [scope, T.drop 1 base]
            -- Unscoped: the whole rendered name is the single component to check.
            Nothing -> [rendered]

-- ── response bounds ──────────────────────────────────────────────────────────

{- | Resource budget for a single upstream response. Every field is a hard
ceiling enforced fail-closed: exceeding one aborts with a 'LimitError' rather
than returning a truncated or partially-parsed result. These bound the
algorithmic-complexity DoS a hostile or compromised upstream can inflict by
returning a huge or pathological document.
-}
data Limits = Limits
    { maxBodyBytes :: Int
    -- ^ Largest response body, in bytes, 'boundedRead' will accumulate before
    -- aborting. Bounds memory on the metadata path (artifacts are streamed, not
    -- buffered).
    , maxVersionCount :: Int
    -- ^ Largest number of versions a parsed packument may carry
    -- ('checkVersionCount'); bounds per-version rule evaluation.
    , maxNestingDepth :: Int
    -- ^ Deepest JSON nesting a decoded document may reach ('checkNestingDepth');
    -- bounds stack\/CPU on pathologically nested input.
    }
    deriving stock (Eq, Show)

{- | Sane defaults for 'Limits'. Generous enough for real registry documents and
tight enough to fail closed on pathological input: a 16 MiB metadata body, 100k
versions, and 64 levels of JSON nesting. Override per deployment as needed.
-}
defaultLimits :: Limits
defaultLimits =
    Limits
        { maxBodyBytes = 16 * 1024 * 1024
        , maxVersionCount = 100_000
        , maxNestingDepth = 64
        }

-- | Which 'Limits' ceiling a response exceeded.
data LimitError
    = -- | The body exceeded 'maxBodyBytes'; carries the configured ceiling.
      BodyTooLarge Int
    | -- | The packument carried more than 'maxVersionCount' versions; carries the
      -- count seen and the ceiling.
      TooManyVersions Int Int
    | -- | JSON nesting exceeded 'maxNestingDepth'; carries the ceiling.
      TooDeeplyNested Int
    deriving stock (Eq, Show)

{- | Read a streamed body chunk-by-chunk, aborting as soon as the accumulated
size would exceed 'maxBodyBytes'. Polymorphic over the producing monad so the
streaming fetch can run it in 'IO' while tests drive it purely.

@readChunk@ is a chunk producer following the @http-client@ @BodyReader@ contract:
each call yields the next chunk, and an __empty__ 'ByteString' signals end of
input. 'boundedRead' pulls chunks until EOF and returns the concatenated body, or
stops at the first chunk that pushes the running total past 'maxBodyBytes' and
returns @'Left' ('BodyTooLarge' …)@ — __fail-closed__, never a truncated body. A
zero or negative 'maxBodyBytes' rejects any non-empty body. The bound is checked
__before__ a chunk is retained, so memory never exceeds the limit plus one chunk.
-}
boundedRead :: (Monad m) => Limits -> m ByteString -> m (Either LimitError ByteString)
boundedRead limits readChunk = go 0 []
  where
    cap = maxBodyBytes limits
    go !seen acc = do
        chunk <- readChunk
        if BS.null chunk
            then pure (Right (BS.concat (reverse acc)))
            else
                let seen' = seen + BS.length chunk
                 in if seen' > cap
                        then pure (Left (BodyTooLarge cap))
                        else go seen' (chunk : acc)

{- | Reject a parsed packument carrying more than 'maxVersionCount' versions,
returning it unchanged when within budget. __Pure and total.__

Applied after a document is projected to 'Ecluse.Package.PackageInfo' but before
per-version rule evaluation, so the cost of evaluating rules over every version is
bounded by configuration rather than by what an upstream returns. Counts the
'Ecluse.Package.infoVersions' map; on breach returns @'Left' ('TooManyVersions'
count cap)@, otherwise the document unchanged so it threads through a parse
pipeline.
-}
checkVersionCount :: Limits -> PackageInfo -> Either LimitError PackageInfo
checkVersionCount limits info =
    if count > cap
        then Left (TooManyVersions count cap)
        else Right info
  where
    cap = maxVersionCount limits
    count = Map.size (infoVersions info)

{- | Reject a decoded JSON document nested deeper than 'maxNestingDepth',
returning it unchanged when within budget. __Pure and total.__

Run at the JSON-decode boundary, before projecting a document to domain types, so
a pathologically nested payload is refused before any deep traversal. Depth counts
container nesting: a scalar is depth @1@, and each enclosing 'Object'\/'Array'
adds one. An empty container counts as a leaf (depth @1@), since it forces no
descent. Traversal short-circuits at the first sub-tree to breach the ceiling, so
a deeply-nested branch costs no more than the ceiling to reject.
-}
checkNestingDepth :: Limits -> Value -> Either LimitError Value
checkNestingDepth limits value =
    if within cap value
        then Right value
        else Left (TooDeeplyNested cap)
  where
    cap = maxNestingDepth limits

    -- True iff @v@ fits within @budget@ remaining levels. Decrement per nested
    -- container and fail fast at zero, so a huge subtree is not fully walked.
    within :: Int -> Value -> Bool
    within budget v =
        budget >= 1 && case v of
            Object o -> all (within (budget - 1)) (KeyMap.elems o)
            Array xs -> all (within (budget - 1)) (V.toList xs)
            String _ -> True
            Number _ -> True
            Bool _ -> True
            Null -> True
