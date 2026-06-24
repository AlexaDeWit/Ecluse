{- | The resolving-fetch SSRF guard: a connection-time recheck of every
__resolved__ outbound IP.

The pure "Ecluse.Security" layer gates an outbound target by host — the allowlist
('Ecluse.Security.isAllowedUpstreamHost') and the internal-range block over an IP
/literal/ ('Ecluse.Security.isBlockedTarget'). What it structurally cannot see is
where a __DNS name__ resolves: an allowlisted hostname whose A\/AAAA record points
at @169.254.169.254@, loopback, or an RFC1918 address would pass the pure gate and
then have the proxy connect to an internal service. That is the classic
SSRF-via-DNS (and DNS-rebinding) gap.

This module closes it at the one place it can be closed — the @http-client@
__connection hook__. 'guardedManagerSettings' wraps the manager's connect function
so that, for every outbound connection, the destination host is resolved and
__every__ resolved address is tested against the same internal-range block before
the socket is used; a connection to an internal address is refused with a
'BlockedTarget' rather than opened. Because the check runs at connect time (not at
URL-build time), it sees the address actually being dialled, narrowing the
resolve-then-connect TOCTOU window a separate up-front resolution would leave wide.

The host /allowlist/ stays where it belongs — gating the URL before a request is
ever built (see "Ecluse.Server.Pipeline") — since at the connection hook only the
bare host and port are known, not whether it is a sanctioned upstream. This layer
is purely the resolved-IP backstop to that allowlist, the @security.md@ invariant 3
"defence-in-depth behind invariant 2".

The recheck is __origin-aware__: 'newGuardedTlsManager' is for the __untrusted__
origins (the public-upstream fetch and every artifact stream), while the __trusted__ private
upstream — an operator-configured target that may legitimately live on an internal
address — uses the unguarded 'newTrustedTlsManager'. Only an attacker-influenced
target needs the backstop, so only those fetches carry it (see @security.md@ →
"Network egress is a shared responsibility").
-}
module Ecluse.Security.Egress (
    -- * The guarded manager
    guardedManagerSettings,
    newGuardedTlsManager,

    -- * The trusted manager
    newTrustedTlsManager,

    -- * The connection-time refusal
    BlockedTarget (..),

    -- * The resolved-IP decision (pure)
    blockedResolvedAddrs,
    sockAddrHostText,
) where

import Data.Text qualified as T
import Network.HTTP.Client (Manager, ManagerSettings (managerRawConnection, managerTlsConnection), newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Socket (
    AddrInfo (addrAddress),
    HostAddress,
    HostAddress6,
    SockAddr (SockAddrInet, SockAddrInet6, SockAddrUnix),
    defaultHints,
    getAddrInfo,
    hostAddress6ToTuple,
    hostAddressToTuple,
 )
import Numeric (showHex)
import UnliftIO.Exception (throwIO)

import Ecluse.Security (LoweredHostSet, isBlockedTarget)

-- ── the connection-time refusal ───────────────────────────────────────────────

{- | Raised when an outbound connection's destination host resolves to an internal
address the proxy must not reach.

Carries the host name that was being dialled and the blocked resolved literals, so
a refusal is diagnosable (the operator sees /which/ name resolved to /what/
internal address). It is thrown from the connection hook before the socket is used,
so it surfaces to the fetch caller exactly as a connection failure would —
'Ecluse.Server.Stream.streamUpstreamWhen' treats it as a recoverable miss on the
private-origin fetch, and the buffered fetches as an upstream error — never a served body.
-}
data BlockedTarget = BlockedTarget
    { blockedHost :: Text
    -- ^ The host name the connection was being opened to.
    , blockedAddresses :: [Text]
    -- ^ The resolved IP literals that the internal-range block refused.
    }
    deriving stock (Eq, Show)

instance Exception BlockedTarget

-- ── the guarded manager ───────────────────────────────────────────────────────

{- | Wrap a base 'ManagerSettings' so every outbound connection is refused if its
destination host resolves to a blocked internal address.

Both the plain ('managerRawConnection') and TLS ('managerTlsConnection') connect
functions are wrapped: before the base connector opens the socket, the host is
resolved and 'blockedResolvedAddrs' tests every resolved address against the
internal-range block; any hit throws 'BlockedTarget' and no socket is opened.

@allowedInternal@ is the same per-host opt-in the pure block honours, so an
operator who deliberately points the proxy at an internal upstream by /name/ can
opt that host's resolved address in here too. A host that does not resolve, or
resolves only to permitted addresses, is connected exactly as the base settings
would — this adds a gate, never a behaviour change for legitimate targets.
-}
guardedManagerSettings :: LoweredHostSet -> ManagerSettings -> ManagerSettings
guardedManagerSettings allowedInternal base =
    base
        { managerRawConnection = vet <$> managerRawConnection base
        , managerTlsConnection = vet <$> managerTlsConnection base
        }
  where
    -- Wrap one connector: vet the resolved addresses of @host@, then delegate. The
    -- @Maybe HostAddress@ is the proxy-supplied pre-resolved address, passed
    -- through untouched (a configured proxy is the operator's deliberate hop).
    vet connect mAddr host port = do
        checkResolved allowedInternal (toText host)
        connect mAddr host port

{- | Build a TLS-capable 'Manager' guarded by the resolved-IP recheck.

The default 'tlsManagerSettings' wrapped by 'guardedManagerSettings' — the
data-plane manager the composition root installs for the __untrusted__ origins (the
public-upstream metadata fetch and every artifact stream), so the resolved-IP
recheck applies there. The trusted private upstream uses 'newTrustedTlsManager' instead.
-}
newGuardedTlsManager :: LoweredHostSet -> IO Manager
newGuardedTlsManager allowedInternal =
    newManager (guardedManagerSettings allowedInternal tlsManagerSettings)

-- ── the trusted manager ───────────────────────────────────────────────────────

{- | Build a plain TLS-capable 'Manager' with __no__ resolved-IP recheck, for the
trusted private upstream.

The private base URL is operator-configured and deliberately trusted (it may
legitimately resolve to an internal address — a registry on the private network),
so it is not subject to the internal-range recheck the public\/artifact fetches
carry. The trust boundary is per origin: only an __untrusted__ target (a public
upstream, or a public @dist.tarball@) can steer the proxy somewhere unintended, so
only those go through 'newGuardedTlsManager'.
-}
newTrustedTlsManager :: IO Manager
newTrustedTlsManager = newManager tlsManagerSettings

-- Resolve @host@ and refuse the connection if any resolved address is internal.
-- A resolution failure is left to the base connector to surface as its own
-- connection error (the name simply will not connect); only a successful
-- resolution to a blocked address is turned into a 'BlockedTarget' here.
checkResolved :: LoweredHostSet -> Text -> IO ()
checkResolved allowedInternal host = do
    resolved <- getAddrInfo (Just defaultHints) (Just (toString host)) Nothing
    case blockedResolvedAddrs allowedInternal (map addrAddress resolved) of
        [] -> pass
        blocked -> throwIO (BlockedTarget host blocked)

-- ── the resolved-IP decision (pure) ───────────────────────────────────────────

{- | The blocked IP literals among a set of resolved socket addresses.

Each 'SockAddr' is rendered to its canonical IP-literal text ('sockAddrHostText')
and tested with the pure internal-range block ('Ecluse.Security.isBlockedTarget')
under the given per-host opt-in. The result is the (possibly empty) list of literals
that were refused: empty means every resolved address is permitted. A non-IP
address (a Unix socket) cannot be an outbound HTTP target and is ignored.

Exposed pure so the connection-hook decision can be unit-tested over constructed
addresses without performing DNS or opening a socket.
-}
blockedResolvedAddrs :: LoweredHostSet -> [SockAddr] -> [Text]
blockedResolvedAddrs allowedInternal =
    filter (isBlockedTarget allowedInternal) . mapMaybe sockAddrHostText

{- | The canonical IP-literal text of a socket address, or 'Nothing' for an
address with no IP host (a Unix-domain socket).

An IPv4 address renders dotted-quad (@10.0.0.1@) and an IPv6 address renders its
eight colon-separated hex groups, uncompressed (@fe80:0:0:0:0:0:0:1@) — the
uncompressed form 'Ecluse.Security.isBlockedTarget' parses — so the rendered
literal feeds the internal-range block directly. The port, flow info, and scope id
are irrelevant to the host check and dropped.
-}
sockAddrHostText :: SockAddr -> Maybe Text
sockAddrHostText = \case
    SockAddrInet _ addr -> Just (renderV4 addr)
    SockAddrInet6 _ _ addr _ -> Just (renderV6 addr)
    SockAddrUnix _ -> Nothing

-- An IPv4 'HostAddress' as its dotted-quad literal.
renderV4 :: HostAddress -> Text
renderV4 addr =
    let (a, b, c, d) = hostAddressToTuple addr
     in T.intercalate "." (map show [a, b, c, d])

-- An IPv6 'HostAddress6' as its eight colon-separated hex groups, uncompressed.
renderV6 :: HostAddress6 -> Text
renderV6 addr =
    let (a, b, c, d, e, f, g, h) = hostAddress6ToTuple addr
     in T.intercalate ":" (map hexGroup [a, b, c, d, e, f, g, h])
  where
    hexGroup :: Word16 -> Text
    hexGroup w = toText (showHex w "")
