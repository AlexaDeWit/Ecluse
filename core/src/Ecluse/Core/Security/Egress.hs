{- | The resolving-fetch SSRF guard: a connection-time recheck of every
__resolved__ outbound IP.

The pure "Ecluse.Core.Security" layer gates an outbound target by host — the allowlist
('Ecluse.Core.Security.isAllowedUpstreamHost') and the internal-range block over an IP
/literal/ ('Ecluse.Core.Security.isBlockedTarget'). What it structurally cannot see is
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
module Ecluse.Core.Security.Egress (
    -- * The guarded manager
    guardedManagerSettings,
    newGuardedTlsManager,

    -- * The trusted manager
    newTrustedTlsManager,

    -- * The connection-time refusal
    BlockedTarget (..),

    -- * The resolved-IP decision (pure)
    blockedResolvedAddrs,
) where

import Data.IP (IP, fromSockAddr)
import Network.HTTP.Client (Manager, ManagerSettings (managerRawConnection, managerTlsConnection), newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Socket (
    AddrInfo (addrAddress),
    SockAddr,
    defaultHints,
    getAddrInfo,
 )
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Security (LoweredHostSet, hostOptedIn, isBlockedIP)

-- ── the connection-time refusal ───────────────────────────────────────────────

{- | Raised when an outbound connection's destination host resolves to an internal
address the proxy must not reach.

Carries the host name that was being dialled and the blocked resolved literals, so
a refusal is diagnosable (the operator sees /which/ name resolved to /what/
internal address). It is thrown from the connection hook before the socket is used,
so it surfaces to the fetch caller exactly as a connection failure would —
'Ecluse.Core.Server.Stream.streamUpstreamWhen' treats it as a recoverable miss on the
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

Each 'SockAddr' is converted directly to an @iproute@ @IP@ and tested with the shared
internal-range block ('Ecluse.Core.Security.isBlockedIP') under the given per-host
opt-in — the same block and exemption the pure host layer applies, so a resolved
address gates against identical ranges. The result is the (possibly empty) list of
canonical literals that were refused, for the 'BlockedTarget' diagnostic: empty
means every resolved address is permitted. A non-IP address (a Unix socket) cannot
be an outbound HTTP target and is ignored.

Exposed pure so the connection-hook decision can be unit-tested over constructed
addresses without performing DNS or opening a socket.
-}
blockedResolvedAddrs :: LoweredHostSet -> [SockAddr] -> [Text]
blockedResolvedAddrs allowedInternal =
    mapMaybe (blockedLiteral . fst) . mapMaybe fromSockAddr
  where
    -- The IP's canonical literal, or 'Nothing' if it is permitted. Rendering once
    -- gives both the opt-in key and the diagnostic text, so the address is parsed
    -- (by 'fromSockAddr') and its membership tested exactly once.
    blockedLiteral :: IP -> Maybe Text
    blockedLiteral ip =
        let lit = show ip
         in if not (hostOptedIn allowedInternal lit) && isBlockedIP ip
                then Just lit
                else Nothing
