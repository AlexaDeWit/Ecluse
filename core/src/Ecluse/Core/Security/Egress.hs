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
so that, for every outbound connection, the destination host is resolved __once__ and
__every__ resolved address is tested against the same internal-range block before
the socket is used; if any resolved address is internal the whole answer is refused with
a 'BlockedTarget' rather than opened (so an attacker cannot smuggle an internal address
among public siblings). The connection is then dialled over the __vetted__ addresses,
handed to the connector as its pre-resolved dial targets and __failed over__ among them:
the socket opens to an address the recheck just saw rather than to a fresh, independently
re-resolved one, so time-of-check equals time-of-use and the resolve-then-connect
rebinding race is closed — yet a multi-IP upstream with frequent DNS rotation keeps its
connection-time failover, because the proxy only ever dials addresses it vetted. A host
that resolves only over IPv6 cannot be pinned through the connector's IPv4-only address
parameter, so it falls back to the connector's own resolution — the one residual rebinding
window, narrow and IPv6-only.

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

    -- * The connection-time dial (failover among vetted addresses)
    dialVetted,

    -- * The resolved-IP decision (pure)
    vettedInetAddrs,
    blockedResolvedAddrs,
) where

import Control.Exception (IOException)
import Data.IP (IP, fromSockAddr)
import Network.HTTP.Client (Manager, ManagerSettings (managerRawConnection, managerTlsConnection), newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Socket (
    AddrInfo (addrAddress, addrSocketType),
    HostAddress,
    SockAddr (SockAddrInet),
    SocketType (Stream),
    defaultHints,
    getAddrInfo,
 )
import UnliftIO.Exception (catch, throwIO)

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
resolved once and 'blockedResolvedAddrs' tests __every__ resolved address against the
internal-range block; if any address is internal the whole answer is refused with a
'BlockedTarget' and no socket is opened (so an attacker cannot smuggle an internal IP
among public siblings). When the addresses pass, the connection is dialled over the
vetted IPv4 set as the connector's pre-resolved dial targets, __failing over__ among them
('dialVetted') — the socket opens to an address the recheck just saw rather than to a
re-resolution, so a multi-IP upstream with DNS rotation keeps connection-time failover
while the rebinding race stays closed (the proxy only ever dials addresses it vetted). An
IPv6-only host, whose addresses are not pinnable through the connector's IPv4-only
parameter, falls back to the connector's own resolution.

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
    -- Wrap one connector: resolve @host@ once, refuse the connection if any resolved
    -- address is internal, and dial only the vetted addresses — failing over among them
    -- ('dialVetted') so a multi-IP upstream with DNS rotation keeps connection-time
    -- failover while never dialling an address the recheck did not see (the TOCTOU stays
    -- closed). A caller-supplied address (a configured proxy is the operator's deliberate
    -- hop) is honoured untouched.
    vet connect mAddr host port = case mAddr of
        Just _ -> connect mAddr host port
        Nothing -> do
            vetted <- checkResolved allowedInternal (toText host)
            dialVetted connect host port vetted

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

-- ── the connection-time dial (failover among vetted addresses) ────────────────

{- | Dial @host@ at @port@ over a list of vetted addresses, __failing over__ among them:
each is tried in turn as the connector's pinned 'HostAddress', and a connection failure
(an 'IOException' — refused\/timed-out\/unreachable) moves on to the next; the first to
connect wins, and if every address is exhausted the last error is rethrown. An empty list
— an IPv6-only host, whose addresses are not pinnable through the connector's IPv4-only
parameter — falls back to the connector's own resolution (@connect@ 'Nothing').

Only 'IOException's are caught, so a dial that fails over is a genuine connection failure;
an asynchronous exception (a request timeout, thread cancellation) is /not/ an
'IOException', so it propagates and aborts the whole attempt rather than being mistaken for
a dead address. The connector closes its own socket on a throw, so a failed attempt leaks
nothing. Failover never falls back to a re-resolution mid-list (only a wholly empty vetted
list does), so the proxy only ever dials an address the recheck vetted.

Exposed so the failover decision can be exercised over a recording connector without
performing DNS or opening real sockets. The result type is left polymorphic — the failover
never inspects the connection it returns — so a test can stand in any connection value.
-}
dialVetted ::
    (Maybe HostAddress -> String -> Int -> IO conn) ->
    String ->
    Int ->
    [HostAddress] ->
    IO conn
dialVetted connect host port = go
  where
    go [] = connect Nothing host port
    go [ip] = connect (Just ip) host port
    go (ip : rest) = connect (Just ip) host port `catch` \(_ :: IOException) -> go rest

-- Resolve @host@ once and refuse the connection if any resolved address is internal,
-- returning the vetted IPv4 addresses to dial, in resolution order. A resolution failure
-- is left to the base connector to surface as its own connection error (the name simply
-- will not connect); only a successful resolution containing a blocked address is turned
-- into a 'BlockedTarget' here, refusing the whole answer. An IPv6-only host yields an
-- empty list — its addresses are not pinnable through the connector's IPv4-only parameter,
-- so it falls back to the connector's own resolution (the residual rebinding window; see
-- issue #426). The @Stream@ hint keeps the resolver from returning one entry per socket
-- type, so each address appears once.
checkResolved :: LoweredHostSet -> Text -> IO [HostAddress]
checkResolved allowedInternal host = do
    resolved <- map addrAddress <$> getAddrInfo (Just streamHints) (Just (toString host)) Nothing
    either (throwIO . BlockedTarget host) pure (vettedInetAddrs allowedInternal resolved)
  where
    streamHints = defaultHints{addrSocketType = Stream}

-- ── the resolved-IP decision (pure) ───────────────────────────────────────────

{- | The connection decision over a resolved address set: 'Left' the blocked internal
literals if __any__ resolved address (IPv4 or IPv6) is internal — a mixed public+internal
answer is refused __wholesale__, so an attacker cannot smuggle an internal address among
public siblings and have the proxy dial the siblings — otherwise 'Right' the vetted IPv4
addresses to dial, in resolution order. IPv6 addresses are vetted by the block but not
returned: the connector's pinned address parameter is IPv4-only, so an IPv6-only host
yields 'Right' @[]@ and is left to the connector's own resolution.

Exposed pure so the wholesale-block and address-extraction decision can be unit-tested over
constructed addresses without performing DNS.
-}
vettedInetAddrs :: LoweredHostSet -> [SockAddr] -> Either [Text] [HostAddress]
vettedInetAddrs allowedInternal addrs =
    case blockedResolvedAddrs allowedInternal addrs of
        [] -> Right (mapMaybe inetAddr addrs)
        blocked -> Left blocked
  where
    inetAddr (SockAddrInet _ ha) = Just ha
    inetAddr _ = Nothing

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
