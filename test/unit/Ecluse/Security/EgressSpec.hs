module Ecluse.Security.EgressSpec (spec) where

import Data.Set qualified as Set
import Network.HTTP.Client (
    Response,
    httpLbs,
    parseRequest,
    responseStatus,
 )
import Network.HTTP.Types (status200)
import Network.Socket (
    SockAddr (SockAddrInet, SockAddrInet6, SockAddrUnix),
    tupleToHostAddress,
    tupleToHostAddress6,
 )
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
import UnliftIO.Exception (try)

import Ecluse.Security (lowerCaseHosts)
import Ecluse.Security.Egress (
    BlockedTarget (..),
    blockedResolvedAddrs,
    newGuardedTlsManager,
 )

-- ── fixtures ─────────────────────────────────────────────────────────────────

-- | An IPv4 socket address from its four octets (the port is irrelevant here).
v4 :: (Word8, Word8, Word8, Word8) -> SockAddr
v4 octets = SockAddrInet 443 (tupleToHostAddress octets)

{- | An IPv6 socket address from its eight 16-bit groups (port/flow/scope are
irrelevant to the host check).
-}
v6 :: (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16) -> SockAddr
v6 groups = SockAddrInet6 443 0 (tupleToHostAddress6 groups) 0

-- ── spec ─────────────────────────────────────────────────────────────────────

spec :: Spec
spec = do
    blockedResolvedAddrsSpec
    guardedManagerSpec
    showInstancesSpec

-- ── the resolved-IP decision ──────────────────────────────────────────────────

{- | The decision the connection hook makes over a host's resolved addresses. The
deny paths are exercised hardest: a name that resolves to /any/ internal address
must be refused, since that is exactly the DNS-rebinding / resolve-to-internal SSRF
the pure host layer cannot see.
-}
blockedResolvedAddrsSpec :: Spec
blockedResolvedAddrsSpec = describe "blockedResolvedAddrs" $ do
    let noOptIn = lowerCaseHosts Set.empty

    describe "permits resolutions that are entirely public" $ do
        it "permits a single public IPv4 address (no blocked literals)" $
            blockedResolvedAddrs noOptIn [v4 (93, 184, 216, 34)] `shouldBe` []
        it "permits a public IPv6 address" $
            blockedResolvedAddrs noOptIn [v6 (0x2606, 0x2800, 0x220, 1, 0x248, 0x1893, 0x25c8, 0x1946)]
                `shouldBe` []
        it "permits an empty resolution set" $
            blockedResolvedAddrs noOptIn [] `shouldBe` []
        it "ignores a Unix-domain address (not an outbound HTTP target)" $
            blockedResolvedAddrs noOptIn [SockAddrUnix "/tmp/sock"] `shouldBe` []

    describe "refuses any resolution into an internal range" $ do
        it "refuses the cloud instance-metadata address" $
            blockedResolvedAddrs noOptIn [v4 (169, 254, 169, 254)] `shouldBe` ["169.254.169.254"]
        it "refuses loopback" $
            blockedResolvedAddrs noOptIn [v4 (127, 0, 0, 1)] `shouldBe` ["127.0.0.1"]
        it "refuses RFC1918 10/8" $
            blockedResolvedAddrs noOptIn [v4 (10, 1, 2, 3)] `shouldBe` ["10.1.2.3"]
        it "refuses the unspecified / this-host address 0.0.0.0" $
            blockedResolvedAddrs noOptIn [v4 (0, 0, 0, 0)] `shouldBe` ["0.0.0.0"]
        it "refuses IPv6 loopback (reported as the canonical compressed literal)" $
            -- The blocked literal is rendered in its canonical, compressed form for
            -- the diagnostic — the form an operator reads and writes.
            blockedResolvedAddrs noOptIn [v6 (0, 0, 0, 0, 0, 0, 0, 1)] `shouldBe` ["::1"]
        it "refuses IPv6 link-local" $
            blockedResolvedAddrs noOptIn [v6 (0xfe80, 0, 0, 0, 0, 0, 0, 1)] `shouldBe` ["fe80::1"]
        it "refuses an IPv6 unique-local address (fc00::/7)" $
            blockedResolvedAddrs noOptIn [v6 (0xfd00, 0, 0, 0, 0, 0, 0, 1)] `shouldBe` ["fd00::1"]
        it "refuses the AWS IMDSv6 endpoint fd00:ec2::254" $
            blockedResolvedAddrs noOptIn [v6 (0xfd00, 0xec2, 0, 0, 0, 0, 0, 0x254)]
                `shouldBe` ["fd00:ec2::254"]

    describe "a mixed resolution is refused if any address is internal" $ do
        it "reports the internal address even when a public one is also present" $
            -- The killer case: a name resolving to both a public and an internal
            -- address must not be connected on the strength of the public one.
            blockedResolvedAddrs noOptIn [v4 (93, 184, 216, 34), v4 (169, 254, 169, 254)]
                `shouldBe` ["169.254.169.254"]
        it "reports every internal address among several" $
            blockedResolvedAddrs noOptIn [v4 (10, 0, 0, 1), v4 (93, 184, 216, 34), v4 (127, 0, 0, 1)]
                `shouldBe` ["10.0.0.1", "127.0.0.1"]

    describe "the explicit per-IP opt-in is honoured" $ do
        it "permits a resolved internal address that is opted in" $
            blockedResolvedAddrs (lowerCaseHosts (Set.singleton "10.0.0.5")) [v4 (10, 0, 0, 5)]
                `shouldBe` []
        it "still refuses a different internal address than the opted-in one" $
            blockedResolvedAddrs (lowerCaseHosts (Set.singleton "10.0.0.5")) [v4 (10, 0, 0, 6)]
                `shouldBe` ["10.0.0.6"]
        it "permits a resolved IPv6 address opted in by its canonical compressed form" $
            -- The opt-in key is the address's canonical compressed literal, so an
            -- opt-in written that way suppresses the block for the resolved address.
            blockedResolvedAddrs (lowerCaseHosts (Set.singleton "fe80::1")) [v6 (0xfe80, 0, 0, 0, 0, 0, 0, 1)]
                `shouldBe` []
        it "still refuses an IPv6 address opted in only by its uncompressed spelling" $
            -- The uncompressed spelling is not the key, so it does not opt in — and
            -- the refused literal is reported in the canonical compressed form.
            blockedResolvedAddrs (lowerCaseHosts (Set.singleton "fe80:0:0:0:0:0:0:1")) [v6 (0xfe80, 0, 0, 0, 0, 0, 0, 1)]
                `shouldBe` ["fe80::1"]

-- ── the live connection hook ──────────────────────────────────────────────────

{- | The guarded manager applied to a real connection. These exercise the
connection hook end-to-end (resolution + the throw\/delegate decision) without any
external network: a literal @127.0.0.1@ resolves to itself, so the refusal fires
deterministically, and the opt-in path hits an in-process Warp server on loopback.
-}
guardedManagerSpec :: Spec
guardedManagerSpec = describe "the guarded manager (live connection hook)" $ do
    it "refuses a connection whose host resolves to an internal address" $ do
        -- No server is needed: the guard throws BlockedTarget at the connection
        -- hook, before any socket to loopback is opened.
        manager <- newGuardedTlsManager noOptIn
        request <- parseRequest "http://127.0.0.1:9/"
        result <- try (httpLbs request manager) :: IO (Either SomeException (Response LByteString))
        case result of
            Left e -> case fromException e of
                Just (BlockedTarget host addrs) -> do
                    host `shouldBe` "127.0.0.1"
                    -- getAddrInfo may return the literal once per socket type, so
                    -- assert the content rather than an exact arity.
                    addrs `shouldSatisfy` (not . null)
                    addrs `shouldSatisfy` all (== "127.0.0.1")
                Nothing -> expectationFailure ("expected BlockedTarget, got " <> show e)
            Right _ -> expectationFailure "expected the loopback connection to be refused"

    it "refuses an HTTPS connection to an internal address too (the TLS connector)" $ do
        -- The same guard wraps the TLS connector: an https:// target resolving to
        -- loopback is refused at the hook, before any TLS handshake is attempted.
        manager <- newGuardedTlsManager noOptIn
        request <- parseRequest "https://127.0.0.1:9/"
        result <- try (httpLbs request manager) :: IO (Either SomeException (Response LByteString))
        case result of
            Left e -> case fromException e of
                Just (BlockedTarget host _) -> host `shouldBe` "127.0.0.1"
                Nothing -> expectationFailure ("expected BlockedTarget, got " <> show e)
            Right _ -> expectationFailure "expected the loopback TLS connection to be refused"

    it "permits a connection to an internal address that is explicitly opted in" $
        -- With 127.0.0.1 opted in to the internal block, the guard delegates and a
        -- local server is reached: the permit branch of the hook.
        testWithApplication (pure helloApp) $ \port -> do
            manager <- newGuardedTlsManager (lowerCaseHosts (Set.singleton "127.0.0.1"))
            request <- parseRequest ("http://127.0.0.1:" <> show port <> "/")
            response <- httpLbs request manager
            responseStatus response `shouldBe` status200
  where
    noOptIn = lowerCaseHosts Set.empty

    -- A trivial WAI app answering 200 on loopback, for the opt-in permit path.
    helloApp _request respond = respond (responseLBS status200 [] "ok")

-- ── Show instance ─────────────────────────────────────────────────────────────

showInstancesSpec :: Spec
showInstancesSpec = describe "BlockedTarget" $ do
    let target = BlockedTarget "internal.example.com" ["169.254.169.254"]
    it "renders a diagnosable refusal" $
        show target
            `shouldBe` ( "BlockedTarget {blockedHost = \"internal.example.com\", blockedAddresses = [\"169.254.169.254\"]}" ::
                            Text
                       )
    it "exposes the dialled host and the blocked literals through its accessors" $ do
        blockedHost target `shouldBe` "internal.example.com"
        blockedAddresses target `shouldBe` ["169.254.169.254"]
    it "compares by value (Eq)" $ do
        target `shouldBe` BlockedTarget "internal.example.com" ["169.254.169.254"]
        target `shouldNotBe` BlockedTarget "internal.example.com" ["10.0.0.1"]
