module Ecluse.SecurityResolverOracleSpec (spec) where

import Control.Exception (IOException, try)
import Data.IP (fromSockAddr)
import Data.Set qualified as Set
import Network.Socket (
    AddrInfo (addrAddress, addrFamily, addrFlags),
    AddrInfoFlag (AI_NUMERICHOST),
    Family (AF_INET),
    SockAddr,
    defaultHints,
    getAddrInfo,
 )
import Test.Hspec

import Ecluse.Core.Security (LoweredHostSet, isBlockedIP, isBlockedTarget, lowerCaseHosts)
import Ecluse.Core.Security.Egress (blockedResolvedAddrs)

{- | Smoke tier: validate the SSRF literal recogniser's IPv4 octet coercion against
the /real/ libc resolver. The hand-rolled recogniser in "Ecluse.Core.Security"
('isBlockedTarget') reads a leading-zero octet as octal and a @0x@ octet as hex,
matching what @inet_aton@ — and therefore 'Network.Socket.getAddrInfo' — coerces a
spelling to; the resolved address is the __ground-truth oracle__ for that coercion.

Two checks:

  1. /Four-part agreement./ For a corpus of dotted-quad spellings (plain decimal,
     leading-zero octal, @0x@ hex, and malformed/overflowing octets), resolve each
     numerically via 'getAddrInfo' and assert the literal block's verdict
     ('isBlockedTarget') equals whether that resolved address is internal
     ('isBlockedIP'). A spelling the resolver rejects as non-numeric (an
     invalid-octal @08@, an overflowing @0400@\/@256@) must likewise be a non-literal
     here, so neither blocks it. This is the check that would have /failed/ on the
     pre-fix decimal misreading (it blocked @0012.0.0.1@'s sibling @12.0.0.1@ shape
     and under-blocked the octal @10.0.0.1@).

  2. /Residual boundary./ The recogniser deliberately models only the four-part
     form. The __short__ @inet_aton@ forms (a bare 32-bit number, a @127.1@) are left
     to the connection-time resolved-IP recheck. For those, assert the literal layer
     does /not/ block (it is not a literal here) but the 'Ecluse.Core.Security.Egress'
     backstop ('blockedResolvedAddrs') /does/ catch the resolved internal address — so
     the residual is covered, just one layer down. Extending the recogniser to the
     short forms would flip this; update it here if so.

Numeric resolution (@AI_NUMERICHOST@) needs no network, so this is reliable rather
than flaky, but it lives in the non-gating smoke tier because it depends on the
host platform's resolver: it pends rather than fails if 'getAddrInfo' cannot even
resolve a plain literal.
-}
spec :: Spec
spec = describe "IPv4 literal classification vs the real resolver (getAddrInfo)" $ do
    available <- runIO (isJust <$> resolveNumeric "127.0.0.1")
    if not available
        then it "resolver oracle" $ pendingWith "getAddrInfo numeric resolution unavailable on this host"
        else do
            fourPartAgreementSpec
            residualBoundarySpec

-- | Every four-part spelling classifies exactly as its resolved address does.
fourPartAgreementSpec :: Spec
fourPartAgreementSpec =
    describe "the literal block agrees with the resolved address on every four-part spelling" $
        for_ fourPartCorpus $ \host ->
            it (toString (host <> " classifies as the resolver resolves it")) $ do
                resolved <- resolveNumeric host
                isBlockedTarget noOptIn host `shouldBe` resolvedInternal resolved

{- | The short @inet_aton@ forms the recogniser does not model: not blocked at the
literal layer, but caught by the connection-time resolved-IP recheck.
-}
residualBoundarySpec :: Spec
residualBoundarySpec =
    describe "short inet_aton forms are the connect-time recheck's residual, not the literal layer's" $
        for_ residualCorpus $ \host ->
            it (toString (host <> " is missed by the literal layer but caught by the resolved-IP recheck")) $ do
                resolved <- resolveNumeric host
                case resolved of
                    Nothing -> pendingWith (toString (host <> ": resolver did not resolve it"))
                    Just sa -> do
                        -- The four-part recogniser does not model the short form…
                        isBlockedTarget noOptIn host `shouldBe` False
                        -- …yet its resolved address is internal, and the Egress backstop catches it.
                        blockedResolvedAddrs noOptIn [sa] `shouldSatisfy` (not . null)

-- ── corpora ──────────────────────────────────────────────────────────────────

{- | Dotted-quad spellings whose literal-layer verdict must match the resolver's.
The leading-zero and @0x@ entries are the load-bearing ones; the malformed entries
(invalid octal, overflow) the resolver rejects, so the recogniser must not treat
them as literals either.
-}
fourPartCorpus :: [Text]
fourPartCorpus =
    [ "127.0.0.1" -- plain decimal, loopback (internal)
    , "10.0.0.1" -- plain decimal, RFC1918 (internal)
    , "8.8.8.8" -- plain decimal, public
    , "93.184.216.34" -- plain decimal, public
    , "0012.0.0.1" -- octal 10.0.0.1 (internal) — the reported under-block
    , "0177.0.0.1" -- octal 127.0.0.1 (internal)
    , "010.0.0.1" -- octal 8.0.0.1 (public) — the over-block a decimal reading caused
    , "0127.0.0.1" -- octal 87.0.0.1 (public)
    , "0x7f.0.0.1" -- hex 127.0.0.1 (internal)
    , "0xff.0.0.1" -- hex 255.0.0.1 (public)
    , "08.0.0.1" -- 8 is not an octal digit: not a literal; the resolver rejects it too
    , "0400.0.0.1" -- octal 0400 = 256 overflows an octet: not a literal; resolver rejects it
    , "256.0.0.1" -- decimal 256 overflows an octet: not a literal; resolver rejects it
    ]

-- | Short @inet_aton@ forms of @127.0.0.1@ — fewer than four parts, not modelled here.
residualCorpus :: [Text]
residualCorpus =
    [ "2130706433" -- the 32-bit form of 127.0.0.1
    , "0x7f000001" -- the hex 32-bit form of 127.0.0.1
    , "127.1" -- the two-part short form of 127.0.0.1
    ]

-- ── resolution ───────────────────────────────────────────────────────────────

{- | Resolve @host@ as a numeric IPv4 literal through the real 'getAddrInfo' (the
@inet_aton@ coercion path), returning its 'SockAddr' or 'Nothing' if the resolver
rejects it. @AI_NUMERICHOST@ forbids a DNS lookup, so a non-numeric spelling fails
locally rather than touching the network — the same coercion the connection-time
recheck sees, isolated from name resolution.
-}
resolveNumeric :: Text -> IO (Maybe SockAddr)
resolveNumeric host = do
    let hints = defaultHints{addrFlags = [AI_NUMERICHOST], addrFamily = AF_INET}
    result <- try (getAddrInfo (Just hints) (Just (toString host)) Nothing)
    pure $ case result of
        Left (_ :: IOException) -> Nothing
        Right [] -> Nothing
        Right (ai : _) -> Just (addrAddress ai)

-- | Whether a resolved address (if any) is one of the internal ranges 'isBlockedIP' blocks.
resolvedInternal :: Maybe SockAddr -> Bool
resolvedInternal = maybe False (maybe False (isBlockedIP . fst) . fromSockAddr)

-- | No deliberately-internal host is opted in: the strictest configuration.
noOptIn :: LoweredHostSet
noOptIn = lowerCaseHosts Set.empty
