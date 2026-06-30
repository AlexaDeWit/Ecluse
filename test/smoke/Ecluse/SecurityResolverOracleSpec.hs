module Ecluse.SecurityResolverOracleSpec (spec) where

import Control.Exception (IOException, try)
import Data.IP (IP, fromSockAddr)
import Data.Set qualified as Set
import Data.Text qualified as T
import Hedgehog (Gen)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Network.Socket (
    AddrInfo (addrAddress, addrFamily, addrFlags),
    AddrInfoFlag (AI_NUMERICHOST),
    Family (AF_INET),
    SockAddr,
    defaultHints,
    getAddrInfo,
 )
import Numeric (showHex, showOct)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Security (LoweredHostSet, isBlockedIP, isBlockedTarget, lowerCaseHosts, parseIpLiteral)

{- | Smoke tier: a /generative, live/ differential between the SSRF literal
recogniser's IPv4 octet coercion and the __real__ libc resolver. The hand-rolled
recogniser in "Ecluse.Core.Security" reads a leading-zero octet as octal and a
@0x@ octet as hex -- exactly what @inet_aton@, and therefore
'Network.Socket.getAddrInfo', coerces a spelling to -- so the resolved address is
the ground-truth oracle and no verdict is hard-coded here.

The property generates four-part dotted-quad spellings whose octets are rendered in
a random mix of bases (decimal, leading-zero octal, @0x@ hex), biased toward the
internal ranges so blocked cases actually occur, with an occasional malformed octet
(invalid-octal @08@, overflowing @0400@\/@256@\/@0x100@). For each spelling it
resolves numerically (@AI_NUMERICHOST@ -- local, no DNS) and asserts:

  * the resolver accepts it as address @a@ ⟹ @'isBlockedTarget' noOptIn spelling ==
    'isBlockedIP' a@ (our literal-layer block decision agrees with how the resolver
    classifies the address it would actually dial); and
  * the resolver rejects it ⟹ @'parseIpLiteral' spelling == 'Nothing'@ (we never
    claim a literal the resolver will not accept).

A 'H.cover' guard requires that blocked, not-blocked, and resolver-rejected results
each appear across a run, so a degenerate generator cannot pass vacuously.

The short @inet_aton@ forms (a bare 32-bit number, a @127.1@) are deliberately /not/
modelled by the four-part recogniser; they are treated as names the host allowlist
constrains, so this oracle covers only the four-part dotted-quad recogniser.

Numeric resolution needs no network, so this is reliable rather than flaky, but it
lives in the non-gating smoke tier because it depends on the host platform's
resolver: it pends rather than fails if 'getAddrInfo' cannot even resolve a plain
literal.
-}
spec :: Spec
spec = describe "IPv4 literal classification vs the real resolver (getAddrInfo)" $ do
    available <- runIO (isJust <$> resolveNumeric "127.0.0.1")
    if not available
        then it "resolver oracle" $ pendingWith "getAddrInfo numeric resolution unavailable on this host"
        else generativeOracleSpec

{- | The resolver is the oracle: over generated four-part spellings, our literal
block must agree with how the resolver classifies the address it resolves to, and
we must claim no literal the resolver rejects.
-}
generativeOracleSpec :: Spec
generativeOracleSpec =
    describe "the literal block agrees with the real resolver over generated spellings" $
        modifyMaxSuccess (const 200) $
            it "isBlockedTarget matches isBlockedIP of the resolved address (no hard-coded verdicts)" $
                hedgehog $ do
                    spelling <- H.forAll genSpelling
                    resolved <- H.evalIO (resolveNumeric spelling)
                    let mip = resolved >>= ipOf
                    H.annotateShow mip
                    -- Non-vacuity: every meaningful class must occur across the run.
                    H.cover 2 "blocked (resolves to an internal address)" (maybe False isBlockedIP mip)
                    H.cover 2 "not blocked (resolves to a public address)" (maybe False (not . isBlockedIP) mip)
                    H.cover 2 "resolver-rejected (malformed spelling)" (isNothing mip)
                    case mip of
                        -- The resolver accepted it: our verdict must match the resolved address.
                        Just a -> isBlockedTarget noOptIn spelling H.=== isBlockedIP a
                        -- The resolver rejected it: we must not claim a literal either.
                        Nothing -> H.assert (isNothing (parseIpLiteral spelling))

{- | A four-part dotted-quad spelling whose octets are rendered in a random mix of
bases (decimal, leading-zero octal, @0x@ hex), so it spans exactly the @inet_aton@
octet coercions the recogniser models. One spelling in ten has one octet replaced
by a malformed token (an invalid-octal @08@, an overflowing @0400@\/@256@\/@0x100@),
which both the recogniser and the resolver reject.
-}
genSpelling :: Gen Text
genSpelling = do
    rendered <- traverse renderOctet =<< genOctets
    Gen.frequency
        [ (9, pure (T.intercalate "." rendered))
        , (1, corruptOne rendered)
        ]
  where
    corruptOne parts = do
        i <- Gen.int (Range.constant 0 3)
        bad <- genMalformedToken
        pure (T.intercalate "." [if j == i then bad else p | (j, p) <- zip [0 :: Int ..] parts])

{- | Four octet /values/ for an address, biased toward the internal ranges (uniform
random bytes almost never land in RFC1918), so the blocked arm is well exercised
alongside public and edge addresses.
-}
genOctets :: Gen [Word8]
genOctets =
    Gen.frequency
        [ (5, internalAddr)
        , (3, replicateM 4 anyByte) -- the whole space is ~98% public
        , (2, Gen.element edgeAddrs)
        ]
  where
    anyByte = Gen.word8 Range.constantBounded
    internalAddr =
        Gen.choice
            [ (\b c d -> [127, b, c, d]) <$> anyByte <*> anyByte <*> anyByte -- loopback 127/8
            , (\b c d -> [10, b, c, d]) <$> anyByte <*> anyByte <*> anyByte -- RFC1918 10/8
            , (\b c d -> [172, b, c, d]) <$> Gen.word8 (Range.constant 16 31) <*> anyByte <*> anyByte -- 172.16/12
            , (\c d -> [192, 168, c, d]) <$> anyByte <*> anyByte -- 192.168/16
            , (\c d -> [169, 254, c, d]) <$> anyByte <*> anyByte -- link-local 169.254/16
            , (\b c d -> [100, b, c, d]) <$> Gen.word8 (Range.constant 64 127) <*> anyByte <*> anyByte -- CGNAT 100.64/10
            , (\b c d -> [0, b, c, d]) <$> anyByte <*> anyByte <*> anyByte -- this-host 0/8
            ]
    edgeAddrs =
        [ [0, 0, 0, 0] -- unspecified (blocked)
        , [255, 255, 255, 255] -- broadcast (public)
        , [100, 64, 0, 0] -- CGNAT low edge (blocked)
        , [100, 127, 255, 255] -- CGNAT high edge (blocked)
        , [169, 254, 169, 254] -- the cloud metadata address (blocked)
        , [172, 16, 0, 0] -- 172.16/12 low edge (blocked)
        , [172, 32, 0, 1] -- just above 172.16/12 (public)
        , [8, 8, 8, 8] -- public
        , [1, 1, 1, 1] -- public
        , [203, 0, 113, 1] -- TEST-NET-3 (public)
        ]

{- | Render one octet value in a randomly chosen @inet_aton@ base: decimal, a
leading-zero octal (@0NNN@), or a @0x@ hex -- each of which a resolver coerces back
to the same value.
-}
renderOctet :: Word8 -> Gen Text
renderOctet b =
    Gen.element
        [ show b -- decimal
        , "0" <> toText (showOct b "") -- leading-zero octal
        , "0x" <> toText (showHex b "") -- 0x hex
        ]

{- | A malformed octet token both the recogniser and the resolver reject: an
invalid-octal digit, or a value that overflows a single octet in some base.
-}
genMalformedToken :: Gen Text
genMalformedToken =
    Gen.element
        [ "08" -- 8 is not an octal digit
        , "09" -- 9 is not an octal digit
        , "0400" -- octal 256 overflows an octet
        , "0777777" -- octal, far over an octet
        , "256" -- decimal overflow
        , "300" -- decimal overflow
        , "0x100" -- hex 256 overflows an octet
        , "0x1ff" -- hex 511 overflows an octet
        ]

{- | Resolve @host@ as a numeric IPv4 literal through the real 'getAddrInfo' (the
@inet_aton@ coercion path), returning its 'SockAddr' or 'Nothing' if the resolver
rejects it. @AI_NUMERICHOST@ forbids a DNS lookup, so a non-numeric spelling fails
locally rather than touching the network -- the same coercion the connection-time
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

-- | The @iproute@ 'IP' of a resolved socket address (always 'Just' for an AF_INET result).
ipOf :: SockAddr -> Maybe IP
ipOf = fmap fst . fromSockAddr

-- | No deliberately-internal host is opted in: the strictest configuration.
noOptIn :: LoweredHostSet
noOptIn = lowerCaseHosts Set.empty
