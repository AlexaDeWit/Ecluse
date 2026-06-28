module Ecluse.Security.EgressOriginSpec (spec) where

import Data.Aeson (Value, encode, object, (.=))
import Network.HTTP.Client (Manager, ManagerSettings (managerRawConnection), defaultManagerSettings, makeConnection)
import Network.HTTP.Types (status200)
import Network.Socket (HostAddress, SockAddr (SockAddrInet, SockAddrInet6), tupleToHostAddress, tupleToHostAddress6)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (Port, testWithApplication)
import System.IO.Error (doesNotExistErrorType, mkIOError)
import Test.Hspec
import UnliftIO.Exception (throwIO, try, tryAny)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Registry (RegistryResponse (responseBody))
import Ecluse.Core.Registry.Npm (
    MetadataForm (Abbreviated),
    NpmClientConfig (NpmClientConfig, npmBaseUrl, npmLimits, npmManager, npmToken),
    fetchMetadataForm,
    noValidators,
 )
import Ecluse.Core.Security (defaultLimits, lowerCaseHosts)
import Ecluse.Core.Security.Egress (
    BlockedTarget (BlockedTarget),
    dialVetted,
    guardedManagerSettings,
    newGuardedTlsManager,
    newTrustedTlsManager,
    vettedInetAddrs,
 )

{- | Per-origin trust split, exercised through the real npm fetch path against an
in-process upstream on loopback.

@127.0.0.1@ is itself an internal address, so it doubles as the "internal target"
the split must treat differently per origin — no Docker, no external network. The
public\/artifact fetches run over the __guarded__ manager (resolved-IP recheck) and
the private origin over the __trusted__ manager (no recheck), so:

* the public-origin fetch __refuses__ a fetch to the loopback upstream ('BlockedTarget'),
  proving an untrusted target on an internal address is blocked at connect time;
* the private-origin fetch __reaches__ the same loopback upstream, proving an
  operator-trusted private registry on an internal address still works.

This is the integration proof that the manager split behaves correctly per origin.
-}
spec :: Spec
spec = do
    describe "egress trust split (per-origin, against a loopback upstream)" $ do
        it "the guarded public-origin fetch refuses a target that resolves to an internal address" $
            -- An empty internal opt-in, as the composition root installs: loopback is
            -- refused. Drives the same fetch primitive the public-origin fetch uses.
            withUpstream $ \port -> do
                manager <- newGuardedTlsManager (lowerCaseHosts mempty)
                result <- try (fetchMetadata manager port) :: IO (Either SomeException RegistryResponse)
                case result of
                    Left e -> case fromException e of
                        Just (BlockedTarget host _) -> host `shouldBe` "127.0.0.1"
                        Nothing -> expectationFailure ("expected BlockedTarget, got " <> show e)
                    Right _ -> expectationFailure "expected the guarded public-origin fetch to refuse loopback"

        it "the trusted private-origin fetch reaches a target on an internal address" $
            -- The private base URL is operator-configured and trusted, so its manager
            -- carries no recheck: the same loopback upstream the public-origin fetch refused is
            -- reached and its body returned.
            withUpstream $ \port -> do
                manager <- newTrustedTlsManager
                response <- fetchMetadata manager port
                responseBody response `shouldBe` toStrict (encode packument)

        it "pins the vetted address: the guarded connector dials the resolved IP rather than re-resolving" $ do
            -- A public IP literal (TEST-NET-3, RFC 5737) the internal-range block permits:
            -- the guard resolves it (a literal, so no DNS), finds it not blocked, and pins it.
            -- The recording base connector captures the @Maybe HostAddress@ the guard hands
            -- it; a 'Just' of the vetted IPv4 proves the dial is pinned to the address that
            -- was just checked (time-of-check = time-of-use), closing the rebinding race —
            -- rather than left as 'Nothing' for the connector to re-resolve independently.
            captured <- newIORef (Nothing :: Maybe (Maybe HostAddress))
            let wrapped = guardedManagerSettings (lowerCaseHosts mempty) (recordingSettings captured)
            connector <- managerRawConnection wrapped
            _ <- tryAny (connector Nothing "203.0.113.10" 443)
            seen <- readIORef captured
            seen `shouldBe` Just (Just (tupleToHostAddress (203, 0, 113, 10)))

    describe "dialVetted (connection-time failover among vetted addresses)" $ do
        it "fails over to the next vetted address when an earlier one's dial fails" $ do
            -- Two public (vetted) addresses: the first refuses the dial (an 'IOException'),
            -- the second connects. dialVetted must try the first, fail over, and connect via
            -- the second — never dialling anything outside the vetted list. The recording
            -- connector proves both were tried, in order, and only those.
            seen <- newIORef ([] :: [Maybe HostAddress])
            let deadIP = tupleToHostAddress (203, 0, 113, 10)
                goodIP = tupleToHostAddress (203, 0, 113, 11)
                connector mAddr _host _port = do
                    modifyIORef' seen (<> [mAddr])
                    if mAddr == Just deadIP
                        then throwIO (mkIOError doesNotExistErrorType "simulated dial failure" Nothing Nothing) -- an IOException: dialVetted fails over
                        else makeConnection (pure "") (const (pure ())) (pure ())
            _ <- dialVetted connector "rotating.example" 443 [deadIP, goodIP]
            order <- readIORef seen
            order `shouldBe` [Just deadIP, Just goodIP]

        it "propagates a non-IOException dial failure instead of failing over (async-safety)" $ do
            -- dialVetted catches only 'IOException's. A non-'IOException' (here standing in for
            -- an async exception — a request timeout or cancellation) must abort the whole
            -- attempt: it propagates, and no failover to the second address is attempted.
            seen <- newIORef ([] :: [Maybe HostAddress])
            let ipA = tupleToHostAddress (203, 0, 113, 10)
                ipB = tupleToHostAddress (203, 0, 113, 11)
                connector :: Maybe HostAddress -> String -> Int -> IO ()
                connector mAddr _host _port = do
                    modifyIORef' seen (<> [mAddr])
                    throwIO StopConnect -- not an IOException
            result <- try (dialVetted connector "h" 443 [ipA, ipB])
            order <- readIORef seen
            order `shouldBe` [Just ipA]
            case result of
                Left StopConnect -> pass
                Right _ -> expectationFailure "expected the non-IOException to propagate, not fail over"

        it "falls back to the connector's own resolution for an empty (IPv6-only) vetted list" $ do
            -- An IPv6-only host yields no pinnable IPv4 address; dialVetted then dials with
            -- 'Nothing', leaving the connector to resolve (the residual tracked by #426).
            seen <- newIORef (Nothing :: Maybe (Maybe HostAddress))
            let connector :: Maybe HostAddress -> String -> Int -> IO ()
                connector mAddr _host _port = do
                    writeIORef seen (Just mAddr)
                    throwIO StopConnect
            _ <- tryAny (dialVetted connector "ipv6.example" 443 [])
            recorded <- readIORef seen
            recorded `shouldBe` Just Nothing

    describe "vettedInetAddrs (wholesale internal-block + IPv4 extraction)" $ do
        it "returns the vetted IPv4 addresses in resolution order when all pass" $
            vettedInetAddrs (lowerCaseHosts mempty) [inet (203, 0, 113, 10), inet (203, 0, 113, 11)]
                `shouldBe` Right [tupleToHostAddress (203, 0, 113, 10), tupleToHostAddress (203, 0, 113, 11)]

        it "refuses the whole answer if any address is internal (no public sibling leaks through)" $
            -- A mixed public+internal answer (the smuggling case) is blocked wholesale, so the
            -- public sibling is never returned to be dialled.
            vettedInetAddrs (lowerCaseHosts mempty) [inet (203, 0, 113, 10), inet (169, 254, 169, 254)]
                `shouldBe` Left ["169.254.169.254"]

        it "refuses the whole answer when the internal address is IPv6 (the IPv6 block branch)" $
            -- The same wholesale block over an internal IPv6 sibling (loopback @::1@): the
            -- public IPv4 must NOT leak through. Without the IPv6 branch of the internal-range
            -- block this would return @Right [203.0.113.10]@, so this asserts that branch.
            vettedInetAddrs (lowerCaseHosts mempty) [inet (203, 0, 113, 10), inet6 (0, 0, 0, 0, 0, 0, 0, 1)]
                `shouldBe` Left ["::1"]

        it "yields an empty dial list for an IPv6-only (unpinnable) answer" $
            vettedInetAddrs (lowerCaseHosts mempty) [inet6 (0x2001, 0x0db8, 0, 0, 0, 0, 0, 1)]
                `shouldBe` Right []

-- A public IPv4 socket address (port elided) for the pure decision tests.
inet :: (Word8, Word8, Word8, Word8) -> SockAddr
inet quad = SockAddrInet 0 (tupleToHostAddress quad)

-- An IPv6 socket address (port/flow/scope elided) for the pure decision tests.
inet6 :: (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16) -> SockAddr
inet6 groups = SockAddrInet6 0 0 (tupleToHostAddress6 groups) 0

-- A base 'ManagerSettings' whose raw connector records the @Maybe HostAddress@ it is
-- handed and then aborts (it never opens a socket): the probe for what the guard pins.
recordingSettings :: IORef (Maybe (Maybe HostAddress)) -> ManagerSettings
recordingSettings ref =
    defaultManagerSettings
        { managerRawConnection = pure $ \mAddr _host _port -> do
            writeIORef ref (Just mAddr)
            throwIO StopConnect
        }

-- Raised by the recording connector once it has captured its address argument, so no
-- socket is ever opened.
data StopConnect = StopConnect
    deriving stock (Show)

instance Exception StopConnect

-- Fetch the package's metadata through the npm client over the given manager — the
-- exact primitive both serve fetches use, differing only in which manager they carry.
fetchMetadata :: Manager -> Port -> IO RegistryResponse
fetchMetadata manager port =
    fetchMetadataForm (clientConfig manager port) Abbreviated noValidators thing

-- An npm client config pointed at the loopback upstream on @port@.
clientConfig :: Manager -> Port -> NpmClientConfig
clientConfig manager port =
    NpmClientConfig
        { npmBaseUrl = "http://127.0.0.1:" <> show port
        , npmManager = manager
        , npmToken = Nothing
        , npmLimits = defaultLimits
        }

-- Run an action against an in-process upstream serving the packument on loopback.
withUpstream :: (Port -> IO a) -> IO a
withUpstream = testWithApplication (pure app)
  where
    app _request respond = respond (responseLBS status200 [] (encode packument))

-- A minimal packument body the upstream serves; the test asserts on the bytes, not
-- their structure, so an opaque object is enough.
packument :: Value
packument = object ["name" .= ("thing" :: Text), "versions" .= object []]

thing :: PackageName
thing = mkPackageName Npm Nothing "thing"
