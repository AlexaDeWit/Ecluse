-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Loopback peers below HTTP, for transport faults a WAI stub cannot raise: a raw TCP listener, and a TLS one with a certificate made per server.
module Ecluse.Test.Stub.Socket (
    -- * Listeners
    withRawServer,
    awaitRequestHead,
    withTlsServer,
    withTlsLostAnswerThenSilent,
    withTlsLostAnswerThenSlow,
    withTlsResetThenSilent,
    withResetAfterHeadThenFull,
    trustingTlsManager,

    -- * A peer that never answers
    silentPeer,

    -- * Peers that reset mid-body
    resetOnClose,
    resetAfterPartialBody,
    resetAfterPartialBodyOverTcp,
    Cut (..),
    cutHalfwayThenServe,
) where

import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.ByteArray (convert)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Hourglass (Date (Date), DateTime (DateTime), Month (January), TimeOfDay (TimeOfDay))
import Data.X509 (
    Certificate (..),
    CertificateChain (CertificateChain),
    DistinguishedName (DistinguishedName),
    Extensions (Extensions),
    PrivKey (PrivKeyEd25519),
    PubKey (PubKeyEd25519),
    PubKeyALG (PubKeyALG_Ed25519),
    SignatureALG (SignatureALG_IntrinsicHash),
    objectToSignedExact,
 )
import Network.Connection (TLSSettings (TLSSettingsSimple))
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.Socket (
    Family (AF_INET),
    SockAddr (SockAddrInet),
    Socket,
    SocketOption (Linger),
    SocketType (Stream),
    StructLinger (StructLinger),
    accept,
    bind,
    close,
    connect,
    defaultProtocol,
    listen,
    setSockOpt,
    socket,
    socketPort,
    tupleToHostAddress,
 )
import Network.Socket.ByteString (recv, sendAll)
import Network.TLS qualified as TLS
import Network.Wai.Handler.Warp (openFreePort)
import Numeric (showHex)
import UnliftIO.Async (withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (bracket, finally)

import Ecluse.Core.Fault.Http (typedBodyReads)

-- | Run the handler on every connection the listener accepts, then close the connection.
withRawServer :: (Socket -> IO ()) -> (Int -> IO a) -> IO a
withRawServer handler action = bracket openFreePort (close . snd) $ \(port, listener) ->
    withAsync (forever (accept listener >>= \(conn, _) -> handler conn `finally` close conn)) $ \_ ->
        action port

-- | 'withRawServer' with the handler run after a completed TLS handshake. Reach it through 'trustingTlsManager'.
withTlsServer :: (Socket -> TLS.Context -> IO ()) -> (Int -> IO a) -> IO a
withTlsServer handler action = do
    params <- selfSignedServerParams
    withRawServer (\conn -> TLS.contextNew conn params >>= \context -> TLS.handshake context >> handler conn context) action

{- | Over TLS, answer the first connection's first request, close it on the second unanswered, and
never finish a later connection's handshake: http-client's retry of a reused connection then stalls.
-}
withTlsLostAnswerThenSilent :: (Int -> IO a) -> IO a
withTlsLostAnswerThenSilent = withTlsWarmThen loseAnswer silently

-- | 'withTlsLostAnswerThenSilent' with a later connection that finishes its handshake, and then answers, each after its delay in microseconds.
withTlsLostAnswerThenSlow :: Int -> Int -> (Int -> IO a) -> IO a
withTlsLostAnswerThenSlow handshakeDelay answerDelay = withTlsWarmThen loseAnswer $ \params conn -> do
    threadDelay handshakeDelay
    context <- TLS.contextNew conn params
    TLS.handshake context
    awaitTlsHead context ""
    threadDelay answerDelay
    TLS.sendData context "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok"

-- | 'withTlsLostAnswerThenSilent' with the first connection reset once it answers, so the next request's head fails to send.
withTlsResetThenSilent :: (Int -> IO a) -> IO a
withTlsResetThenSilent = withTlsWarmThen (\conn _ -> resetOnClose conn) silently

-- Answer the first connection's first request over TLS, then end it as given. Later connections take the other handler.
withTlsWarmThen :: (Socket -> TLS.Context -> IO ()) -> (TLS.ServerParams -> Socket -> IO ()) -> (Int -> IO a) -> IO a
withTlsWarmThen ending later action = do
    params <- selfSignedServerParams
    connections <- newIORef (0 :: Int)
    let serve conn = do
            seen <- atomicModifyIORef' connections (\n -> (n + 1, n))
            if seen > 0
                then later params conn
                else do
                    context <- TLS.contextNew conn params
                    TLS.handshake context
                    awaitTlsHead context ""
                    TLS.sendData context "HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\nwarm"
                    ending conn context
    withRawServer serve action

-- Take the request head, then close without answering.
loseAnswer :: Socket -> TLS.Context -> IO ()
loseAnswer _ context = awaitTlsHead context "" >> TLS.bye context

-- Never finish the handshake.
silently :: TLS.ServerParams -> Socket -> IO ()
silently _ _ = threadDelay 20_000_000

selfSignedServerParams :: IO TLS.ServerParams
selfSignedServerParams = do
    credential <- selfSignedCredential
    pure TLS.defaultParamsServer{TLS.serverShared = TLS.defaultShared{TLS.sharedCredentials = TLS.Credentials [credential]}}

{- | Serve one response before a full TCP accept queue prevents later handshakes.
Reset the existing connection and signal after its next request head.
-}
withResetAfterHeadThenFull :: MVar () -> (Int -> IO a) -> IO a
withResetAfterHeadThenFull reset action =
    bracket (socket AF_INET Stream defaultProtocol) close $ \listener -> do
        bind listener (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
        listen listener 1
        port <- fromIntegral <$> socketPort listener
        withAsync (serveThenFill listener port) (\_ -> action port)
  where
    serveThenFill listener port = bracket (fst <$> accept listener) close $ \conn -> do
        awaitRequestHead conn ""
        sendAll conn "HTTP/1.1 200 OK\r\nContent-Length: 4\r\n\r\nwarm"
        -- A queue of one takes two waiting connections, and the kernel drops any after them.
        bracket (replicateM 2 (dial port)) (traverse_ close) $ \_ -> do
            awaitRequestHead conn ""
            resetOnClose conn
            close conn
            putMVar reset ()
            threadDelay 20_000_000
    dial port = do
        client <- socket AF_INET Stream defaultProtocol
        client <$ connect client (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))

-- | A manager under 'typedBodyReads', as every production manager is, that skips certificate validation so it accepts 'withTlsServer'.
trustingTlsManager :: IO Manager
trustingTlsManager = newManager (typedBodyReads (mkManagerSettings (TLSSettingsSimple True False False TLS.defaultSupported) Nothing))

-- | Hold the connection and send nothing, so a client waits for headers that never come.
silentPeer :: Socket -> IO ()
silentPeer _ = forever (threadDelay 1_000_000)

-- | Make the connection's close send RST instead of FIN, through a zero linger time.
resetOnClose :: Socket -> IO ()
resetOnClose conn = setSockOpt conn Linger (StructLinger 1 0)

-- | Answer a 200 that promises twenty body bytes, send five, then reset the connection. A HEAD gets the headers alone.
resetAfterPartialBody :: Socket -> TLS.Context -> IO ()
resetAfterPartialBody conn context = do
    request <- TLS.recvData context
    TLS.sendData context (toLazy (partialAnswer request))
    resetOnClose conn

-- | 'resetAfterPartialBody' over plain TCP, for 'withRawServer'.
resetAfterPartialBodyOverTcp :: Socket -> IO ()
resetAfterPartialBodyOverTcp conn = do
    request <- recv conn 4096
    sendAll conn (partialAnswer request)
    resetOnClose conn

-- | How a peer ends an answer it cut off halfway through the body.
data Cut
    = -- | RST, which the client reads as a socket error.
      ResetMidBody
    | -- | FIN, which the client reads as a body shorter than its length.
      CloseMidBody
    | -- | FIN inside the data of a chunked answer, which the client reads as broken chunk framing.
      CloseMidChunk
    deriving stock (Eq, Show)

-- | A 'withRawServer' handler that serves @body@, but cuts the first @cuts@ answers off halfway through it, and a reader of the requests it took.
cutHalfwayThenServe :: Cut -> Int -> LByteString -> IO (Socket -> IO (), IO Int)
cutHalfwayThenServe cut cuts body = do
    answered <- newIORef 0
    let serve conn = do
            attempt <- atomicModifyIORef' answered (\n -> (n + 1, n))
            awaitRequestHead conn ""
            if attempt < cuts
                then sendAll conn (cutAnswer cut body) >> when (cut == ResetMidBody) (resetOnClose conn)
                else sendAll conn (lengthHeading body <> LBS.toStrict body)
    pure (serve, readIORef answered)

-- The first half of the answer: the length, or one chunk's size, promises the whole body.
cutAnswer :: Cut -> LByteString -> ByteString
cutAnswer cut body = heading <> LBS.toStrict (LBS.take (LBS.length body `div` 2) body)
  where
    heading = case cut of
        CloseMidChunk -> "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\nConnection: close\r\n\r\n" <> encodeUtf8 (toText (showHex (LBS.length body) "")) <> "\r\n"
        _ -> lengthHeading body

-- The head of a closing answer whose length promises the whole body.
lengthHeading :: LByteString -> ByteString
lengthHeading body = "HTTP/1.1 200 OK\r\nContent-Length: " <> show (LBS.length body) <> "\r\nConnection: close\r\n\r\n"

-- | Consume request headers so a close leaves no unread bytes that would turn it into a reset.
awaitRequestHead :: Socket -> ByteString -> IO ()
awaitRequestHead conn seen = unless ("\r\n\r\n" `BS.isInfixOf` seen) $ do
    chunk <- recv conn 4096
    unless (BS.null chunk) (awaitRequestHead conn (seen <> chunk))

-- The request head in full over TLS.
awaitTlsHead :: TLS.Context -> ByteString -> IO ()
awaitTlsHead context seen = unless ("\r\n\r\n" `BS.isInfixOf` seen) $ do
    chunk <- TLS.recvData context
    unless (BS.null chunk) (awaitTlsHead context (seen <> chunk))

partialAnswer :: ByteString -> ByteString
partialAnswer request = "HTTP/1.1 200 OK\r\nContent-Length: 20\r\nETag: \"one\"\r\nConnection: close\r\n\r\n" <> body
  where
    body = if "HEAD " `BS.isPrefixOf` request then "" else "short"

-- A key made per server, so no private key is ever committed. The client skips validation, so only the key pair matters.
selfSignedCredential :: IO TLS.Credential
selfSignedCredential = do
    secret <- Ed25519.generateSecretKey
    let public = Ed25519.toPublic secret
        sign bytes = (convert (Ed25519.sign secret public bytes), ed25519, ())
        (signed, ()) = objectToSignedExact sign (selfSigned (PubKeyEd25519 public))
    pure (CertificateChain [signed], PrivKeyEd25519 secret)

selfSigned :: PubKey -> Certificate
selfSigned key =
    Certificate
        { certVersion = 2
        , certSerial = 1
        , certSignatureAlg = ed25519
        , certIssuerDN = DistinguishedName []
        , certValidity = (DateTime (Date 2000 January 1) midnight, DateTime (Date 2100 January 1) midnight)
        , certSubjectDN = DistinguishedName []
        , certPubKey = key
        , certExtensions = Extensions Nothing
        }
  where
    midnight = TimeOfDay 0 0 0 0

ed25519 :: SignatureALG
ed25519 = SignatureALG_IntrinsicHash PubKeyALG_Ed25519
