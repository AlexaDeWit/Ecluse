-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Loopback peers below HTTP, for transport faults a WAI stub cannot raise: a raw TCP listener, and a TLS one with a certificate made per server.
module Ecluse.Test.Stub.Socket (
    -- * Listeners
    withRawServer,
    withTlsServer,
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
import Network.Socket (Socket, SocketOption (Linger), StructLinger (StructLinger), accept, close, setSockOpt)
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
    credential <- selfSignedCredential
    let params = TLS.defaultParamsServer{TLS.serverShared = TLS.defaultShared{TLS.sharedCredentials = TLS.Credentials [credential]}}
    withRawServer (\conn -> TLS.contextNew conn params >>= \context -> TLS.handshake context >> handler conn context) action

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

-- The request head in full, so a clean close leaves nothing unread that would turn it into a reset.
awaitRequestHead :: Socket -> ByteString -> IO ()
awaitRequestHead conn seen = unless ("\r\n\r\n" `BS.isInfixOf` seen) $ do
    chunk <- recv conn 4096
    unless (BS.null chunk) (awaitRequestHead conn (seen <> chunk))

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
