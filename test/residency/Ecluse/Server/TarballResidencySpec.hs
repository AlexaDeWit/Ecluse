{- | S54 -- the bounded-memory streaming gate: peak live bytes on the tarball relay
are __invariant in artifact size__, proving constant-memory passthrough rather than
size-proportional buffering.

== Why an invariant, not an absolute

A "peak residency < N MB" assertion is machine- and RTS-dependent, the classic flaky
residency test. This suite instead streams a small and a large artifact through the
same path and asserts the peaks differ by no more than a fixed margin: if the relay
buffered, the large probe's live bytes would exceed the small one's by roughly the
artifact size (~100 MiB), two orders of magnitude past the margin, so the gate is
deterministic in the property it checks rather than in an absolute number.

== Why sampling during the stream, in its own process

'GHC.Stats.max_live_bytes' is a process-lifetime high-water mark, so inside a shared
suite process earlier examples would drown the delta. This suite therefore runs as
its own executable (with @-T@ baked into its @ghc-options@), and the consumer forces
a major collection every few chunks and tracks its __own__ per-probe maximum of
'gcdetails_live_bytes', current live data, immune to process history.

The consumer drives the WAI 'Application' directly and discards chunks as they
arrive: the probe exercises Écluse's relay pump (upstream reader to 'StreamingBody'
writer) without a buffering test client in the way. The served body itself is a lazy
'LByteString' of one shared 64 KiB chunk, so the fixture holds ~64 KiB resident no
matter the size it serves.
-}
module Ecluse.Server.TarballResidencySpec (spec) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import GHC.Stats (GCDetails (gcdetails_live_bytes), RTSStats (gc), getRTSStats, getRTSStatsEnabled)
import Network.HTTP.Types (hAuthorization, statusCode)
import Network.Wai (Application, Request (requestHeaders), responseToStream)
import Network.Wai.Internal (ResponseReceived (ResponseReceived))
import Network.Wai.Test (defaultRequest, setPath)
import System.Mem (performMajorGC)
import Test.Hspec

import Ecluse.Core.Queue.Memory (newInMemoryQueue)
import Ecluse.Server.Pipeline.TestSupport (
    artifactUpstream,
    privateArtifactHit,
    privateArtifactMiss,
    withProxyEnv,
    withProxyEnvQueue,
 )

spec :: Spec
spec = describe "bounded-memory streaming (S54): residency invariant on the tarball relay" $ do
    it "runs with RTS stats enabled (this suite must be built with -with-rtsopts=-T)" $
        getRTSStatsEnabled `shouldReturn` True

    it "private-hit relay: peak live bytes are size-invariant (1 MiB vs 100 MiB)" $ do
        smallPeak <- privateHitPeak smallArtifactBytes
        largePeak <- privateHitPeak largeArtifactBytes
        largePeak `shouldBeWithinMarginOf` smallPeak

    it "gated public relay: peak live bytes are size-invariant (gate, stream, enqueue)" $ do
        smallPeak <- publicMissPeak smallArtifactBytes
        largePeak <- publicMissPeak largeArtifactBytes
        largePeak `shouldBeWithinMarginOf` smallPeak

-- The probe sizes from the slice: two orders of magnitude apart, so buffering
-- dwarfs the margin. Both are whole multiples of the shared chunk.
smallArtifactBytes, largeArtifactBytes :: Int
smallArtifactBytes = 1 * 1024 * 1024
largeArtifactBytes = 100 * 1024 * 1024

{- | The allowed peak-live growth from the small probe to the large one. Generous
against fixture noise (connection buffers, chunk copies, GC estimation), still ~6x
below what even partial buffering of the large artifact would add.
-}
residencyMarginBytes :: Integer
residencyMarginBytes = 16 * 1024 * 1024

shouldBeWithinMarginOf :: Word64 -> Word64 -> Expectation
shouldBeWithinMarginOf largePeak smallPeak = do
    let grewBy = toInteger largePeak - toInteger smallPeak
    unless (grewBy <= residencyMarginBytes) . expectationFailure $
        "peak live bytes grew with artifact size: small probe "
            <> showMiB (toInteger smallPeak)
            <> ", large probe "
            <> showMiB (toInteger largePeak)
            <> " (grew "
            <> showMiB grewBy
            <> ", margin "
            <> showMiB residencyMarginBytes
            <> ") -- the tarball relay is buffering, not streaming"
  where
    showMiB n = show (n `div` (1024 * 1024)) <> " MiB"

{- | Peak live bytes streaming an artifact of the given size on the trusted
private-hit leg (the pure relay: no gate, no enqueue).
-}
privateHitPeak :: Int -> IO Word64
privateHitPeak size = do
    privateUp <- privateArtifactHit "1.0.0" (syntheticArtifact size)
    publicUp <- artifactUpstream "1.0.0" (syntheticArtifact chunkBytes)
    withProxyEnv privateUp publicUp Nothing $ \app _env -> do
        (st, served, peak) <- streamProbe (Just "client-token") app
        st `shouldBe` 200
        served `shouldBe` fromIntegral size
        pure peak

{- | Peak live bytes streaming an artifact of the given size on the gated public
leg (private miss: policy gate, public stream, mirror-job enqueue).
-}
publicMissPeak :: Int -> IO Word64
publicMissPeak size = do
    privateUp <- privateArtifactMiss
    publicUp <- artifactUpstream "1.0.0" (syntheticArtifact size)
    queue <- newInMemoryQueue
    withProxyEnvQueue queue privateUp publicUp Nothing $ \app _env _publicPort -> do
        (st, served, peak) <- streamProbe Nothing app
        st `shouldBe` 200
        served `shouldBe` fromIntegral size
        pure peak

{- | Drive one tarball request through the 'Application', consuming the streaming
body chunk-by-chunk and discarding it, sampling live bytes as the stream flows.
Returns the status code, total bytes served (so the caller can prove the stream
really flowed), and the peak sampled live bytes.
-}
streamProbe :: Maybe Text -> Application -> IO (Int, Int64, Word64)
streamProbe bearer app = do
    bytesRef <- newIORef (0 :: Int64)
    chunksRef <- newIORef (0 :: Int)
    peakRef <- newIORef (0 :: Word64)
    statusRef <- newIORef (0 :: Int)
    let headers = maybe [] (\t -> [(hAuthorization, "Bearer " <> encodeUtf8 t)]) bearer
        req = setPath defaultRequest{requestHeaders = headers} "/npm/thing/-/thing-1.0.0.tgz"
    _ <- app req $ \resp -> do
        let (st, _responseHeaders, withBody) = responseToStream resp
        writeIORef statusRef (statusCode st)
        withBody $ \body -> body (onChunk bytesRef chunksRef peakRef) pass
        pure ResponseReceived
    samplePeak peakRef
    (,,) <$> readIORef statusRef <*> readIORef bytesRef <*> readIORef peakRef
  where
    onChunk bytesRef chunksRef peakRef chunk = do
        modifyIORef' bytesRef (+ LBS.length (Builder.toLazyByteString chunk))
        seen <- readIORef chunksRef
        writeIORef chunksRef (seen + 1)
        when (seen `mod` sampleEveryChunks == 0) (samplePeak peakRef)

{- | Force a major collection and fold the resulting live-bytes reading into the
running maximum. Forcing the GC makes the sample deterministic: the reading is
what is genuinely live mid-stream, not whatever the collector last happened to see.
-}
samplePeak :: IORef Word64 -> IO ()
samplePeak peakRef = do
    performMajorGC
    stats <- getRTSStats
    modifyIORef' peakRef (max (gcdetails_live_bytes (gc stats)))

{- | Sampling cadence: the large probe streams thousands of chunks, so this yields
on the order of a hundred forced collections over a small heap, cheap, and dense
enough that a growing buffer cannot hide between samples.
-}
sampleEveryChunks :: Int
sampleEveryChunks = 64

{- | A lazy body of one shared strict chunk: the whole served artifact costs the
fixture ~64 KiB resident regardless of its size, so any size-proportional
growth the probe observes belongs to the relay under test.
-}
syntheticArtifact :: Int -> LByteString
syntheticArtifact size = LBS.fromChunks (replicate (size `div` chunkBytes) sharedChunk)

sharedChunk :: ByteString
sharedChunk = BS.replicate chunkBytes 0x74

chunkBytes :: Int
chunkBytes = 64 * 1024
