module Ecluse.Server.Pipeline.InternalSpec (spec) where

import Data.Text qualified as T
import GHC.IO.Handle (hClose, hDuplicate, hDuplicateTo)
import Katip (Environment (Environment), closeScribes)
import Test.Hspec
import UnliftIO (bracket)
import UnliftIO.Temporary (withSystemTempFile)

import Ecluse.Ecosystem (Ecosystem (Npm))
import Ecluse.Log (LogFormat (JsonLog), newLogEnv)
import Ecluse.Package (mkPackageName)
import Ecluse.Server.Pipeline.Internal (PackumentNameMismatch (PackumentNameMismatch), logDecodeFailure, logNameMismatch)

spec :: Spec
spec = do
    describe "logDecodeFailure" $
        it "logs a WARNING tagged with this module and the package, naming the decode failure" $ do
            -- Drive the real JSONL stdout scribe and capture the line, so the
            -- structured `module` / `package` fields and the severity are asserted on
            -- the exact bytes an operator would see.
            logged <- captureStdout $ do
                logEnv <- newLogEnv JsonLog (Environment "test")
                logDecodeFailure logEnv (mkPackageName Npm Nothing "is-odd")
                void (closeScribes logEnv)
            logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Warning\""
            logged `shouldSatisfy` T.isInfixOf "\"module\":\"Ecluse.Server.Pipeline.Internal\""
            logged `shouldSatisfy` T.isInfixOf "\"package\":\"is-odd\""
            logged `shouldSatisfy` T.isInfixOf "did not decode"

    describe "logNameMismatch" $
        it "logs a WARNING carrying both names and the origin when an upstream reports a different package" $ do
            -- The serve path drives this with a scribe-less LogEnv (katip then never
            -- forces the structured payload), so the warning's actual bytes — the
            -- requested name, the upstream's reported name, and the origin — are pinned
            -- here against the real JSONL scribe an operator reads.
            logged <- captureStdout $ do
                logEnv <- newLogEnv JsonLog (Environment "test")
                logNameMismatch logEnv (mkPackageName Npm Nothing "thing") "http://upstream.test" "other"
                void (closeScribes logEnv)
            logged `shouldSatisfy` T.isInfixOf "\"sev\":\"Warning\""
            logged `shouldSatisfy` T.isInfixOf "\"module\":\"Ecluse.Server.Pipeline.Internal\""
            logged `shouldSatisfy` T.isInfixOf "\"package\":\"thing\""
            logged `shouldSatisfy` T.isInfixOf "\"upstreamName\":\"other\""
            logged `shouldSatisfy` T.isInfixOf "\"origin\":\"http://upstream.test\""
            logged `shouldSatisfy` T.isInfixOf "different package"

    describe "PackumentNameMismatch" $
        it "has usable Eq/Show (the typed-throw contract)" $ do
            -- A distinct typed exception, caught by the origin fetcher and recovered via
            -- 'fromException'; its derived instances back the catch and any audit show.
            show PackumentNameMismatch `shouldBe` ("PackumentNameMismatch" :: Text)
            PackumentNameMismatch `shouldBe` PackumentNameMismatch

{- | Run an 'IO' action with 'stdout' redirected to a temporary file, returning
everything written — so a scribe's output is assertable with no network. The original
'stdout' is restored on every exit path. (Mirrors the local helper in "Ecluse.LogSpec"
and "Ecluse.Server.PipelineSpec"; kept local to avoid exporting a test-only utility.)
-}
captureStdout :: IO () -> IO Text
captureStdout act =
    withSystemTempFile "ecluse-pipeline-internal-log.txt" $ \path tmpHandle ->
        bracket (hDuplicate stdout) restore $ \_saved -> do
            hFlush stdout
            hDuplicateTo tmpHandle stdout
            act
            hFlush stdout
            hClose tmpHandle
            decodeUtf8 <$> readFileBS path
  where
    restore saved = do
        hFlush stdout
        hDuplicateTo saved stdout
        hClose saved
