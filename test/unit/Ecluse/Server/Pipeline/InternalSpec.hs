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
import Ecluse.Server.Pipeline.Internal (logDecodeFailure)

spec :: Spec
spec =
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
