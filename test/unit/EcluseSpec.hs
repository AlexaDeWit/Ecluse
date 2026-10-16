-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | What the entry point sets up before it reads the command line.
module EcluseSpec (spec) where

import Data.Text qualified as T
import GHC.IO.Encoding (getLocaleEncoding, setLocaleEncoding, textEncodingName)
import System.Environment (withArgs)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hGetEncoding, hSetEncoding, mkTextEncoding)
import Test.Hspec
import UnliftIO (bracket, try)

import Ecluse (run)
import Ecluse.Test.Log (captureStdout)

spec :: Spec
spec =
    describe "run" $
        it "writes UTF-8 on both standard handles when the host has no locale" $
            withoutLocale $ do
                help <- captureStdout (try (withArgs ["--help"] run) `shouldReturn` Left ExitSuccess)
                help `shouldSatisfy` T.isInfixOf "Écluse"
                fmap textEncodingName <$> hGetEncoding stderr `shouldReturn` Just "UTF-8"
                textEncodingName <$> getLocaleEncoding `shouldReturn` "UTF-8"

-- On a host with no locale, GHC starts a process with ASCII in the locale and on both handles.
withoutLocale :: IO a -> IO a
withoutLocale act = do
    ascii <- mkTextEncoding "ASCII"
    bracket saved restore (const (setEncodings ascii (Just ascii) (Just ascii) >> act))
  where
    saved = (,,) <$> getLocaleEncoding <*> hGetEncoding stdout <*> hGetEncoding stderr
    restore (locale, out, err) = setEncodings locale out err
    setEncodings locale out err = do
        setLocaleEncoding locale
        traverse_ (hSetEncoding stdout) out
        traverse_ (hSetEncoding stderr) err
