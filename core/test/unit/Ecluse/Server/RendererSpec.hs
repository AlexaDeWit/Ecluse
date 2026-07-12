-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Server.RendererSpec (spec) where

import Test.Hspec

import Ecluse.Core.Server.Renderer (appendHelp, mkHelpMessage)

{- | The ecosystem-neutral part of denial rendering: the operator help message is
trimmed at construction, so it joins a denial with exactly one separating space and an
all-blank value contributes nothing. (How the joined text is then wrapped into body
bytes is each mount's 'Ecluse.Core.Server.Renderer.MountRenderer', exercised by the
per-ecosystem serve specs.)
-}
spec :: Spec
spec = describe "HelpMessage -- trimmed at construction" $ do
    it "appends the message trimmed of surrounding whitespace, one separating space" $
        appendHelp (Just (mkHelpMessage "  Contact support.  ")) "denied"
            `shouldBe` "denied Contact support."
    it "an all-whitespace message collapses to blank and appends nothing" $
        appendHelp (Just (mkHelpMessage " \t ")) "denied" `shouldBe` "denied"
