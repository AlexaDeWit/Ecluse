-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TemplateHaskell #-}

{- | The committed default configuration, embedded into the binary at compile time.

Both the default-policy build ('Ecluse.Config.defaultPolicy') and the merged
configuration load ('Ecluse.Config.loadConfig') read the same @config\/default.yaml@.
Embedding it once here gives them a single shared binding instead of two independent
Template Haskell splices. It also confines the one accepted @STAN-0212@
(unsafe-function) observation the 'embedFile' splice expands to. This module carries
nothing but the embed, so its source lines never shift. The @.stan.toml@ exclude can
therefore name the file itself, rather than a line and column that rots on every edit
above it.
-}
module Ecluse.Config.DefaultConfig (defaultConfigBytes) where

import Data.FileEmbed (embedFile)

{- | The committed default configuration document, embedded verbatim from
@config\/default.yaml@ at compile time.
-}
defaultConfigBytes :: ByteString
defaultConfigBytes = $(embedFile "config/default.yaml")
