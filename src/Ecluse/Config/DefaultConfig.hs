-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TemplateHaskell #-}

{- | The committed @config\/default.yaml@, embedded into the binary at compile time and read by
both the default-policy build ('Ecluse.Config.defaultPolicy') and the merged configuration load
('Ecluse.Config.loadConfig').

The module carries nothing but the embed, so its source lines never shift and the @.stan.toml@
exclude for the one accepted @STAN-0212@ observation of the 'embedFile' splice can name the file
itself rather than a line and column.
-}
module Ecluse.Config.DefaultConfig (defaultConfigBytes) where

import Data.FileEmbed (embedFile)

-- | The committed default configuration document, embedded verbatim at compile time.
defaultConfigBytes :: ByteString
defaultConfigBytes = $(embedFile "config/default.yaml")
