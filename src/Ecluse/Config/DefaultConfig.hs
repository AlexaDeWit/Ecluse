-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TemplateHaskell #-}

-- | The single embedded default document used by configuration and policy loading.
module Ecluse.Config.DefaultConfig (defaultConfigBytes) where

import Data.FileEmbed (embedFile)

-- | The committed default configuration document, embedded verbatim at compile time.
defaultConfigBytes :: ByteString
defaultConfigBytes = $(embedFile "config/default.yaml")
