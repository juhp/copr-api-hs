{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Copyright: (c) 2020 Jens Petersen
SPDX-License-Identifier: GPL-2.0-only
Maintainer: Jens Petersen <petersen@redhat.com>

Copr REST client library
-}

module Web.Fedora.Copr
  (coprChroots)
where

import Data.Aeson.Types (Object)
import qualified Data.HashMap.Lazy as H
import Data.List (sort)
import Data.Text (Text)

import Web.Fedora.Copr.API

coprChroots :: String -> String -> String -> IO [Text]
coprChroots server owner project = do
  proj <- coprGetProject server owner project
  case lookupKey "chroot_repos" proj :: Maybe Object of
    Nothing ->
        case lookupKey "error" proj of
          Just err -> error err
          Nothing -> return []
    Just obj -> return $ (reverse . sort . H.keys) obj
