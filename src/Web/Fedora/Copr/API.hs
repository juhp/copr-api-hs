{-# LANGUAGE CPP #-}

{- |
Copyright: (c) 2020 Jens Petersen
SPDX-License-Identifier: GPL-2.0-only
Maintainer: Jens Petersen <petersen@redhat.com>

Copr REST client library
-}

module Web.Fedora.Copr.API
  ( coprGetProject
  , coprGetProjectsList
  , coprSearchProjects
  , coprGetBuild
  , coprGetBuildSourceChroot
  , coprGetBuildSourceConfig
  , coprGetBuildPackageList
  , coprGetBuildChroot
  , coprGetBuildChrootList
  , coprGetBuildChrootConfig
  , coprMockChrootList
  , coprGetPackage
  , coprGetPackageList
  , coprGetProjectChroot
  , coprGetProjectChrootBuildConfig
  , queryCopr
  , maybeKey
  , makeKey
  , makeItem
  , lookupKey
  , lookupKey'
  )
where

import Data.Aeson.Types
import Network.HTTP.Query

-- # Projects

-- | List project details
--
-- https://pagure.io/copr/copr/blob/master/f/python/copr/v3/proxies/project.py#_9
coprGetProject :: String -> String -> String -> IO Value
coprGetProject server owner project = do
  let path = "project"
      params = [makeItem "ownername" owner, makeItem "projectname" project]
  queryCopr server path params

-- | List projects of owner
--
-- https://pagure.io/copr/copr/blob/master/f/python/copr/v3/proxies/project.py#_26
coprGetProjectsList :: String -> String -> IO Value
-- FIXME limit=int or maybe pagination?
coprGetProjectsList server owner = do
  let path = "project/list"
      params = makeKey "ownername" owner
  queryCopr server path params

-- | search projects by query string
--
-- https://pagure.io/copr/copr/blob/master/f/python/copr/v3/proxies/project.py#_43
coprSearchProjects :: String -> String -> IO Value
coprSearchProjects server query = do
  let path = "project/search"
      params = makeKey "query" query
  queryCopr server path params

-- # Builds

-- | get build
--
-- https://pagure.io/copr/copr/blob/master/f/python/copr/v3/proxies/build.py#_10
coprGetBuild :: String -> Int -> IO Value
coprGetBuild server bid = do
  let path = "build" +/+ show bid
  queryCopr server path []

-- | get srpm build
--
-- https://pagure.io/copr/copr/blob/master/f/python/copr/v3/proxies/build.py#_22
coprGetBuildSourceChroot :: String -> Int -> IO Value
coprGetBuildSourceChroot server bid = do
  let path = "build/source-chroot" +/+ show bid
  queryCopr server path []

-- | get build source config
--
-- https://pagure.io/copr/copr/blob/master/f/python/copr/v3/proxies/build.py#_34
coprGetBuildSourceConfig :: String -> Int -> IO Value
coprGetBuildSourceConfig server bid = do
  let path = "build/source-build-config" +/+ show bid
  queryCopr server path []

-- | get list of packages
--
-- https://pagure.io/copr/copr/blob/master/f/python/copr/v3/proxies/build.py#_46
coprGetBuildPackageList :: String -> Query -> IO Value
coprGetBuildPackageList server params = do
  let path = "build/list"
  queryCopr server path params

-- # Build chroot

-- | get build chroot
--
-- https://pagure.io/copr/copr/blob/master/f/python/copr/v3/proxies/build_chroot.py#_8
coprGetBuildChroot :: String -> Int -> String -> IO Value
coprGetBuildChroot server bid chroot = do
  let path = "build-chroot" +/+ show bid +/+ chroot
  queryCopr server path []

-- | list of build chroots
--
-- https://pagure.io/copr/copr/blob/master/f/python/copr/v3/proxies/build_chroot.py#_25
coprGetBuildChrootList :: String -> Int -> IO Value
coprGetBuildChrootList server bid = do
  let path = "build-chroot/list" +/+ show bid
  queryCopr server path []

-- | get build config for chroot
--
-- https://pagure.io/copr/copr/blob/master/f/python/copr/v3/proxies/build_chroot.py#_44
coprGetBuildChrootConfig :: String -> Int -> String -> IO Value
coprGetBuildChrootConfig server bid chroot = do
  let path = "build-chroot/build-config" +/+ show bid +/+ chroot
  queryCopr server path []

-- # Mock chroot

-- | list of all available mock chroots
--
-- https://pagure.io/copr/copr/blob/master/f/python/copr/v3/proxies/mock_chroot.py
coprMockChrootList :: String -> IO Value
coprMockChrootList server = do
  let path = "mock-chroots/list"
  queryCopr server path []

-- # Package

-- | Get project package details
--
-- https://pagure.io/copr/copr/blob/master/f/python/copr/v3/proxies/package.py#_9
coprGetPackage :: String -> String -> String -> String -> IO Value
coprGetPackage server owner project package = do
  let path = "package"
      params = [makeItem "ownername" owner,
                makeItem "projectname" project,
                makeItem "packagename" package]
  queryCopr server path params

-- | List project packages
--
-- https://pagure.io/copr/copr/blob/master/f/python/copr/v3/proxies/package.py#_28
coprGetPackageList :: String -> String -> String -> IO Value
coprGetPackageList server owner project = do
  let path = "package/list"
      params = [makeItem "ownername" owner, makeItem "projectname" project]
  queryCopr server path params

-- # Project chroot

-- | get build chroot
--
-- https://pagure.io/copr/copr/blob/master/f/python/copr/v3/proxies/project_chroot.py#_10
coprGetProjectChroot :: String -> String -> String -> String -> IO Value
coprGetProjectChroot server owner project chroot = do
  let path = "project-chroot"
      params = [makeItem "ownername" owner,
                makeItem "projectname" project,
                makeItem "chrootname" chroot]
  queryCopr server path params

-- | list of build chroots
--
-- https://pagure.io/copr/copr/blob/master/f/python/copr/v3/proxies/project_chroot.py#_29
coprGetProjectChrootBuildConfig :: String -> String -> String -> String -> IO Value
coprGetProjectChrootBuildConfig server owner project chroot = do
  let path = "project-chroot/build-config"
      params = [makeItem "ownername" owner,
                makeItem "projectname" project,
                makeItem "chrootname" chroot]
  queryCopr server path params


-- | low-level query
queryCopr :: String -> String -> Query -> IO Value
queryCopr server path params =
  let url = "https://" ++ server +/+ "api_3" +/+ path
  in webAPIQuery url params
