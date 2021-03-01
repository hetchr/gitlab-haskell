{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Branches
-- Description : Queries about repository branches
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Branches where

import Data.Either
import qualified Data.Text as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Types.Status

-- | Get a list of repository branches from a project, sorted by name
-- alphabetically.
branches :: Project -> GitLab [Branch]
branches project = do
  result <- branches' (project_id project)
  return (fromRight (error "branches error") result)

-- | Get a list of repository branches from a project given its
-- project ID, sorted by name alphabetically.
branches' :: Int -> GitLab (Either Status [Branch])
branches' projectId =
  gitlab addr
  where
    addr = RelativeUrl $
      "/projects/"
        <> T.pack (show projectId)
        <> "/repository"
        <> "/branches"
