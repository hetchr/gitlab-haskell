{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Discussions
-- Description : Queries about discussions, which are a set of related notes on snippets, issues, epics, merge requests and commits.
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2021
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Discussions where

import Data.Text (Text)
import qualified Data.Text as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Types.Status

-- | gets all discussion for a commit for a project.
commitDiscussions ::
  -- | the project
  Project ->
  -- | commit hash
  Text ->
  GitLab (Either Status [Discussion])
commitDiscussions proj = commitDiscussions' (project_id proj)

-- | gets all discussion for a commit for a project given its project ID.
commitDiscussions' ::
  -- | the project ID
  Int ->
  -- | commit hash
  Text ->
  GitLab (Either Status [Discussion])
commitDiscussions' projId commitHash = do
  let urlPath =
        RelativeUrl $ T.pack $
          "/projects/"
            <> show projId
            <> "/repository"
            <> "/commits/"
            <> T.unpack commitHash
            <> "/discussions"
  gitlab urlPath
