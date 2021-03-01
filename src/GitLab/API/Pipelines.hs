{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Pipelines
-- Description : Queries about project pipelines
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Pipelines where

import Data.Either
import qualified Data.Text as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Types.Status

-- | returns the pipelines for a project.
pipelines ::
  -- | the project
  Project ->
  GitLab [Pipeline]
pipelines p = do
  result <- pipelines' (project_id p)
  return (fromRight (error "pipelines error") result)

-- | returns the pipelines for a project given its project ID.
pipelines' ::
  -- | the project ID
  Int ->
  GitLab (Either Status [Pipeline])
pipelines' projectId =
  gitlabWithAttrs
    addr
    "&sort=desc" -- most recent first
  where
    addr = RelativeUrl $
      "/projects/"
        <> T.pack (show projectId)
        <> "/pipelines"
