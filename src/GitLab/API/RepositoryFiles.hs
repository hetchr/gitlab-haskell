{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : RepositoryFiles
-- Description : Queries about project repository files
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.RepositoryFiles where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Types.Status
import Network.HTTP.Types.URI

-- | Get a list of repository files and directories in a project.
repositoryFiles ::
  -- | the project
  Project ->
  -- | the file path
  Text ->
  -- | name of the branch, tag or commit
  Text ->
  GitLab (Either Status (Maybe RepositoryFile))
repositoryFiles project = repositoryFiles' (project_id project)

-- | Get a list of repository files and directories in a project given
-- the project's ID.
repositoryFiles' ::
  -- | project ID
  Int ->
  -- | the file path
  Text ->
  -- | name of the branch, tag or commit
  Text ->
  GitLab (Either Status (Maybe RepositoryFile))
repositoryFiles' projectId filePath reference =
  gitlabWithAttrsOne addr ("&ref=" <> reference)
  where
    addr =
      RelativeUrl $
        "/projects/"
          <> T.pack (show projectId)
          <> "/repository"
          <> "/files"
          <> "/"
          <> T.decodeUtf8 (urlEncode False (T.encodeUtf8 filePath))

-- | Get raw data for a given file blob hash.
repositoryFileBlob ::
  -- | project ID
  Int ->
  -- | blob SHA
  Text ->
  GitLab (Either Status String)
repositoryFileBlob projectId blobSha =
  gitlabReqText addr
  where
    addr =
      RelativeUrl $
        "/projects/"
          <> T.pack (show projectId)
          <> "/repository"
          <> "/blobs/"
          <> blobSha
          <> "/raw"
