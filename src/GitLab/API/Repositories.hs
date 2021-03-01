{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Repositories
-- Description : Queries about project repositories
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Repositories where

import Control.Monad.IO.Unlift
import qualified Data.ByteString.Lazy as BSL
import Data.Either
import qualified Data.Text as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Types.Status

-- | returns a list of repository files and directories in a project.
repositories ::
  -- | the project
  Project ->
  GitLab [Repository]
repositories project =
  fromRight (error "repositories error") <$> repositories' (project_id project)

-- | returns a list of repository files and directories in a project
-- given its project ID.
repositories' ::
  -- | the project ID
  Int ->
  GitLab (Either Status [Repository])
repositories' projectId =
  gitlab addr
  where
    addr = RelativeUrl $ 
      "/projects/" 
        <> T.pack (show projectId)
        <> "/repository"
        <> "/tree"

-- | get a file archive of the repository files. For example:
--
-- > getFileArchive myProject TarGz "/tmp/myProject.tar.gz"
getFileArchive ::
  -- | project
  Project ->
  -- | file format
  ArchiveFormat ->
  -- | file path to store the archive
  FilePath ->
  GitLab (Either Status ())
getFileArchive project = getFileArchive' (project_id project)

-- | get a file archive of the repository files as a
-- 'BSL.ByteString'. For example:
--
-- > getFileArchiveBS myProject TarGz "/tmp/myProject.tar.gz"
getFileArchiveBS ::
  -- | project
  Project ->
  -- | file format
  ArchiveFormat ->
  GitLab (Either Status BSL.ByteString)
getFileArchiveBS project = getFileArchiveBS' (project_id project)

-- | get a file archive of the repository files using the project's
--   ID. For example:
--
-- > getFileArchive' 3453 Zip "/tmp/myProject.zip"
getFileArchive' ::
  -- | project ID
  Int ->
  -- | file format
  ArchiveFormat ->
  -- | file path to store the archive
  FilePath ->
  GitLab (Either Status ())
getFileArchive' projectId format path = do
  attempt <- getFileArchiveBS' projectId format
  case attempt of
    Left st -> return (Left st)
    Right archiveData ->
      Right <$> liftIO (BSL.writeFile path archiveData)

-- | get a file archive of the repository files as a 'BSL.ByteString'
--   using the project's ID. For example:
--
-- > getFileArchiveBS' 3453 Zip "/tmp/myProject.zip"
getFileArchiveBS' ::
  -- | project ID
  Int ->
  -- | file format
  ArchiveFormat ->
  GitLab (Either Status BSL.ByteString)
getFileArchiveBS' projectId format =
  gitlabReqByteString addr
  where
    addr =
      RelativeUrl $
        "/projects/"
          <> T.pack (show projectId)
          <> "/repository"
          <> "/archive"
          <> T.pack (show format)
