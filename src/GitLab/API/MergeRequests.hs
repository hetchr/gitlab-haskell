{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : MergeRequests
-- Description : Queries about merge requests against projects
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.MergeRequests where

import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Types.Status

-- | returns the merge requests for a project.
mergeRequests ::
  -- | the project
  Project ->
  GitLab [MergeRequest]
mergeRequests p = do
  result <- mergeRequests' (project_id p)
  return (fromRight (error "mergeRequests error") result)

-- | returns the merge requests for a project given its project ID.
mergeRequests' ::
  -- | project ID
  Int ->
  GitLab (Either Status [MergeRequest])
mergeRequests' projectId =
  gitlabWithAttrs addr "&scope=all"
  where
    addr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/merge_requests"

-- | Creates a merge request.
createMergeRequest ::
  -- | project
  Project ->
  -- | source branch
  Text ->
  -- | target branch
  Text ->
  -- | target project ID
  Int ->
  -- | merge request title
  Text ->
  -- | merge request description
  Text ->
  GitLab (Either Status MergeRequest)
createMergeRequest project =
  createMergeRequest' (project_id project)

-- | Creates a merge request.
createMergeRequest' ::
  -- | project ID
  Int ->
  -- | source branch
  Text ->
  -- | target branch
  Text ->
  -- | target project ID
  Int ->
  -- | merge request title
  Text ->
  -- | merge request description
  Text ->
  GitLab (Either Status MergeRequest)
createMergeRequest' projectId sourceBranch targetBranch targetProjectId mrTitle mrDescription =
  gitlabPost addr dataBody
  where
    dataBody :: Text
    dataBody =
      "source_branch=" <> sourceBranch <> "&target_branch=" <> targetBranch
        <> "&target_project_id="
        <> T.pack (show targetProjectId)
        <> "&title="
        <> mrTitle
        <> "&description="
        <> mrDescription
    addr = T.pack $ "/projects/" <> show projectId <> "/merge_requests"

-- | Accepts a merge request.
acceptMergeRequest ::
  -- | project
  Project ->
  -- | merge request IID
  Int ->
  GitLab (Either Status MergeRequest)
acceptMergeRequest project =
  acceptMergeRequest' (project_id project)

-- | Accepts a merge request.
acceptMergeRequest' ::
  -- | project ID
  Int ->
  -- | merge request IID
  Int ->
  GitLab (Either Status MergeRequest)
acceptMergeRequest' projectId mergeRequestIid = gitlabPost addr dataBody
  where
    dataBody :: Text
    dataBody =
      T.pack $
        "id=" <> show projectId <> "&merge_request_iid=" <> show mergeRequestIid
    addr =
      T.pack $
        "/projects/" <> show projectId <> "/merge_requests/"
          <> show mergeRequestIid
          <> "/merge"
