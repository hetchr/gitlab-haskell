{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Issues
-- Description : Queries about issues created against projects
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Issues where

import qualified Data.Aeson as J
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Types.Status

-- | returns all issues against a project.
projectOpenedIssues ::
  -- | the project
  Project ->
  GitLab [Issue]
projectOpenedIssues p = do
  result <- projectOpenedIssues' (project_id p)
  return (fromRight (error "projectOpenedIssues error") result)

-- | returns all issues against a project given its project ID.
projectOpenedIssues' ::
  -- | the project ID
  Int ->
  GitLab (Either Status [Issue])
projectOpenedIssues' projectId = do
  let path = RelativeUrl $ "/projects/" <> T.pack (show projectId) <> "/issues"
  gitlab path

-- gitlabReq path "&state=opened"

-- | gets all issues create by a user.
userIssues ::
  -- | the user
  User ->
  GitLab [Issue]
userIssues usr =
  gitlabWithAttrsUnsafe addr attrs
  where
    addr = RelativeUrl $ "/issues"
    attrs =
      T.pack $
        "&author_id="
          <> show (user_id usr)
          <> "&scope=all"

-- | create a new issue.
newIssue ::
  -- | project
  Project ->
  -- | issue title
  Text ->
  -- | issue description
  Text ->
  GitLab (Either Status (Maybe Issue))
newIssue project =
  newIssue' (project_id project)

-- | create a new issue.
newIssue' ::
  -- | project ID
  Int ->
  -- | issue title
  Text ->
  -- | issue description
  Text ->
  GitLab (Either Status (Maybe Issue))
newIssue' projectId issueTitle issueDescription =
  gitlabPost addr dataBody
  where
    dataBody :: Text
    dataBody =
      "title="
        <> issueTitle
        <> "&description="
        <> issueDescription
    addr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/issues"

-- | edits an issue. see <https://docs.gitlab.com/ee/api/issues.html#edit-issue>
editIssue ::
  ProjectId ->
  IssueId ->
  EditIssueReq ->
  GitLab (Either Status Issue)
editIssue projId issueId editIssueReq = do
  let path = RelativeUrl $
        "/projects/" <> T.pack (show projId)
          <> "/issues/"
          <> T.pack (show issueId)
  gitlabPut
    path
    ( Data.Text.Lazy.toStrict
        ( Data.Text.Lazy.Encoding.decodeUtf8
            (J.encode editIssueReq)
        )
    )
