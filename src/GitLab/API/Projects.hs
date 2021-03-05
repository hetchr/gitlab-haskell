{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Projects
-- Description : Queries about projects
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Projects where

import Data.Either
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GitLab.API.Commits
import GitLab.API.Issues
import GitLab.API.Members
import GitLab.API.Pipelines
import GitLab.API.Users
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls
import Network.HTTP.Types.Status
import Network.HTTP.Types.URI
import UnliftIO.Async

-- | gets all projects.
allProjects :: GitLab [Project]
allProjects =
  gitlabWithAttrsUnsafe (RelativeUrl "/projects") "&statistics=true"

allMaintainableProjectsSummary :: GitLab [ProjectSummary]
allMaintainableProjectsSummary =
  gitlabWithAttrsUnsafe (RelativeUrl "/projects") "&min_access_level=40&membership=true"

-- | gets all forks of a project. Supports use of namespaces.
--
-- > projectForks "project1"
-- > projectForks "group1/project1"
projectForks ::
  -- | name or namespace of the project
  Text ->
  GitLab (Either Status [Project])
projectForks projectName = do
  let urlPath =
        RelativeUrl $
          "/projects/"
            <> T.decodeUtf8 (urlEncode False (T.encodeUtf8 projectName))
            <> "/forks"
  gitlab urlPath

-- | searches for a 'Project' with the given project ID, returns
-- 'Nothing' if a project with the given ID is not found.
searchProjectId ::
  -- | project ID
  Int ->
  GitLab (Either Status (Maybe Project))
searchProjectId projectId = do
  let urlPath = RelativeUrl $ T.pack ("/projects/" <> show projectId)
  gitlabWithAttrsOne urlPath "&statistics=true"

-- | gets all projects with the given project name.
--
-- > projectsWithName "project1"
projectsWithName ::
  -- | project name being searched for.
  Text ->
  GitLab [Project]
projectsWithName projectName =
  filter (\project -> projectName == project_path project)
    <$> gitlabWithAttrsUnsafe (RelativeUrl "/projects") ("&search=" <> projectName)

-- | gets a project with the given name for the given username. E.g.
--
-- > projectsNamespaceName "user1" "project1"
--
-- looks for "user1/project1"
projectsWithNameAndUser :: Text -> Text -> GitLab (Either Status (Maybe Project))
projectsWithNameAndUser username projectName =
  gitlabWithAttrsOne
    ( RelativeUrl $
        "/projects/"
          <> T.decodeUtf8
            (urlEncode False (T.encodeUtf8 (username <> "/" <> projectName)))
    )
    "&statistics=true"

-- | returns 'True' if a project has multiple committers, according to
-- the email addresses of the commits.
multipleCommitters :: Project -> GitLab Bool
multipleCommitters project = do
  emailAddresses <- commitsEmailAddresses project
  return (length (nub emailAddresses) > 1)

-- | gets the email addresses in the author information in all commit
-- for a project.
commitsEmailAddresses :: Project -> GitLab [Text]
commitsEmailAddresses project = do
  result <- commitsEmailAddresses' (project_id project)
  return (fromRight (error "commitsEmailAddresses error") result)

-- | gets the email addresses in the author information in all commit
-- for a project defined by the project's ID.
commitsEmailAddresses' :: Int -> GitLab (Either Status [Text])
commitsEmailAddresses' projectId = do
  -- (commits :: [Commit]) <- projectCommits' projectId
  attempt <- projectCommits' projectId
  case attempt of
    Left httpStatus -> return (Left httpStatus)
    Right (commits :: [Commit]) ->
      return (Right (map author_email commits))

-- | gets all projects for a user given their username.
--
-- > userProjects "harry"
userProjects' :: Text -> GitLab (Maybe [Project])
userProjects' username = do
  userMaybe <- searchUser username
  case userMaybe of
    Nothing -> return Nothing
    Just usr -> Just <$> gitlabUnsafe (urlPath (user_id usr))
  where
    urlPath usrId = RelativeUrl $ "/users/" <> T.pack (show usrId) <> "/projects"

-- | gets all projects for a user.
--
-- > userProjects myUser
userProjects :: User -> GitLab (Maybe [Project])
userProjects theUser =
  userProjects' (user_username theUser)

-- | gets the 'GitLab.Types.Project' against which the given 'Issue'
-- was created.
projectOfIssue :: Issue -> GitLab Project
projectOfIssue issue = do
  result <- searchProjectId (issue_project_id issue)
  case fromRight (error "projectOfIssue error") result of
    Nothing -> error "projectOfIssue error"
    Just proj -> return proj

-- | finds all issues created by a user.
--
-- > issuesCreatedByUser "user1"
--
-- returns a (user,projects) tuple, where user is the 'User' found
-- for the given searched username, and a list of 'Project's that the
-- user has created issues for.
issuesCreatedByUser :: Text -> GitLab (Maybe (User, [Project]))
issuesCreatedByUser username = do
  user_maybe <- searchUser username
  case user_maybe of
    Nothing -> return Nothing
    Just usr -> do
      usersIssues <- userIssues usr
      projects <- mapConcurrently projectOfIssue usersIssues
      return (Just (usr, projects))

-- | searches for all projects with the given name, and returns a list
-- of triples of: 1) the found project, 2) the list of issues for the
-- found projects, and 3) a list of users who've created issues.
issuesOnForks ::
  -- | name or namespace of the project
  Text ->
  GitLab [(Project, [Issue], [User])]
issuesOnForks projectName = do
  projects <- projectsWithName projectName
  mapM processProject projects
  where
    processProject ::
      Project ->
      GitLab (Project, [Issue], [User])
    processProject proj = do
      (openIssues :: [Issue]) <- projectOpenedIssues proj
      let authors = map issue_author openIssues
      return (proj, openIssues, authors)

-- | returns a (namespace,members) tuple for the given 'Project',
-- where namespace is the namespace of the project
-- e.g. "user1/project1", and members is a list of (username,name)
-- tuples about all members of the project.
projectMemebersCount :: Project -> GitLab (Text, [(Text, Text)])
projectMemebersCount project = do
  friends <- count
  return (namespace_name (namespace project), friends)
  where
    count = do
      let addr =
            RelativeUrl $
              "/projects/" <> T.pack (show (project_id project)) <> "/members/all"
      (res :: [Member]) <- gitlabUnsafe addr
      return (map (\x -> (member_username x, member_name x)) res)

-- | returns 'True' is the last commit for a project passes all
-- continuous integration tests.
projectCISuccess ::
  -- | the name or namespace of the project
  Project ->
  GitLab Bool
projectCISuccess project = do
  pipes <- pipelines project
  case pipes of
    [] -> return False
    (x : _) -> return (pipeline_status x == "success")

-- | searches for a username, and returns a user ID for that user, or
-- 'Nothing' if a user cannot be found.
namespacePathToUserId ::
  -- | name or namespace of project
  Text ->
  GitLab (Maybe Int)
namespacePathToUserId namespacePath = do
  user_maybe <- searchUser namespacePath
  case user_maybe of
    Nothing -> return Nothing
    Just usr -> return (Just (user_id usr))

-- | gets all diffs in a project for a given commit SHA.
projectDiffs :: Project -> Text -> GitLab (Either Status [Diff])
projectDiffs proj =
  projectDiffs' (project_id proj)

-- | gets all diffs in a project for a given project ID, for a given
-- commit SHA.
projectDiffs' :: Int -> Text -> GitLab (Either Status [Diff])
projectDiffs' projId commitSha =
  gitlab
    ( RelativeUrl $
        "/projects/"
          <> T.pack (show projId)
          <> "/repository/commits/"
          <> commitSha
          <> "/diff/"
    )

-- | add a group to a project.
addGroupToProject ::
  -- | group ID
  Int ->
  -- | project ID
  Int ->
  -- | level of access granted
  AccessLevel ->
  GitLab (Either Status (Maybe GroupShare))
addGroupToProject groupId projectId access =
  gitlabPost addr dataBody
  where
    dataBody :: Text
    dataBody =
      "group_id="
        <> T.pack (show groupId)
        <> "&group_access="
        <> T.pack (show access)
    addr =
      "/projects/"
        <> T.pack (show projectId)
        <> "/share"
