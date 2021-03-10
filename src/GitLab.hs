{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : GitLab
-- Description : Contains the 'runGitLab' function to run GitLab actions
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab
  ( runGitLab,
    runGitLabDbg,
    runGitLabWithManager,
    module GitLab.Types,
    module GitLab.API.Pipelines,
    module GitLab.API.Groups,
    module GitLab.API.Members,
    module GitLab.API.Commits,
    module GitLab.API.Projects,
    module GitLab.API.Users,
    module GitLab.API.Issues,
    module GitLab.API.Branches,
    module GitLab.API.Jobs,
    module GitLab.API.MergeRequests,
    module GitLab.API.Repositories,
    module GitLab.API.RepositoryFiles,
    module GitLab.API.Tags,
    module GitLab.API.Todos,
    module GitLab.API.Version,
    module GitLab.API.Notes,
    module GitLab.API.Discussions,
    module GitLab.SystemHooks.GitLabSystemHooks,
    module GitLab.SystemHooks.Types,
    module GitLab.SystemHooks.Rules,
  )
where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import GitLab.API.Branches
import GitLab.API.Commits
import GitLab.API.Discussions
import GitLab.API.Groups
import GitLab.API.Issues
import GitLab.API.Jobs
import GitLab.API.Members
import GitLab.API.MergeRequests
import GitLab.API.Notes
import GitLab.API.Pipelines
import GitLab.API.Projects
import GitLab.API.Repositories
import GitLab.API.RepositoryFiles
import GitLab.API.Tags
import GitLab.API.Todos
import GitLab.API.Users
import GitLab.API.Version
import GitLab.SystemHooks.GitLabSystemHooks
import GitLab.SystemHooks.Rules
import GitLab.SystemHooks.Types
import GitLab.Types
import Network.Connection (TLSSettings (..))
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import System.IO

-- | runs a GitLab action.
--
-- Internally, this creates a single 'Manager', whichs keeps track of
-- open connections for keep-alive and which is shared between
-- multiple threads and requests.
--
-- An example of its use is:
--
-- > projectsWithIssuesEnabled :: IO [Project]
-- > projectsWithIssuesEnabled =
-- >   runGitLabyConfig $ filter (issueEnabled . issues_enabled) <$> allProjects
-- >   where
-- >     myConfig = defaultGitLabServer
-- >         { url = "https://gitlab.example.com"
-- >         , token = "my_access_token" }
-- >     issueEnabled Nothing = False
-- >     issueEnabled (Just b) = b
runGitLab :: GitLabServerConfig -> GitLab a -> IO a
runGitLab cfg action = do
  liftIO $ hSetBuffering stdout LineBuffering
  let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
  manager <- liftIO $ newManager settings
  runGitLabWithManager manager cfg action

-- | The same as 'runGitLab', except that it also takes a connection
-- manager as an argument.
runGitLabWithManager :: Manager -> GitLabServerConfig -> GitLab a -> IO a
runGitLabWithManager manager cfg action = do
  -- test the token access
  tokenTest <- runReaderT gitlabVersion (GitLabState cfg manager)
  case tokenTest of
    Left (Status 401 "Unauthorized") -> error "access token not accepted."
    Left st -> error ("unexpected HTTP status: " <> show st)
    Right _versionInfo ->
      -- it worked, run the user code.
      runReaderT action (GitLabState cfg manager)

-- | Only useful for testing GitLab actions that lift IO actions with
-- liftIO. Cannot speak to a GitLab server. Only useful for the
-- gitlab-haskell tests.
runGitLabDbg :: GitLab a -> IO a
runGitLabDbg action = do
  liftIO $ hSetBuffering stdout LineBuffering
  runReaderT action undefined
