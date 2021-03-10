{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : GitLab.Types
-- Description : Haskell records corresponding to JSON data from GitLab API calls
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.Types
  ( GitLab,
    GitLabState (..),
    GitLabServerConfig (..),
    defaultGitLabServer,
    ArchiveFormat (..),
    Member (..),
    Namespace (..),
    Reviewer (..),
    Links (..),
    Owner (..),
    Permissions (..),
    ProjectId,
    Project (..),
    ProjectSummary (..),
    ProjectStats (..),
    User (..),
    Milestone (..),
    MilestoneState (..),
    TimeStats (..),
    IssueId,
    Issue (..),
    Pipeline (..),
    Commit (..),
    CommitTodo (..),
    CommitStats (..),
    Tag (..),
    Diff (..),
    Repository (..),
    Job (..),
    Artifact (..),
    Group (..),
    GroupShare (..),
    Branch (..),
    RepositoryFile (..),
    MergeRequest (..),
    Todo (..),
    TodoProject (..),
    TodoAction (..),
    TodoTarget (..),
    TodoState (..),
    Version (..),
    URL,
    EditIssueReq (..),
    Discussion (..),
  )
where

import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import GHC.Generics
import Network.HTTP.Conduit

-- | type synonym for all GitLab actions.
type GitLab a = ReaderT GitLabState IO a

-- | state used by GitLab actions, used internally.
data GitLabState = GitLabState
  { serverCfg :: GitLabServerConfig,
    httpManager :: Manager
  }

-- | configuration data specific to a GitLab server.
data GitLabServerConfig = GitLabServerConfig
  { url :: Text,
    -- | personal access token, see <https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html>
    token :: Text,
    -- | milliseconds
    timeout :: Int,
    -- | how many times to retry a HTTP request before giving up and returning an error.
    retries :: Int,
    -- | write system hook events to files in the system temporary
    -- directory.
    debugSystemHooks :: Bool
  }

-- | default settings, the 'url' and 'token' values will need to be overwritten.
defaultGitLabServer :: GitLabServerConfig
defaultGitLabServer =
  GitLabServerConfig
    { url = "https://gitlab.com",
      token = "",
      timeout = 15000000, -- 15 seconds
      retries = 5,
      debugSystemHooks = False
    }

-- https://docs.gitlab.com/ee/api/repositories.html#get-file-archive
-- tar.gz, tar.bz2, tbz, tbz2, tb2, bz2, tar, and zip

-- | archive format for file archives of repositories.
-- See 'GitLab.API.Repositories.getFileArchive' in 'GitLab.API.Repositories'.
data ArchiveFormat
  = -- | ".tar.gz"
    TarGz
  | -- | ".tar.bz2"
    TarBz2
  | -- | ".tbz"
    Tbz
  | -- | ".tbz2"
    Tbz2
  | -- | ".tb2"
    Tb2
  | -- | ".bz2"
    Bz2
  | -- | ".tar"
    Tar
  | -- | ".zip"
    Zip

instance Show ArchiveFormat where
  show TarGz = ".tar.gz"
  show TarBz2 = ".tar.bz2"
  show Tbz = ".tbz"
  show Tbz2 = ".tbz2"
  show Tb2 = ".tb2"
  show Bz2 = ".bz2"
  show Tar = ".tar"
  show Zip = ".zip"

-- | member of a project.
data Member = Member
  { member_id :: Int,
    member_name :: Text,
    member_username :: Text,
    member_state :: Text,
    member_avatar_uri :: Maybe Text,
    member_web_url :: Maybe Text,
    access_level :: Int,
    expires_at :: Maybe Text
  }
  deriving (Generic, Show)

-- | namespaces.
data Namespace = Namespace
  { namespace_id :: Int,
    namespace_name :: Text,
    namespace_path :: Text,
    kind :: Text,
    full_path :: Text,
    parent_id :: Maybe Int
  }
  deriving (Generic, Show)

-- | links.
data Links = Links
  { self :: Text,
    issues :: Maybe Text,
    merge_requests :: Maybe Text,
    repo_branches :: Text,
    link_labels :: Text,
    link_events :: Text,
    members :: Text
  }
  deriving (Generic, Show)

-- | owners.
data Owner = Ownwer
  { owner_id :: Int,
    owner_name :: Text,
    owner_username :: Text,
    state :: Text,
    owner_avatar_url :: Maybe Text,
    owner_web_url :: Text
  }
  deriving (Generic, Show)

-- | permissions.
data Permissions = Permissions
  { project_access :: Maybe Object,
    group_access :: Maybe Object
  }
  deriving (Generic, Show)

-- | projects.
data Project = Project
  { project_id :: Int,
    description :: Maybe Text,
    project_name :: Text,
    name_with_namespace :: Text,
    project_path :: Text,
    project_path_with_namespace :: Text,
    project_created_at :: Text,
    default_branch :: Maybe Text,
    tag_list :: [Text], -- check
    ssh_url_to_repo :: Text,
    http_url_to_repo :: Text,
    project_web_url :: Text,
    readme_url :: Maybe Text, -- check
    project_avatar_url :: Maybe Text, -- check
    star_count :: Int,
    forks_count :: Int,
    last_activity_at :: Text,
    namespace :: Namespace,
    _links :: Maybe Links,
    archived :: Maybe Bool,
    visibility :: Maybe Text,
    owner :: Maybe Owner,
    resolve_outdated_diff_discussions :: Maybe Bool,
    container_registry_enabled :: Maybe Bool,
    issues_enabled :: Maybe Bool,
    merge_requests_enabled :: Maybe Bool,
    wiki_enabled :: Maybe Bool,
    jobs_enabled :: Maybe Bool,
    snippets_enabled :: Maybe Bool,
    shared_runners_enabled :: Maybe Bool,
    lfs_enabled :: Maybe Bool,
    creator_id :: Maybe Int,
    forked_from_project :: Maybe Project,
    import_status :: Maybe String,
    open_issues_count :: Maybe Int,
    public_jobs :: Maybe Bool,
    ci_config_path :: Maybe Text, -- check null
    shared_with_groups :: Maybe [Object],
    only_allow_merge_if_pipeline_succeeds :: Maybe Bool,
    request_access_enabled :: Maybe Bool,
    only_allow_merge_if_all_discussions_are_resolved :: Maybe Bool,
    printing_merge_request_link_enabled :: Maybe Bool,
    merge_method :: Maybe Text,
    permissions :: Maybe Permissions,
    project_stats :: Maybe ProjectStats
  }
  deriving (Generic, Show)

-- | project summaries.
data ProjectSummary = ProjectSummary
  { summary_project_id :: Int,
    summary_project_name :: Text
  }
  deriving (Generic, Show)

-- | project statistics.
data ProjectStats = ProjectStats
  { commit_count :: Int,
    storage_size :: Int,
    repository_size :: Int,
    wiki_size :: Maybe Int,
    lfs_objects_size :: Maybe Int,
    job_artifacts_size :: Maybe Int,
    packages_size :: Maybe Int
  }
  deriving (Generic, Show)

-- | registered users.
data User = User
  { user_id :: Int,
    user_username :: Text,
    user_name :: Text,
    user_state :: Text,
    user_avatar_uri :: Maybe Text,
    user_web_url :: Maybe Text
  }
  deriving (Generic, Show)

-- | milestone state.
data MilestoneState
  = MSActive
  | MSClosed
  deriving (Show)

instance FromJSON MilestoneState where
  parseJSON (String "active") = return MSActive
  parseJSON (String "closed") = return MSClosed
  parseJSON x = unexpected x

-- | milestones.
data Milestone = Milestone
  { milestone_project_id :: Maybe Int,
    milestone_group_id :: Maybe Int,
    milestone_description :: Text,
    milestone_state :: MilestoneState,
    milestone_due_date :: Maybe UTCTime,
    milestone_iid :: Int,
    milestone_created_at :: Maybe UTCTime,
    milestone_title :: Text,
    milestone_id :: Int,
    milestone_updated_at :: UTCTime,
    milestone_web_url :: URL
  }
  deriving (Generic, Show)

instance FromJSON Milestone where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 10})

-- | time stats.
data TimeStats = TimeStats
  { time_estimate :: Int,
    total_time_spent :: Int,
    human_time_estimate :: Maybe Int,
    human_total_time_spent :: Maybe Int
  }
  deriving (Generic, Show)

-- | alias for project id
type ProjectId = Int

-- | alias for issue id
type IssueId = Int

-- | project issues.
data Issue = Issue
  { issue_state :: Text,
    issue_description :: Maybe Text,
    issue_author :: User,
    milestone :: Maybe Milestone,
    issue_project_id :: ProjectId,
    assignees :: Maybe [User],
    assignee :: Maybe User,
    updated_at :: Text,
    closed_at :: Maybe Text,
    closed_by :: Maybe User,
    issue_id :: IssueId,
    issue_title :: Text,
    issue_created_at :: Text,
    iid :: Int,
    issue_labels :: [Text],
    upvotes :: Int,
    downvotes :: Int,
    user_notes_count :: Int,
    issue_due_date :: Maybe Text,
    issue_web_url :: Text,
    confidential :: Bool,
    weight :: Maybe Text, -- Int?
    discussion_locked :: Maybe Bool,
    time_stats :: TimeStats
  }
  deriving (Generic, Show)

-- | project pipelines
data Pipeline = Pipeline
  { pipeline_id :: Int,
    sha :: Text,
    pipeline_ref :: Text,
    pipeline_status :: Text,
    pipeline_web_url :: Maybe Text
  }
  deriving (Generic, Show)

-- | code commits.
data Commit = Commit
  { commit_id :: Text,
    short_id :: Text,
    title :: Text,
    author_name :: Text,
    author_email :: Text,
    authored_date :: Text,
    committer_name :: Text,
    committer_email :: Text,
    committed_date :: Text,
    commit_created_at :: Text,
    message :: Text,
    commit_parent_ids :: Maybe [String],
    last_pipeline :: Maybe Pipeline,
    commit_stats :: Maybe CommitStats,
    commit_status :: Maybe Text
  }
  deriving (Generic, Show)

-- | summary of a code commit for TODOs.
data CommitTodo = CommitTodo
  { todo_commit_id :: Text,
    todo_commit_short_id :: Text,
    todo_commit_created_at :: Text,
    todo_parent_ids :: Maybe [String]
  }
  deriving (Generic, Show)

-- | commit stats.
data CommitStats = Stats
  { additions :: Int,
    deletions :: Int,
    total :: Int
  }
  deriving (Generic, Show)

-- | tags.
data Tag = Tag
  { tag_commit :: Commit,
    tag_release :: Maybe Release,
    tag_name :: Text,
    tag_target :: Text,
    tag_message :: Maybe Text,
    tag_protected :: Bool
  }
  deriving (Generic, Show)

data Release = Release
  { release_tag_name :: Text,
    release_description :: Text
  }
  deriving (Generic, Show)

-- | diff between two commits.
data Diff = Diff
  { diff :: Text,
    new_path :: Text,
    old_path :: Text,
    a_mode :: Maybe Text,
    b_mode :: Maybe Text,
    new_file :: Bool,
    renamed_file :: Bool,
    deleted_file :: Bool
  }
  deriving (Generic, Show)

-- | repositories.
data Repository = Repository
  { repository_id :: Text,
    repository_name :: Text,
    repository_type :: Text,
    repository_path :: Text,
    mode :: Text
  }
  deriving (Generic, Show)

-- | jobs.
data Job = Job
  { job_commit :: Commit,
    job_coverage :: Maybe Text, -- ?
    job_created_at :: Text,
    job_started_at :: Text,
    job_finished_at :: Text,
    job_duration :: Double,
    job_artifacts_expire_at :: Maybe Text,
    job_id :: Int,
    job_name :: Text,
    job_pipeline :: Pipeline,
    job_ref :: Text,
    job_artifacts :: [Artifact],
    -- , runner :: Maybe Text
    job_stage :: Text,
    job_status :: Text,
    job_tag :: Bool,
    job_web_url :: Text,
    job_user :: User
  }
  deriving (Generic, Show)

-- | artifacts.
data Artifact = Artifact
  { file_type :: Text,
    size :: Int,
    filename :: Text,
    file_format :: Maybe Text
  }
  deriving (Generic, Show)

-- | groups.
data Group = Group
  { group_id :: Int,
    group_name :: Text,
    group_path :: Text,
    group_description :: Text,
    group_visibility :: Text,
    group_lfs_enabled :: Bool,
    group_avatar_url :: Maybe Text,
    group_web_url :: Text,
    group_request_access_enabled :: Bool,
    group_full_name :: Text,
    group_full_path :: Text,
    group_file_template_project_id :: Maybe Int,
    group_parent_id :: Maybe Int
  }
  deriving (Generic, Show)

-- | response to sharing a project with a group.
data GroupShare = GroupShare
  { share_id :: Int,
    share_project_id :: Int,
    share_group_id :: Int,
    share_group_access :: Int,
    share_expires_at :: Maybe Text
  }
  deriving (Generic, Show)

-- | code branches.
data Branch = Branch
  { branch_name :: Text,
    merged :: Bool,
    protected :: Bool,
    branch_default :: Bool,
    developers_can_push :: Bool,
    developers_can_merge :: Bool,
    can_push :: Bool,
    branch_commit :: Commit
  }
  deriving (Generic, Show)

-- | files in a repository.
data RepositoryFile = RepositoryFile
  { repository_file_file_name :: Text,
    repository_file_file_path :: Text,
    repository_file_size :: Int,
    encoding :: Text,
    content :: Text,
    content_sha256 :: Text,
    ref :: Text,
    blob_id :: Text,
    repository_file_commit_id :: Text,
    last_commit_id :: Text
  }
  deriving (Generic, Show)

-- | project merge requests.
data MergeRequest = MergeRequest
  { merge_request_id :: Int,
    merge_request_iid :: Int,
    merge_request_project_id :: Int,
    merge_request_title :: Text,
    merge_request_description :: Text,
    merge_request_state :: Text,
    merge_request_merged_by :: Maybe User,
    merge_request_merged_at :: Maybe Text,
    merge_request_closed_by :: Maybe User,
    merge_request_closed_at :: Maybe Text,
    merge_request_created_at :: Text,
    merge_request_updated_at :: Text,
    merge_request_target_branch :: Text,
    merge_request_source_branch :: Text,
    merge_request_upvotes :: Int,
    merge_request_downvotes :: Int,
    merge_request_author :: User,
    merge_request_assignee :: Maybe User,
    merge_request_reviewers :: Maybe [Reviewer],
    merge_request_source_project_id :: Int,
    merge_request_target_project_id :: Int,
    merge_request_labels :: [Text],
    merge_request_work_in_progress :: Bool,
    merge_request_milestone :: Maybe Milestone,
    merge_request_merge_when_pipeline_succeeds :: Bool,
    merge_request_merge_status :: Text,
    merge_request_sha :: Text,
    merge_request_merge_commit_sha :: Maybe Text,
    merge_request_user_notes_count :: Int,
    merge_request_discussion_locked :: Maybe Bool,
    merge_request_should_remove_source_branch :: Maybe Bool,
    merge_request_force_remove_source_branch :: Maybe Bool,
    merge_request_allow_collaboration :: Maybe Bool,
    merge_request_allow_maintainer_to_push :: Maybe Bool,
    merge_request_web_url :: Text,
    merge_request_time_stats :: TimeStats,
    merge_request_squash :: Bool,
    merge_request_changes_count :: Maybe String,
    merge_request_pipeline :: Maybe Pipeline,
    merge_request_diverged_commits_count :: Maybe Int,
    merge_request_rebase_in_progress :: Maybe Bool,
    merge_request_has_conflicts :: Bool,
    merge_request_blocking_discussions_resolved :: Maybe Bool,
    merge_request_approvals_before_merge :: Maybe Bool -- ?
  }
  deriving (Generic, Show)

data Reviewer = Reviewer
  { reviewerId :: Int,
    reviewerName :: Text,
    reviewerUserName :: Text,
    reviewerState :: Text,
    reviewerAvatarUrl :: Text,
    reviewerWebUrl :: Text
  }
  deriving
    ( Show,
      Eq,
      Generic
    )

{-
    "reviewers": [{
    "id": 2,
    "name": "Sam Bauch",
    "username": "kenyatta_oconnell",
    "state": "active",
    "avatar_url": "https://www.gravatar.com/avatar/956c92487c6f6f7616b536927e22c9a0?s=80&d=identicon",
    "web_url": "http://gitlab.example.com//kenyatta_oconnell"
  }],
-}
instance FromJSON Reviewer where
  parseJSON (Object v) =
    Reviewer
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "username"
      <*> v .: "state"
      <*> v .: "avatar_url"
      <*> v .: "web_url"

-- | TODO actions.
data TodoAction
  = TAAssigned
  | TAMentioned
  | TABuildFailed
  | TAMarked
  | TAApprovalRequired
  | TAUnmergeable
  | TADirectlyAddressed
  deriving (Show)

instance FromJSON TodoAction where
  parseJSON (String "assigned") = return TAAssigned
  parseJSON (String "mentioned") = return TAMentioned
  parseJSON (String "build_failed") = return TABuildFailed
  parseJSON (String "marked") = return TAMarked
  parseJSON (String "approval_required") = return TAApprovalRequired
  parseJSON (String "unmergeable") = return TAUnmergeable
  parseJSON (String "directly_addressed") = return TADirectlyAddressed
  parseJSON x = unexpected x

-- | TODO targets.
data TodoTarget
  = TTIssue Issue
  | TTMergeRequest MergeRequest
  | TTCommit CommitTodo
  deriving (Show)

-- | URL is a synonym for 'Text'.
type URL = Text

-- | TODO states.
data TodoState
  = TSPending
  | TSDone
  deriving (Show)

instance FromJSON TodoState where
  parseJSON (String "pending") = return TSPending
  parseJSON (String "done") = return TSDone
  parseJSON x = unexpected x

-- | A project TODO.
data TodoProject = TP
  { tp_id :: Int,
    tp_description :: Text,
    tp_name :: Text,
    tp_name_with_namespace :: Text,
    tp_path :: Text,
    tp_path_with_namespace :: Text,
    tp_created_at :: UTCTime
  }
  deriving (Generic, Show)

instance FromJSON TodoProject where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 3})

-- | TODOs.
data Todo = Todo
  { todo_id :: Int,
    todo_project :: TodoProject,
    todo_author :: User,
    todo_action_name :: TodoAction,
    todo_target :: TodoTarget,
    todo_target_url :: URL,
    todo_body :: Text,
    todo_state :: TodoState,
    todo_created_at :: UTCTime
  }
  deriving (Show)

instance FromJSON Todo where
  parseJSON = withObject "Todo" $ \v ->
    Todo
      <$> v .: "id"
      <*> v .: "project"
      <*> v .: "author"
      <*> v .: "action_name"
      <*> ( v .: "target_type" >>= \case
              "MergeRequest" -> TTMergeRequest <$> v .: "target"
              "Issue" -> TTIssue <$> v .: "target"
              "Commit" -> TTCommit <$> v .: "target"
              (_ :: Text) -> fail ""
          )
      <*> v .: "target_url"
      <*> v .: "body"
      <*> v .: "state"
      <*> v .: "created_at"

-- | version of the GitLab instance.
data Version = Version
  { version :: Text,
    revision :: Text
  }
  deriving (Generic, Show)

-- | An edit issue request.
data EditIssueReq = EditIssueReq
  { edit_issue_id :: ProjectId,
    edit_issue_issue_iid :: IssueId,
    edit_issue_title :: Maybe Text,
    edit_issue_description :: Maybe Text,
    edit_issue_confidential :: Maybe Bool,
    edit_issue_assignee_ids :: Maybe [Int],
    edit_issue_milestone_id :: Maybe Int,
    edit_issue_labels :: Maybe [Text],
    edit_issue_state_event :: Maybe Text,
    edit_issue_updated_at :: Maybe Text,
    edit_issue_due_date :: Maybe Text,
    edit_issue_weight :: Maybe Int,
    edit_issue_discussion_locked :: Maybe Bool,
    edit_issue_epic_id :: Maybe Int,
    edit_issue_epic_iid :: Maybe Int
  }
  deriving (Generic, Show)

-- | Discussions https://docs.gitlab.com/ee/api/discussions.html
data Discussion = Discussion
  { discussion_id :: Text,
    discussion_individual_note :: Bool,
    discussion_notes :: [Note]
  }
  deriving (Generic, Show)

-- | Notes
data Note = Note
  { note_id :: Int,
    -- https://docs.gitlab.com/ee/api/discussions.html#list-project-commit-discussion-items
    note_type :: Maybe Text, -- TODO create type for this, e.g. from "DiscussionNote"
    note_body :: Text,
    note_attachment :: Maybe Text,
    note_author :: Owner,
    --  -- TODO parse these as date type
    note_created_at :: Text,
    note_updated_at :: Text,
    note_system :: Bool,
    note_noteable_id :: Maybe Int,
    note_noteable_type :: Maybe Text, -- create type e.g. from "Commit"
    note_noteable_iid :: Maybe Int,
    note_resolvable :: Bool
  }
  deriving (Generic, Show)

-----------------------------
-- JSON GitLab parsers below
-----------------------------

bodyNoPrefix :: String -> String
bodyNoPrefix "commit_created_at" = "created_at"
bodyNoPrefix "commit_id" = "id"
bodyNoPrefix "commit_status" = "status"
bodyNoPrefix "commit_parent_ids" = "parent_ids"
bodyNoPrefix "todo_commit_id" = "id"
bodyNoPrefix "todo_commit_short_id" = "short_id"
bodyNoPrefix "todo_commit_created_at" = "created_at"
bodyNoPrefix "todo_parent_ids" = "parent_ids"
bodyNoPrefix "issue_author" = "author"
bodyNoPrefix "issue_created_at" = "created_at"
bodyNoPrefix "issue_description" = "description"
bodyNoPrefix "issue_due_date" = "due_date"
bodyNoPrefix "issue_id" = "id"
bodyNoPrefix "issue_labels" = "labels"
bodyNoPrefix "issue_project_id" = "project_id"
bodyNoPrefix "issue_state" = "state"
bodyNoPrefix "issue_title" = "title"
bodyNoPrefix "issue_web_url" = "web_url"
bodyNoPrefix "link_events" = "events"
bodyNoPrefix "link_labels" = "labels"
bodyNoPrefix "member_avatar_url" = "avatar_url"
bodyNoPrefix "member_id" = "id"
bodyNoPrefix "member_name" = "name"
bodyNoPrefix "member_state" = "state"
bodyNoPrefix "member_username" = "username"
bodyNoPrefix "member_web_url" = "we_url"
bodyNoPrefix "namespace_id" = "id"
bodyNoPrefix "namespace_name" = "name"
bodyNoPrefix "namespace_path" = "path"
bodyNoPrefix "owner_avatar_url" = "avatar_url"
bodyNoPrefix "owner_id" = "id"
bodyNoPrefix "owner_name" = "name"
bodyNoPrefix "owner_username" = "username"
bodyNoPrefix "owner_web_url" = "web_url"
bodyNoPrefix "pipeline_id" = "id"
bodyNoPrefix "pipeline_status" = "status"
bodyNoPrefix "pipeline_web_url" = "web_url"
bodyNoPrefix "project_avatar_url" = "avatar_url"
bodyNoPrefix "project_created_at" = "created_at"
bodyNoPrefix "project_id" = "id"
bodyNoPrefix "project_name" = "name"
bodyNoPrefix "project_path" = "path"
bodyNoPrefix "project_path_with_namespace" = "path_with_namespace"
bodyNoPrefix "project_web_url" = "web_url"
bodyNoPrefix "summary_project_id" = "id"
bodyNoPrefix "summary_project_name" = "name"
bodyNoPrefix "repository_id" = "id"
bodyNoPrefix "repository_name" = "name"
bodyNoPrefix "repository_path" = "path"
bodyNoPrefix "repository_type" = "type"
bodyNoPrefix "user_avatar_uri" = "avatar_url"
bodyNoPrefix "user_id" = "id"
bodyNoPrefix "user_name" = "name"
bodyNoPrefix "user_state" = "state"
bodyNoPrefix "user_username" = "username"
bodyNoPrefix "user_web_url" = "web_url"
bodyNoPrefix "event_title" = "title"
bodyNoPrefix "event_project_id" = "project_id"
bodyNoPrefix "pipeline_ref" = "ref"
bodyNoPrefix "branch_name" = "name"
bodyNoPrefix "branch_default" = "default"
bodyNoPrefix "branch_commit" = "commit"
bodyNoPrefix "repository_file_file_name" = "file_name"
bodyNoPrefix "repository_file_file_path" = "file_path"
bodyNoPrefix "repository_file_size" = "size"
bodyNoPrefix "repository_file_commit_id" = "commit_id"
bodyNoPrefix "merge_request_id" = "id"
bodyNoPrefix "merge_request_iid" = "iid"
bodyNoPrefix "merge_request_project_id" = "project_id"
bodyNoPrefix "merge_request_title" = "title"
bodyNoPrefix "merge_request_description" = "description"
bodyNoPrefix "merge_request_state" = "state"
bodyNoPrefix "merge_request_merged_by" = "merged_by"
bodyNoPrefix "merge_request_merged_at" = "merged_at"
bodyNoPrefix "merge_request_closed_by" = "closed_by"
bodyNoPrefix "merge_request_closed_at" = "closed_at"
bodyNoPrefix "merge_request_created_at" = "created_at"
bodyNoPrefix "merge_request_updated_at" = "updated_at"
bodyNoPrefix "merge_request_target_branch" = "target_branch"
bodyNoPrefix "merge_request_source_branch" = "source_branch"
bodyNoPrefix "merge_request_upvotes" = "upvotes"
bodyNoPrefix "merge_request_downvotes" = "downvotes"
bodyNoPrefix "merge_request_author" = "author"
bodyNoPrefix "merge_request_assignee" = "assignee"
bodyNoPrefix "merge_request_source_project_id" = "source_project_id"
bodyNoPrefix "merge_request_target_project_id" = "target_project_id"
bodyNoPrefix "merge_request_labels" = "labels"
bodyNoPrefix "merge_request_work_in_progress" = "work_in_progress"
bodyNoPrefix "merge_request_milestone" = "milestone"
bodyNoPrefix "merge_request_merge_when_pipeline_succeeds" = "merge_when_pipeline_succeeds"
bodyNoPrefix "merge_request_merge_status" = "merge_status"
bodyNoPrefix "merge_request_sha" = "sha"
bodyNoPrefix "merge_request_merge_commit_sha" = "merge_commit_sha"
bodyNoPrefix "merge_request_user_notes_count" = "user_notes_count"
bodyNoPrefix "merge_request_discussion_locked" = "discussion_locked"
bodyNoPrefix "merge_request_should_remove_source_branch" = "should_remove_source_branch"
bodyNoPrefix "merge_request_force_remove_source_branch" = "force_remove_source_branch"
bodyNoPrefix "merge_request_allow_collaboration" = "allow_collaboration"
bodyNoPrefix "merge_request_allow_maintainer_to_push" = "allow_maintainer_to_push"
bodyNoPrefix "merge_request_web_url" = "web_url"
bodyNoPrefix "merge_request_time_stats" = "time_stats"
bodyNoPrefix "merge_request_squash" = "squash"
bodyNoPrefix "merge_request_approvals_before_merge" = "approvals_before_merge"
bodyNoPrefix "merge_request_allow_contribution" = "allow_contribution"
bodyNoPrefix "merge_request_changes_count" = "changes_count"
bodyNoPrefix "merge_request_pipeline" = "pipeline"
bodyNoPrefix "merge_request_diverged_commits_count" = "diverged_commits_count"
bodyNoPrefix "merge_request_rebase_in_progress" = "rebase_in_progress"
bodyNoPrefix "merge_request_has_conflicts" = "has_conflicts"
bodyNoPrefix "merge_request_blocking_discussions_resolved" = "blocking_discussions_resolved"
bodyNoPrefix "project_stats" = "statistics"
bodyNoPrefix "commit_stats" = "stats"
bodyNoPrefix "share_id" = "id"
bodyNoPrefix "share_project_id" = "project_id"
bodyNoPrefix "share_group_id" = "group_id"
bodyNoPrefix "share_group_access" = "group_access"
bodyNoPrefix "share_expires_at" = "expires_at"
bodyNoPrefix "group_id" = "id"
bodyNoPrefix "group_name" = "name"
bodyNoPrefix "group_path" = "path"
bodyNoPrefix "group_description" = "description"
bodyNoPrefix "group_visibility" = "visibility"
bodyNoPrefix "group_lfs_enabled" = "lfs_enabled"
bodyNoPrefix "group_avatar_url" = "avatar_url"
bodyNoPrefix "group_web_url" = "web_url"
bodyNoPrefix "group_request_access_enabled" = "request_access_enabled"
bodyNoPrefix "group_full_name" = "full_name"
bodyNoPrefix "group_full_path" = "full_path"
bodyNoPrefix "group_file_template_project_id" = "file_template_project_id"
bodyNoPrefix "group_parent_id" = "parent_id"
bodyNoPrefix "job_commit" = "commit"
bodyNoPrefix "job_coverage" = "coverage"
bodyNoPrefix "job_created_at" = "created_at"
bodyNoPrefix "job_started_at" = "started_at"
bodyNoPrefix "job_finished_at" = "finished_at"
bodyNoPrefix "job_duration" = "duration"
bodyNoPrefix "job_artifacts_expire_at" = "artifacts_expire_at"
bodyNoPrefix "job_id" = "id"
bodyNoPrefix "job_name" = "name"
bodyNoPrefix "job_pipeline" = "pipeline"
bodyNoPrefix "job_ref" = "ref"
bodyNoPrefix "job_artifacts" = "artifacts"
bodyNoPrefix "job_stage" = "stage"
bodyNoPrefix "job_status" = "status"
bodyNoPrefix "job_tag" = "tag"
bodyNoPrefix "job_web_url" = "web_url"
bodyNoPrefix "job_user" = "user"
bodyNoPrefix "discussion_id" = "id"
bodyNoPrefix "discussion_individual_note" = "individual_note"
bodyNoPrefix "discussion_notes" = "notes"
bodyNoPrefix "note_id" = "id"
bodyNoPrefix "note_type" = "type"
bodyNoPrefix "note_body" = "body"
bodyNoPrefix "note_attachment" = "attachment"
bodyNoPrefix "note_author" = "author"
bodyNoPrefix "note_created_at" = "created_at"
bodyNoPrefix "note_updated_at" = "updated_at"
bodyNoPrefix "note_system" = "system"
bodyNoPrefix "note_noteable_id" = "noteable_id"
bodyNoPrefix "note_noteable_type" = "noteable_type"
bodyNoPrefix "note_noteable_iid" = "iid"
bodyNoPrefix "note_resolvable" = "resolvable"
-- TODO field names for Issues data type
bodyNoPrefix s = s

-- TODO refactor bodyNoPrefix function above into smaller
--    String -> String
-- functions like those below.

tagPrefix :: String -> String
tagPrefix "tag_commit" = "commit"
tagPrefix "tag_release" = "release"
tagPrefix "tag_name" = "name"
tagPrefix "tag_target" = "target"
tagPrefix "tag_message" = "message"
tagPrefix "tag_protected" = "protected"
tagPrefix s = s

releasePrefix :: String -> String
releasePrefix "release_tag_name" = "tag_name"
releasePrefix "release_description" = "description"
releasePrefix s = s

instance FromJSON TimeStats where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON Issue where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON User where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON Commit where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON CommitTodo where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON Tag where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = tagPrefix
          }
      )

instance FromJSON Release where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = releasePrefix
          }
      )

instance FromJSON CommitStats where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON Pipeline where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON Member where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON Permissions where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON Owner where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON Links where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON Namespace where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON Project where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON ProjectSummary where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON ProjectStats where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON Repository where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON Job where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON Artifact where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON Group where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON GroupShare where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON Branch where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON RepositoryFile where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON MergeRequest where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON Diff where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON Version where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance ToJSON EditIssueReq where
  toEncoding =
    genericToEncoding
      defaultOptions
        { fieldLabelModifier = drop (T.length "edit_issue_"),
          omitNothingFields = True
        }

instance FromJSON Discussion where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )

instance FromJSON Note where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = bodyNoPrefix
          }
      )
