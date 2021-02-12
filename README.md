# A Haskell library for the GitLab web API

### /!\ This is a fork of [gitlab-haskell](https://gitlab.com/robstewart57/gitlab-haskell)

This library interacts with a GitLab server's API. It supports queries
about and updates to:

* Branches
* Commits
* Groups
* Issues
* Jobs
* Members
* Merge requests
* Pipelines
* Projects
* Repositories
* Repository files
* Users
* Discussions

The library parses JSON results into Haskell data types in the
`GitLab.Types` module, allowing you to work with statically typed
GitLab data with data types and functions that the library
provides. E.g.

    searchUser     :: Text -> GitLab (Maybe User)
    userProjects   :: User -> GitLab (Maybe [Project])
    projectCommits :: Project -> GitLab [Commit]

## Example

Run all GitLab actions with `runGitLab`: 

    runGitLab ::
       => GitLabServerConfig
       -> GitLab a
       -> IO a

For example:

    myProjects <-
      runGitLab
        (defaultGitLabServer
           { url = "https://gitlab.example.com"
           , token="my_token"} )
        (searchUser "joe" >>= userProjects . fromJust)

This library can also be used to implement rule based GitLab file
system hooks that, when deployed a GitLab server, react in real time
to GitLab events like project creation, new users, merge requests etc.

The rule based API for implementing file hooks is:

    receive :: [Rule] -> GitLab ()

    class (FromJSON a) => SystemHook a where
      match   :: String -> (a -> GitLab ()) -> Rule
      matchIf :: String -> (a -> GitLab Bool) -> (a -> GitLab ()) -> Rule

For more details about the file system hooks support, see post:
[GitLab automation with file hook rules](https://www.macs.hw.ac.uk/~rs46/posts/2020-06-06-gitlab-system-hooks.html).

For the complete `gitlab-haskell` API, see the [hackage documentation](https://hackage.haskell.org/package/gitlab-haskell).

The `gitlab-tools` command line tool for bulk GitLab transactions uses
this library [link](https://gitlab.com/robstewart57/gitlab-tools).
