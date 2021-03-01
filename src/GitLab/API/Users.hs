{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Users
-- Description : Queries about registered users
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.API.Users where

import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls

-- | all registered users.
allUsers :: GitLab [User]
allUsers = do
  let path = RelativeUrl "/users"
  gitlabUnsafe path

-- | searches for a user given a user ID. Returns @Just User@ if the
-- user is found, otherwise @Nothing@.
userId ::
  -- | username to search for
  Int ->
  GitLab (Maybe User)
userId usrId = do
  let path =
        RelativeUrl $
          "/users/"
            <> T.pack (show usrId)
  res <- gitlabOne path
  case res of
    Left _err -> return Nothing
    Right Nothing -> return Nothing
    Right (Just user) -> return (Just user)

-- | searches for a user given a username. Returns @Just User@ if the
-- user is found, otherwise @Nothing@.
searchUser ::
  -- | username to search for
  Text ->
  GitLab (Maybe User)
searchUser username = do
  let path = RelativeUrl "/users"
      attrs = "&username=" <> username
  res <- gitlabWithAttrsUnsafe path attrs
  case res of
    [] -> return Nothing
    (user : _) -> return (Just user)

-- | searches for users given a list of usernames, returns them in
-- alphabetical order of their usernames.
orderedUsers ::
  -- | usernames to search for
  [Text] ->
  GitLab [User]
orderedUsers usernames = do
  users <- catMaybes <$> mapM searchUser usernames
  return (orderUsersByName users)
  where
    orderUsersByName :: [User] -> [User]
    orderUsersByName =
      sortBy (\u1 u2 -> compare (user_name u1) (user_name u2))
