{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Todos
Description : Queries about todos for users.
Copyright   : (c) Rob Stewart, Heriot-Watt University, 2019
License     : BSD3
Maintainer  : robstewart57@gmail.com
Stability   : stable
-}
module GitLab.API.Todos where

import Control.Monad.IO.Class
import GitLab.Types
import GitLab.WebRequests.GitLabWebCalls

-- | returns all pending todos for the user, as defined by the access token.
todos :: (MonadIO m) => GitLab m [Todo]
todos = gitlab "/todos"
