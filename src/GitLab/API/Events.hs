{-
 This module is not currently being used.
-}

module GitLab.API.Events () where

import qualified Data.Text as T
import GitLab.Types

-- | Get a list of events.
issueEvents :: GitLab [Issue]
issueEvents = undefined

-- gitlabWithAttrs addr attrs
-- where
--   addr =
--     "/events"
--   attrs =
--     "&target_type=issue"
