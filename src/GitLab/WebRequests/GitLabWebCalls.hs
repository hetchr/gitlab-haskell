{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | internal module to support modules in GitLab.API
module GitLab.WebRequests.GitLabWebCalls
  ( gitlab,
    gitlabUnsafe,
    gitlabWithAttrs,
    gitlabWithAttrsUnsafe,
    gitlabOne,
    gitlabWithAttrsOne,
    gitlabReqJsonOne,
    gitlabPost,
    gitlabPut,
    gitlabDelete,
    gitlabReqText,
    gitlabReqByteString,
    --------------------------------------------- NEW
    PostResult (..),
    PutResult (..),
    GetResult (..),
    RelativeUrl (..),
    gitlabReqJsonManyBuilder,
    gitlabPutBuilder',
    gitlabPostBuilder', -- this is the new one
    buildFields,
  )
where

import qualified Control.Exception as Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.Aeson.Text as A
import Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Text (Text)
import Data.Either
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as T
import GitLab.Types
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Network.HTTP.Types.URI
import Text.Read

data GetResult a
  = GHttpFailure Status
  | GRequestSuccess a
  deriving
    (Eq, Show)

data PostResult a
  = NoResponse
  | HttpFailure Status
  | RequestSuccess a
  deriving
    (Eq, Show)

data PutResult a
  = PutIgnoringPayload
  | PutFailure Status
  | PutRequestSuccess a

-- this could just be called path part of the url
-- this can be turned into an smart constructor eventually
newtype RelativeUrl = RelativeUrl {relativeUrl :: Text}

type BodyBuilder = [(Text, A.Value)]

type TextBody = Text

type TextAttrs = Text

newtype GitLabException = GitLabException String
  deriving (Eq, Show)

instance Exception.Exception GitLabException

-- In this module, "unsafe" functions are those that discard HTTP
-- error code responses, e.g. 404, 409.

gitlabDelete ::
  -- | the URL to post to
  RelativeUrl ->
  GitLab (Either Status ())
gitlabDelete urlPath = do
  cfg <- serverCfg <$> ask
  manager <- httpManager <$> ask
  let url' = url cfg <> "/api/v4" <> relativeUrl urlPath
  let request' = parseRequest_ (T.unpack url')
      request =
        request'
          { method = "DELETE",
            requestHeaders =
              [ ("PRIVATE-TOKEN", T.encodeUtf8 (token cfg)),
                ("content-type", "application/json")
              ],
            requestBody = RequestBodyBS BS.empty
          }
  resp <- liftIO $ tryGitLab 0 request (retries cfg) manager Nothing
  if successStatus (responseStatus resp)
    then return (Right ())
    else return (Left (responseStatus resp))

tryGitLab ::
  -- | the current retry count
  Int ->
  -- | the GitLab request
  Request ->
  -- | maximum number of retries permitted
  Int ->
  -- | HTTP manager
  Manager ->
  -- | the exception to report if maximum retries met
  Maybe HttpException ->
  IO (Response BSL.ByteString)
tryGitLab i request maxRetries manager lastException
  | i == maxRetries = error (show lastException)
  | otherwise =
    httpLbs request manager
      `Exception.catch` \ex -> tryGitLab (i + 1) request maxRetries manager (Just ex)

gitlabPut ::
  FromJSON b =>
  -- | the URL to post to
  RelativeUrl ->
  -- | the data to post
  TextBody ->
  GitLab (Either Status b)
gitlabPut urlPath dataBody = do
  cfg <- serverCfg <$> ask
  manager <- httpManager <$> ask
  let url' = url cfg <> "/api/v4" <> relativeUrl urlPath
  let request' = parseRequest_ (T.unpack url')
      request =
        request'
          { method = "PUT",
            requestHeaders =
              [ ("PRIVATE-TOKEN", T.encodeUtf8 (token cfg)),
                ("content-type", "application/json")
              ],
            requestBody = RequestBodyBS (T.encodeUtf8 dataBody)
          }
  resp <- liftIO $ tryGitLab 0 request (retries cfg) manager Nothing
  if successStatus (responseStatus resp)
    then
      return
        ( case parseBSOne (responseBody resp) of
            Just x -> Right x
            Nothing ->
              Left $
                mkStatus 409 "unable to parse PUT response"
        )
    else return (Left (responseStatus resp))

parseBSOne :: FromJSON a => BSL.ByteString -> Maybe a
parseBSOne bs =
  case eitherDecode bs of
    Left _err -> Nothing
    -- useful when debugging
    Right xs -> Just xs

parseBSMany :: FromJSON a => BSL.ByteString -> IO [a]
parseBSMany bs =
  case eitherDecode bs of
    Left s -> Exception.throwIO $ GitLabException s
    Right xs -> return xs

gitlabReqJsonMany :: (FromJSON a) => RelativeUrl -> TextAttrs -> GitLab (Either Status [a])
gitlabReqJsonMany urlPath attrs =
  go 1 []
  where
    go i accum = do
      cfg <- serverCfg <$> ask
      manager <- httpManager <$> ask
      let url' =
            url cfg
              <> "/api/v4"
              <> relativeUrl urlPath
              <> "?per_page=100"
              <> "&page="
              <> T.pack (show i)
              <> T.decodeUtf8 (urlEncode False (T.encodeUtf8 attrs))
      let request' = parseRequest_ (T.unpack url')
          request =
            request'
              { requestHeaders =
                  [("PRIVATE-TOKEN", T.encodeUtf8 (token cfg))],
                responseTimeout = responseTimeoutMicro (timeout cfg)
              }
      resp <- liftIO $ tryGitLab 0 request (retries cfg) manager Nothing
      if successStatus (responseStatus resp)
        then do
          moreResults <- liftIO $ parseBSMany (responseBody resp)
          let numPages = totalPages resp
              accum' = accum ++ moreResults
          if numPages == i
            then return (Right accum')
            else go (i + 1) accum'
        else return (Left (responseStatus resp))

{-
        { method = "POST",
          requestHeaders = [("PRIVATE-TOKEN", T.encodeUtf8 (token cfg)), ("Content-type", "application/json")],
          requestBody = RequestBodyBS (T.encodeUtf8 encodedBody)
-}

-- not sure what this was planned for
--
-- gitlabReqOneIO :: Manager -> GitLabServerConfig -> (BSL.ByteString -> output) -> Text -> Text -> IO (Either Status output)
-- gitlabReqOneIO manager cfg parser urlPath attrs = go
--   where
--     go = do
--       let url' =
--             url cfg
--               <> "/api/v4"
--               <> urlPath
--               <> "?per_page=100"
--               <> "&page=1"
--               <> attrs
--       let request' = parseRequest_ (T.unpack url')
--           request =
--             request'
--               { requestHeaders =
--                   [("PRIVATE-TOKEN", T.encodeUtf8 (token cfg))],
--                 responseTimeout = responseTimeoutMicro (timeout cfg)
--               }
--       resp <- tryGitLab 0 request (retries cfg) manager Nothing
--       if successStatus (responseStatus resp)
--         then return (Right (parser (responseBody resp)))
--         else return (Left (responseStatus resp))

gitlabReqOne :: (BSL.ByteString -> output) -> RelativeUrl -> TextAttrs -> GitLab (Either Status output)
gitlabReqOne parser urlPath attrs = go
  where
    go = do
      cfg <- serverCfg <$> ask
      manager <- httpManager <$> ask
      let url' =
            url cfg
              <> "/api/v4"
              <> relativeUrl urlPath
              <> "?per_page=100"
              <> "&page=1"
              <> attrs
      let request' = parseRequest_ (T.unpack url')
          request =
            request'
              { requestHeaders =
                  [("PRIVATE-TOKEN", T.encodeUtf8 (token cfg))],
                responseTimeout = responseTimeoutMicro (timeout cfg)
              }
      resp <- liftIO $ tryGitLab 0 request (retries cfg) manager Nothing
      if successStatus (responseStatus resp)
        then return (Right (parser (responseBody resp)))
        else return (Left (responseStatus resp))

-- not sure what this was planned for
--
-- gitlabReqJsonOneIO :: (FromJSON a) => Manager -> GitLabServerConfig -> Text -> Text -> IO (Either Status (Maybe a))
-- gitlabReqJsonOneIO mgr cfg urlPath attrs =
--   gitlabReqOneIO mgr cfg parseBSOne urlPath attrs

gitlabReqJsonOne :: (FromJSON a) => RelativeUrl -> Text -> GitLab (Either Status (Maybe a))
gitlabReqJsonOne =
  gitlabReqOne parseBSOne

gitlabReqText :: RelativeUrl -> GitLab (Either Status String)
gitlabReqText urlPath = gitlabReqOne C.unpack urlPath ""

gitlabReqByteString :: RelativeUrl -> GitLab (Either Status BSL.ByteString)
gitlabReqByteString urlPath = gitlabReqOne Prelude.id urlPath ""

gitlab :: FromJSON a => RelativeUrl -> GitLab (Either Status [a])
gitlab addr = gitlabReqJsonMany addr ""

gitlabUnsafe :: (FromJSON a) => RelativeUrl -> GitLab [a]
gitlabUnsafe addr =
  fromRight (error "gitlabUnsafe error") <$> gitlab addr

-- not sure what this was planned for
--
-- gitlabOneIO :: (FromJSON a) => Manager -> GitLabServerConfig -> Text -> IO (Either Status (Maybe a))
-- gitlabOneIO mgr cfg addr = gitlabReqJsonOneIO mgr cfg addr ""

gitlabOne :: (FromJSON a) => RelativeUrl -> GitLab (Either Status (Maybe a))
gitlabOne addr = gitlabReqJsonOne addr ""

gitlabWithAttrs :: (FromJSON a) => RelativeUrl -> Text -> GitLab (Either Status [a])
gitlabWithAttrs = gitlabReqJsonMany

gitlabWithAttrsUnsafe :: (FromJSON a) => RelativeUrl -> TextAttrs -> GitLab [a]
gitlabWithAttrsUnsafe gitlabURL attrs =
  fromRight (error "gitlabWithAttrsUnsafe error") <$> gitlabReqJsonMany gitlabURL attrs

gitlabWithAttrsOne :: (FromJSON a) => RelativeUrl -> Text -> GitLab (Either Status (Maybe a))
gitlabWithAttrsOne = gitlabReqJsonOne

-- not currently used.
-- gitlabWithAttrsOneUnsafe :: (MonadIO m, FromJSON a) => Text -> Text -> GitLab (Maybe a)
-- gitlabWithAttrsOneUnsafe gitlabURL attrs =
--   fromRight (error "gitlabWithAttrsUnsafe error") <$> gitlabReqJsonOne gitlabURL attrs

totalPages :: Response a -> Int
totalPages resp =
  let hdrs = responseHeaders resp
   in findPages hdrs
  where
    findPages [] = 1 -- error "cannot find X-Total-Pages in header"
    findPages (("X-Total-Pages", bs) : _) =
      case readMaybe (T.unpack (T.decodeUtf8 bs)) of
        Just s -> s
        Nothing -> error "cannot find X-Total-Pages in header"
    findPages (_ : xs) = findPages xs

successStatus :: Status -> Bool
successStatus (Status n _msg) =
  n >= 200 && n <= 226

buildFields :: BodyBuilder -> Text
buildFields =
  LT.toStrict . A.encodeToLazyText . A.object

gitlabPost ::
  (FromJSON b) =>
  -- | the URL to post to
  Text ->
  -- | the data to post
  Text ->
  GitLab (Either Status (Maybe b))
gitlabPost urlPath dataBody = do
  cfg <- serverCfg <$> ask
  manager <- httpManager <$> ask
  let url' = url cfg <> "/api/v4" <> urlPath
  let request' = parseRequest_ (T.unpack url')
      request =
        request'
          { method = "POST",
            requestHeaders =
              [("PRIVATE-TOKEN", T.encodeUtf8 (token cfg))],
            requestBody = RequestBodyBS (T.encodeUtf8 dataBody)
          }
  resp <- liftIO $ tryGitLab 0 request (retries cfg) manager Nothing
  if successStatus (responseStatus resp)
    then
      return
        ( case parseBSOne (responseBody resp) of
            Just x -> Right (Just x)
            Nothing -> Right Nothing
          -- Nothing ->
          --   Left $
          --     mkStatus 409 "unable to parse POST response"
        )
    else return (Left (responseStatus resp))

-------------
-- BUILDER --
-------------

-- This functions uses the builder abstratction type

-- | Will not try to parse the response.
gitlabPostBuilder' ::
  RelativeUrl ->
  BodyBuilder ->
  GitLab (PostResult ())
gitlabPostBuilder' urlPath body = do
  -- This function can be modified to return the parsed
  -- response instead, if needed.
  let encodedBody = buildFields body
  cfg <- serverCfg <$> ask
  manager <- httpManager <$> ask
  let url' = url cfg <> "/api/v4" <> relativeUrl urlPath
  let request' = parseRequest_ (T.unpack url')
      request =
        request'
          { method = "POST",
            requestHeaders = [("PRIVATE-TOKEN", T.encodeUtf8 (token cfg)), ("Content-type", "application/json")],
            responseTimeout = responseTimeoutMicro (timeout cfg),
            requestBody = RequestBodyBS (T.encodeUtf8 encodedBody)
          }
  resp <- liftIO $ tryGitLab 0 request (retries cfg) manager Nothing
  if successStatus (responseStatus resp)
    then return (RequestSuccess ())
    else return (HttpFailure (responseStatus resp))

gitlabReqJsonManyBuilder :: (FromJSON a) => RelativeUrl -> BodyBuilder -> GitLab (GetResult [a])
gitlabReqJsonManyBuilder urlPath body =
  go 1 []
  where
    go i accum = do
      cfg <- serverCfg <$> ask
      manager <- httpManager <$> ask
      let url' =
            url cfg
              <> "/api/v4"
              <> relativeUrl urlPath
              <> "?per_page=100"
              <> "&page="
              <> T.pack (show i)
      let request' = parseRequest_ (T.unpack url')
          request =
            request'
              { requestHeaders = [("PRIVATE-TOKEN", T.encodeUtf8 (token cfg)), ("Content-type", "application/json")],
                responseTimeout = responseTimeoutMicro (timeout cfg),
                requestBody = RequestBodyBS (T.encodeUtf8 $ buildFields body)
              }
      resp <- liftIO $ tryGitLab 0 request (retries cfg) manager Nothing
      if successStatus (responseStatus resp)
        then do
          moreResults <- liftIO $ parseBSMany (responseBody resp)
          let numPages = totalPages resp
              accum' = accum ++ moreResults
          if numPages == i
            then return (GRequestSuccess accum')
            else go (i + 1) accum'
        else return (GHttpFailure (responseStatus resp))

gitlabPutBuilder' :: RelativeUrl -> BodyBuilder -> GitLab (PutResult ())
gitlabPutBuilder' urlPath bodyBuilder = do
  cfg <- serverCfg <$> ask
  manager <- httpManager <$> ask
  let url' = url cfg <> "/api/v4" <> relativeUrl urlPath
  let request' = parseRequest_ (T.unpack url')
      request =
        request'
          { method = "PUT",
            requestHeaders =
              [ ("PRIVATE-TOKEN", T.encodeUtf8 (token cfg)),
                ("content-type", "application/json")
              ],
            requestBody = RequestBodyBS (T.encodeUtf8 $ buildFields bodyBuilder)
          }
  resp <- liftIO $ tryGitLab 0 request (retries cfg) manager Nothing
  if successStatus (responseStatus resp)
    then return PutIgnoringPayload
    else return (PutFailure (responseStatus resp))
