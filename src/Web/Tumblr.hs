{-# LANGUAGE FlexibleContexts #-}

module Web.Tumblr where

import Control.Applicative
import Control.Arrow
import Control.Monad.Reader

import Data.Aeson
import Data.Attoparsec
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Maybe
import Data.Monoid

import Network.HTTP.Conduit
import Network.HTTP.Types

import Web.Authenticate.OAuth

newtype AvatarSize = AvatarSize {getAvatarSize :: Int}

data PostType = Text | Quote | Link | Answer | Video | Audio | Photo | Chat deriving (Eq, show)
  
data PostFilter = PlainText | Raw deriving (Eq, Show)

class HasAPIKey k where
  getAPIKey :: k -> ByteString
  
instance HasAPIKey ByteString where
  getAPIKey = id
  
instance HasAPIKey OAuth where
  getAPIKey = oauthConsumerKey

tumblrBaseRequest :: Request m
tumblrBaseRequest = def {
  host = B.pack "api.tumblr.com"
  }

reduceFirst :: ByteString -> ByteString
reduceFirst = fromMaybe B.empty . fmap (uncurry B.cons . first toLower) . B.uncons

renderQueryCull :: Bool -> Query -> ByteString
renderQueryCull b = renderQuery b . filter (isJust . snd)

type BaseHostname = ByteString

tumblrInfo :: (HasAPIKey k, MonadBaseControl IO m, MonadResource m, MonadReader k m) => BaseHostname -> Manager -> m Value
tumblrInfo baseHostname manager = do
  apiKey <- getAPIKey <$> ask
  let myRequest = tumblrBaseRequest {path = B.pack "/v2/blog/" <> baseHostname <> B.pack "/info?api_key=" <> apiKey}
  resp <- responseBody <$> http myRequest manager
  resp $$+- sinkParser json
  
tumblrAvatar :: (MonadBaseControl IO m, MonadResource m) => BaseHostname -> Maybe AvatarSize -> Manager -> m Value
tumblrAvatar baseHostname msize manager = do
  let myRequest = tumblrBaseRequest {path = B.pack "/v2/blog/" <> baseHostname <> B.pack "/avatar" <> maybe B.empty (B.pack . show . getAvatarSize) msize,
                                     checkStatus = \stat -> if stat == movedPermanently301 then const Nothing else checkStatus def stat
                                    }
  resp <- responseBody <$> http myRequest manager
  resp $$+- sinkParser json

tumblrLikes :: (HasAPIKey k, MonadBaseControl IO m, MonadResource m, MonadReader k m) => BaseHostname -> Maybe Int -> Maybe Int -> Manager -> m Value
tumblrLikes baseHostname mlimit moffset manager = do
  apiKey <- getAPIKey <$> ask
  let myRequest = tumblrBaseRequest {path = B.pack "/v2/blog/" <> baseHostname <> B.pack "/likes?api_key=" <> apiKey <> maybe B.empty ((B.pack "&limit=" <>) . B.pack . show) mlimit <> maybe B.empty ((B.pack "&offset=" <>) . B.pack . show) moffset}
  resp <- responseBody <$> http myRequest manager
  resp $$+- sinkParser json
  
tumblrFollowers :: (MonadBaseControl IO m, MonadResource m, MonadReader OAuth m) => BaseHostname -> Maybe Int -> Maybe Int -> Credential -> Manager -> m Value
tumblrFollowers baseHostname mlimit moffset credential manager = do
  oauth <- ask
  myRequest <- signOAuth oauth credential $ tumblrBaseRequest {path = B.pack "/v2/blog" <> baseHostname <> B.pack "/followers" <> renderQueryCull True [(B.pack "limit", B.pack . show <$> mlimit), (B.pack "offset", B.pack . show <$> moffset)]}
  resp <- responseBody <$> http myRequest manager
  resp $$+- sinkParser json
  
tumblrPosts :: (HasAPIKey k, MonadBaseControl IO m, MonadResource m, MonadReader k m) => BaseHostname -> Maybe PostType -> Maybe Int -> Maybe String -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe PostFilter -> Manager -> m Value
tumblrPosts baseHostname mtype mid mtag mlimit moffset mrebloginfo mnotesinfo mfilter manager = do
  apiKey <- getAPIKey <$> ask
  let myRequest = tumblrBaseRequest {path = B.pack "/v2/blog/" <> baseHostname <> B.pack "/posts" <> maybe B.empty (B.cons '/' . reduceFirst . B.pack . show) mtype <> renderQueryCull True [
                                        (B.pack "api_key", Just apiKey),
                                        (B.pack "id", B.pack . show <$> mid),
                                        (B.pack "tag", B.pack <$> mtag),
                                        (B.pack "limit", B.pack . show <$> mlimit),
                                        (B.pack "offset", B.pack . show <$> moffset),
                                        (B.pack "reblog_info", reduceFirst . B.pack . show <$> mrebloginfo),
                                        (B.pack "notes_info", reduceFirst . B.pack . show <$> mnotesinfo),
                                        (B.pack "filter", reduceFirst . B.pack . show <$> mfilter)]}
  resp <- responseBody <$> http myRequest manager
  resp $$+- sinkParser json

tumblrQueuedPosts :: (MonadBaseControl IO m, MonadResource m, MonadReader OAuth m) => BaseHostname -> Maybe Int -> Maybe Int -> Maybe PostFilter -> Credential -> Manager -> m Value
tumblrQueuedPosts baseHostname mlimit moffset mfilter credential manager = do
  oauth <- ask
  myRequest <- signOAuth oauth credential $ tumblrBaseRequest {path = B.pack "/v2/blog/" <> baseHostname <> B.pack "/posts/queue" <> renderQueryCull True [
                                                                  (B.pack "limit", B.pack . show <$> mlimit),
                                                                  (B.pack "offset", B.pack . show <$> moffset),
                                                                  (B.pack "filter", reduceFirst . B.pack . show <$> mfilter)]}
  resp <- responseBody <$> http myRequest manager
  resp $$+- sinkParser json
  
tumblrDraftPosts :: (MonadBaseControl IO m, MonadResource m, MonadReader OAuth m) => BaseHostname -> Maybe PostFilter -> Credential -> Manager -> m Value
tumblrDraftPosts baseHostname mfilter credential manager = do
  oauth <- ask
  myRequest <- signOAuth oauth credential $ tumblrBaseRequest {path = B.pack "/v2/blog/" <> baseHostname <> B.pack "/posts/draft" <> maybe B.empty (B.append (B.pack "?filter=") . reduceFirst . B.pack . show) mfilter}
  resp <- responseBody <$> http myRequest manager
  resp $$+- sinkParser json
  
tumblrSubmissionPosts :: (MonadBaseControl IO m, MonadResource m, MonadReader OAuth m) => BaseHostname -> Maybe Int -> Maybe PostFilter -> Credential -> Manager -> m Value
tumblrSubmissionPosts baseHostname moffset mfilter credential manager = do
  oauth <- ask
  myRequest <- signOAuth oauth credential $ tumblrBaseRequest {path = B.pack "/v2/blog/" <> baseHostname <> B.pack "/posts/submission" <> renderQueryCull True [
                                                                  (B.pack "offset", B.pack . show <$> moffset),
                                                                  (B.pack "filter", reduceFirst . B.pack  . show <$> mfilter)]}
  resp <- responseBody <$> http myRequest manager
  resp $$+- sinkParser json