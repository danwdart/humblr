module Web.Tumblr where

import Control.Applicative
import Control.Monad.Reader

import Data.Aeson
import Data.Attoparsec
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Maybe
import Data.Monoid

import Network.HTTP.Conduit
import Network.HTTP.Types

import Web.Authenticate.OAuth

newtype AvatarSize = AvatarSize {getAvatarSize :: Int}

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

type BaseHostname = ByteString

tumblrInfo :: (HasAPIKey k, MonadBaseControl IO m, MonadResource m, MonadReader k m) => BaseHostname -> Manager -> m Value
tumblrInfo baseHostname manager = do
  apiKey <- getAPIKey <$> ask
  let myRequest = tumblrBaseRequest {path = B.pack "/v2/blog/" <> baseHostname <> B.pack "/info?api_key=" <> apiKey}
  resp <- responseBody <$> http myRequest manager
  resp $$+- sinkParser json
  
tumblrAvatar :: (MonadBaseControl IO m, MonadResource m) => BaseHostname -> Maybe AvatarSize -> Manager -> m Value
tumblrAvatar baseHostname msize manager = do
  let myRequest = tumblrBaseRequest {path = B.pack "/v2/blog" <> baseHostname <> B.pack "/avatar" <> maybe B.empty (B.pack . show . getAvatarSize) msize,
                                     checkStatus = \stat -> if stat == movedPermanently301 then const Nothing else checkStatus def stat
                                    }
  resp <- responseBody <$> http myRequest manager
  resp $$+- sinkParser json


-- it is not clear whether this takes optional parameters like Avatar or like Followers, so I have opted for the latter for now.
tumblrLikes :: (HasAPIKey k, MonadBaseControl IO m, MonadResource m, MonadReader k m) => BaseHostname -> Maybe Int -> Maybe Int -> Manager -> m Value
tumblrLikes baseHostname mlimit moffset manager = do
  apiKey <- getAPIKey <$> ask
  let myRequest = tumblrBaseRequest {path = B.pack "/v2/blog" <> baseHostname <> B.pack "/likes?api_key=" <> apiKey <> maybe B.empty ((B.pack "&limit=" <>) . B.pack . show) mlimit <> maybe B.empty ((B.pack "&offset=" <>) . B.pack . show) moffset}
  resp <- responseBody <$> http myRequest manager
  resp $$+- sinkParser json
  
tumblrFollowers :: (MonadBaseControl IO m, MonadResource m, MonadReader OAuth m) => BaseHostname -> Maybe Int -> Maybe Int -> Credential -> Manager -> m Value
tumblrFollowers baseHostname mlimit moffset credential manager = do
  oauth <- ask
  myRequest <- signOAuth oauth credential $ tumblrBaseRequest {path = B.pack "/v2/blog" <> baseHostname <> B.pack "/followers" <> renderQuery True [(B.pack "limit", B.pack . show <$> mlimit), (B.pack "offset", B.pack . show <$> moffset)]}
  undefined