{-# LANGUAGE FlexibleContexts, DoAndIfThenElse #-}

module Web.Tumblr where

import Control.Applicative
import Control.Arrow
import Control.Monad.Reader

import Data.Aeson
import Data.Attoparsec
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Conduit
import Data.Conduit.Attoparsec
import qualified Data.Conduit.Binary as CB
import Data.Maybe
import Data.Monoid

import Network.HTTP.Conduit
import Network.HTTP.Types

import Web.Authenticate.OAuth(Credential)
import Web.Authenticate.OAuth

import Web.Tumblr.Types
import Web.Tumblr.Helpers
import qualified Data.HashMap.Strict as HM

newtype AvatarSize = AvatarSize {getAvatarSize :: Int}

data PostType = Text | Quote | Link | Answer | Video | Audio | Photo | Chat deriving (Eq, Show)
  
data PostFilter = PlainText | Raw deriving (Eq, Show)

class HasAPIKey k where
  getAPIKey :: k -> ByteString
  
instance HasAPIKey ByteString where
  getAPIKey = id
  
instance HasAPIKey OAuth where
  getAPIKey = oauthConsumerKey

 
tumblrOAuth :: ByteString -- ^ The Tumblr API key
            -> ByteString -- ^ The Tumblr API secret to use
            -> OAuth
tumblrOAuth key secret = newOAuth { oauthServerName = "tumblr"
                                  , oauthRequestUri = "http://www.tumblr.com/oauth/request_token"
                                  , oauthAccessTokenUri = "http://www.tumblr.com/oauth/access_token"
                                  , oauthAuthorizeUri = "http://www.tumblr.com/oauth/authorize"
                                  , oauthConsumerKey = key
                                  , oauthConsumerSecret = secret }                      
                         
-- FIXME: this one is more or less just a sample and will not very well in webapps
-- | Obtain authorization information.
--   The user is sent to Tumblr to authorize your app and then has to paste the verifier.
--   TODO: Cleaner, more elegant solution
--   TODO: Store the obtained tokens
tumblrAuthorize :: (MonadBaseControl IO m, MonadResource m) 
                  => OAuth 
                  -> Manager 
                  -> m Credential
tumblrAuthorize oauth mgr = do
  tempCred <- getTemporaryCredential oauth mgr
  let authURL = authorizeUrl oauth tempCred
  verifier <- liftIO $ do 
    exit <- openBrowserOn authURL
    if exit /= ExitSuccess then
      putStrLn ("Failed to open browser! Go to " ++ authURL)
    else 
      return ()
    putStrLn "Enter the verifier (oauth_verifier field in the URL): "
    getLine
  
  let tempCred' = injectVerifier (B.pack verifier) tempCred
  cred <- getAccessToken oauth tempCred' mgr
  return cred

tumblrBaseRequest :: Request m
tumblrBaseRequest = def {
  host = B.pack "api.tumblr.com"
  }

reduceFirst :: ByteString -> ByteString
reduceFirst = fromMaybe B.empty . fmap (uncurry B.cons . first toLower) . B.uncons

renderQueryCull :: Bool -> Query -> ByteString
renderQueryCull b = renderQuery b . filter (isJust . snd)

type BaseHostname = ByteString

jsonValue :: (FromJSON a) => Parser a
jsonValue = json >>= \v -> case fromJSON v of
  Error s -> fail s
  Success x -> case HM.lookup "response" x of
    Nothing -> fail "Invalid response data"
    Just w  ->  case fromJSON w of
      Error s -> fail s
      Success x -> return x

-- | This method returns general information about the blog, such as the title, number of posts, and other high-level data.
tumblrInfo :: (HasAPIKey k, MonadBaseControl IO m, MonadResource m, MonadReader k m) => 
             BaseHostname -> Manager -> m BlogInfo
tumblrInfo baseHostname manager = do
  apiKey <- getAPIKey <$> ask
  let myRequest = tumblrBaseRequest {path = B.pack "/v2/blog/" <> baseHostname <> B.pack "/info?api_key=" <> apiKey}
  resp <- responseBody <$> http myRequest manager
  resp $$+- sinkParser jsonValue
  
-- | Retrieve a Blog Avatar
-- You can get a blog's avatar in 9 different sizes. The default size is 64x64.
tumblrAvatar :: (MonadBaseControl IO m, MonadResource m) 
               => BaseHostname 
               -> Maybe AvatarSize -- ^ The size of the avatar (square, one value for both length and width). Must be one of the values: 16, 24, 30, 40, 48, 64, 96, 128, 512
               -> Manager -> m LB.ByteString
tumblrAvatar baseHostname msize manager = do
  let myRequest = tumblrBaseRequest {path = B.pack "/v2/blog/" <> baseHostname <> B.pack "/avatar" <> 
                                            maybe B.empty (B.pack . show . getAvatarSize) msize,
                                     checkStatus = \stat -> if stat == movedPermanently301 
                                                           then const (const Nothing) 
                                                           else checkStatus def stat
                                    } 
  resp <- responseBody <$> http myRequest manager
  resp $$+- CB.sinkLbs

-- | Retrieve Blog's Likes
-- This method can be used to retrieve the publicly exposed likes from a blog.
tumblrLikes :: (HasAPIKey k, MonadBaseControl IO m, MonadResource m, MonadReader k m) 
              => BaseHostname 
              -> Maybe Int -- ^ The number of results to return: 1–20, inclusive. Default: 20
              -> Maybe Int -- ^ Liked post number to start at. Default: 0
              -> Manager -> m Likes
tumblrLikes baseHostname mlimit moffset manager = do
  apiKey <- getAPIKey <$> ask
  let myRequest = tumblrBaseRequest {path = B.pack "/v2/blog/" <> baseHostname <> 
                                            B.pack "/likes?api_key=" <> apiKey <> 
                                            maybe B.empty ((B.pack "&limit=" <>) . B.pack . show) mlimit <> 
                                            maybe B.empty ((B.pack "&offset=" <>) . B.pack . show) moffset }
  resp <- responseBody <$> http myRequest manager
  resp $$+- sinkParser jsonValue
  

-- | Retrieve a Blog's Followers
tumblrFollowers :: (MonadBaseControl IO m, MonadResource m, MonadReader OAuth m) 
                  => BaseHostname 
                  -> Maybe Int -- ^ The number of results to return: 1–20, inclusive. Default: 20
                  -> Maybe Int -- ^ Result to start at. Default: 0 (first follower)
                  -> Credential -- ^ OAuth authentication credentials
                  -> Manager -> m Followers
tumblrFollowers baseHostname mlimit moffset credential manager = do
  oauth <- ask
  myRequest <- signOAuth oauth credential $ 
              tumblrBaseRequest {path = B.pack "/v2/blog/" <> baseHostname <> 
                                        B.pack "/followers" <> 
                                        renderQueryCull True [(B.pack "limit", B.pack . show <$> mlimit), 
                                                              (B.pack "offset", B.pack . show <$> moffset)]}
  resp <- responseBody <$> http myRequest manager
  resp $$+- sinkParser jsonValue
  
-- | Retrieve Published Posts
tumblrPosts :: (HasAPIKey k, MonadBaseControl IO m, MonadResource m, MonadReader k m) 
              => BaseHostname 
              -> Maybe PostType -- ^ The type of post to return.
              -> Maybe Int -- ^ A specific post ID. Returns the single post specified or (if not found) a 404 error. 
              -> Maybe String -- ^ tag to which to limit the response
              -> Maybe Int -- ^ limit
              -> Maybe Int -- ^ Offset: Post number to start at.
              -> Maybe Bool -- ^ Indicates whether to return reblog information (specify true or false). Returns the various reblogged_ fields. UNUSED.
              -> Maybe Bool -- ^  	Indicates whether to return notes information (specify true or false). Returns note count and note metadata. UNUSED.
              -> Maybe PostFilter -- ^ Specifies the post format to return, other than HTML.
              -> Manager -> m Posts
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
  resp $$+- sinkParser jsonValue

-- | Retrieve Queued Posts
tumblrQueuedPosts :: (MonadBaseControl IO m, MonadResource m, MonadReader OAuth m) 
                    => BaseHostname 
                    -> Maybe Int -- ^ The number of results to return: 1–20, inclusive. Default: 20
                    -> Maybe Int -- ^ Post number to start at. Default: 0
                    -> Maybe PostFilter -- ^ Specifies the post format to return, other than HTML.
                    -> Credential -- ^ OAuth authentication credentials
                    -> Manager -> m JustPosts
tumblrQueuedPosts baseHostname mlimit moffset mfilter credential manager = do
  oauth <- ask
  myRequest <- signOAuth oauth credential $ tumblrBaseRequest {path = B.pack "/v2/blog/" <> baseHostname <> B.pack "/posts/queue" <> renderQueryCull True [
                                                                  (B.pack "limit", B.pack . show <$> mlimit),
                                                                  (B.pack "offset", B.pack . show <$> moffset),
                                                                  (B.pack "filter", reduceFirst . B.pack . show <$> mfilter)]}
  resp <- responseBody <$> http myRequest manager
  resp $$+- sinkParser jsonValue
  
-- | Retrieve Draft Posts
tumblrDraftPosts :: (MonadBaseControl IO m, MonadResource m, MonadReader OAuth m) 
                   => BaseHostname 
                   -> Maybe PostFilter -- ^ Specifies the post format to return, other than HTML.
                   -> Credential -- ^ OAuth authentication credentials
                   -> Manager -> m JustPosts
tumblrDraftPosts baseHostname mfilter credential manager = do
  oauth <- ask
  myRequest <- signOAuth oauth credential $ tumblrBaseRequest {path = B.pack "/v2/blog/" <> baseHostname <> 
                                                                     B.pack "/posts/draft" <> 
                                                                     maybe B.empty (B.append (B.pack "?filter=") . reduceFirst . B.pack . show) mfilter}
  resp <- responseBody <$> http myRequest manager
  resp $$+- sinkParser jsonValue
  
-- | Retrieve Submission Posts
tumblrSubmissionPosts :: (MonadBaseControl IO m, MonadResource m, MonadReader OAuth m) 
                        => BaseHostname 
                        -> Maybe Int -- ^ Post number to start at. Default: 0
                        -> Maybe PostFilter -- ^ Specifies the post format to return, other than HTML.
                        -> Credential -- ^ OAuth authentication credentials
                        -> Manager -> m JustPosts
tumblrSubmissionPosts baseHostname moffset mfilter credential manager = do
  oauth <- ask
  myRequest <- signOAuth oauth credential $ tumblrBaseRequest {path = B.pack "/v2/blog/" <> baseHostname <> B.pack "/posts/submission" <> renderQueryCull True [
                                                                  (B.pack "offset", B.pack . show <$> moffset),
                                                                  (B.pack "filter", reduceFirst . B.pack  . show <$> mfilter)]}
  resp <- responseBody <$> http myRequest manager
  resp $$+- sinkParser jsonValue
