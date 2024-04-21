{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Web.Tumblr where

import Conduit
import Control.Arrow
#if MIN_VERSION_base(4,18,0)
import Control.Monad              (unless)
#endif
import Control.Monad.Reader
import Data.Aeson
import Data.Attoparsec.ByteString
import Data.ByteString            (ByteString)
import Data.ByteString.Char8      qualified as B
import Data.ByteString.Lazy       qualified as LB
import Data.Char
import Data.Conduit.Attoparsec
import Data.Conduit.Binary        qualified as CB
import Data.HashMap.Strict        qualified as HM
import Data.Maybe
import Network.HTTP.Conduit
import Network.HTTP.Types
import Web.Authenticate.OAuth
import Web.Browser
import Web.Tumblr.Types

newtype AvatarSize = AvatarSize {getAvatarSize :: Int}

data PostType = Text | Quote | Link | Answer | Video | Audio | Photo | Chat deriving stock (Eq, Show)

data PostFilter = PlainText | Raw deriving stock (Eq, Show)

class HasAPIKey k where
  getAPIKey :: k → ByteString

instance HasAPIKey ByteString where
  getAPIKey = id

instance HasAPIKey OAuth where
  getAPIKey = oauthConsumerKey

tumblrOAuth ∷
  -- | The Tumblr API key
  ByteString →
  -- | The Tumblr API secret to use
  ByteString →
  OAuth
tumblrOAuth key secret =
  newOAuth
    { oauthServerName = "tumblr",
      oauthRequestUri = "https://www.tumblr.com/oauth/request_token",
      oauthAccessTokenUri = "https://www.tumblr.com/oauth/access_token",
      oauthAuthorizeUri = "https://www.tumblr.com/oauth/authorize",
      oauthConsumerKey = key,
      oauthConsumerSecret = secret
    }

-- FIXME: this one is more or less just a sample and will not very well in webapps

-- | Obtain authorization information.
--   The user is sent to Tumblr to authorize your app and then has to paste the verifier.
--   TODO: Cleaner, more elegant solution
--   TODO: Store the obtained tokens
tumblrAuthorize ∷
  (MonadResource m) ⇒
  OAuth →
  Manager →
  m Credential
tumblrAuthorize oauth mgr = do
  tempCred <- getTemporaryCredential oauth mgr
  let authURL = authorizeUrl oauth tempCred
  verifier <- liftIO $ do
    browserSuccess <- openBrowser authURL
    unless browserSuccess $
        putStrLn ("Failed to open browser! Go to " <> authURL)
    putStrLn "Enter the verifier (oauth_verifier field in the URL): "
    getLine
  let tempCred' = injectVerifier (B.pack verifier) tempCred
  getAccessToken oauth tempCred' mgr

tumblrBaseRequest ∷ Request
tumblrBaseRequest =
  defaultRequest
    { host = B.pack "api.tumblr.com"
    }

reduceFirst ∷ ByteString → ByteString
reduceFirst = maybe B.empty (uncurry B.cons . first toLower) . B.uncons

renderQueryCull ∷ Bool → Query → ByteString
renderQueryCull b = renderQuery b . filter (isJust . snd)

type BaseHostname = ByteString

jsonValue ∷ (FromJSON a) ⇒ Parser a
jsonValue =
  json >>= \v -> case fromJSON v of
    Error s -> fail s
    Success x -> case HM.lookup "response" x of
      Nothing -> fail "Invalid response data"
      Just w -> case fromJSON w of
        Error s   -> fail s
        Success y -> pure y

-- | This method returns general information about the blog, such as the title, number of posts, and other high-level data.
-- | /info
tumblrInfo ∷
  (HasAPIKey k, MonadThrow m, MonadResource m, MonadReader k m) ⇒
  BaseHostname →
  Manager →
  m BlogInfo
tumblrInfo baseHostname manager = do
  apiKey <- asks getAPIKey
  let myRequest = tumblrBaseRequest {path = B.pack "/v2/blog/" <> baseHostname <> B.pack "/info?api_key=" <> apiKey}
  resp <- responseBody <$> http myRequest manager
  sealConduitT resp $$+- sinkParser jsonValue

-- | Retrieve a Blog Avatar
-- You can get a blog's avatar in 9 different sizes. The default size is 64x64.
-- /avatar
tumblrAvatar ∷
  (MonadResource m) ⇒
  BaseHostname →
  -- | The size of the avatar (square, one value for both length and width). Must be one of the values: 16, 24, 30, 40, 48, 64, 96, 128, 512
  Maybe AvatarSize →
  Manager →
  m LB.ByteString
tumblrAvatar baseHostname msize manager = do
  let myRequest =
        tumblrBaseRequest
          { path =
              B.pack "/v2/blog/" <> baseHostname <> B.pack "/avatar"
                <> maybe B.empty (B.pack . show . getAvatarSize) msize
            {- checkStatus = \stat ->
              if stat == movedPermanently301
                then const (const Nothing)
                else checkStatus def stat -}
          }
  resp <- responseBody <$> http myRequest manager
  sealConduitT resp $$+- CB.sinkLbs

-- | Retrieve Blog's Likes
-- This method can be used to retrieve the publicly exposed likes from a blog.
-- /likes
tumblrLikes ∷
  (HasAPIKey k, MonadResource m, MonadReader k m, MonadThrow m) ⇒
  BaseHostname →
  -- | The number of results to return: 1–20, inclusive. Default: 20
  Maybe Int →
  -- | Liked post number to start at. Default: 0
  Maybe Int →
  Manager →
  m Likes
tumblrLikes baseHostname mlimit moffset manager = do
  apiKey <- asks getAPIKey
  let myRequest =
        tumblrBaseRequest
          { path =
              B.pack "/v2/blog/" <> baseHostname
                <> B.pack "/likes?api_key="
                <> apiKey
                <> maybe B.empty ((B.pack "&limit=" <>) . B.pack . show) mlimit
                <> maybe B.empty ((B.pack "&offset=" <>) . B.pack . show) moffset
          }
  resp <- responseBody <$> http myRequest manager
  sealConduitT resp $$+- sinkParser jsonValue

-- | Retrieve a Blog's Followers
-- /followers
tumblrFollowers ∷
  (MonadResource m, MonadReader OAuth m, MonadThrow m) ⇒
  BaseHostname →
  -- | The number of results to return: 1–20, inclusive. Default: 20
  Maybe Int →
  -- | Result to start at. Default: 0 (first follower)
  Maybe Int →
  -- | OAuth authentication credentials
  Credential →
  Manager →
  m Followers
tumblrFollowers baseHostname mlimit moffset credential manager = do
  oauth <- ask
  myRequest <-
    signOAuth oauth credential $
      tumblrBaseRequest
        { path =
            B.pack "/v2/blog/" <> baseHostname
              <> B.pack "/followers"
              <> renderQueryCull
                True
                [ (B.pack "limit", B.pack . show <$> mlimit),
                  (B.pack "offset", B.pack . show <$> moffset)
                ]
        }
  resp <- responseBody <$> http myRequest manager
  sealConduitT resp $$+- sinkParser jsonValue

-- /following

-- /followed_by


-- | Retrieve Published Posts
-- /posts
tumblrPosts ∷
  (HasAPIKey k, MonadResource m, MonadReader k m, MonadThrow m) ⇒
  BaseHostname →
  -- | The type of post to return.
  Maybe PostType →
  -- | A specific post ID. Returns the single post specified or (if not found) a 404 error.
  Maybe Int →
  -- | tag to which to limit the response
  Maybe String →
  -- | limit
  Maybe Int →
  -- | Offset: Post number to start at.
  Maybe Int →
  -- | Indicates whether to return reblog information (specify true or false). Returns the various reblogged_ fields. UNUSED.
  Maybe Bool →
  -- |  	Indicates whether to return notes information (specify true or false). Returns note count and note metadata. UNUSED.
  Maybe Bool →
  -- | Specifies the post format to return, other than HTML.
  Maybe PostFilter →
  Manager →
  m Posts
tumblrPosts baseHostname mtype mid mtag mlimit moffset mrebloginfo mnotesinfo mfilter' manager = do
  apiKey <- asks getAPIKey
  let myRequest =
        tumblrBaseRequest
          { path =
              B.pack "/v2/blog/" <> baseHostname <> B.pack "/posts" <> maybe B.empty (B.cons '/' . reduceFirst . B.pack . show) mtype
                <> renderQueryCull
                  True
                  [ (B.pack "api_key", Just apiKey),
                    (B.pack "id", B.pack . show <$> mid),
                    (B.pack "tag", B.pack <$> mtag),
                    (B.pack "limit", B.pack . show <$> mlimit),
                    (B.pack "offset", B.pack . show <$> moffset),
                    (B.pack "reblog_info", reduceFirst . B.pack . show <$> mrebloginfo),
                    (B.pack "notes_info", reduceFirst . B.pack . show <$> mnotesinfo),
                    (B.pack "filter", reduceFirst . B.pack . show <$> mfilter')
                  ]
          }
  resp <- responseBody <$> http myRequest manager
  sealConduitT resp $$+- sinkParser jsonValue

-- | Retrieve Queued Posts
-- /posts/queue
tumblrQueuedPosts ∷
  (MonadResource m, MonadReader OAuth m, MonadThrow m) ⇒
  BaseHostname →
  -- | The number of results to return: 1–20, inclusive. Default: 20
  Maybe Int →
  -- | Post number to start at. Default: 0
  Maybe Int →
  -- | Specifies the post format to return, other than HTML.
  Maybe PostFilter →
  -- | OAuth authentication credentials
  Credential →
  Manager →
  m JustPosts
tumblrQueuedPosts baseHostname mlimit moffset mfilter' credential manager = do
  oauth <- ask
  myRequest <-
    signOAuth oauth credential $
      tumblrBaseRequest
        { path =
            B.pack "/v2/blog/" <> baseHostname <> B.pack "/posts/queue"
              <> renderQueryCull
                True
                [ (B.pack "limit", B.pack . show <$> mlimit),
                  (B.pack "offset", B.pack . show <$> moffset),
                  (B.pack "filter", reduceFirst . B.pack . show <$> mfilter')
                ]
        }
  resp <- responseBody <$> http myRequest manager
  sealConduitT resp $$+- sinkParser jsonValue

-- POST /posts/queue/reorder - TODO
-- POST /posts/queue/shuffle - TODO

-- | Retrieve Draft Posts
-- /posts/draft
tumblrDraftPosts ∷
  (MonadResource m, MonadReader OAuth m, MonadThrow m) ⇒
  BaseHostname →
  -- | Specifies the post format to return, other than HTML.
  Maybe PostFilter →
  -- | OAuth authentication credentials
  Credential →
  Manager →
  m JustPosts
tumblrDraftPosts baseHostname mfilter' credential manager = do
  oauth <- ask
  myRequest <-
    signOAuth oauth credential $
      tumblrBaseRequest
        { path =
            B.pack "/v2/blog/" <> baseHostname
              <> B.pack "/posts/draft"
              <> maybe B.empty (B.append (B.pack "?filter=") . reduceFirst . B.pack . show) mfilter'
        }
  resp <- responseBody <$> http myRequest manager
  sealConduitT resp $$+- sinkParser jsonValue

-- | Retrieve Submission Posts
-- /posts/submission
tumblrSubmissionPosts ∷
  (MonadResource m, MonadReader OAuth m, MonadThrow m) ⇒
  BaseHostname →
  -- | Post number to start at. Default: 0
  Maybe Int →
  -- | Specifies the post format to return, other than HTML.
  Maybe PostFilter →
  -- | OAuth authentication credentials
  Credential →
  Manager →
  m JustPosts
tumblrSubmissionPosts baseHostname moffset mfilter' credential manager = do
  oauth <- ask
  myRequest <-
    signOAuth oauth credential $
      tumblrBaseRequest
        { path =
            B.pack "/v2/blog/" <> baseHostname <> B.pack "/posts/submission"
              <> renderQueryCull
                True
                [ (B.pack "offset", B.pack . show <$> moffset),
                  (B.pack "filter", reduceFirst . B.pack . show <$> mfilter')
                ]
        }
  resp <- responseBody <$> http myRequest manager
  sealConduitT resp $$+- sinkParser jsonValue

{-
/post (legacy) - will not implement
/post/edit (legacy) - will not implement
/post/reblog (legacy) - will not implement
-}

-- POST /posts
-- GET /posts/{postId}

-- PUT /posts/{postId}
{-
tumblrEditPost ::
  (MonadResource m, MonadReader OAuth m, MonadThrow m) =>
  BaseHostname ->
  String ->
  Post ->
  Credential ->
  Manager ->
  m Post
tumblrEditPost baseHostname postId newPost credential manager = do
  oauth <- ask
  myRequest <-
    signOAuth oauth credential $
      tumblrBaseRequest
        { path =
            B.pack "/v2/blog/" <> baseHostname <> B.pack "/posts/" <> B.pack postId
        , method = B.pack "POST"
        , requestBody = RequestBodyLBS $ encode newPost
        }
  resp <- responseBody <$> http myRequest manager
  sealConduitT resp $$+- sinkParser jsonValue
-}

-- POST /post/delete
-- GET /notes

-- GET /user/info
-- GET /user/dashboard
-- GET /user/likes
-- GET /user/following
-- POST /user/follow
-- POST /user/unfollow
-- POST /user/like
-- POST /user/unlike
-- GET /user/filtered_content
-- POST /user/filtered_content
-- DELETE /user/filtered_content
-- GET /tagged
