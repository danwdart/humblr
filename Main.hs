{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy         as LB
import qualified Network.HTTP.Conduit         as NetConduit
import qualified Web.Tumblr                   as Tumblr
import qualified Web.Tumblr.Types             as Tumblr.Types


oauth = Tumblr.tumblrOAuth
        "[OAuth Consumer Key]"
        "[Secret Key]"

getTumblrInfo mgr hostname = runResourceT $ runReaderT (Tumblr.tumblrInfo hostname mgr) oauth

getTumblrAvatar mgr hostname = runResourceT $ Tumblr.tumblrAvatar hostname Nothing mgr

getTumblrLikes mgr hostname = runResourceT $ runReaderT (Tumblr.tumblrLikes hostname Nothing Nothing mgr) oauth

getTumblrPosts mgr hostname = runResourceT $ runReaderT (Tumblr.tumblrPosts hostname Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing mgr) oauth

getTumblrFollowers mgr hostname = runResourceT $ do
   credential <- Tumblr.tumblrAuthorize oauth mgr
   runReaderT (Tumblr.tumblrFollowers hostname Nothing Nothing credential mgr) oauth

getTumblrQueuedPosts mgr hostname = runResourceT $ do
   credential <- Tumblr.tumblrAuthorize oauth mgr
   runReaderT (Tumblr.tumblrQueuedPosts hostname Nothing Nothing Nothing credential mgr) oauth

getTumblrDraftPosts mgr hostname = runResourceT $ do
   credential <- Tumblr.tumblrAuthorize oauth mgr
   runReaderT (Tumblr.tumblrDraftPosts hostname Nothing credential mgr) oauth

getTumblrSubmissionPosts mgr hostname = runResourceT $ do
   credential <- Tumblr.tumblrAuthorize oauth mgr
   runReaderT (Tumblr.tumblrSubmissionPosts hostname Nothing Nothing credential mgr) oauth

main = do
  mgr <- NetConduit.newManager NetConduit.def
  let hostname = "144c.tumblr.com"
  val <- getTumblrInfo mgr hostname
  -- val <- getTumblrAvatar mgr hostname -- returns a ByteString
  -- val <- getTumblrLikes mgr hostname
  -- val <- getTumblrPosts mgr hostname
  -- val <- getTumblrFollowers mgr hostname
  -- val <- getTumblrSubmissionPosts mgr hostname
  NetConduit.closeManager mgr
  print val
