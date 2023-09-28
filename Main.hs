{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.ByteString.Lazy         qualified as LB
import Network.HTTP.Conduit         qualified as NetConduit
import Web.Tumblr                   qualified as Tumblr
import Web.Tumblr.Types             qualified as Tumblr.Types


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
