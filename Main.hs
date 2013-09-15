{-# LANGUAGE OverloadedStrings #-}

import qualified Network.HTTP.Conduit as NetConduit
import qualified Web.Tumblr as Tumblr
import qualified Web.Tumblr.Types as Tumblr.Types
import Control.Monad.Trans.Resource
import Control.Monad.Reader
import Data.ByteString(ByteString)
import qualified Data.ByteString.Lazy as LB

apiKey :: ByteString
apiKey = "bz5NED3ASSK1XBHjnfb0WSigo5Y2jxYvhm09u4OQn21isU4twQ"

getTumblrInfo mgr hostname = runResourceT $ runReaderT (Tumblr.tumblrInfo hostname mgr) apiKey

getTumblrAvatar mgr hostname = runResourceT $ Tumblr.tumblrAvatar hostname Nothing mgr

getTumblrLikes mgr hostname = runResourceT $ runReaderT (Tumblr.tumblrLikes hostname Nothing Nothing mgr) apiKey

getTumblrPosts mgr hostname = runResourceT $ runReaderT (Tumblr.tumblrPosts hostname Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing mgr) apiKey

main = do  
  mgr <- NetConduit.newManager NetConduit.def
  val <- getTumblrLikes mgr "144c.tumblr.com"
  NetConduit.closeManager mgr
  print val