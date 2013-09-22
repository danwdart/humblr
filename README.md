humblr
======

Haskell wrapper for the Tumblr API

Usage
-----

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Network.HTTP.Conduit as NetConduit
import qualified Web.Tumblr as Tumblr
import qualified Web.Tumblr.Types as Tumblr.Types
import Control.Monad.Trans.Resource
import Control.Monad.Reader
import Data.ByteString(ByteString)

apiKey :: ByteString
apiKey = "[the API key you get after registering an app with Tumblr]"

getTumblrInfo mgr hostname = runResourceT $ runReaderT (Tumblr.tumblrInfo hostname mgr) apiKey

main = do  
  mgr <- NetConduit.newManager NetConduit.def
  val <- getTumblrInfo mgr "someblog.tumblr.com"
  NetConduit.closeManager mgr
  print val
```