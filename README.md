humblr
======

Haskell wrapper for the Tumblr API

Usage
-----

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Conduit qualified as NetConduit
import Web.Tumblr qualified as Tumblr
import Web.Tumblr.Types qualified as Tumblr.Types
import Control.Monad.Trans.Resource
import Control.Monad.Reader
import Data.ByteString(ByteString)

apiKey :: ByteString
apiKey = "[the API key you get after registering an app with Tumblr]"

getTumblrInfo mgr hostname = runResourceT $ runReaderT (Tumblr.tumblrInfo hostname mgr) apiKey

main = do  
  mgr <- NetConduit.newManager NetConduit.tlsManagerSettings
  val <- getTumblrInfo mgr "someblog.tumblr.com"
  print val
```