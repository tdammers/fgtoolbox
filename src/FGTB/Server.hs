{-#LANGUAGE OverloadedStrings #-}
module FGTB.Server
where

import FGTB.Actions
import FGTB.API
import FGTB.Types
import FGTB.FGData
import Web.Scotty.Trans
import Data.Proxy
import qualified Data.Text.Lazy as LText

runServer :: FGData -> IO ()
runServer fgdata =
  scottyT 5000 id app
  where
    app :: ScottyT LText.Text IO ()
    app = do
      get "/api/vornav/:from/:to" $
        apiAction (Proxy :: Proxy VornavRequest) fgdata
