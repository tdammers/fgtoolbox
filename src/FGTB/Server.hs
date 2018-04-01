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

runServer :: IO FGData -> IO ()
runServer loadFGD = do
  putStrLn "Loading FGData..."
  fgdata <- loadFGD
  scottyT 5000 id $ app fgdata
  where
    app :: FGData -> ScottyT LText.Text IO ()
    app fgdata = do
      get "/api/vornav/:from/:to" $
        apiAction (Proxy :: Proxy VornavRequest) fgdata
      get "/api/printroute/:waypoints" $
        apiAction (Proxy :: Proxy PrintRouteRequest) fgdata
      get "/api/info/:waypoints" $
        apiAction (Proxy :: Proxy WPInfoRequest) fgdata
