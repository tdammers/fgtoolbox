{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TemplateHaskell #-}
module FGTB.Server
where

import FGTB.Actions
import FGTB.API
import FGTB.Types
import FGTB.FGData
import Web.Scotty.Trans
import Data.Proxy
import qualified Data.Text.Lazy as LText
import Data.FileEmbed (embedStringFile)
import System.FilePath

runServer :: IO FGData -> IO ()
runServer loadFGD = do
  putStrLn "Loading FGData..."
  fgdata <- loadFGD
  scottyT 5000 id $ app fgdata
  where
    app :: FGData -> ScottyT LText.Text IO ()
    app fgdata = do
      get "/" $ do
        setHeader "Content-Type" "text/html;charset=utf8"
        file $ "client/index.html"
      get "/static/:filename" $ do
        filename <- param "filename"
        let contentType = case takeExtension filename of
              ".js" -> "text/javascript;charset=utf8"
              ".css" -> "text/css;charset=utf8"
              ".html" -> "text/html;charset=utf8"
              _ -> "application/binary"
        setHeader "Content-Type" contentType
        file $ "client" </> filename
      get "/static/zepto.js" $ do
        setHeader "Content-Type" "text/javascript"
        text $(embedStringFile "client/zepto.js")
      get "/api/vornav/:from/:to" $
        apiAction (Proxy :: Proxy VornavRequest) fgdata
      get "/api/printroute/:waypoints" $
        apiAction (Proxy :: Proxy PrintRouteRequest) fgdata
      get "/api/info/:waypoints" $
        apiAction (Proxy :: Proxy WPInfoRequest) fgdata
