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
import Control.Monad
import Control.Monad.IO.Class

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
        file $ "client/html/index.html"
      get "/static/:dir/:filename" $ do
        dirname <- takeBaseName <$> param "dir"
        -- when (dirname `notElem` ["js", "css"]) next
        filename <- param "filename"
        let contentType = case takeExtension filename of
              ".js" -> "text/javascript;charset=utf8"
              ".css" -> "text/css;charset=utf8"
              _ -> "application/binary"
        setHeader "Content-Type" contentType
        file $ "client" </> dirname </> filename
      get "/api/vornav/:from/:to" $
        apiAction (Proxy :: Proxy VornavRequest) (pure fgdata)
      get "/api/printroute/:waypoints" $
        apiAction (Proxy :: Proxy PrintRouteRequest) (pure fgdata)
      get "/api/info/:waypoints" $
        apiAction (Proxy :: Proxy WPInfoRequest) (pure fgdata)
      get "/api/wind" $
        apiAction (Proxy :: Proxy WindCalcRequest) (pure fgdata)
