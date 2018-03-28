{-#LANGUAGE OverloadedStrings #-}
module Main where

import FGTB.Types
import FGTB.Parse
import FGTB.Map
import FGTB.Route
import FGTB.FGData
import FGTB.Action.Class
import FGTB.Actions
import FGTB.CLI

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as LBS8
import System.FilePath
import qualified Codec.Compression.GZip as GZip
import qualified Graphics.Rendering.Cairo as Cairo
import Text.Printf
import System.Environment
import Data.List
import Data.Maybe
import Data.Ini (readIniFile)
import qualified Data.Ini as Ini
import qualified Data.Text as Text
import System.IO
import Data.Proxy

main :: IO ()
main = do
  args <- getArgs
  home <- getEnv "HOME"
  let fgrootDef = "/usr/share/games/flightgear"
  fgIni <- (readIniFile $ home </> ".fgfs/FlightGear/FlightGear.ini")
           >>= either error return
  let fgrootIni =
        either (const fgrootDef) Text.unpack $
          Ini.lookupValue "General" "fg-root" fgIni
  fgroot <- fromMaybe fgrootIni
              <$> lookupEnv "FGROOT"
  let cacheDir = home </> ".fgtoolbox"
      loadFGD = loadFGDataCached cacheDir fgroot
  case args of
    "vornav":rem -> do
        cliAction
          (Proxy :: Proxy VornavRequest)
          loadFGD
          rem
    "printroute":rem -> do
        fgdata <- loadFGDataCached cacheDir fgroot
        cliAction
          (Proxy :: Proxy PrintRouteRequest)
          loadFGD
          rem
    "info":rem -> do
        fgdata <- loadFGDataCached cacheDir fgroot
        cliAction
          (Proxy :: Proxy WPInfoRequest)
          loadFGD
          rem
    xs -> error $ "Invalid arguments"
