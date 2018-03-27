{-#LANGUAGE OverloadedStrings #-}
module Main where

import FGTB.Types
import FGTB.Parse
import FGTB.Map
import FGTB.Route
import FGTB.FGData
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

mkLat :: Int -> Double -> Latitude
mkLat deg min = Latitude $ fromIntegral deg + min / 60

mkLng :: Int -> Double -> Longitude
mkLng deg min = Longitude $ fromIntegral deg + min / 60

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
  case args of
    "vornav":rem -> case rem of
      [fromID, toID] -> do
        fgdata <- loadFGDataCached cacheDir fgroot
        runVorToVor fgdata (mkNavID fromID) (mkNavID toID)
      xs -> error $ "Invalid arguments for VOR-to-VOR navigation"
    "printroute":rem -> do
      fgdata <- loadFGDataCached cacheDir fgroot
      runPrintRoute fgdata (map mkNavID rem)
    xs -> error $ "Invalid arguments"

runPrintRoute :: FGData -> [NavID] -> IO ()
runPrintRoute fgd ids = do
  let waypoints = fgdWaypoints fgd
  case resolveRoute ids waypoints of
    Left wpID -> do
      error $ "Waypoint not found or ambiguous: " ++ show wpID
    Right wps -> do
      printWPs wps
  where
    printWPs :: [Waypoint] -> IO ()
    printWPs [] = do
      return ()
    printWPs [x] = do
      printWPInfo x
    printWPs (x:x':xs) = do
      printWPInfo x
      printLegInfo x x'
      printWPs (x':xs)

    printWPInfo :: Waypoint -> IO ()
    printWPInfo wp = do
      printf "%s\n" (waypointID wp)

    printLegInfo :: Waypoint -> Waypoint -> IO ()
    printLegInfo a b = do
      let (dist, bearFrom, bearTo) =
            llDiff (waypointLoc a) (waypointLoc b)
      printf "%3.1f nm, %03.1f° T\n" dist bearFrom

runVorToVor :: FGData -> NavID -> NavID -> IO ()
runVorToVor fgd fromID toID = do
  let waypoints = fgdWaypoints fgd
      navs = fgdNavs fgd
      vors = filter isVor navs
      ndbs = filter isNdb navs
      findWP wpID = case filter (\wp -> waypointID wp == wpID) waypoints of
        x:_ -> return x
        [] -> error $ "Waypoint not found: " ++ show wpID

  wpFrom <- findWP fromID
  wpTo <- findWP toID

  -- Cairo.withSVGSurface "map.svg" 3600 1800 $ \cairo ->
  --   Cairo.renderWith cairo $ do
  --     Cairo.scale 10 10
  --     renderNavs navs

  let positionFrom = waypointLoc wpFrom
      positionTo = waypointLoc wpTo
      routeMay = vorToVor (ndbs ++ vors) wpFrom wpTo

  printf "Routing from %s (%s) to %s (%s)\n"
    (waypointID wpFrom)
    (waypointTyN wpFrom)
    (waypointID wpTo)
    (waypointTyN wpTo)

  case routeMay of
    Nothing -> printf "No route found\n"
    Just (route, cost) -> do
      forM_ route $ \wp -> do
        printf " %s" (waypointID wp)
      printf " (%1.1f nm)\n"
        (wpRouteLength positionFrom positionTo route)
