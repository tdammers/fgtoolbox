module Main where

import FGTB.Types
import FGTB.Parse
import FGTB.Map
import FGTB.Route
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as LBS8
import System.FilePath
import qualified Codec.Compression.GZip as GZip
import qualified Graphics.Rendering.Cairo as Cairo
import Text.Printf
import System.Environment
import Data.List

tee :: (a -> IO b) -> IO a -> IO a
tee sidechannel action = do
  val <- action
  sidechannel val
  return val

printCount :: [a] -> IO ()
printCount [x] =
  printf "Found 1 item\n"
printCount xs =
  printf "Found %d items\n" (length xs)

loadGZip8 :: FilePath -> IO String
loadGZip8 fp =
  LBS8.unpack . GZip.decompress <$> LBS8.readFile fp

loadFixes :: FilePath -> IO [Fix]
loadFixes fgroot = do
  putStrLn "Loading fixes..."
  tee printCount $ parseFixes <$> loadGZip8 (fgroot </> "Navaids" </> "fix.dat.gz")

loadNavs :: FilePath -> IO [Nav]
loadNavs fgroot = do
  putStrLn "Loading navs..."
  tee printCount $ parseNavs <$> loadGZip8 (fgroot </> "Navaids" </> "nav.dat.gz")

loadAirports :: FilePath -> IO [Airport]
loadAirports fgroot = do
  putStrLn "Loading airports..."
  tee printCount $ parseAirports <$> loadGZip8 (fgroot </> "Airports" </> "apt.dat.gz")

mkLat :: Int -> Double -> Latitude
mkLat deg min = Latitude $ fromIntegral deg + min / 60

mkLng :: Int -> Double -> Longitude
mkLng deg min = Longitude $ fromIntegral deg + min / 60

main :: IO ()
main = do
  args <- getArgs
  case args of
    "vornav":rem -> case rem of
      [fromID, toID] -> runVorToVor (mkNavID fromID) (mkNavID toID)
      xs -> error $ "Invalid arguments for VOR-to-VOR navigation"
    xs -> error $ "Invalid arguments"

runVorToVor fromID toID = do
  let fgroot = "/usr/share/games/flightgear"
  fixes <- loadFixes fgroot
  navs <- loadNavs fgroot
  airports <- loadAirports fgroot
  let waypoints =
        concat
          [ map NavWP navs
          , map FixWP fixes
          , map AirportWP airports
          ]
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
