{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE MultiParamTypeClasses #-}
module FGTB.Actions
where

import FGTB.Types
import FGTB.Parse
import FGTB.Map
import FGTB.Route
import FGTB.FGData
import FGTB.Action.Class
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
import Data.Text (Text)
import qualified Data.Text as Text
import System.IO
import Debug.Trace

-- * VOR-to-VOR Navigation

data VornavRequest
  = VornavRequest NavID NavID

instance FromArgs VornavRequest where
  fromArgs = \case
    [fromID, toID] -> return $ VornavRequest (mkNavID fromID) (mkNavID toID)
    _ -> Left "Expected exactly two waypoints"

data VornavResponse
  = VornavResponse Waypoint Waypoint [Waypoint] Distance

instance PrintableResult VornavResponse where
  printResult prn (VornavResponse wpFrom wpTo route dist) = do
    prn $ printf "Route from %s (%s) to %s (%s)\n"
      (waypointID wpFrom)
      (waypointTyN wpFrom)
      (waypointID wpTo)
      (waypointTyN wpTo)
    forM_ route $ \wp -> do
      prn $ printf " %s" (waypointID wp)
    prn $ printf " (%1.1f nm)\n" dist

instance Action VornavRequest VornavResponse where
  runAction fgd (VornavRequest fromID toID) = do
    let waypoints = fgdWaypoints fgd
        navs = fgdNavs fgd
        vors = filter isVor navs
        ndbs = filter isNdb navs
        findWP wpID = case filter (\wp -> waypointID wp == wpID) waypoints of
          x:_ -> return x
          [] -> error $ "Waypoint not found: " ++ show wpID

    wpFrom <- findWP fromID
    wpTo <- findWP toID
    let positionFrom = waypointLoc wpFrom
        positionTo = waypointLoc wpTo
        routeMay = vorToVor (ndbs ++ vors) wpFrom wpTo
    (route, _) <- maybe (error "No route found") return routeMay
    let dist = wpRouteLength positionFrom positionTo route

    return $
      VornavResponse
        wpFrom
        wpTo
        route
        dist

-- * Printing routes

data PrintRouteRequest
  = PrintRouteRequest [NavID]

instance FromArgs PrintRouteRequest where
  fromArgs = return . PrintRouteRequest . map mkNavID

data Leg
  = Leg Distance Bearing Waypoint Waypoint

data PrintRouteResponse
  = PrintRouteResponse
      Waypoint
      [Leg]

instance PrintableResult PrintRouteResponse where
  printResult prn (PrintRouteResponse startWP legs) = do
    let printWP wp = do
          prn $ printf "%s (%s)\n" (waypointID wp) (waypointName wp)
    forM_ legs $ \(Leg dist bearing wpFrom wpTo) -> do
      let prnNav wp =
            case wp of
              NavWP nav ->
                prn $ printf "    %s %s %05.1f\n"
                  (show $ navFreq nav)
                  (navID nav)
                  (bearing - navNorth nav)
              _ -> pure ()
      prnNav wpFrom
      prn $ printf "  %3.1f nm, %05.1f° T\n" dist bearing
      prnNav wpTo
      printWP wpTo

instance Action PrintRouteRequest PrintRouteResponse where
  runAction fgd (PrintRouteRequest ids) = do
    let waypoints = fgdWaypoints fgd
    case resolveRoute ids waypoints of
      Left wpID -> do
        error $ "Waypoint not found or ambiguous: " ++ show wpID
      Right (wp:wps) -> do
        let startWP = wp
            legs = map (uncurry makeLeg) (zip (wp:wps) wps)
            makeLeg a b =
              let (dist, bearing, _) = llDiff (waypointLoc a) (waypointLoc b)
              in Leg dist bearing a b
        return $ PrintRouteResponse startWP legs

-- * WP Info

data WPInfoRequest
  = WPInfoRequest [NavID]

instance FromArgs WPInfoRequest where
  fromArgs = return . WPInfoRequest . map mkNavID

data WPInfo
  = WPInfo
      Waypoint
      WPDetails
      [(Distance, Bearing, Bearing, Nav)]

data RunwayInfo
  = RunwayInfo
      Text
      Distance

data WPDetails
  = AirportInfo
      Text
      [RunwayInfo]
  | NavInfo
      Text
      NavFreq
      Distance
  | OtherWPInfo

data WPInfoResponse
  = WPInfoResponse
      [WPInfo]

instance PrintableResult WPInfoResponse where
  printResult prn (WPInfoResponse items) = do
    forM_ items $ \(WPInfo wp wpinfo vors) -> do
      prn $ printf "%s (%s)\n  %s\n"
        (waypointID wp)
        (waypointTyN wp)
        (prettyLatLng $ waypointLoc wp)
      case wpinfo of
        AirportInfo name rwys -> do
          prn $ printf "  %s\n" name
          prn $ printf "  Runways:\n"
          forM_ rwys $ \(RunwayInfo name length) -> do
            prn $ printf
              "    - %s (%1.0f ft)\n"
              name
              (nmToFeet length)
        NavInfo name freq range -> do
          prn $ printf "  %s %s (range: %5.1f nm)\n"
            (show freq)
            name
            range
        _ -> pure ()
      prn $ "  Nearby navaids:\n"
      forM_ vors $ \(dist, radial, bearing, nav)  -> do
        prn $ printf "    - %5.1f nm %s %05.1f (%05.1f°T) [%6s]\n"
          dist (navID nav) radial bearing (show $ navFreq nav)

instance Action WPInfoRequest WPInfoResponse where
  runAction fgd (WPInfoRequest ids) =
    WPInfoResponse <$> mapM runWP (ids >>= flip findWaypoints (fgdWaypoints fgd))
    where
      waypoints = fgdWaypoints fgd
      navs = fgdNavs fgd
      vors = filter (\nav -> isVor nav || isNdb nav) navs

      runWP :: Waypoint -> IO WPInfo
      runWP wp = do
        let nearbyVors = nearestNavs (waypointLoc wp) $ vorsInRange (waypointLoc wp) vors
            vorInfo vor =
              let (dist, bearing, _) = llDiff (navLoc vor) (waypointLoc wp)
                  radialRaw = bearing - navNorth vor
                  radial =
                    if radialRaw < 1 then
                      radialRaw + 360
                    else if radialRaw >= 361 then
                      radialRaw - 360
                    else
                      radialRaw
              in (dist, radial, bearing, vor)
            details =
              case wp of
                AirportWP ap -> 
                  AirportInfo
                    (airportName ap)
                    [ RunwayInfo
                        (Text.pack $ printf "%s/%s" (rwyStartName rwy) (rwyEndName rwy))
                        (llDist (rwyStartLoc rwy) (rwyEndLoc rwy))
                    | rwy <- airportRunways ap
                    ]
                NavWP nav ->
                  NavInfo
                    (navName nav)
                    (navFreq nav)
                    (navRange nav)
                _ ->
                  OtherWPInfo
            nearbyVorsInfo =
              map vorInfo nearbyVors
        return $ WPInfo wp details nearbyVorsInfo
