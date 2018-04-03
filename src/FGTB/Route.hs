{-#LANGUAGE OverloadedLists #-}
module FGTB.Route
where

import FGTB.Types
import Data.List
import qualified Data.Set as Set
import Data.Set (Set)
import FGTB.AStar (aStar)
import Debug.Trace
import qualified Numeric.Units.Dimensional as D
import qualified Numeric.Units.Dimensional.SIUnits as D
import qualified Numeric.Units.Dimensional.NonSI as D
import qualified Geodetics.Geodetic as Geo
import Data.Maybe
import Text.Printf

toCoord :: LatLng -> Geo.Geodetic Geo.WGS84
toCoord (LatLng (Latitude latDeg) (Longitude lngDeg)) =
  Geo.Geodetic
    (latDeg D.*~ D.degree)
    (lngDeg D.*~ D.degree)
    (0 D.*~ D.meter)
    Geo.WGS84

llDiff :: LatLng -> LatLng -> (Distance, Bearing, Bearing)
llDiff from to
  | from == to =
    (Distance 0, Bearing 0, Bearing 180)
  | otherwise =
    fromMaybe (mToNm (1/0), Bearing 0, Bearing 180) $ do
      (distRaw, aziRaw, revAziRaw) <- Geo.groundDistance (toCoord from) (toCoord to)
      let dist = Distance $ distRaw D./~ D.nauticalMile
          bearingFrom = Bearing $ aziRaw D./~ D.degree
          bearingTo = Bearing $ revAziRaw D./~ D.degree
      return (dist, bearingFrom, bearingTo)

llDist :: LatLng -> LatLng -> Distance
llDist from to =
  dist
  where
    (dist, _, _) = llDiff from to

llBearingFrom :: LatLng -> LatLng -> Bearing
llBearingFrom from to =
  bearing
  where
    (_, bearing, _) = llDiff from to

llBearingTo :: LatLng -> LatLng -> Bearing
llBearingTo from to =
  bearing
  where
    (_, _, bearing) = llDiff from to

metersPerNm = 1852
metersPerFoot = 0.3048

mToNm :: Double -> Distance
mToNm m = Distance (m / metersPerNm)

mToFeet :: Double -> Altitude
mToFeet m = Altitude (m / metersPerFoot)

nmToFeet :: Distance -> Altitude
nmToFeet (Distance nm) =
  mToFeet $ nm * metersPerNm

vorsInRange :: LatLng -> [Nav] -> [Nav]
vorsInRange pos = filter (isVorInRange pos)

vorsInRange' :: Distance -> LatLng -> [Nav] -> [Nav]
vorsInRange' addDist pos = filter (isVorInRange' addDist pos)

isVorInRange :: LatLng -> Nav -> Bool
isVorInRange = isVorInRange' 0

isVorInRange' :: Distance -> LatLng -> Nav -> Bool
isVorInRange' addDist pos nav =
  let dist = llDist pos (navLoc nav)
      range = navRange nav + addDist
  in dist <= range * 0.5

nearestNavs :: LatLng -> [Nav] -> [Nav]
nearestNavs pos navs =
  sortOn (llDist pos . navLoc) navs

routeLength :: [LatLng] -> Distance
routeLength [] =
  0
routeLength [x] =
  0
routeLength (x:y:xs) =
  llDist x y + routeLength (y:xs)

wpRouteLength :: LatLng -> LatLng -> [Waypoint] -> Distance
wpRouteLength from to via =
  routeLength ([from] ++ map waypointLoc via ++ [to])

navRouteLength :: LatLng -> LatLng -> [Nav] -> Distance
navRouteLength from to via =
  routeLength ([from] ++ map navLoc via ++ [to])

vorToVor :: [Nav] -> Waypoint -> Waypoint -> Maybe ([Waypoint], Distance)
vorToVor navs from to =
  aStar
    estimate
    edges
    goalCond
    from
    to
  where
    costFactor b =
        case b of
          NavWP (Nav { navTy = NDB }) -> 1.3
          _ -> 1
    estimate a b =
      llDist (waypointLoc a) (waypointLoc b) * costFactor a
    edges a =
      let range = case a of
            NavWP nav -> navRange nav
            _ -> 0
      in -- Prefer navs in range
         [ let d = llDist (waypointLoc a) (navLoc vor)
           in (NavWP vor, d * costFactor (NavWP vor))
         | vor <- vorsInRange' range (waypointLoc a) navs
         ]
    goalCond wp =
      dist <= range
      where
        dist = llDist (waypointLoc wp) (waypointLoc to)
        range = case wp of
          NavWP nav -> navRange nav
          _ -> 0

findWaypoints :: NavID -> [Waypoint] -> [Waypoint]
findWaypoints nid = filter ((== nid) . waypointID)

findWaypoint :: NavID -> [Waypoint] -> Maybe Waypoint
findWaypoint nid waypoints =
  case candidates of
    [x] -> Just x
    [] -> Nothing
  where
    candidates =
      findWaypoints nid waypoints

findWaypointNear :: NavID -> LatLng -> [Waypoint] -> Maybe Waypoint
findWaypointNear nid targetLoc waypoints =
  case candidates of
    x:_ -> Just x
    [] -> Nothing
  where
    candidates =
      sortOn (llDist targetLoc . waypointLoc) $
      findWaypoints nid waypoints

resolveRoute :: [WPSpec] -> [Waypoint] -> Either WPSpec [Waypoint]
resolveRoute navs waypoints = go Nothing navs
  where
    go :: Maybe Waypoint -> [WPSpec] -> Either WPSpec [Waypoint]
    go _ [] = Right []
    go current (x:xs) = do
      wp <- maybe (Left x) Right $ case current of
        Nothing ->
          findWP x waypoints
        Just ref ->
          findWP (WPSpecNearby x (WPSpecLL . waypointLoc $ ref)) waypoints
      rest <- go (Just wp) xs
      return $ wp:rest

findWPs :: WPSpec -> [Waypoint] -> [Waypoint]
findWPs (WPSpecID wpid) wps = filter ((== wpid) . waypointID) wps
findWPs (WPSpecLL ll) _ = [GpsWP ll]
findWPs (WPSpecNearby spec refSpec) wps = do
  refWP <- take 1 $ findWPs refSpec wps
  let refLL = waypointLoc refWP
  sortOn (llDist refLL . waypointLoc) (findWPs spec wps)

findWP :: WPSpec -> [Waypoint] -> Maybe Waypoint
findWP (WPSpecLL ll) _ = Just $ GpsWP ll
findWP (WPSpecNearby spec refSpec) wps = do
  refWP <- findWP refSpec wps
  let refLL = waypointLoc refWP
      candidates = sortOn (llDist refLL . waypointLoc) (findWPs spec wps)
  case candidates of
    [] -> Nothing
    (x:_) -> Just x
findWP spec wps = case findWPs spec wps of
  [] -> Nothing
  (x:_) -> Just x
