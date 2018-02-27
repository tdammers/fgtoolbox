{-#LANGUAGE OverloadedLists #-}
module FGTB.Route
where

import FGTB.Types
import qualified Data.Geo as Geo
import Data.Geo ((!.!))
import qualified Data.Geo.Vincenty as Vincenty
import Data.Geo.Accessor.Value (value)
import Data.List
import qualified Data.Set as Set
import Data.Set (Set)
import FGTB.AStar (aStar)

toCoord :: LatLng -> Geo.Coord
toCoord (LatLng (Latitude latDeg) (Longitude lngDeg)) =
  latDeg !.! lngDeg

llDiff :: LatLng -> LatLng -> (Distance, Bearing, Bearing)
llDiff from to =
  let gc = Vincenty.inverse () (toCoord from) (toCoord to)
      distRaw = Geo.ellipsoidalDistance gc
      dist = mToNm distRaw
      bearingFrom = Bearing . value . Geo.azi $ gc
      bearingTo = Bearing . value . Geo.reverseAzi $ gc
  in (dist, bearingFrom, bearingTo)

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

mToNm :: Double -> Distance
mToNm m = Distance (m / 1852)

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
  in dist <= range

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
          NavWP (Nav { navTy = NDB }) -> 1.1
          _ -> 1
    estimate a b =
      llDist (waypointLoc a) (waypointLoc b) * costFactor a
    edges a =
      let range = case a of
            NavWP nav -> navRange nav
            _ -> 0
      in [ (NavWP vor, llDist (waypointLoc a) (navLoc vor) * costFactor (NavWP vor))
         | vor <- vorsInRange' range (waypointLoc a) navs
         ]
    goalCond wp =
      dist <= range
      where
        dist = llDist (waypointLoc wp) (waypointLoc to)
        range = case wp of
          NavWP nav -> navRange nav
          _ -> 0
