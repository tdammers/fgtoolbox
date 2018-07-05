{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE OverloadedStrings #-}
module FGTB.Geo
where

import FGTB.Types
import qualified Data.Set as Set
import Data.Set (Set)
import FGTB.AStar (aStar)
import Debug.Trace
import qualified Numeric.Units.Dimensional as D
import qualified Numeric.Units.Dimensional.SIUnits as D
import qualified Numeric.Units.Dimensional.NonSI as D
import qualified Geodetics.Geodetic as Geo
import qualified Geodetics.Path as Geo
import qualified Data.Text as Text
import Data.Maybe
import Text.Printf
import Data.Monoid

toCoord :: LatLng -> Geo.Geodetic Geo.WGS84
toCoord (LatLng (Latitude latDeg) (Longitude lngDeg)) =
  Geo.Geodetic
    (latDeg D.*~ D.degree)
    (lngDeg D.*~ D.degree)
    (0 D.*~ D.meter)
    Geo.WGS84

fromCoord :: Geo.Geodetic Geo.WGS84 -> LatLng
fromCoord (Geo.Geodetic lat lng _ _) =
  LatLng
    (Latitude $ lat D./~ D.degree)
    (Longitude $ lng D./~ D.degree)

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

llRadialDist :: LatLng -> Distance -> LatLng -> Bearing -> Maybe LatLng
llRadialDist dmeBase (Distance dmeDist) radialBase (Bearing radial) = do
  let path = Geo.rhumbPath (toCoord radialBase) (radial D.*~ D.degree)
      cond p =
        let (dist, _, _) = fromMaybe (0 D.*~ D.nauticalMile, undefined, undefined) $ Geo.groundDistance (toCoord dmeBase) p
        in dist `compare` (dmeDist D.*~ D.nauticalMile)
  dist <- Geo.bisect
            path
            cond
            (0.05 D.*~ D.nauticalMile)
            (0 D.*~ D.nauticalMile)
            (8000 D.*~ D.nauticalMile)
  let (fixCoords, _, _) = (Geo.pathFunc path) dist
  return $ fromCoord fixCoords

metersPerNm = 1852
metersPerFoot = 0.3048

mToNm :: Double -> Distance
mToNm m = Distance (m / metersPerNm)

mToFeet :: Double -> Altitude
mToFeet m = Altitude (m / metersPerFoot)

nmToFeet :: Distance -> Altitude
nmToFeet (Distance nm) =
  mToFeet $ nm * metersPerNm

