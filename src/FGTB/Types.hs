{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE OverloadedStrings #-}
module FGTB.Types
where

import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Printf
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

newtype Latitude =
  -- In degrees
  Latitude Double
  deriving (Read, Show, Eq, Ord, Num, Fractional)

newtype Longitude =
  -- In degrees
  Longitude Double
  deriving (Read, Show, Eq, Ord, Num, Fractional)

data LatLng =
  -- In degrees
  LatLng { lat :: Latitude, lng :: Longitude }
  deriving (Read, Show, Eq)

avgLatLng :: [LatLng] -> LatLng
avgLatLng [] = LatLng 0 0
avgLatLng lls =
  LatLng
    ((sum . map lat $ lls) / fromIntegral count)
    ((sum . map lng $ lls) / fromIntegral count)
  where
    count = length lls

newtype Distance =
  -- In nautical miles
  Distance Double
  deriving (Read, Show, Eq, Ord, Num, PrintfArg, Fractional)

newtype Altitude =
  -- In feet
  Altitude Double
  deriving (Read, Show, Eq, Ord, Num, PrintfArg)

newtype Bearing =
  -- In degrees
  Bearing Double
  deriving (Read, Show, Eq, Ord, Num)

newtype NavID = NavID Text
  deriving (IsString, Read, Show, Eq, Ord, PrintfArg)

mkNavID :: String -> NavID
mkNavID = NavID . Text.pack

data Fix = Fix { fixID :: NavID, fixLoc :: LatLng }
  deriving (Read, Show, Eq)

data NavTy
  = NDB -- 2
  | VOR -- 3
  | ILS -- 4
  | LOC -- 5
  | GS -- 6
  | OM -- 7
  | MM -- 8
  | IM -- 9
  | VORDME -- 12
  | TACAN -- 13
  deriving (Read, Show, Eq, Enum, Ord, Bounded)

newtype NavFreq = NavFreq Integer -- in kHz
  deriving (Eq, Ord)

instance Show NavFreq where
  show (NavFreq freq)
    | freq < 1000 =
        -- kHz / MF band
        show freq
    | otherwise =
        -- MHz / VHF band
        let raw = show freq
            prefLen = length raw - 2
            intpart = take prefLen raw
            fracpart = drop prefLen raw
        in intpart ++ "." ++ fracpart

instance PrintfArg NavFreq where
  formatArg (NavFreq freq)
    | freq < 1000 = formatArg freq
    | otherwise = formatArg (fromIntegral freq / 1000 :: Double)

data Nav =
  Nav
    { navTy :: NavTy
    , navID :: NavID
    , navLoc :: LatLng
    , navAlt :: Altitude
    , navFreq :: NavFreq
    , navName :: Text
    , navNorth :: Bearing
    , navRange :: Distance
    }
    deriving (Show, Eq)

isVor :: Nav -> Bool
isVor nav =
  navTy nav == VOR &&
  (not $ ("I" `Text.isPrefixOf` name) && (Text.length name == 4))
  where
    NavID name = navID nav

isNdb :: Nav -> Bool
isNdb nav =
  navTy nav == NDB &&
  ("NDB" `Text.isSuffixOf` navName nav)

data Airport
  = Airport
      { airportID :: NavID
      , airportName :: Text
      , airportRunways :: [Runway]
      }
      deriving (Show, Eq)

data Runway
  = Runway
      { rwyStartLoc :: LatLng
      , rwyStartName :: Text
      , rwyEndLoc :: LatLng
      , rwyEndName :: Text
      }
      deriving (Show, Eq)

airportLoc :: Airport -> LatLng
airportLoc ap = avgLatLng $
  [ avgLatLng [rwyStartLoc rwy, rwyEndLoc rwy]
  | rwy <- airportRunways ap
  ]

data Waypoint
  = NavWP Nav
  | FixWP Fix
  | AirportWP Airport
  | GpsWP LatLng
  deriving (Show, Eq)

waypointID :: Waypoint -> NavID
waypointID (NavWP nav) = navID nav
waypointID (FixWP fix) = fixID fix
waypointID (GpsWP ll) = mkNavID $ "GPS" ++ show ll
waypointID (AirportWP ap) = airportID ap

waypointLoc :: Waypoint -> LatLng
waypointLoc (NavWP nav) = navLoc nav
waypointLoc (FixWP fix) = fixLoc fix
waypointLoc (GpsWP ll) = ll
waypointLoc (AirportWP ap) = airportLoc ap

waypointTyN :: Waypoint -> Text
waypointTyN (NavWP nav) = Text.pack . show . navTy $ nav
waypointTyN (FixWP _) = "FIX"
waypointTyN (GpsWP _) = "GPS"
waypointTyN (AirportWP _) = "AIRPORT"

findWaypoint :: NavID -> [Waypoint] -> [Waypoint]
findWaypoint nid = filter ((== nid) . waypointID)
