{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE TemplateHaskell #-}
module FGTB.Types
where

import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Printf
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Aeson (FromJSON (..), ToJSON (..), (.:))
import qualified Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON, defaultOptions, Options (..))
import Data.Char (toLower, toUpper)
import Data.Monoid
import Text.Read (readMaybe)

newtype Latitude =
  -- In degrees
  Latitude Double
  deriving (Read, Show, Eq, Ord, Num, Fractional, FromJSON, ToJSON)

newtype Longitude =
  -- In degrees
  Longitude Double
  deriving (Read, Show, Eq, Ord, Num, Fractional, FromJSON, ToJSON)

data LatLng =
  -- In degrees
  LatLng { lat :: Latitude, lng :: Longitude }
  deriving (Read, Show, Eq)

instance ToJSON LatLng where
  toJSON ll = toJSON (lat ll, lng ll)

instance FromJSON LatLng where
  parseJSON = \case
    JSON.Object obj ->
      LatLng <$> obj .: "lat"
             <*> obj .: "lng"
    val@(JSON.Array _) -> do
      (latVal, lngVal) <- parseJSON val
      return $ LatLng latVal lngVal
    x -> fail $ "Invalid LatLng value: " <> show x

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
  deriving (Read, Show, Eq, Ord, Num, PrintfArg, Fractional, ToJSON, FromJSON)

newtype Altitude =
  -- In feet
  Altitude Double
  deriving (Read, Show, Eq, Ord, Num, PrintfArg, ToJSON, FromJSON)

newtype Bearing =
  -- In degrees
  Bearing Double
  deriving (Read, Show, Eq, Ord, Num, PrintfArg, ToJSON, FromJSON)

newtype NavID = NavID Text
  deriving (IsString, Read, Show, Eq, Ord, PrintfArg, ToJSON, FromJSON)

mkNavID :: String -> NavID
mkNavID = NavID . Text.pack

data Fix = Fix { fixID :: NavID, fixLoc :: LatLng }
  deriving (Read, Show, Eq)

deriveJSON
  defaultOptions { fieldLabelModifier = map toLower . drop 3 }
  ''Fix

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

instance ToJSON NavTy where
  toJSON = toJSON . Text.pack . show

instance FromJSON NavTy where
  parseJSON val = do
    valueMay <- readMaybe .
                map toUpper .
                Text.unpack <$>
                parseJSON val
    case valueMay of
      Nothing -> fail $ "Invalid NAV type: " <> show val
      Just x -> return x

newtype NavFreq = NavFreq Integer -- in kHz
  deriving (Eq, Ord, ToJSON, FromJSON)

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

deriveJSON
  defaultOptions { fieldLabelModifier = map toLower . drop 3 }
  ''Nav

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

deriveJSON
  defaultOptions { fieldLabelModifier = map toLower . drop 3 }
  ''Runway

deriveJSON
  defaultOptions { fieldLabelModifier = map toLower . drop 7 }
  ''Airport

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
