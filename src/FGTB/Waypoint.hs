{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE TemplateHaskell #-}
module FGTB.Waypoint
where

import FGTB.Types
import FGTB.Geo

import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Printf
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=), object)
import qualified Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON, defaultOptions, Options (..))
import Data.Char (toLower, toUpper)
import Data.Monoid
import Text.Read (readMaybe)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Control.Applicative
import Data.Maybe

offsetLoc :: NavOffset -> LatLng
offsetLoc ofs =
  fromMaybe (waypointLoc . offsetDistBase $ ofs) $
  llRadialDist
    (waypointLoc . offsetDistBase $ ofs)
    (offsetDist ofs)
    (waypointLoc . offsetRadialBase $ ofs)
    (offsetRadial ofs)

data NavOffset
  = NavOffset
      { offsetRadialBase :: Waypoint
      , offsetRadial :: Bearing
      , offsetDistBase :: Waypoint
      , offsetDist :: Distance
      }
      deriving (Show, Eq)

instance ToJSON NavOffset where
  toJSON (NavOffset radialBase radial distBase dist) =
    object
      [ "loc" .= radialBase
      , "radial" .= radial
      , "dme" .= distBase
      , "dist" .= dist
      ]

data Waypoint
  = NavWP Nav
  | FixWP Fix
  | AirportWP Airport
  | OffsetWP NavOffset
  | GpsWP LatLng
  deriving (Show, Eq)

instance ToJSON Waypoint where
  toJSON = \case
    NavWP nav -> toJSON nav
    FixWP fix -> toJSON fix
    AirportWP ap -> toJSON ap
    GpsWP ll -> toJSON ll

instance Ord Waypoint where
  compare a b =
    case compare (waypointID a) (waypointID b) of
      EQ -> compare (waypointName a) (waypointName b)
      x -> x

waypointID :: Waypoint -> NavID
waypointID (NavWP nav) = navID nav
waypointID (FixWP fix) = fixID fix
waypointID (GpsWP ll) = mkNavID $ "GPS" ++ show ll
waypointID (OffsetWP ofs)
  | offsetRadialBase ofs == offsetDistBase ofs =
      mkNavID $
        printf "%3.1f%s%3.0f"
          (offsetDist ofs)
          (waypointID $ offsetRadialBase ofs)
          (offsetRadial ofs)
  | otherwise =
      mkNavID $
        printf "%3.1f%s,%s%3.0f"
          (offsetDist ofs)
          (waypointID $ offsetDistBase ofs)
          (waypointID $ offsetRadialBase ofs)
          (offsetRadial ofs)
waypointID (AirportWP ap) = airportID ap

waypointLoc :: Waypoint -> LatLng
waypointLoc (NavWP nav) = navLoc nav
waypointLoc (FixWP fix) = fixLoc fix
waypointLoc (GpsWP ll) = ll
waypointLoc (OffsetWP ofs) = offsetLoc ofs
waypointLoc (AirportWP ap) = airportLoc ap

waypointTyN :: Waypoint -> Text
waypointTyN (NavWP nav) = Text.pack . show . navTy $ nav
waypointTyN (FixWP _) = "FIX"
waypointTyN (GpsWP _) = "GPS"
waypointTyN (OffsetWP _) = "RNAV"
waypointTyN (AirportWP _) = "AIRPORT"

waypointName :: Waypoint -> Text
waypointName (NavWP nav) = navName nav
waypointName (AirportWP ap) = airportName ap
waypointName (FixWP _) = "fix"
waypointName (OffsetWP ofs) =
  Text.pack $
    printf "%3.1f DME %s, R%3.0f %s"
      (offsetDist ofs)
      (waypointName $ offsetDistBase ofs)
      (offsetRadial ofs)
      (waypointName $ offsetRadialBase ofs)

waypointName (GpsWP ll) = Text.pack . prettyLatLng $ ll
