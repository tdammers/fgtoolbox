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
import Debug.Trace
-- import qualified Control.Monad.Combinators as P

newtype Latitude =
  -- In degrees
  Latitude { unLatitude :: Double }
  deriving (Read, Show, Eq, Ord, Num, Fractional, FromJSON, ToJSON)

mkLat :: Int -> Double -> Latitude
mkLat deg min = Latitude $ fromIntegral deg + min / 60

newtype Longitude =
  -- In degrees
  Longitude { unLongitude :: Double }
  deriving (Read, Show, Eq, Ord, Num, Fractional, FromJSON, ToJSON)

mkLng :: Int -> Double -> Longitude
mkLng deg min = Longitude $ fromIntegral deg + min / 60

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

prettyLatLng :: LatLng -> String
prettyLatLng (LatLng lat lng) =
  prettyLat lat ++ " " ++ prettyLng lng

prettyLat :: Latitude -> String
prettyLat (Latitude l)
  | l >= 0 = prettyAngle fmt (abs l) ++ "N"
  | otherwise = prettyAngle fmt (abs l) ++ "S"
  where
    fmt = "%02i°%02i'%02.4f\""

prettyLng :: Longitude -> String
prettyLng (Longitude l)
  | l >= 0 = prettyAngle fmt (abs l) ++ "E"
  | otherwise = prettyAngle fmt (abs l) ++ "W"
  where
    fmt = "%03i°%02i'%02.4f\""

prettyAngle :: String -> Double -> String
prettyAngle fmt alpha =
  printf fmt
    deg min sec
  where
    deg = floor alpha :: Int
    min = floor (alpha * 60) `mod` 60 :: Int
    sec = alpha * 3600 - fromIntegral deg * 3600 - fromIntegral min * 60 :: Double

newtype Distance =
  -- In nautical miles
  Distance Double
  deriving (Read, Show, Eq, Ord, Num, PrintfArg, Fractional, ToJSON, FromJSON)

newtype Speed =
  -- In knots
  Speed Double
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

unNavID :: NavID -> Text
unNavID (NavID nid) = nid

data NavOffsetSpec
  = NavOffsetSpec
      { offsetSpecRadialBase :: NavID
      , offsetSpecRadial :: Bearing
      , offsetSpecDistBase :: NavID
      , offsetSpecDist :: Distance
      }
      deriving (Show)

data WPSpec
  = WPSpecID NavID -- ^ Look up waypoint by ID
  | WPSpecOfs NavOffsetSpec
  | WPSpecLL LatLng -- ^ Make GPS waypoint based on lat/lon
  | WPSpecNearby WPSpec WPSpec -- ^ Find waypoint nearest to other waypoint
  deriving (Show)

parseWPSpec :: String -> WPSpec
parseWPSpec src =
  trace ("src: " ++ src) $
  fromMaybe (WPSpecID . NavID . Text.pack $ src) $
    P.parseMaybe specP src
  where
    specP :: P.Parsec () String WPSpec
    specP = do
      lhs <- P.try llSpecP <|> P.try offsetSpecP <|> idSpecP
      rhsMay <- P.optional $ P.char '@' *> specP
      case rhsMay of
        Nothing -> return lhs
        Just rhs -> return $ WPSpecNearby lhs rhs

    llSpecP :: P.Parsec () String WPSpec
    llSpecP = WPSpecLL <$> (LatLng <$> latP <* P.char ',' <*> lngP)

    idSpecP :: P.Parsec () String WPSpec
    idSpecP =
      WPSpecID <$> navIDP

    navIDP :: P.Parsec () String NavID
    navIDP =
      NavID . Text.pack <$> P.many (P.oneOf (['A'..'Z'] :: [Char]))

    offsetSpecP :: P.Parsec () String WPSpec
    offsetSpecP = do
      dist <- Distance <$> doubleP
      traceM $ "dist: " ++ show dist
      distNav <- navIDP
      traceM $ "distNav: " ++ show distNav
      radialNav <- fmap (fromMaybe distNav) . P.optional $ do
        P.char ','
        navIDP
      traceM $ "radialNav: " ++ show radialNav
      radial <- Bearing <$> doubleP
      traceM $ "radial: " ++ show radial
      return . WPSpecOfs $ NavOffsetSpec radialNav radial distNav dist
      

    latP :: P.Parsec () String Latitude
    latP = do
      angle <- angleP
      sign <- (P.char 'N' *> pure id)
              <|> (P.char 'S' *> pure negate)
      return $ Latitude (sign angle)

    lngP :: P.Parsec () String Longitude
    lngP = do
      angle <- angleP
      sign <- (P.char 'E' *> pure id)
              <|> (P.char 'W' *> pure negate)
      return $ Longitude (sign angle)

    angleP :: P.Parsec () String Double
    angleP = do
      degrees <- intP
      P.char '°'
      minutes <- intP
      P.char '\''
      seconds <- doubleP
      return $
        seconds / 3600 +
        fromIntegral minutes / 60 +
        fromIntegral degrees

    intP :: P.Parsec () String Int
    intP = readTraced <$> P.many P.digitChar

    doubleP :: P.Parsec () String Double
    doubleP = do
      intpart <- P.some P.digitChar
      fracpart <- fmap (fromMaybe "0") . P.optional $ do
        P.char '.'
        P.many P.digitChar
      return . readTraced $ intpart ++ "." ++ fracpart

readTraced :: Read a => String -> a
readTraced str =
  trace str $ read str

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

