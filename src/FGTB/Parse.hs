{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE OverloadedStrings #-}
module FGTB.Parse
where

import FGTB.Types
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe
import Debug.Trace

parseFix :: String -> Maybe Fix
parseFix str = case words str of
  latStr:lngStr:name:_ -> do
    pos <- parseLatLng latStr lngStr
    let navID = mkNavID name
    return $ Fix navID pos
  _ -> Nothing

parseFixes :: String -> [Fix]
parseFixes =
  catMaybes . map parseFix . drop 3 . lines

parseAirports :: String -> [Airport]
parseAirports src =
  go rules
  where
    rules = map (Text.words . Text.pack) . lines $ src
    go :: [[Text]] -> [Airport]
    go [] = []
    go (rule:rules) = case rule of
      "1":args ->
        case args of
          _:_:_:nidStr:nameStrs ->
            let (mine, rest) = break ((== ["1"]) . take 1) rules
                runways = catMaybes
                        . map parseRunway
                        . filter ((== ["100"]) . take 1)
                        $ mine
                name = Text.unwords $ nameStrs
                nid = NavID nidStr
            in Airport nid name runways : go rest
          _ -> go rules
      _ ->
        go rules

parseRunway :: [Text] -> Maybe Runway
parseRunway [ "100"
            , _ , _ , _ , _ , _ , _ , _
            , startNameStr
            , startLatStr
            , startLngStr
            , _ , _ , _ , _ , _ , _
            , endNameStr
            , endLatStr
            , endLngStr
            , _ , _ , _ , _ , _ , _
            ] = do
  startLoc <- parseLatLng (Text.unpack startLatStr) (Text.unpack startLngStr)
  let startName = startNameStr
  endLoc <- parseLatLng (Text.unpack endLatStr) (Text.unpack endLngStr)
  let endName = endNameStr
  return $ Runway startLoc startName endLoc endName
parseRunway _ =
  Nothing

parseNav :: String -> Maybe Nav
parseNav str = case words str of
  tyStr:latStr:lngStr:altStr:freqStr:rangeStr:dirStr:name:descStrs -> do
    -- 3  37.34916667 -105.81552778   7535 11390 130   13.0 ALS  ALAMOSA VORTAC
    -- ty lat          lon            alt? freq  rng bearng ID   name
    ty <- parseNavTy tyStr
    pos <- parseLatLng latStr lngStr
    alt <- parseAltitude altStr
    freq <- parseKHz freqStr
    range <- parseDistance rangeStr
    bearing <- parseBearing dirStr
    let nID = mkNavID name
    let desc = Text.pack $ unwords descStrs
    return Nav
            { navTy = ty
            , navID = nID
            , navLoc = pos
            , navAlt = alt
            , navFreq = freq
            , navName = desc
            , navNorth = bearing
            , navRange = range
            }
  _ -> Nothing

parseNavTy :: String -> Maybe NavTy
parseNavTy = \case
  "2" -> Just NDB
  "3" -> Just VOR
  "4" -> Just ILS
  "5" -> Just LOC
  "6" -> Just GS
  "7" -> Just OM
  "8" -> Just MM
  "9" -> Just IM
  "12" -> Just VORDME
  "13" -> Just TACAN
  _ -> Nothing

parseNavs :: String -> [Nav]
parseNavs =
  catMaybes . map parseNav . drop 3 . lines

parseLatLng :: String -> String -> Maybe LatLng
parseLatLng latStr lngStr = do
  lat <- parseLat latStr
  lng <- parseLng lngStr
  return $ LatLng lat lng

parseLat :: String -> Maybe Latitude
parseLat = Just . Latitude . read

parseLng :: String -> Maybe Longitude
parseLng = Just . Longitude . read

parseAltitude :: String -> Maybe Altitude
parseAltitude = Just . Altitude . read

parseKHz :: String -> Maybe NavFreq
parseKHz = Just . NavFreq . read

parseDistance :: String -> Maybe Distance
parseDistance = Just . Distance . read

parseBearing :: String -> Maybe Bearing
parseBearing = Just . Bearing . read


