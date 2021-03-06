{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE PartialTypeSignatures #-}
module FGTB.Actions
where

import FGTB.Types
import FGTB.Waypoint
import FGTB.Geo
import FGTB.Parse
import FGTB.Map
import FGTB.Route
import FGTB.FGData
import FGTB.Action.Class
import FGTB.CLI
import FGTB.API

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
import Data.Monoid
import Web.Scotty.Trans (param)
import qualified Web.Scotty.Trans as Scotty
import Data.Aeson (Value (Object), object, (.=), ToJSON (..))
import Control.Exception
import Data.Char (toLower)
import Text.Read (readMaybe)

-- * VOR-to-VOR Navigation

data VornavRequest
  = VornavRequest WPSpec WPSpec

instance FromArgs VornavRequest where
  fromArgs = \case
    [fromID, toID] -> return $ VornavRequest (parseWPSpec fromID) (parseWPSpec toID)
    _ -> Left "Expected exactly two waypoints"

instance FromHttpRequest VornavRequest where
  fromHttpRequest = do
    fromID <- parseWPSpec <$> Scotty.param "from"
    toID <- parseWPSpec <$> Scotty.param "to"
    return $ VornavRequest fromID toID

data VornavResponse
  = VornavResponse Waypoint Waypoint [Waypoint] Distance

instance ToJSON VornavResponse where
  toJSON (VornavResponse from to waypoints dist) =
    object
      [ "from" .= from
      , "to" .= to
      , "waypoints" .= waypoints
      , "dist" .= dist
      ]

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
  runAction loadFGD (VornavRequest fromID toID) = do
    fgd <- loadFGD
    let waypoints = fgdWaypoints fgd
        navs = fgdNavs fgd
        vors = filter isVor navs
        ndbs = filter isNdb navs

    wpFrom <-
      maybe (throw $ UserException "Waypoint not found") return $
      findWP fromID waypoints
    wpTo <-
      maybe (throw $ UserException "Waypoint not found") return $
      findWP toID waypoints
    let positionFrom = waypointLoc wpFrom
        positionTo = waypointLoc wpTo
        routeMay = vorToVor (ndbs ++ vors) wpFrom wpTo
    (route, _) <- maybe (throw $ UserException "No route found") return routeMay
    let dist = wpRouteLength positionFrom positionTo route

    return $
      VornavResponse
        wpFrom
        wpTo
        route
        dist

-- * Printing routes

data PrintRouteRequest
  = PrintRouteRequest [WPSpec]

instance FromArgs PrintRouteRequest where
  fromArgs = return . PrintRouteRequest . map parseWPSpec

instance FromHttpRequest PrintRouteRequest where
  fromHttpRequest = do
    PrintRouteRequest . map parseWPSpec . words <$> param "waypoints"

data Leg
  = Leg Distance Bearing Waypoint Waypoint

instance ToJSON Leg where
  toJSON (Leg dist bearing wpFrom wpTo) =
    object
      [ "dist" .= dist
      , "bearing" .= bearing
      , "from" .= wpFrom
      , "to" .= wpTo
      ]

data PrintRouteResponse
  = PrintRouteResponse
      Waypoint
      [Leg]

instance ToJSON PrintRouteResponse where
  toJSON (PrintRouteResponse startWP legs) =
    toJSON $ toJSON startWP : map toJSON legs

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
  runAction loadFGD (PrintRouteRequest ids) = do
    fgd <- loadFGD
    let waypoints = fgdWaypoints fgd
    case resolveRoute ids waypoints of
      Left wpID -> do
        throw $ UserException $ "Waypoint not found or ambiguous: " ++ show wpID
      Right (wp:wps) -> do
        let startWP = wp
            legs = map (uncurry makeLeg) (zip (wp:wps) wps)
            makeLeg a b =
              let (dist, bearing, _) = llDiff (waypointLoc a) (waypointLoc b)
              in Leg dist bearing a b
        return $ PrintRouteResponse startWP legs

-- * WP XML Procedures
--
data XmlProcType
  = XmlSID
  | XmlSTAR
  deriving (Show, Eq)

data WPXmlProcRequest
  = WPXmlProcRequest XmlProcType [WPSpec]

instance FromArgs WPXmlProcRequest where
  fromArgs = \case
    [] -> return $ WPXmlProcRequest XmlSID []
    "sid":xs -> return . WPXmlProcRequest XmlSID $ map parseWPSpec xs
    "star":xs -> return . WPXmlProcRequest XmlSTAR $ map parseWPSpec xs
    x:xs -> fail "Please specify 'sid' or 'star'"

instance FromHttpRequest WPXmlProcRequest where
  fromHttpRequest = do
    tyStr <- param "type"
    ty <- case tyStr of
      "sid" -> return XmlSID
      "star" -> return XmlSTAR
      x -> fail $ "invalid type: " ++ x
    wps <- map parseWPSpec . words <$> param "waypoints"
    return $ WPXmlProcRequest ty wps

data WPXmlProcResponse
  = WPXmlProcResponse
      XmlProcType
      [WPInfo]

instance ToJSON WPXmlProcResponse where
  toJSON (WPXmlProcResponse ty wps) =
    object
      [ "type" .= (map toLower . drop 3 . show $ ty)
      , "waypoints" .= wps
      ]

xmlProcTypeName :: XmlProcType -> String
xmlProcTypeName XmlSID = "Sid"
xmlProcTypeName XmlSTAR = "Star"

instance PrintableResult WPXmlProcResponse where
  printResult prn (WPXmlProcResponse ty items) = do
    let listElemName = xmlProcTypeName ty ++ "_Waypoint"
    let tag :: String -> String -> _ ()
        tag elemName content =
          prn $ printf "    <%s>%s</%s>\n" elemName content elemName
    forM_ (zip ([1..] :: [Int]) items) $ \(id, WPInfo wp details _) -> do
      prn $ printf "<%s ID=\"%i\">\n" listElemName id
      tag "Name" (Text.unpack . unNavID $ waypointID wp)
      tag "Type" "Normal"
      tag "Latitude" (show . unLatitude . lat $ waypointLoc wp)
      tag "Longitude" (show . unLongitude . lng $ waypointLoc wp)
      tag "Speed" "0"
      tag "Altitude" "0"
      tag "AltitudeCons" "0"
      tag "Flytype" "Fly-By"
      tag "Sp_Turn" "Auto"
      prn $ printf "</%s>\n" listElemName

-- * WP Info

data WPInfoRequest
  = WPInfoRequest [WPSpec]

instance FromArgs WPInfoRequest where
  fromArgs = return . WPInfoRequest . map parseWPSpec

instance FromHttpRequest WPInfoRequest where
  fromHttpRequest = do
    WPInfoRequest . map parseWPSpec . words <$> param "waypoints"

data WPInfo
  = WPInfo
      Waypoint
      WPDetails
      [(Distance, Bearing, Bearing, Nav)]

instance ToJSON WPInfo where
  toJSON (WPInfo waypoint details navs) =
    let Object base = toJSON details
        Object ext = object
          [ "id" .= waypointID waypoint
          , "navs" .=
              [ object
                  [ "dist" .= dist
                  , "bearing" .= bearing
                  , "radial" .= radial
                  , "nav" .= nav
                  ]
              | (dist, bearing, radial, nav)
              <- navs
              ]
          ]
    in Object (base <> ext)

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
      Text

instance ToJSON WPDetails where
  toJSON = \case
    AirportInfo name rwys ->
      object
        [ "name" .= name
        , "runways" .=
            [ object
                [ "name" .= name
                , "length" .= length
                ]
            | RunwayInfo name length <- rwys
            ]
        ]
    NavInfo name freq dist ->
      object
        [ "name" .= name
        , "freq" .= freq
        , "dist" .= dist
        ]
    OtherWPInfo name ->
      object
        [ "name" .= name
        ]

data WPInfoResponse
  = WPInfoResponse
      [WPInfo]

instance ToJSON WPInfoResponse where
  toJSON (WPInfoResponse wpInfos) =
    toJSON wpInfos

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

instance Action WPXmlProcRequest WPXmlProcResponse where
  runAction loadFGD (WPXmlProcRequest ty ids) = do
    WPInfoResponse rdata <- runAction loadFGD (WPInfoRequest ids)
    return $ WPXmlProcResponse ty rdata

instance Action WPInfoRequest WPInfoResponse where
  runAction = runWPInfoRequest

runWPInfoRequest loadFGD (WPInfoRequest ids) = do
    fgd <- loadFGD
    let waypoints = fgdWaypoints fgd
        navs = fgdNavs fgd
        vors = filter (\nav -> isVor nav || isNdb nav) navs

        runWP :: WPSpec -> Maybe Waypoint -> IO WPInfo
        runWP spec Nothing = do
          throw $ UserException $ "Waypoint not found: " <> show spec
        runWP _ (Just wp) = do
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
                    OtherWPInfo (waypointName wp)
              nearbyVorsInfo =
                map vorInfo nearbyVors
          return $ WPInfo wp details nearbyVorsInfo
    WPInfoResponse <$>
      mapM (\id -> runWP id $ findWP id (fgdWaypoints fgd)) ids

data WindCalcRequest =
  WindCalcRequest
    { wcrqCourse :: Bearing
    , wcrqWindDir :: Bearing
    , wcrqWindSpeed :: Speed
    , wcrqAirspeed :: Speed
    }

readEither :: Read a => String -> String -> Either String a
readEither msg src = case readMaybe src of
  Nothing -> Left $ "No parse: " ++ msg
  Just x -> Right x

instance FromArgs WindCalcRequest where
  fromArgs [crsStr, airspeedStr, dirStr, speedStr] =
    WindCalcRequest
      <$> (Bearing <$> readEither "course" crsStr)
      <*> (Bearing <$> readEither "wind direction" dirStr)
      <*> (Speed <$> readEither "wind speed" speedStr)
      <*> (Speed <$> readEither "airspeed" airspeedStr)
  fromArgs _ =
    Left "Invalid args, expected COURSE AIRSPEED WIND_DIR WIND_SPEED"

instance FromHttpRequest WindCalcRequest where
  fromHttpRequest =
    WindCalcRequest
      <$> (Bearing <$> Scotty.param "course")
      <*> (Bearing <$> Scotty.param "wind-dir")
      <*> (Speed <$> Scotty.param "wind-speed")
      <*> (Speed <$> Scotty.param "airspeed")

data WindCalcResponse =
  WindCalcResponse
    { wcrpHeading :: Bearing
    }

instance Action WindCalcRequest WindCalcResponse where
  runAction _ rq =
    return . WindCalcResponse $
      solveWindTriangle
        (wcrqCourse rq)
        (wcrqAirspeed rq)
        (wcrqWindDir rq, wcrqWindSpeed rq)

instance PrintableResult WindCalcResponse where
  printResult prn (WindCalcResponse heading) =
    prn $ show heading

instance ToJSON WindCalcResponse where
  toJSON (WindCalcResponse heading) =
    object
      [ "heading" .= heading ]
