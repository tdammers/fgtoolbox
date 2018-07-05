{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE TemplateHaskell #-}
module FGTB.FGData
where

import FGTB.Types
import FGTB.Waypoint
import FGTB.Parse
import qualified Data.ByteString.Lazy.Char8 as LBS8
import System.FilePath
import qualified Codec.Compression.GZip as GZip
import Text.Printf
import Data.Aeson (FromJSON (..), ToJSON (..), (.:))
import qualified Data.Aeson as JSON
import Data.Aeson.TH (deriveJSON, defaultOptions, Options (..))
import Data.Char (toLower, toUpper)
import System.IO
import Control.Exception

data FGData =
  FGData
    { fgdAirports :: [Airport]
    , fgdNavs :: [Nav]
    , fgdFixes :: [Fix]
    }

deriveJSON
  defaultOptions { fieldLabelModifier = map toLower . drop 3 }
  ''FGData

fgdWaypoints :: FGData -> [Waypoint]
fgdWaypoints (FGData airports navs fixes) =
  concat
    [ map NavWP navs
    , map FixWP fixes
    , map AirportWP airports
    ]

loadFGData :: FilePath -> IO FGData
loadFGData fgroot = do
  airports <- loadAirports fgroot
  navs <- loadNavs fgroot
  fixes <- loadFixes fgroot
  return $ FGData airports navs fixes

loadFGDataCache :: FilePath -> FilePath -> IO (Maybe FGData)
loadFGDataCache cacheDir fgroot =
  go `catch` handle
  where
    go = do
      let filename = cacheDir </> "cache.json"
      JSON.decodeFileStrict filename >>= \case
        Just (fgroot', fgdata) ->
          if fgroot' == fgroot then
            return fgdata
          else
            return Nothing
        Nothing ->
          return Nothing
    handle :: SomeException -> IO (Maybe FGData)
    handle err = do
      hPutStrLn stderr $ show err
      return Nothing

saveFGDataCache :: FilePath -> FilePath -> FGData -> IO ()
saveFGDataCache cacheDir fgroot fgdata = do
  let filename = cacheDir </> "cache.json"
  JSON.encodeFile filename (fgroot, fgdata)

loadFGDataCached :: FilePath -> FilePath -> IO FGData
loadFGDataCached cacheDir fgroot = do
  loadFGDataCache cacheDir fgroot >>= \case
    Just fgdata ->
      return fgdata
    Nothing -> do
      fgdata <- loadFGData fgroot
      saveFGDataCache cacheDir fgroot fgdata
      return fgdata

tee :: (a -> IO b) -> IO a -> IO a
tee sidechannel action = do
  val <- action
  sidechannel val
  return val

printCount :: [a] -> IO ()
printCount [x] =
  printf "Found 1 item\n"
printCount xs =
  printf "Found %d items\n" (length xs)

loadGZip8 :: FilePath -> IO String
loadGZip8 fp =
  LBS8.unpack . GZip.decompress <$> LBS8.readFile fp

loadFixes :: FilePath -> IO [Fix]
loadFixes fgroot = do
  putStrLn "Loading fixes..."
  tee printCount $ parseFixes <$> loadGZip8 (fgroot </> "Navaids" </> "fix.dat.gz")

loadNavs :: FilePath -> IO [Nav]
loadNavs fgroot = do
  putStrLn "Loading navs..."
  tee printCount $ parseNavs <$> loadGZip8 (fgroot </> "Navaids" </> "nav.dat.gz")

loadAirports :: FilePath -> IO [Airport]
loadAirports fgroot = do
  putStrLn "Loading airports..."
  tee printCount $ parseAirports <$> loadGZip8 (fgroot </> "Airports" </> "apt.dat.gz")

