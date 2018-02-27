module FGTB.Map
where

import FGTB.Types
import Graphics.Rendering.Cairo

renderFixes :: [Fix] -> Render ()
renderFixes = mapM_ renderFix

renderFix :: Fix -> Render ()
renderFix (Fix navID (LatLng (Latitude lat) (Longitude lng))) = do
  save

  translate (180 + lng) (90 - lat)
  scale 0.1 0.1

  let sin30 = 0.5
  let cos30 = cos (pi / 6)

  newPath
  moveTo 0 1
  lineTo sin30 (-cos30)
  lineTo (-sin30) (-cos30)
  lineTo 0 1
  stroke

  restore

renderNavs :: [Nav] -> Render ()
renderNavs = mapM_ renderNav

renderNav :: Nav -> Render ()
renderNav nav = do
  let LatLng (Latitude lat) (Longitude lng) = navLoc nav
  save

  translate (180 + lng) (90 - lat)
  scale 0.1 0.1

  newPath
  moveTo (-1) (-1)
  lineTo 1 (-1)
  lineTo 1 1
  lineTo (-1) 1
  lineTo (-1) (-1)
  stroke

  restore
