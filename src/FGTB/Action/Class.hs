{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FunctionalDependencies #-}
module FGTB.Action.Class
where

import FGTB.FGData

class Action rq rp | rq -> rp where
  runAction :: IO FGData -> rq -> IO rp
