{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FunctionalDependencies #-}
module FGTB.Action.Class
where

import FGTB.FGData

class Action rq rp | rq -> rp where
  runAction :: FGData -> rq -> IO rp
