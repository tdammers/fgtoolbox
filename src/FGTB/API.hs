{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE ScopedTypeVariables #-}
module FGTB.API
where

import FGTB.Action.Class
import FGTB.FGData
import Data.Proxy
import Web.Scotty.Trans as Scotty
import Data.Aeson (ToJSON)
import Control.Monad.IO.Class

class FromHttpRequest a where
  fromHttpRequest :: forall e m.
                     ( Scotty.ScottyError e
                     , Monad m
                     )
                  => Scotty.ActionT e m a

apiAction :: forall rq rp e m.
             ( Action rq rp
             , FromHttpRequest rq
             , ToJSON rp
             , MonadIO m
             , Scotty.ScottyError e
             )
          => Proxy rq
          -> FGData
          -> Scotty.ActionT e m ()
apiAction _ fgdata = do
  rq :: rq <- fromHttpRequest
  rp :: rp <- liftIO $ runAction fgdata rq
  Scotty.json rp
