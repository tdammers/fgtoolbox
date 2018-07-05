{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE TemplateHaskell #-}
module FGTB.API
where

import FGTB.Action.Class
import FGTB.FGData
import Data.Proxy
import Web.Scotty.Trans as Scotty
import Data.Aeson (ToJSON (toJSON), object, (.=))
import Data.Aeson.TH
import Control.Monad.IO.Class
import Control.Exception
import Data.String

newtype UserException = UserException String
  deriving (Show)

instance Exception UserException where
  displayException (UserException msg) = msg

instance ToJSON UserException where
  toJSON (UserException msg) =
    object [ "error" .= msg ]

class FromHttpRequest a where
  fromHttpRequest :: forall e m.
                     ( Scotty.ScottyError e
                     , Monad m
                     )
                  => Scotty.ActionT e m a

data ErrorResponse e =
  ErrorResponse
    { message :: e
    }

deriveJSON defaultOptions ''ErrorResponse

io :: forall e m a.
      ( Scotty.ScottyError e
      , IsString e
      , MonadIO m
      )
      => IO a
      -> Scotty.ActionT e m a
io action = do
  liftIO (go `catch` handle) >>= either raise pure
  where
    go = Right <$> action
    handle :: UserException -> IO (Either e a)
    handle err = pure . Left . fromString . displayException $ err

apiAction :: forall rq rp e m.
             ( Action rq rp
             , FromHttpRequest rq
             , ToJSON rp
             , MonadIO m
             , Scotty.ScottyError e
             , ToJSON e
             , IsString e
             )
          => Proxy rq
          -> IO FGData
          -> Scotty.ActionT e m ()
apiAction _ loadFGD =
  go `Scotty.rescue` handle
  where
    go = do
      rq :: rq <- fromHttpRequest
      rp :: rp <- io $ runAction loadFGD rq
      Scotty.json rp
    handle e =
      Scotty.json $ ErrorResponse e
