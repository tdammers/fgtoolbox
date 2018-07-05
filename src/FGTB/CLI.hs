{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE ScopedTypeVariables #-}
module FGTB.CLI
where

import FGTB.Action.Class
import FGTB.FGData
import Data.Proxy

class FromArgs a where
  fromArgs :: [String] -> Either String a

class PrintableResult a where
  printResult :: Monad m => (String -> m ()) -> a -> m ()

instance PrintableResult () where
  printResult _ () = pure ()

cliAction :: forall rq rp.
             (Action rq rp, FromArgs rq, PrintableResult rp)
          => Proxy rq
          -> IO FGData
          -> [String]
          -> IO ()
cliAction _ loadFGData args = do
  rq :: rq <- either error return $ fromArgs args
  rp :: rp <- runAction loadFGData rq
  printResult putStr rp
