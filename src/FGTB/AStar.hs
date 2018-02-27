{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE LambdaCase #-}
module FGTB.AStar
where

import Control.Monad.State
import Data.Set (Set)
import Control.Lens
import Control.Lens.TH
import Data.List
import Debug.Trace

data AStarState node cost
 = AStarState
    { _openNodes :: [(node, [node], cost)]
    , _closedNodes :: [node]
    }
makeLenses ''AStarState

aStar :: (Show node, Eq node, Ord cost, Num cost)
      => (node -> node -> cost) -- heuristic
      -> (node -> [(node, cost)]) -- graph edge function
      -> (node -> Bool) -- goal condition
      -> node
      -> node
      -> Maybe ([node], cost)
aStar h edges goalCond from to =
  evalState go (AStarState [(from, [], 0)] [])
  where
    go = do
      use openNodes >>= \case
        ((current, history, cost):others) -> do
          open <- use openNodes
          closed <- use closedNodes
          if goalCond current then
            return $ Just (reverse (to:current:history), cost)
          else if current `elem` closed then do
            openNodes .= others
            go
          else do
            openNodes .= others
            closedNodes <>= [current]
            let candidates = edges current
            forM_ candidates $ \(candidate, candCost) -> do
              when (candidate `notElem` closed) $
                openNodes %= ((candidate, current:history, cost + candCost):)
            openNodes %= sortOn (\(candidate, _, cost) -> cost + h candidate to)
            go
        _ ->
          return Nothing
