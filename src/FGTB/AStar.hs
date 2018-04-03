{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE LambdaCase #-}
module FGTB.AStar
where

import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Lens
import Control.Lens.TH
import Data.List
import Debug.Trace

data OpenNode node cost =
  OpenNode
    { _openNodeCurrent :: node
    , _openNodeHistory :: [node]
    , _openNodeBaseCost :: cost
    , _openNodeTotalCost :: cost
    }
    deriving (Eq)
makeLenses ''OpenNode

instance (Eq cost, Eq node, Ord node, Ord cost) => Ord (OpenNode node cost) where
  compare a b =
    case compare (a ^. openNodeTotalCost) (b ^. openNodeTotalCost) of
      EQ -> compare (a ^. openNodeCurrent) (b ^. openNodeCurrent)
      x -> x

data AStarState node cost
 = AStarState
    { _openNodes :: Set (OpenNode node cost)
    , _closedNodes :: Set node
    }
makeLenses ''AStarState

aStar :: (Show node, Show cost, Ord node, Eq node, Ord cost, Num cost)
      => (node -> String) -- pretty formatter
      -> (node -> node -> cost) -- heuristic
      -> (node -> [(node, cost)]) -- graph edge function
      -> (node -> Bool) -- goal condition
      -> node
      -> node
      -> Maybe ([node], cost)
aStar fmtNode h edges goalCond from to =
  evalState go (AStarState [OpenNode from [] 0 (h from to)] [])
  where
    go = do
      open <- use openNodes
      if Set.null open then
        return Nothing
      else do
        let (OpenNode current history baseCost totalCost, others) = Set.deleteFindMin open
        -- traceM $ fmtNode current
        -- traceM $ show totalCost
        -- traceM $ show (totalCost - baseCost)
        closed <- use closedNodes
        if goalCond current then
          return $ Just (reverse (to:current:history), baseCost)
        else if current `Set.member` closed then do
          openNodes .= others
          go
        else do
          openNodes .= others
          closedNodes <>= [current]
          let candidates = edges current
          forM_ candidates $ \(candidate, candCost) -> do
            when (candidate `notElem` closed) $
              let baseCost' = baseCost + candCost
                  totalCost' = baseCost' + h candidate to
                  node = OpenNode
                            candidate
                            (current:history)
                            baseCost'
                            totalCost'
              in openNodes %= (Set.insert node)
          go
