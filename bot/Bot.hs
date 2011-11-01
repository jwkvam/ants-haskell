module Bot where

import Data.List
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import System.IO

import Ants

-- | Generates orders for an Ant in all directions
generateOrders :: GameParams -> GameState -> [[Order]]
generateOrders gp gs = map directions $ myAnts $ ants gs 
    where
        directions a = map (Order a) [North .. West]
 
-- | Avoid Collitions
-- Whe get the set of destinations and generating Order
filterCollidingOrders :: World -> [[Order]] -> [Order]
filterCollidingOrders w orders = M.elems theMap
    where
        theMap = foldl' insertIfNotInMap M.empty orders
        insertIfNotInMap :: M.Map Point Order -> [Order] -> M.Map Point Order
        insertIfNotInMap m [] = m
        insertIfNotInMap m (o:os) = case M.lookup (destination w o) m of
            Just _ -> insertIfNotInMap m os
            Nothing -> M.insert (destination w o) o m


isDestinationPassable :: World -> Order -> Bool
isDestinationPassable w o = pos `elem` [Land, Unknown] || isDead pos || isEnemyHill pos
    where
        newPoint = destination w o
        pos = (tile (w %! newPoint))

        isEnemyHill (HillTile owner) = owner /= Me
        isEnemyHill _ = False

{- |
 - Implement this function to create orders.
 - It uses the IO Monad so algorithms can call timeRemaining.
 -
 - GameParams data holds values that are constant throughout the game
 - GameState holds data that changes between each turn
 - for each see Ants module for more information
 -}
doTurn :: GameParams -> GameState -> IO [Order]
doTurn gp gs = do
  -- generate orders for all ants belonging to me
  let orders = map (filter (isDestinationPassable (world gs))) (generateOrders gp gs)
  -- this shows how to check the remaining time
  elapsedTime <- timeRemaining gs
  hPutStrLn stderr $ show elapsedTime
  -- wrap list of orders back into a monad
  return (filterCollidingOrders (world gs) orders)
