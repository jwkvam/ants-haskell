module Bot where

import Data.List
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import System.IO
import Data.Function

import Debug.Trace

import Ants

-- | Given a origin and destination get the initial movement
getDirections :: GameParams -> Point -> Point -> [Direction]
getDirections gp (x1,y1) (x2,y2) =
    selectConds [
                    (c1&&c11,North), 
                    (c1&&c12,South), 
                    (c2&&c21,South), 
                    (c2&&c22,North), 
                    (c3&&c31,West), 
                    (c3&&c32,East), 
                    (c4&&c41,East), 
                    (c4&&c42,West)] 
    where
        selectConds = map snd .  filter (\t@(a,b) -> a)
        c1 = x1 < x2
        c11 = x2-x1>=halfRows
        c12 = x2-x1<=halfRows
        c2 = x2 < x1
        c21 =x1-x2>=halfRows
        c22 =x2-x2<=halfRows
        c3 = y1<y2
        c31 = y2-y1>=halfCols
        c32 = y2-y1<=halfCols
        c4 = y2<y1
        c41 = y1-y2>=halfCols
        c42 = y1-y2<=halfCols
        halfRows = rows gp `div` 2
        halfCols = cols gp `div` 2


-- | Generates orders for an Ants looking for food
generateOrders :: GameParams -> GameState -> [[Order]]
generateOrders gp gs = map allDirections $ myAnts $ ants gs
    where
        foodTargets = foldl' accMap M.empty distsToFood
            where
                accMap = \m (a,f) -> M.insertWith (flip const) a (getDirections gp (pointAnt a) f) m 

        hillsTargets = foldl' accMap M.empty distsEnemyHills
            where
                accMap = \m (a,h) -> M.insertWith (flip const) a (getDirections gp (pointAnt a) h) m 


        distsEnemyHills = map snd $ sortBy (compare `on`fst) [(distance gp (pointAnt a) f,(a,f)) | a <- myAnts $ ants gs, f <- map pointHill $ filter isEnemy's $ hills gs]
        distsToFood = map snd $ sortBy (compare `on`fst) [(distance gp (pointAnt a) f,(a,f)) | a <- myAnts $ ants gs, f <- food gs]
        allDirections a = map (Order a) $ trace (show antDirections) (antDirections ++ [x | x <- [North .. West], notElem x antDirections])
             where
                dirsEnemyHills = M.findWithDefault [] a hillsTargets
                dirsFood = M.findWithDefault [] a foodTargets
                antDirections = trace (show dirsEnemyHills) (dirsEnemyHills++dirsFood)

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
