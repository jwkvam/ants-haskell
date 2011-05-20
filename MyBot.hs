module Main where

import Data.List
import Data.Maybe (mapMaybe)
import System.IO

import Ants

-- | Picks the first "passable" order in a list
-- returns Nothing if no such order exists
tryOrder :: World -> [Order] -> Maybe Order
tryOrder w = find (passable w)

-- | Generates orders for an Ant in all directions
generateOrders :: Ant -> [Order]
generateOrders a = map (Order a) [North .. West]

{- | 
 - Implement this function to create orders.
 - It uses the IO Monad so algorithms can call timeRemaining.
 -}
doTurn :: GameParams -> GameState -> IO [Order]
doTurn gp gs = do
  let generatedOrders = map generateOrders $ myAnts $ ants gs
      orders = mapMaybe (tryOrder (world gs)) generatedOrders
  elapsedTime <- timeRemaining gs
  hPutStrLn stderr $ show elapsedTime
  return orders

main :: IO ()
main = game doTurn

-- vim: set expandtab:
