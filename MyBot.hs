module Main where

import Data.List
import Data.Maybe (mapMaybe)

import Ants

tryOrder :: World -> [Order] -> Maybe Order
tryOrder w = find (passable w)

enumOrders :: Ant -> [Order]
enumOrders a = map (Order a) [North .. West]

doTurn :: GameParams -> GameState -> [Order]
doTurn gp gs = 
  let generatedOrders = map enumOrders $ myAnts $ ants gs
  in mapMaybe (tryOrder (world gs)) generatedOrders

main :: IO ()
main = game doTurn
