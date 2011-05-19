module Main where

import Data.List
import Data.Maybe (catMaybes)

import Ants

tryOrder :: GameState -> [Order] -> Maybe Order
tryOrder gs = find (passable $ world gs)

enumOrders :: Ant -> [Order]
enumOrders a = map (Order a) [North .. West]

doTurn :: GameParams -> GameState -> [Order]
doTurn gp gs = 
  let generatedOrders = map enumOrders $ myAnts $ ants gs
  in catMaybes $ map (tryOrder gs) generatedOrders

main = game doTurn
