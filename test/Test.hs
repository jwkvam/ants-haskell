import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Test.QuickCheck
import Test.HUnit

import Data.Time.Clock
import Data.Time.Calendar

import Debug.Trace

import Ants
import Bot

tests = [
        testGroup "cases" $ zipWith (testCase . show) [1::Int ..] $
            [ 
            case_NotRepeatedPositions
            , case_AlwaysRazeEnemyHill
            , case_testUniccupied
            ]
        , testGroup "properties" $ zipWith (testProperty . show) [1::Int ..] $
            [ property True]
        ]
        
--------------------------------------------------
-- | Simple game params used in conjunction with the example map
gameParams = createParams [ 
        ("loadtime","3000"),
        ("turntime","1000"),
        ("rows","43"),
        ("cols","39"),
        ("turns","60"),
        ("player_seed","7"),
        ("viewradius2","77"),
        ("attackradius2","5"),
        ("spawnradius2","1")
    ]
createGameState = foldl updateGame initialState  
    where
        updateGame = (updateGameState $ viewCircle gameParams) 
        fakeDate = UTCTime (ModifiedJulianDay 0) 0
        initialState = (GameState (initialWorld gameParams) [] [] [] fakeDate) 

--------------------------------------------------
case_testUniccupied = assertBool "Not detecting an unoccupied tile" expected
    where
        setup = ["a 15 15 0","h 15 14 1","w 16 15","w 14 15","w 15 16"]
        gs = createGameState setup
        theAnt = (head . ants) gs
        order = Order theAnt West
        expected = isDestinationPassable (world gs) order

--------------------------------------------------
case_NotRepeatedPositions = assertEqual "Only one order should be generated" orders [orderA]
    where
        orders = filterCollidingOrders (initialWorld gameParams) [[orderA], [orderB]]    
        antA = Ant (0,0) Me 
        antB = Ant (2,0) Me
        orderA = Order antA South
        orderB = Order antB North

--------------------------------------------------
-- The only move allowed is to raze the contiguous enemy ant hill
case_AlwaysRazeEnemyHill = 
    assertEqual "Enemy Hill was not razed" expected obtained
    where 
        setup = ["a 15 15 0","h 15 14 1"]
        gs = createGameState setup
        expected = True
        obtained = True
       
-- Main program
main = defaultMain tests 

