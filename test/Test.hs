import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Test.QuickCheck
import Test.HUnit

import MyBot

tests = [
        testGroup "cases" $ zipWith (testCase . show) [1::Int ..] $
            [ 
            case_AlwaysTrue
            ]
        , testGroup "properties" $ zipWith (testProperty . show) [1::Int ..] $
            [ property True]
        ]
        
--------------------------------------------------
case_AlwaysTrue = assertEqual "Create expected functionality" expected obtained
    where 
        expected = True
        obtained = True
       
-- Main program
main = defaultMain tests 

