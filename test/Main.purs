module Test.Main where

import Types
import Main
import ReadConfig

import Data.Map as Map

import Test.Assert

testData :: Array (Array String)
testData =
  [ ["1", "1", "3"]
  , ["1", "1", "3"]
  , ["2", "2", "2"]
  ]


testConfig = mkConfig { throw: Left } Map.empty ["scaleX = 50", "scaleY = 100"]

main :: Eff _ Unit
main = do
  let (calcMap :: Map.Map String Pos) = calcPos testData
  let (manualMap :: Map.Map String Pos) = Map.fromFoldable
       [ (Tuple "1" (Pos { xBot: 1, xTop: 0, yLeft: 0, yRight: 1 }))
       , (Tuple "2" (Pos { xBot: 2, xTop: 2, yLeft: 0, yRight: 2 }))
       , (Tuple "3" (Pos { xBot: 1, xTop: 0, yLeft: 2, yRight: 2 }))
       ]
  assert (calcMap == manualMap)
  assert ((testConfig <#> _.scaleX) == Right 50)
  assert ((testConfig <#> _.scaleY) == Right 100)
