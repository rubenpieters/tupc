module Test.Config.Json where

import Types
import Tupc
import Config.Json

import Data.Map as Map

import Test.Assert

-- example 1 -- parseJsonConfigContent

testExample1Data = { jsonConfig: { scaleX: 50, scaleY: 50 }, content: ["113", "113", "222"] }

testExample1_rawToJsonConfigContent =
  parseJsonConfigContent { throw: Left } testExample1Data

testExample1Output = Right $ Map.fromFoldable
  [ (Tuple "1" (Pos { xBot: 1, xTop: 0, yLeft: 0, yRight: 1 }))
  , (Tuple "2" (Pos { xBot: 2, xTop: 2, yLeft: 0, yRight: 2 }))
  , (Tuple "3" (Pos { xBot: 1, xTop: 0, yLeft: 2, yRight: 2 }))
  ]


-- main

main :: Eff _ Unit
main = do
  assert $ testExample1_rawToJsonConfigContent == testExample1Output
