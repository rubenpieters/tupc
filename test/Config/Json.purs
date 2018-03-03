module Test.Config.Json where

import Types
import Config.Json

import Data.Map as Map
import Data.SubRecord as SubRecord

import Test.Assert

-- example 1 -- parseJsonConfigContent

testExample1Data :: SubJsonConfigContent
testExample1Data =
  { subJsonConfig: SubRecord.mkSubRecord
      { scale: 50
      , scaleX: Nothing
      , scaleY: Nothing
      , directionX: XRight
      , directionY: YDown
      , originX: XLeft
      , originY: YUp
      , ignore: []
      , ignoreExtra: []
      }
  , content: ["113", "113", "222"] }

testExample1_parseJsonConfigContent :: Either String (Map.Map Char Pos)
testExample1_parseJsonConfigContent =
  parseJsonConfigContent { throw: Left } testExample1Data

testExample1Output :: forall a. Either a (Map.Map Char Pos)
testExample1Output = Right $ Map.fromFoldable
  [ (Tuple '1' (Pos { xLeft: 0, xRight: 100, yTop: 0, yBot: 100 }))
  , (Tuple '2' (Pos { xLeft: 0, xRight: 150, yTop: 100, yBot: 150 }))
  , (Tuple '3' (Pos { xLeft: 100, xRight: 150, yTop: 0, yBot: 100 }))
  ]


-- main

main :: Eff _ Unit
main = do
  assert $ testExample1_parseJsonConfigContent == testExample1Output
