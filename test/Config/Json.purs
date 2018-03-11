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
      , directionX: DirXRight
      , directionY: DirYDown
      , originX: OriXLeft
      , originY: OriYUp
      , ignore: []
      , ignoreExtra: []
      }
  , content: ["113", "113", "222"] }

testExample1_parseJsonConfigContent :: Either String (Map.Map Char EnrichedPos)
testExample1_parseJsonConfigContent =
  parseJsonConfigContent { throw: Left } testExample1Data

testExample1Output :: forall a. Either a (Map.Map Char EnrichedPos)
testExample1Output = Right $ Map.fromFoldable
  [ (Tuple '1' (EnrichedPos { xLeft: 0, xRight: 100, yTop: 0, yBot: 100, xWidth: 100, yHeight: 100, xCenter: 50, yCenter: 50 }))
  , (Tuple '2' (EnrichedPos { xLeft: 0, xRight: 150, yTop: 100, yBot: 150, xWidth: 150, yHeight: 50, xCenter: 75, yCenter: 125 }))
  , (Tuple '3' (EnrichedPos { xLeft: 100, xRight: 150, yTop: 0, yBot: 100, xWidth: 50, yHeight: 100, xCenter: 125, yCenter: 50 }))
  ]


-- main

main :: Eff _ Unit
main = do
  log (show testExample1_parseJsonConfigContent)
  assert $ testExample1_parseJsonConfigContent == testExample1Output
