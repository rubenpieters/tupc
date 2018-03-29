module Test.Config.Apply where

import Tupc.Internal
import Content.Parse
import Config.Apply

import Data.Map as Map

import Test.Assert

testData :: Map.Map Char Pos
testData = Map.fromFoldable
  [ (Tuple '1' (Pos { xLeft: 0, xRight: 2, yTop: 0, yBot: 2 }))
  , (Tuple '2' (Pos { xLeft: 0, xRight: 3, yTop: 2, yBot: 3 }))
  , (Tuple '3' (Pos { xLeft: 2, xRight: 3, yTop: 0, yBot: 2 }))
  ]

testBounds :: { maxX :: Int, maxY :: Int }
testBounds = { maxX: 3, maxY: 3 }

-- config test scale over scaleX/scaleY

test1Config :: JsonConfig
test1Config =
  { scale: 1
  , scaleX: Just 4
  , scaleY: Just 8
  , ignore: []
  , ignoreExtra: []
  , originX: OriXLeft
  , originY: OriYUp
  , directionX: DirXRight
  , directionY: DirYDown
  }

test1Result :: Map.Map Char EnrichedPos
test1Result = Map.fromFoldable
  [ (Tuple '1' (EnrichedPos { xLeft: 0, xRight: 8, yTop: 0, yBot: 16, xWidth: 8, yHeight: 16, xCenter: 4, yCenter: 8 }))
  , (Tuple '2' (EnrichedPos { xLeft: 0, xRight: 12, yTop: 16, yBot: 24, xWidth: 12, yHeight: 8, xCenter: 6, yCenter: 20 }))
  , (Tuple '3' (EnrichedPos { xLeft: 8, xRight: 12, yTop: 0, yBot: 16, xWidth: 4, yHeight: 16, xCenter: 10, yCenter: 8 }))
  ]

-- config test scale

test2Config :: JsonConfig
test2Config =
  { scale: 10
  , scaleX: Nothing
  , scaleY: Nothing
  , ignore: []
  , ignoreExtra: []
  , originX: OriXLeft
  , originY: OriYUp
  , directionX: DirXRight
  , directionY: DirYDown
  }

test2Result :: Map.Map Char EnrichedPos
test2Result = Map.fromFoldable
  [ (Tuple '1' (EnrichedPos { xLeft: 0, xRight: 20, yTop: 0, yBot: 20, xWidth: 20, yHeight: 20, xCenter: 10, yCenter: 10 }))
  , (Tuple '2' (EnrichedPos { xLeft: 0, xRight: 30, yTop: 20, yBot: 30, xWidth: 30, yHeight: 10, xCenter: 15, yCenter: 25 }))
  , (Tuple '3' (EnrichedPos { xLeft: 20, xRight: 30, yTop: 0, yBot: 20, xWidth: 10, yHeight: 20, xCenter: 25, yCenter: 10 }))
  ]

-- config test origin Right/Down

test3Config :: JsonConfig
test3Config =
  { scale: 2
  , scaleX: Nothing
  , scaleY: Nothing
  , ignore: []
  , ignoreExtra: []
  , originX: OriXRight
  , originY: OriYDown
  , directionX: DirXRight
  , directionY: DirYDown
  }

test3Result :: Map.Map Char EnrichedPos
test3Result = Map.fromFoldable
  [ (Tuple '1' (EnrichedPos { xLeft: (-6), xRight: (-2), yTop: (-6), yBot: (-2), xWidth: 4, yHeight: 4, xCenter: (-4), yCenter: (-4) }))
  , (Tuple '2' (EnrichedPos { xLeft: (-6), xRight: 0, yTop: (-2), yBot: 0, xWidth: 6, yHeight: 2, xCenter: (-3), yCenter: (-1) }))
  , (Tuple '3' (EnrichedPos { xLeft: (-2), xRight: 0, yTop: (-6), yBot: (-2), xWidth: 2, yHeight: 4, xCenter: (-1), yCenter: (-4) }))
  ]

-- config test origin Right/Down direction Left/Up

test4Config :: JsonConfig
test4Config =
  { scale: 2
  , scaleX: Nothing
  , scaleY: Nothing
  , ignore: []
  , ignoreExtra: []
  , originX: OriXRight
  , originY: OriYDown
  , directionX: DirXLeft
  , directionY: DirYUp
  }

test4Result :: Map.Map Char EnrichedPos
test4Result = Map.fromFoldable
  [ (Tuple '1' (EnrichedPos { xLeft: 6, xRight: 2, yTop: 6, yBot: 2, xWidth: 4, yHeight: 4, xCenter: 4, yCenter: 4 }))
  , (Tuple '2' (EnrichedPos { xLeft: 6, xRight: 0, yTop: 2, yBot: 0, xWidth: 6, yHeight: 2, xCenter: 3, yCenter: 1 }))
  , (Tuple '3' (EnrichedPos { xLeft: 2, xRight: 0, yTop: 6, yBot: 2, xWidth: 2, yHeight: 4, xCenter: 1, yCenter: 4 }))
  ]

main :: Eff _ Unit
main = do
  log "test1"
  let test1 = testData # applyConfig test1Config testBounds
  log (show test1)
  assert $ test1 == test1Result
  log "test2"
  let test2 = testData # applyConfig test2Config testBounds
  log (show test2)
  assert $ test2 == test2Result
  log "test3"
  let test3 = testData # applyConfig test3Config testBounds
  log (show test3)
  assert $ test3 == test3Result
  log "test4"
  let test4 = testData # applyConfig test4Config testBounds
  log (show test4)
  assert $ test4 == test4Result


