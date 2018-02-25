module Test.Config.Apply where

import Types
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
  , scaleX: Just 3
  , scaleY: Just 7
  , ignore: []
  , ignoreExtra: []
  , originX: XLeft
  , originY: YUp
  , directionX: XRight
  , directionY: YDown
  }

test1Result :: Map.Map Char Pos
test1Result = Map.fromFoldable
  [ (Tuple '1' (Pos { xLeft: 0, xRight: 6, yTop: 0, yBot: 14 }))
  , (Tuple '2' (Pos { xLeft: 0, xRight: 9, yTop: 14, yBot: 21 }))
  , (Tuple '3' (Pos { xLeft: 6, xRight: 9, yTop: 0, yBot: 14 }))
  ]

-- config test scale

test2Config :: JsonConfig
test2Config =
  { scale: 10
  , scaleX: Nothing
  , scaleY: Nothing
  , ignore: []
  , ignoreExtra: []
  , originX: XLeft
  , originY: YUp
  , directionX: XRight
  , directionY: YDown
  }

test2Result :: Map.Map Char Pos
test2Result = Map.fromFoldable
  [ (Tuple '1' (Pos { xLeft: 0, xRight: 20, yTop: 0, yBot: 20 }))
  , (Tuple '2' (Pos { xLeft: 0, xRight: 30, yTop: 20, yBot: 30 }))
  , (Tuple '3' (Pos { xLeft: 20, xRight: 30, yTop: 0, yBot: 20 }))
  ]

-- config test origin Right/Down

test3Config :: JsonConfig
test3Config =
  { scale: 1
  , scaleX: Nothing
  , scaleY: Nothing
  , ignore: []
  , ignoreExtra: []
  , originX: XRight
  , originY: YDown
  , directionX: XRight
  , directionY: YDown
  }

test3Result :: Map.Map Char Pos
test3Result = Map.fromFoldable
  [ (Tuple '1' (Pos { xLeft: (-3), xRight: (-1), yTop: (-3), yBot: (-1) }))
  , (Tuple '2' (Pos { xLeft: (-3), xRight: 0, yTop: (-1), yBot: 0 }))
  , (Tuple '3' (Pos { xLeft: (-1), xRight: 0, yTop: (-3), yBot: (-1) }))
  ]

-- config test origin Right/Down direction Left/Up

test4Config :: JsonConfig
test4Config =
  { scale: 1
  , scaleX: Nothing
  , scaleY: Nothing
  , ignore: []
  , ignoreExtra: []
  , originX: XRight
  , originY: YDown
  , directionX: XLeft
  , directionY: YUp
  }

test4Result :: Map.Map Char Pos
test4Result = Map.fromFoldable
  [ (Tuple '1' (Pos { xLeft: 3, xRight: 1, yTop: 3, yBot: 1 }))
  , (Tuple '2' (Pos { xLeft: 3, xRight: 0, yTop: 1, yBot: 0 }))
  , (Tuple '3' (Pos { xLeft: 1, xRight: 0, yTop: 3, yBot: 1 }))
  ]

main :: Eff _ Unit
main = do
  log "test1"
  let test1 = testData # applyConfig test1Config testBounds
  assert $ test1 == test1Result
  log "test2"
  let test2 = testData # applyConfig test2Config testBounds
  assert $ test2 == test2Result
  log "test3"
  let test3 = testData # applyConfig test3Config testBounds
  assert $ test3 == test3Result
  log "test4"
  let test4 = testData # applyConfig test4Config testBounds
  assert $ test4 == test4Result



