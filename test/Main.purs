module Test.Main where

import Types
import Content.Parse
import Config.Parse
import Tupc

import Test.Config.File as Test.Config.File
import Test.Config.Json as Test.Config.Json

import Data.Map as Map

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Test.Assert

testData :: Array (Array Char)
testData =
  [ ['1', '1', '3']
  , ['1', '1', '3']
  , ['2', '2', '2']
  ]

testConfig :: Eff _ JsonConfig
testConfig = mkConfig { throw: throw } tupcDefaults ["scaleX = 50", "scaleY = 100"]

testReadFile :: Eff _ (Map.Map Char Pos)
testReadFile = parseRaw { throw: throw, rawContents: readTextFile UTF8 "examples/test1.txt"}

main :: Eff _ Unit
main = do
  let (calcMap :: Map.Map Char Pos) = toMapPos [] testData
  let (manualMap :: Map.Map Char Pos) = Map.fromFoldable
       [ (Tuple '1' (Pos { xLeft: 0, xRight: 2, yTop: 0, yBot: 2 }))
       , (Tuple '2' (Pos { xLeft: 0, xRight: 3, yTop: 2, yBot: 3 }))
       , (Tuple '3' (Pos { xLeft: 2, xRight: 3, yTop: 0, yBot: 2 }))
       ]
  let (manualMapScaled :: Map.Map Char Pos) = Map.fromFoldable
       [ (Tuple '1' (Pos { xLeft: 0, xRight: 100, yTop: 0, yBot: 100 }))
       , (Tuple '2' (Pos { xLeft: 0, xRight: 150, yTop: 100, yBot: 150 }))
       , (Tuple '3' (Pos { xLeft: 100, xRight: 150, yTop: 0, yBot: 100 }))
       ]
  assert $ calcMap == manualMap
  testConfig' <- testConfig
  assert $ testConfig'.scaleX == Just 50
  assert $ testConfig'.scaleY == Just 100
  fileMap <- testReadFile
  assert $ fileMap == manualMapScaled
  Test.Config.File.main
  Test.Config.Json.main
