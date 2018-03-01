module Test.SubRecord where

import Types

import Data.Symbol

import Test.Assert

testUn :: { x :: Int }
testUn = unSubRecord (\_ -> { x: 99 }) (testMk false)

testMk :: Boolean -> SubRecord ( x :: Int )
testMk b = if b
              then mkSubRecord { x: 1 }
              else mkSubRecord {}

testWithDef :: { x :: Int }
testWithDef = withDefaults { x: 99 } (testMk false)

testInsertSR :: SubRecord ( x :: Int, y :: String)
testInsertSR =
  buildSR (insertSR (SProxy :: SProxy "x") (Just 42) >>>
           insertSR (SProxy :: SProxy "y") Nothing
          ) (mkSubRecord {})

testInsertSRWithDef :: { x :: Int, y :: String}
testInsertSRWithDef = withDefaults { x: 1, y: "default"} testInsertSR

main :: Eff _ Unit
main = do
  assert $ testWithDef.x == 99
  assert $ testInsertSRWithDef.x == 42
  assert $ testInsertSRWithDef.y == "default"
