module Test.Config.File where

import Tupc.Internal
import Config.File

import Data.SubRecord as SubRecord
import Data.String (joinWith)

import Test.Assert

-- example 1 -- rawToJsonConfigContent

testExample1Data :: String
testExample1Data = ["# scaleX = 50", "# scaleY = 50", "113", "113", "222"] # joinWith "\n"

testExample1_rawToJsonConfigContent :: Either String SubJsonConfigContent
testExample1_rawToJsonConfigContent =
  rawToJsonConfigContent { throw: Left, fileContent: pure testExample1Data }

testExample1Output :: forall a. Either a SubJsonConfigContent
testExample1Output = Right
  { subJsonConfig: SubRecord.mkSubRecord { scaleX: Just 50, scaleY: Just 50 }
  , content: ["113", "113", "222"]
  }

-- main

main :: Eff _ Unit
main = do
  assert $ (testExample1_rawToJsonConfigContent <#> _.subJsonConfig <#> SubRecord.get (SProxy :: SProxy "scaleX"))
             == (testExample1Output <#> _.subJsonConfig <#> SubRecord.get (SProxy :: SProxy "scaleX"))
  assert $ (testExample1_rawToJsonConfigContent <#> _.subJsonConfig <#> SubRecord.get (SProxy :: SProxy "scaleY"))
             == (testExample1Output <#> _.subJsonConfig <#> SubRecord.get (SProxy :: SProxy "scaleY"))
  assert $ (testExample1_rawToJsonConfigContent <#> _.content)
             == (testExample1Output <#> _.content)
