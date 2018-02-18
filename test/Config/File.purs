module Test.Config.File where

import Types
import Config.File

import Data.String (joinWith)

import Test.Assert

-- example 1 -- rawToJsonConfigContent

testExample1Data = ["# scaleX = 50", "# scaleY = 50", "113", "113", "222"] # joinWith "\n"

testExample1_rawToJsonConfigContent =
  rawToJsonConfigContent { throw: Left, rawContents: pure testExample1Data }

testExample1Output = Right { jsonConfig: { scaleX: 50, scaleY: 50 }, content: ["113", "113", "222"] }

-- main

main :: Eff _ Unit
main = do
  -- TODO: would be interesting to use an Eq for records, but this doesn't exist yet
  assert $ (testExample1_rawToJsonConfigContent <#> _.jsonConfig.scaleX)
             == (testExample1Output <#> _.jsonConfig.scaleX)
  assert $ (testExample1_rawToJsonConfigContent <#> _.jsonConfig.scaleY)
             == (testExample1Output <#> _.jsonConfig.scaleY)
  assert $ (testExample1_rawToJsonConfigContent <#> _.content)
             == (testExample1Output <#> _.content)
