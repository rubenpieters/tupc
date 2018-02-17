module Config.File where

import Types

import Data.Array as Array
import Data.String (Pattern(..), split, charAt, stripPrefix)

import Data.Traversable

readRawFile :: forall f r.
               (Monad f) =>
               { throw :: forall a. String -> f a
               , rawContents :: f String
               | r } ->
               f ConfigLines
readRawFile k = do
  rawContents <- k.rawContents <#> split (Pattern "\n")
  let { yes: config, no: content }
       = rawContents # Array.filter (\s -> s /= "")
                     # Array.partition isConfigLine
  let (configArray :: Array (Maybe String))
       = config <#> stripPrefix (Pattern "#")
  configArray' :: Array String <- for configArray (maybe (k.throw "No # found on detected config line. Proposed action: report as bug") pure)
  let contentArray = content <#> split (Pattern "")
  pure { config: configArray', content: contentArray }
  where
    isConfigLine :: String -> Boolean
    isConfigLine s = (s # charAt 0) == Just '#'
