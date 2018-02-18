module Config.File where

import Types
import Config.Parse

import Data.Array as Array
import Data.Map as Map
import Data.String (Pattern(..), split, charAt, stripPrefix)

import Data.Traversable

rawToConfigContent :: forall f r.
                      Monad f =>
                      { throw :: forall a. String -> f a
                      , rawContents :: f String
                      | r } ->
                      f ConfigContent
rawToConfigContent k = do
  rawContents <- k.rawContents <#> split (Pattern "\n")
  let { yes: config, no: content }
       = rawContents # Array.filter (\s -> s /= "")
                     # Array.partition isConfigLine
  let (configArray :: Array (Maybe String))
       = config <#> stripPrefix (Pattern "#")
  configArray' :: Array String <- for configArray (maybe (k.throw "No # found on detected config line. Proposed action: report as bug") pure)
  pure { config: configArray', content: content }
  where
    isConfigLine :: String -> Boolean
    isConfigLine s = (s # charAt 0) == Just '#'

rawToJsonConfigContent :: forall f r.
                          Monad f =>
                          { throw :: forall a. String -> f a
                          , rawContents :: f String
                          | r } ->
                          f JsonConfigContent
rawToJsonConfigContent k = do
  { config: config, content: content } <- rawToConfigContent k
  jsonConfig <- mkConfig k Map.empty config
  pure { jsonConfig: jsonConfig, content: content }
