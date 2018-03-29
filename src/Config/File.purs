module Config.File where

import Tupc.Internal
import Config.Parse as Parse

import Data.Array as Array
import Data.String (Pattern(..))
import Data.String as String

rawToConfigContent ::
  forall f r.
  Monad f =>
  { throw :: forall a. String -> f a
  , fileContent :: f String
  | r } ->
  f ConfigContent
rawToConfigContent k = do
  fileContent <- k.fileContent <#> String.split (Pattern "\n")
  let { yes: config, no: content }
       = fileContent # Array.filter (\s -> s /= "")
                     # Array.partition isConfigLine
  let (configArray :: Array (Maybe String))
       = config <#> String.stripPrefix (Pattern "#")
  configArray' :: Array String <- for configArray (maybe (k.throw "No # found on detected config line. Proposed action: report as bug") pure)
  pure { config: configArray', content: content }
  where
    isConfigLine :: String -> Boolean
    isConfigLine s = (s # String.charAt 0) == Just '#'

rawToJsonConfigContent ::
  forall f r.
  Monad f =>
  { throw :: forall a. String -> f a
  , fileContent :: f String
  | r } ->
  f SubJsonConfigContent
rawToJsonConfigContent k = do
  { config: config, content: content } <- rawToConfigContent k
  subJsonConfig <- Parse.mkConfig k config
  pure { subJsonConfig: subJsonConfig, content: content }
