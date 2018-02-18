module Config.Json where

import Types
import Content.Parse
import Config.Parse

import Data.Map as Map
import Data.String (Pattern(..), split)

parseJsonConfigContent :: forall f r.
                         (Monad f) =>
                         { throw :: forall a. String -> f a
                         | r } ->
                         JsonConfigContent -> f (Map.Map String Pos)
parseJsonConfigContent k { jsonConfig: jsonConfig, content: content } = do
  let contentArray = content <#> split (Pattern "")
  pure $ toMapPos contentArray
