module Config.Json where

import Types
import Content.Parse
import Config.Apply

import Data.Map as Map
import Data.String (toCharArray)
import Data.Argonaut (Json, encodeJson)

parseJsonConfigContent :: forall f r.
                          Monad f =>
                          { throw :: forall a. String -> f a
                          | r } ->
                          JsonConfigContent -> f (Map.Map Char Pos)
parseJsonConfigContent k { jsonConfig: jsonConfig, content: content } = do
  let contentArray = content <#> toCharArray
  let unprocessedMapPos = toMapPos contentArray
  pure $ unprocessedMapPos # applyConfig jsonConfig

mapPosToJson :: Map.Map String Pos -> Json
mapPosToJson map = encodeJson map
