module Config.Json where

import Types
import Content.Parse
import Config.Apply

import Data.Map as Map
import Data.String (toCharArray)
import Data.Argonaut (Json, encodeJson)
import Data.SubRecord as SubRecord

parseJsonConfigContent :: forall f r.
                          Monad f =>
                          { throw :: forall a. String -> f a
                          | r } ->
--                          { jsonConfig: SubRow JsonConfig, content: Content } ->
                          { subJsonConfig :: SubRecord OptParams, content :: Content } ->
                          f (Map.Map Char Pos)
parseJsonConfigContent k { subJsonConfig: subJsonConfig, content: content } = do
  let jsonConfig = subJsonConfig # SubRecord.withDefaults tupcDefaultsRecord
  let contentArray = content <#> toCharArray
  let ignored = jsonConfig.ignore <> jsonConfig.ignoreExtra
  let unprocessedMapPos = contentArray # toMapPos ignored
  let bounds = contentArray # getBounds
  pure $ unprocessedMapPos # applyConfig jsonConfig bounds

mapPosToJson :: Map.Map String Pos -> Json
mapPosToJson map = encodeJson map

