module Config.Json where

import Tupc.Internal
import Content.Parse as Parse
import Config.Apply as Apply

import Data.Argonaut (Json)
import Data.String as String
import Data.Argonaut as Argonaut
import Data.SubRecord as SubRecord

parseJsonConfigContent ::
  forall f r.
  Monad f =>
  { throw :: forall a. String -> f a
  | r } ->
  { subJsonConfig :: SubRecord OptParams, content :: Content } ->
  f (Map Char EnrichedPos)
parseJsonConfigContent k { subJsonConfig: subJsonConfig, content: content } = do
  let jsonConfig = subJsonConfig # SubRecord.withDefaults tupcDefaultsRecord
  let contentArray = content <#> String.toCharArray
  let ignored = jsonConfig.ignore <> jsonConfig.ignoreExtra
  let unprocessedMapPos = contentArray # Parse.toMapPos ignored
  let bounds = contentArray # Parse.getBounds
  pure $ unprocessedMapPos # Apply.applyConfig jsonConfig bounds

mapPosToJson :: Map String Pos -> Json
mapPosToJson map = Argonaut.encodeJson map

