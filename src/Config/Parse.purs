module Config.Parse where

import Types

import Data.Map as Map
import Data.Int as Int
import Data.String (Pattern(..))
import Data.String as String
import Data.SubRecord as SubRecord
import Data.SubRecord.Builder as SubRecord

parseInt ::
  forall f r.
  Applicative f =>
  { throw :: forall a. String -> f a
  | r } ->
  String -> f Int
parseInt k s = case Int.fromString s of
  Just i -> pure i
  Nothing -> k.throw "Error parsing int."

parseDirectionX ::
  forall f r.
  Applicative f =>
  { throw :: forall a. String -> f a
  | r } ->
  String -> f DirectionX
parseDirectionX k s = case s of
  "Left" -> pure DirXLeft
  "Right" -> pure DirXRight
  _ -> k.throw "Error parsing directionX."

parseDirectionY ::
  forall f r.
  Applicative f =>
  { throw :: forall a. String -> f a
  | r } ->
  String -> f DirectionY
parseDirectionY k s = case s of
  "Up" -> pure DirYUp
  "Down" -> pure DirYDown
  _ -> k.throw "Error parsing directionY."

parseOriginX ::
  forall f r.
  Applicative f =>
  { throw :: forall a. String -> f a
  | r } ->
  String -> f OriginX
parseOriginX k s = case s of
  "Left" -> pure OriXLeft
  "Right" -> pure OriXRight
  _ -> k.throw "Error parsing originX."

parseOriginY ::
  forall f r.
  Applicative f =>
  { throw :: forall a. String -> f a
  | r } ->
  String -> f OriginY
parseOriginY k s = case s of
  "Up" -> pure OriYUp
  "Down" -> pure OriYDown
  _ -> k.throw "Error parsing originY."

mkConfig ::
  forall l f r.
  Functor l => Traversable l =>
  Monad f =>
  { throw :: forall a. String -> f a
  | r } ->
  l String -> f (SubRecord OptParams)
mkConfig k configLines = do
  let result = configLines <#> String.split (Pattern "=")
                           <#> (_ <#> String.trim)
  result' <- for result (mkConfigKeyVal k)
  let map = Map.fromFoldable result'
  let at x = map # Map.lookup x
  scale <- for (at "scale") (parseInt k)
  scaleX <- for (at "scaleX") (parseInt k)
  scaleY <- for (at "scaleY") (parseInt k)
  directionX <- for (at "directionX") (parseDirectionX k)
  directionY <- for (at "directionY") (parseDirectionY k)
  originX <- for (at "originX") (parseOriginX k)
  originY <- for (at "originY") (parseOriginY k)
  let ignore = at "ignore" <#> String.toCharArray
  let ignoreExtra = at "ignoreExtra" <#> String.toCharArray
  pure $
    SubRecord.build
    ( SubRecord.insert (SProxy :: SProxy "scale") scale >>>
      SubRecord.insert (SProxy :: SProxy "scaleX") (Just scaleX) >>>
      SubRecord.insert (SProxy :: SProxy "scaleY") (Just scaleY) >>>
      SubRecord.insert (SProxy :: SProxy "directionX") directionX >>>
      SubRecord.insert (SProxy :: SProxy "directionY") directionY >>>
      SubRecord.insert (SProxy :: SProxy "originX") originX >>>
      SubRecord.insert (SProxy :: SProxy "originY") originY >>>
      SubRecord.insert (SProxy :: SProxy "ignore") ignore >>>
      SubRecord.insert (SProxy :: SProxy "ignoreExtra") ignoreExtra
    ) (SubRecord.mkSubRecord {})

type ConfigKeyVal = Tuple String String

mergeVal ::
  forall f r.
  Applicative f =>
  { throw :: forall a. String -> f a
  | r } ->
  Array String -> f (Array String)
mergeVal k [] = k.throw
  ("Empty configuration detected.")
mergeVal k [key] = k.throw
  ("No value given for " <> show key <> ". Proposed action: provide value")
mergeVal k l@[key, val] = pure l
mergeVal k l = case uncons l of
    Just { head: key, tail: t } -> pure [key, t # String.joinWith "="]
    Nothing -> k.throw ("During: reading configuration, impossible match. Proposed action: report as bug.")

mkConfigKeyVal ::
  forall f r.
  Applicative f =>
  { throw :: forall a. String -> f a
  | r } ->
  Array String -> f ConfigKeyVal
mkConfigKeyVal k [key, val] = pure (Tuple key val)
mkConfigKeyVal k x = k.throw
  ("During: reading configuration, wrong key value length: " <> show x <> ". proposed action: report as bug")
