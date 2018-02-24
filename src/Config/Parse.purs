module Config.Parse where

import Types

import Data.Map as Map
import Data.Int (fromString)
import Data.String (Pattern(..), split, trim, joinWith)

parseInt :: forall f r.
            Applicative f =>
            { throw :: forall a. String -> f a
            | r } ->
            String -> f Int
parseInt k s = case fromString s of
    Just i -> pure i
    Nothing -> k.throw "Error parsing int."

parseDirectionX :: forall f r.
                   Applicative f =>
                   { throw :: forall a. String -> f a
                   | r } ->
                   String -> f DirectionX
parseDirectionX k s = case s of
    "Left" -> pure XLeft
    "Right" -> pure XRight
    _ -> k.throw "Error parsing directionX."

parseDirectionY :: forall f r.
                   Applicative f =>
                   { throw :: forall a. String -> f a
                   | r } ->
                   String -> f DirectionY
parseDirectionY k s = case s of
    "Up" -> pure YUp
    "Down" -> pure YDown
    _ -> k.throw "Error parsing directionY."

mkConfig :: forall l f r.
            Functor l => Traversable l =>
            Monad f =>
            { throw :: forall a. String -> f a
            | r } ->
            Map.Map String String -> l String -> f JsonConfig
mkConfig k defaults configLines = do
  let result = configLines <#> split (Pattern "=")
                           <#> (_ <#> trim)
  result' <- for result (mkConfigKeyVal k)
  let map = Map.fromFoldable result'
  scaleX <- for (findOptValue "scaleX" map) (parseInt k)
  scaleY <- for (findOptValue "scaleY" map) (parseInt k)
  scale <- findReqValue "scale" map >>= parseInt k
  directionX <- findReqValue "directionX" map >>= parseDirectionX k
  directionY <- findReqValue "directionY" map >>= parseDirectionY k
  originX <- findReqValue "originX" map >>= parseDirectionX k
  originY <- findReqValue "originY" map >>= parseDirectionY k
  pure { scale: scale
       , scaleX: scaleX
       , scaleY: scaleY
       , directionX: directionX
       , directionY: directionY
       , originX: originX
       , originY: originY
       }
  where
    findReqValue :: String -> Map.Map String String -> f String
    findReqValue key map = case map # Map.lookup key of
        Just val -> pure val
        Nothing -> orDefault key
    orDefault :: String -> f String
    orDefault key = case defaults # Map.lookup key of
        Just val -> pure val
        Nothing -> k.throw ("No value given for/no default value for" <> show key <> ". Proposed action: provide value")
    findOptValue :: String -> Map.Map String String -> Maybe String
    findOptValue key map = case map # Map.lookup key of
        Just val -> Just val
        Nothing -> Nothing

type ConfigKeyVal = Tuple String String

mergeVal :: forall f r.
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
    Just { head: key, tail: t } -> pure [key, t # joinWith "="]
    Nothing -> k.throw ("During: reading configuration, impossible match. Proposed action: report as bug.")

mkConfigKeyVal :: forall f r.
                  Applicative f =>
                  { throw :: forall a. String -> f a
                  | r } ->
                  Array String -> f ConfigKeyVal
mkConfigKeyVal k [key, val] = pure (Tuple key val)
mkConfigKeyVal k x = k.throw
  ("During: reading configuration, wrong key value length: " <> show x <> ". proposed action: report as bug")
