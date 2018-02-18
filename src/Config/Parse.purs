module Config.Parse where

import Types

import Data.Map as Map
import Data.Int (fromString)
import Data.String (Pattern(..), split, trim, joinWith)
import Data.Foldable
import Data.Traversable

parseInt :: forall f r.
            Applicative f =>
            { throw :: forall a. String -> f a
            | r } ->
            String -> f Int
parseInt k s = case fromString s of
    Just i -> pure i
    Nothing -> k.throw "Error parsing int."

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
  scaleX <- findValue k "scaleX" map >>= parseInt k
  scaleY <- findValue k "scaleY" map >>= parseInt k
  pure { scaleX: scaleX, scaleY: scaleY }
  where
    findValue :: forall f r.
                 Applicative f =>
                 { throw :: forall a. String -> f a
                 | r } ->
                 String -> Map.Map String String -> f String
    findValue k key map = case map # Map.lookup key of
        Just val -> pure val
        Nothing -> orDefault k key defaults
    orDefault :: forall f r.
                 Applicative f =>
                 { throw :: forall a. String -> f a
                 | r } ->
                 String -> Map.Map String String -> f String
    orDefault k key defaults = case defaults # Map.lookup key of
        Just val -> pure val
        Nothing -> k.throw ("No value given for/no default value for" <> show key <> ". Proposed action: provide value")

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
