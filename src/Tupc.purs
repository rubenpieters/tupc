module Tupc
  ( module Exported
  , module Tupc
  ) where

import Types
import Content.Parse (Pos(..), toMapPos)
import Config.Parse
import Config.File

import Data.Map as Map

import Content.Parse (Pos(..)) as Exported

parseRaw :: forall f r.
             (Monad f) =>
             { throw :: forall a. String -> f a
             , rawContents :: f String
             | r } ->
             f (Map.Map String Pos)
parseRaw k = do
  { config: config, content: content } <- readRawFile k
  parsedConfig <- mkConfig k Map.empty config
  pure $ toMapPos content
