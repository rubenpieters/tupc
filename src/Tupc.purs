module Tupc
  ( module Exported
  , module Tupc
  ) where

import Types
import Config.File
import Config.Json

import Data.Map as Map

import Types (Pos(..)) as Exported

parseRaw :: forall f r.
             Monad f =>
             { throw :: forall a. String -> f a
             , rawContents :: f String
             | r } ->
             f (Map.Map String Pos)
parseRaw k = do
  jsonConfigContent <- rawToJsonConfigContent k
  parseJsonConfigContent k jsonConfigContent
