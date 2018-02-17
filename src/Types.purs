module Types (module Types, module Exported) where

import Prelude as Exported
import Data.Array (uncons, cons) as Exported
import Data.Maybe as Exported
import Data.Tuple as Exported
import Data.Either as Exported
import Data.Foldable as Exported

import Control.Monad.Eff as Exported
import Control.Monad.Eff.Console (log, logShow) as Exported
import Control.Monad.Eff.Exception (throw) as Exported

type ConfigLines =
  -- lines read from configuration part
  -- # should be stripped from the start (TODO: make this into newtype)
  { config :: Array String
  -- lines read from data part
  , content :: Array (Array String)
  }
