module Types (module Types, module Exported) where

import Prelude as Exported
import Data.Array (uncons, cons) as Exported
import Data.Maybe as Exported
import Data.Tuple as Exported
import Data.Either as Exported
import Data.Foldable as Exported
import Data.Traversable as Exported

import Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (:=), (~>), (.?))
import Data.Generic.Rep as Rep
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)

import Control.Monad.Eff as Exported
import Control.Monad.Eff.Console (log, logShow) as Exported
import Control.Monad.Eff.Exception (throw) as Exported

-- raw json representation of configuration parameters
type JsonConfig =
  { scaleX :: Int
  , scaleY :: Int
  }

-- content in data section
-- example:
-- ["113",
--  "113",
--  "222"]
type Content = Array String

type ConfigContent =
  -- lines read from configuration part
  -- # should be stripped from the start (TODO: make this into newtype?)
  { config :: Array String
  , content :: Content
  }

type JsonConfigContent =
  { jsonConfig :: JsonConfig
  , content :: Content
  }

newtype Pos = Pos
  { xTop :: Int
  , xBot :: Int
  , yLeft :: Int
  , yRight :: Int
  }

derive instance genericPos :: Rep.Generic Pos _
instance eqPos :: Eq Pos where
  eq = genericEq
instance showPos :: Show Pos where
  show = genericShow
instance encodeJsonPos :: EncodeJson Pos where
  encodeJson (Pos pos) =
    "xTop" := pos.xTop
    ~> "xBot" := pos.xBot
    ~> "yLeft" := pos.yLeft
    ~> "yRight" := pos.yRight
instance decodeJsonPos :: DecodeJson Pos where
  decodeJson json = do
    obj <- decodeJson json
    xTop <- obj .? "xTop"
    xBot <- obj .? "xBot"
    yLeft <- obj .? "yLeft"
    yRight <- obj .? "yRight"
    pure $ Pos { xTop, xBot, yLeft, yRight }

