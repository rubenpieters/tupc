module Tupc.Internal (module Tupc.Internal, module Exported) where

import Prelude as Exported

import Data.Array (uncons, cons) as Exported
import Data.Maybe as Exported
import Data.Tuple as Exported
import Data.Either as Exported
import Data.Foldable as Exported
import Data.Traversable as Exported
import Data.SubRecord (SubRecord) as Exported
import Data.Symbol as Exported
import Data.Map (Map) as Exported

import Control.Monad.Eff as Exported
import Control.Monad.Eff.Console (log, logShow) as Exported
import Control.Monad.Eff.Exception (throw) as Exported

import Prelude

import Data.Maybe
import Data.Tuple
import Data.Map (Map)
import Data.Map as Map
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (:=), (~>), (.?))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq as Rep
import Data.Generic.Rep.Show as Rep
import Data.SubRecord

data DirectionX
  = DirXLeft
  | DirXRight

derive instance genericDirectionX :: Generic DirectionX _
instance eqDirectionX :: Eq DirectionX where
  eq = Rep.genericEq
instance showDirectionX :: Show DirectionX where
  show = Rep.genericShow

data DirectionY
  = DirYUp
  | DirYDown

derive instance genericDirectionY :: Generic DirectionY _
instance eqDirectionY :: Eq DirectionY where
  eq = Rep.genericEq
instance showDirectionY :: Show DirectionY where
  show = Rep.genericShow

data OriginX
  = OriXLeft
  | OriXRight

derive instance genericOriginX :: Generic OriginX _
instance eqOriginX :: Eq OriginX where
  eq = Rep.genericEq
instance showOriginX :: Show OriginX where
  show = Rep.genericShow

data OriginY
  = OriYUp
  | OriYDown

derive instance genericOriginY :: Generic OriginY _
instance eqOriginY :: Eq OriginY where
  eq = Rep.genericEq
instance showOriginY :: Show OriginY where
  show = Rep.genericShow

-- raw json representation of configuration parameters
type JsonConfig =
  { scale :: Int
  , scaleX :: Maybe Int
  , scaleY :: Maybe Int
  , ignore :: Array Char
  , ignoreExtra :: Array Char
  , originX :: OriginX
  , originY :: OriginY
  , directionX :: DirectionX
  , directionY :: DirectionY
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

type SubJsonConfigContent =
  { subJsonConfig :: SubRecord OptParams
  , content :: Content
  }

newtype Pos = Pos
  { xLeft :: Int
  , xRight :: Int
  , yTop :: Int
  , yBot :: Int
  }

derive instance genericPos :: Generic Pos _
instance eqPos :: Eq Pos where
  eq = Rep.genericEq
instance showPos :: Show Pos where
  show = Rep.genericShow
instance encodeJsonPos :: EncodeJson Pos where
  encodeJson (Pos pos) =
    "xLeft" := pos.xLeft
    ~> "xRight" := pos.xRight
    ~> "yTop" := pos.yTop
    ~> "yBot" := pos.yBot
instance decodeJsonPos :: DecodeJson Pos where
  decodeJson json = do
    obj <- decodeJson json
    xLeft <- obj .? "xLeft"
    xRight <- obj .? "xRight"
    yTop <- obj .? "yTop"
    yBot <- obj .? "yBot"
    pure $ Pos { xLeft, xRight, yTop, yBot }

newtype EnrichedPos = EnrichedPos
  { xLeft :: Int
  , xRight :: Int
  , yTop :: Int
  , yBot :: Int
  , xWidth :: Int
  , yHeight :: Int
  , xCenter :: Int
  , yCenter :: Int
  }

derive instance genericEnrichedPos :: Generic EnrichedPos _
instance eqEnrichedPos :: Eq EnrichedPos where
  eq = Rep.genericEq
instance showEnrichedPos :: Show EnrichedPos where
  show = Rep.genericShow

type OptParams =
  ( scale :: Int
  , scaleX :: Maybe Int
  , scaleY :: Maybe Int
  , ignore :: Array Char
  , ignoreExtra :: Array Char
  , originX :: OriginX
  , originY :: OriginY
  , directionX :: DirectionX
  , directionY :: DirectionY
  )

tupcDefaultsRecord :: JsonConfig
tupcDefaultsRecord =
  { scale: 1
  , scaleX: Nothing
  , scaleY: Nothing
  , ignore: ['+', '-', '|', ' ']
  , ignoreExtra: []
  , originX: OriXLeft
  , originY: OriYUp
  , directionX: DirXRight
  , directionY: DirYDown
  }

tupcDefaults :: Map String String
tupcDefaults = Map.fromFoldable
  [ Tuple "scale" "1"
  , Tuple "directionX" "Right"
  , Tuple "directionY" "Down"
  , Tuple "originX" "Left"
  , Tuple "originY" "Up"
  , Tuple "ignore" "+-|, "
  , Tuple "ignoreExtra" ""
  ]
