module Config.Apply where

import Types

import Data.Map as Map

applyConfig :: forall a. JsonConfig -> Map.Map a Pos -> Map.Map a Pos
applyConfig jsonConfig mapPos = mapPos <#> f
  where
    scale = jsonConfig.scale
    scaleX = jsonConfig.scaleX # fromMaybe scale
    scaleY = jsonConfig.scaleY # fromMaybe scale
    f :: Pos -> Pos
    f (Pos pos) = Pos { xLeft: pos.xLeft * scaleX, xRight: pos.xRight * scaleX, yTop: pos.yTop * scaleY, yBot: pos.yBot * scaleY }
