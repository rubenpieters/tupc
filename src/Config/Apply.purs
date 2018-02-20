module Config.Apply where

import Types

import Data.Map as Map

applyConfig :: forall a. JsonConfig -> Map.Map a Pos -> Map.Map a Pos
applyConfig jsonConfig mapPos = mapPos <#> f
  where
    scaleX = jsonConfig.scaleX
    scaleY = jsonConfig.scaleY
    f :: Pos -> Pos
    f (Pos pos) = Pos { xLeft: pos.xLeft * scaleX, xRight: pos.xRight * scaleX, yTop: pos.yTop * scaleY, yBot: pos.yBot * scaleY }
