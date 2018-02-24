module Config.Apply where

import Types

import Data.Map as Map

applyConfig :: forall a. JsonConfig -> Map.Map a Pos -> Map.Map a Pos
applyConfig jsonConfig mapPos = mapPos <#> f
  where
    scale = jsonConfig.scale
    scaleX = jsonConfig.scaleX # fromMaybe scale
    scaleY = jsonConfig.scaleY # fromMaybe scale
    directionX = jsonConfig.directionX
    directionY = jsonConfig.directionY
    multiplierX = case directionX of
      XLeft -> (-1)
      XRight -> 1
    multiplierY = case directionY of
      YUp -> (-1)
      YDown -> 1
    f :: Pos -> Pos
    f (Pos pos) = Pos { xLeft: pos.xLeft * scaleX * multiplierX
                      , xRight: pos.xRight * scaleX * multiplierX
                      , yTop: pos.yTop * scaleY * multiplierY
                      , yBot: pos.yBot * scaleY * multiplierY
                      }
