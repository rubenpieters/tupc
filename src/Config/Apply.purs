module Config.Apply where

import Types

import Data.Map as Map

applyConfig :: forall a.
               JsonConfig ->
               { maxX :: Int, maxY :: Int } ->
               Map.Map a Pos -> Map.Map a Pos
applyConfig jsonConfig bounds mapPos = mapPos <#> f
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
    originX = jsonConfig.originX
    originY = jsonConfig.originY
    subX = case originX of
      XLeft -> 0
      XRight -> bounds.maxX
    subY = case originY of
      YUp -> 0
      YDown -> bounds.maxY
    f :: Pos -> Pos
    f (Pos pos) = Pos { xLeft: (pos.xLeft - subX) * scaleX * multiplierX
                      , xRight: (pos.xRight - subX) * scaleX * multiplierX
                      , yTop: (pos.yTop - subY) * scaleY * multiplierY
                      , yBot: (pos.yBot - subY) * scaleY * multiplierY
                      }


-- from origin Left direction Right
-- 0 -- 10 -- 20
-- to origin Right direction Right
-- -20 -- -10 -- 0
-- to origin Right direction Left
-- 20 -- 10 -- 0
