module Config.Apply where

import Types

applyConfig ::
  forall a.
  JsonConfig ->
  { maxX :: Int, maxY :: Int } ->
  Map a Pos -> Map a EnrichedPos
applyConfig jsonConfig bounds mapPos = mapPos <#> f
  where
    scale = jsonConfig.scale
    scaleX = jsonConfig.scaleX # fromMaybe scale
    scaleY = jsonConfig.scaleY # fromMaybe scale
    directionX = jsonConfig.directionX
    directionY = jsonConfig.directionY
    multiplierX = case directionX of
      DirXLeft -> (-1)
      DirXRight -> 1
    multiplierY = case directionY of
      DirYUp -> (-1)
      DirYDown -> 1
    originX = jsonConfig.originX
    originY = jsonConfig.originY
    subX = case originX of
      OriXLeft -> 0
      OriXRight -> bounds.maxX
    subY = case originY of
      OriYUp -> 0
      OriYDown -> bounds.maxY
    centerStartX pos = case directionX of
      DirXLeft -> pos.xRight
      DirXRight -> pos.xLeft
    centerStartY pos = case directionY of
      DirYUp -> pos.yBot
      DirYDown -> pos.yTop
    f :: Pos -> EnrichedPos
    f (Pos pos) = EnrichedPos
      let
        xLeft = (pos.xLeft - subX) * scaleX * multiplierX
        xRight = (pos.xRight - subX) * scaleX * multiplierX
        yTop = (pos.yTop - subY) * scaleY * multiplierY
        yBot = (pos.yBot - subY) * scaleY * multiplierY
        xWidth = (pos.xRight - pos.xLeft) * scaleX
        yHeight = (pos.yBot - pos.yTop) * scaleY
        xCenter = centerStartX { xLeft: xLeft, xRight: xRight } + (xWidth / 2)
        yCenter = centerStartY { yTop: yTop, yBot: yBot } + (yHeight / 2)
      in
        { xLeft: xLeft
        , xRight: xRight
        , yTop: yTop
        , yBot: yBot
        , xWidth: xWidth
        , yHeight: yHeight
        , xCenter: xCenter
        , yCenter: yCenter
        }


-- from origin Left direction Right
-- 0 -- 10 -- 20
-- to origin Right direction Right
-- -20 -- -10 -- 0
-- to origin Right direction Left
-- 20 -- 10 -- 0
