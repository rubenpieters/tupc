module Main where

import Types
import ReadConfig
import Config.File

import Data.Array as Array
import Data.Map as Map
import Data.Set as Set

import Data.Generic.Rep as Rep
import Data.Generic.Rep.Eq (genericEq)

-- https://github.com/nh2/haskell-ordnub
ordNub :: forall a. (Ord a) =>
          Array a -> Array a
ordNub l = go Set.empty l
  where
    go s l = case uncons l of
      Nothing -> []
      Just { head: x, tail: xs } ->
        if x `Set.member` s
           then go s xs
           else x `cons` go (Set.insert x s) xs

transpose :: forall a. Array (Array a) -> Array (Array a)
transpose l = case uncons l of
  Nothing -> []
  Just { head: l', tail: xss } -> case uncons l' of
    Nothing ->
      transpose xss
    Just { head: x, tail: xs } ->
      (x `cons` Array.mapMaybe Array.head xss)
        `cons`
      transpose (xs `cons` Array.mapMaybe Array.tail xss)

data FL = FL
  { first :: Int
  , last :: Int
  }

instance semigroupFL :: Semigroup FL where
  append (FL fl1) (FL fl2) = FL { first: min fl1.first fl2.first, last: max fl1.last fl2.last }

instance showFL :: Show FL where
  show (FL fl) = "{ first: " <> show fl.first <> ", last: " <> show fl.last <> " }"

data Pos = Pos
  { xTop :: Int
  , xBot :: Int
  , yLeft :: Int
  , yRight :: Int
  }

instance showPos :: Show Pos where
  show (Pos p) = "{ x: " <> show p.xTop <> " -- " <> show p.xBot <> ", y: " <> show p.yLeft <> " -- " <> show p.yRight <> " }"

derive instance genericPos :: Rep.Generic Pos _
instance eqPos :: Eq Pos where
  eq = genericEq

type Result a = Map.Map a FL

firstLast' :: forall a. (Ord a) =>
              Array (Array a) -> Result a
firstLast' l = firstLast l 0

firstLast :: forall a. (Ord a) =>
             Array (Array a) -> Int -> Result a
firstLast l index = case uncons l of
  Just { head: new, tail: t} ->
    let (newFiltered :: Array a) = ordNub new
        combine map v = map # addElem v index
        mapI = newFiltered # foldl combine Map.empty
        mapRec = firstLast t (index+1)
    in Map.unionWith (<>) mapI mapRec
  Nothing -> Map.empty

addElem :: forall a. (Ord a) =>
           a -> Int -> Result a -> Result a
addElem a index result = result # Map.alter f a
  where
    f :: Maybe FL -> Maybe FL
    f (Just (FL r)) = Just (FL (r { last= index }))
    f Nothing  = Just (FL { first: index, last: index })

calcPos :: forall a. (Ord a) =>
           Array (Array a) -> Map.Map a Pos
calcPos l = combinedMap
  where
    mapX :: Result a
    mapX = firstLast' l
    mapY :: Result a
    mapY = firstLast' (transpose l)
    mapXInj = mapX <#> (\(FL fl) -> Pos { xTop: fl.first, xBot: fl.last, yLeft: 0, yRight: 0 })
    mapYInj = mapY <#> (\(FL fl) -> Pos { xTop: 0, xBot: 0, yLeft: fl.first, yRight: fl.last })
    combinedMap = Map.unionWith combine mapXInj mapYInj
    combine (Pos x) (Pos y) = Pos
      { xTop: x.xTop
      , xBot: x.xBot
      , yLeft: y.yLeft
      , yRight: y.yRight
      }


testData :: Array (Array String)
testData =
  [ ["1", "1", "3"]
  , ["1", "1", "3"]
  , ["2", "2", "2"]
  ]

main :: Eff _ Unit
main = do
  logShow (firstLast' testData)
  logShow (firstLast' (transpose testData))
  logShow (calcPos testData)

