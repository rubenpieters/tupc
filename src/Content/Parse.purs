module Content.Parse where

import Types

import Data.Array as Array
import Data.Map as Map
import Data.Set as Set

import Data.Generic.Rep as Rep
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)

-- https://github.com/nh2/haskell-ordnub
ordNub :: forall a. (Ord a) =>
          Array a -> Array a
ordNub = go Set.empty
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

newtype FL = FL
  { first :: Int
  , last :: Int
  }

instance semigroupFL :: Semigroup FL where
  append (FL fl1) (FL fl2) = FL { first: min fl1.first fl2.first, last: max fl1.last fl2.last }

derive instance genericFL :: Rep.Generic FL _
instance eqFL :: Eq FL where
  eq = genericEq
instance showFL :: Show FL where
  show = genericShow

type Result a = Map.Map a FL

toMapFL :: forall a. (Ord a) =>
           Array (Array a) -> Result a
toMapFL = go 0
  where
    go :: Int -> Array (Array a) -> Result a
    go index l = case uncons l of
      Just { head: new, tail: t} ->
        let (newFiltered :: Array a) = ordNub new
            combine map v = map # addElem v index
            mapI = newFiltered # foldl combine Map.empty
            mapRec = go (index+1) t
        in Map.unionWith (<>) mapI mapRec
      Nothing -> Map.empty

    addElem :: a -> Int -> Result a -> Result a
    addElem a index result = result # Map.alter (f index) a

    f :: Int -> Maybe FL -> Maybe FL
    f index (Just (FL r)) = Just (FL (r { last= index }))
    f index Nothing  = Just (FL { first: index, last: index })

toMapPos :: forall a. (Ord a) =>
            Array (Array a) -> Map.Map a Pos
toMapPos l = combinedMap
  where
    mapX :: Result a
    mapX = toMapFL l
    mapY :: Result a
    mapY = toMapFL (transpose l)
    mapXInj = mapX <#> (\(FL fl) -> Pos { xTop: fl.first, xBot: fl.last, yLeft: 0, yRight: 0 })
    mapYInj = mapY <#> (\(FL fl) -> Pos { xTop: 0, xBot: 0, yLeft: fl.first, yRight: fl.last })
    combinedMap = Map.unionWith combine mapXInj mapYInj
    combine (Pos x) (Pos y) = Pos
      { xTop: x.xTop
      , xBot: x.xBot
      , yLeft: y.yLeft
      , yRight: y.yRight
      }

