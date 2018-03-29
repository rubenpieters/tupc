module Content.Parse where

import Tupc.Internal

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
  append (FL fl1) (FL fl2) = FL { first: min fl1.first fl2.first
                                , last: max fl1.last fl2.last }

derive instance genericFL :: Rep.Generic FL _
instance eqFL :: Eq FL where
  eq = genericEq
instance showFL :: Show FL where
  show = genericShow

type Result a = Map.Map a FL

toMapFL :: forall a. (Ord a) =>
           Array a -> Array (Array a) -> Result a
toMapFL ignored = go 0
  where
    go :: Int -> Array (Array a) -> Result a
    go index l = case uncons l of
      Just { head: new, tail: t} ->
        let (newFiltered :: Array a)
              = new # ordNub
                    # Array.filter (\x -> not (elem x ignored))
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
            Array a -> Array (Array a) -> Map.Map a Pos
toMapPos ignored l = combinedMap
  where
    mapX :: Result a
    mapX = toMapFL ignored (transpose l)
    mapY :: Result a
    mapY = toMapFL ignored l
    mapXInj = mapX <#> (\(FL fl) -> Pos { xLeft: fl.first
                                        , xRight: (fl.last + 1)
                                        , yTop: 0
                                        , yBot: 0
                                        })
    mapYInj = mapY <#> (\(FL fl) -> Pos { xLeft: 0
                                        , xRight: 0
                                        , yTop: fl.first
                                        , yBot: (fl.last + 1)
                                        })
    combinedMap = Map.unionWith combine mapXInj mapYInj
    combine (Pos x) (Pos y) = Pos
      { xLeft: x.xLeft
      , xRight: x.xRight
      , yTop: y.yTop
      , yBot: y.yBot
      }

getBounds :: forall a.
          Array (Array a) -> { maxX :: Int, maxY :: Int }
getBounds a = { maxX: maxX, maxY: maxY}
  where
    maxX = (a <#> length) # maximum
                          # fromMaybe 0
    maxY = a # length
