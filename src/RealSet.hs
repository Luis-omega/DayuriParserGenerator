{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module RealSet(
  RealSet()
  ,toList
  ,append
  ,concat
  ,fromList
  ,elem
  ,singleton
  ,empty
  ,difference
  ,null
  ,enumerate
)where 

import Data.Bifunctor(second)
import Prelude hiding(concat, elem, null)
import qualified Prelude as P

data RealSet a = RealSet {counter::Int,  list::[(Int,a)]}
  deriving (Eq, Functor, Foldable, Traversable, Show)


instance Eq a => Semigroup (RealSet a) where 
  (<>) = concat

instance Eq a => Monoid (RealSet a) where 
  mempty = empty


toList :: RealSet a -> [a]
toList x = [y | (_,y)<- list x]

append :: (Eq a) => a -> RealSet a -> RealSet a
append x xs= 
  if P.elem x (toList xs) then 
    xs
  else 
    let new_counter = 1+ counter xs in
      RealSet new_counter ((new_counter,x):list xs)

concat :: (Eq a) => RealSet a -> RealSet a -> RealSet a
concat (RealSet _ []) b = b
concat a (RealSet _ []) = a
concat a b = fromList $ toList b ++ toList a

fromList :: Eq a => [a] -> RealSet a
fromList = foldr append mempty

elem :: Eq a => a -> RealSet a -> Bool
elem e l = P.elem e $ toList l

singleton x = RealSet 1 [(1,x)]

empty = RealSet 0 []

maxr :: [(Int,a)]-> Int
maxr = foldr max2 0  
  where 
    max2 :: (Int, c) -> Int -> Int
    max2 (n,y) x = if x <n then n else x

delete :: Eq a => a -> RealSet a -> RealSet a
delete x rs = 
    let new = [(n,y) | (n,y)<- list rs , y/=x] in 
      if new == list rs then
        rs
      else 
        RealSet (maxr new) new


null (RealSet _ []) = True
null (RealSet _ x) = False

difference :: Eq a => RealSet a -> RealSet a  ->RealSet a
difference x y = fromList [ z | z<- toList x , notElem z (toList y)   ] 


enumerate :: RealSet a -> [(Int,a)]
enumerate = enumerateList . toList
  where 
    enumerateList [] = []
    enumerateList (x:xs) = 
      let new = enumerateList xs in 
        case new of 
          [] -> [(1,x)]
          ((n,y):ys) -> (n+1,x):new
