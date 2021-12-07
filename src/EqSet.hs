{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

{-|
  Implementation of Sets using 'List' assuming just 'Eq' instance.
  A 'Ord' based implementation can't in general define a 'Ord' 
  for Set, so we have problems building Set of Set.
-}

module EqSet(
  toList
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

newtype EqSet a = EqSet {list::[a]}
  deriving (Eq, Functor, Foldable, Traversable, Show)


instance Eq a => Semigroup (EqSet a) where 
  (<>) = concat

instance Eq a => Monoid (EqSet a) where 
  mempty = empty


toList :: EqSet a -> [a]
toList = list

append :: (Eq a) => a -> EqSet a -> EqSet a
append x xs= 
  if P.elem x (toList xs) then 
    xs
  else 
      EqSet (x:list xs)

concat :: (Eq a) => EqSet a -> EqSet a -> EqSet a
concat (EqSet []) b = b
concat a (EqSet []) = a
concat a b = fromList $ toList a ++ toList b

fromList :: Eq a => [a] -> EqSet a
fromList = foldr append mempty

elem :: Eq a => a -> EqSet a -> Bool
elem e l = P.elem e $ toList l

singleton x = EqSet [x]

empty = EqSet []

delete :: Eq a => a -> EqSet a -> EqSet a
delete x rs = 
    let new = filter (/=x) $ toList rs in 
        EqSet new

null (EqSet []) = True
null (EqSet x) = False

difference :: Eq a => EqSet a -> EqSet a  ->EqSet a
difference x y = fromList $ filter (\z -> notElem z (toList y)) $ toList x

enumerate :: EqSet a -> [(Int,a)]
enumerate = enumerateList . toList
  where 
    enumerateList [] = []
    enumerateList (x:xs) = 
      let new = enumerateList xs in 
        case new of 
          [] -> [(1,x)]
          ((n,y):ys) -> (n+1,x):new
