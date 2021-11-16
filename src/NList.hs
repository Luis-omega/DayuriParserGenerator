{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module NList(
  toList
  ,fromList
  ,NList(..)
  ,toTuple
  ,append
  ,filter
  ,singleton
)where 

import Prelude hiding(head,tail,filter)
import qualified Prelude as P

data NList a = NList {head::a, tail::[a]}
  deriving (Functor, Foldable, Traversable, Eq)

singleton :: a -> NList a
singleton x = NList x []

append :: a -> NList a -> NList a
append x ls= NList x (head ls :tail ls)

toList :: NList a -> [a]
toList ls = head ls : tail ls

fromList :: [a] ->Maybe (NList a)
fromList [] = Nothing
fromList (x:xs) = Just $ NList x xs

toTuple :: NList a -> (a,[a])
toTuple x = (head x, tail x)

filter :: (a->Bool)-> NList a ->Maybe (NList a)
filter f ls = 
  case P.filter f (toList ls) of 
    [] -> Nothing
    x:xs -> Just $ NList x xs  

filter2List :: (a->Bool)-> NList a -> [a]
filter2List f = P.filter f . toList



instance Show a => Show (NList a) where
  show  = show . toList

instance Applicative NList where 
  pure a = NList a []
  (NList fh ft) <*> (NList ah at) = 
    NList (fh ah) (map fh at++ (ft <*> (ah:at)))

instance Monad NList where 
  return = pure
  (NList ah at) >>= f = 
    NList (head $ f ah) $ tail (f ah) ++  (at >>=(toList . f))

instance Semigroup (NList a) where 
  a <> b = NList (head a) (tail a ++ toList b)

instance Ord a => Ord (NList a) where
  a <= b = toList a <= toList b
