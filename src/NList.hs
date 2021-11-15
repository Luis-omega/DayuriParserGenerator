{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module NList where 

import Prelude hiding(head,tail)
data NList a = NList {head::a, tail::[a]}
  deriving (Functor, Foldable, Traversable, Eq)

(#:) = NList
infix 6  #:

toList :: NList a -> [a]
toList ls = head ls : tail ls

fromList :: [a] ->Maybe (NList a)
fromList [] = Nothing
fromList (x:xs) = Just $ x#:xs


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
