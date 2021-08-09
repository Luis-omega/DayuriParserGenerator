{-# LANGUAGE OverloadedStrings #-}
module RawGrammar where

import Data.Text(Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.DList as D
import Data.DList(DList)


class CompilerShow a where
  prettyShow :: a -> DList a
  debugShow :: a -> DList a
  reconstructShow:: a -> DList a
  runShow :: DList a -> a

