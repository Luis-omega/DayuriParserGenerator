{-# LANGUAGE OverloadedStrings #-}
module RawGrammar where

import Data.Text(Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.DList as D
import Data.DList(DList)

-- | Functions for ease of show
class CompilerShow a where
  -- | A nice way to watch the code
  prettyShow :: a -> DList a
  -- | Must be full of annotations and easy parseable
  debugShow :: a -> DList a
  -- | Must attempt to reconstruct the object on file
  reconstructShow:: a -> DList a
  -- | in general don't needed but it could be used to add custom things at render
  runShow :: DList a -> a



