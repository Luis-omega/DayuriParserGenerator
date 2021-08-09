{-# LANGUAGE OverloadedStrings #-}
module RawGrammar where

import Prelude hiding(concat)
import Data.Text(Text, pack, unpack, concat)
import qualified Data.Text as T
import qualified Data.DList as D
import Data.DList(DList)

-- | Functions for ease of show
class CompilerShow a where
  -- | A nice way to watch the code
  prettyShow :: a -> DList Text
  prettyShow = reconstructShow
  -- | Must be full of annotations and easy parseable
  debugShow :: a -> DList Text
  debugShow = reconstructShow
  -- | Must attempt to reconstruct the object on file
  reconstructShow:: a -> DList Text
  
runShow :: CompilerShow a => (a-> DList Text) -> a -> Text
runShow f = concat . D.toList . f  



