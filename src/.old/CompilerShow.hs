{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  MultiParamTypeClasses #-}
module CompilerShow(CompilerShow(..), runShow,  embed, embedString, embedText,  asks) where

import Data.Text(Text, pack, unpack)
import qualified Data.Text as T

import qualified Data.DList as D
import Data.DList(DList)

import Control.Monad.Reader 

type SuspendedText = DList Text
type ContextShow context object = Reader context (object -> SuspendedText)

-- | Functions used for error format, debug format and synthesis format
class CompilerShow c a where
  -- | A nice way to watch the code
  prettyShow :: ContextShow c a
  prettyShow = reconstructShow
  -- | Must be full of annotations and easy parseable
  debugShow ::  ContextShow c a
  debugShow = reconstructShow
  -- | Must attempt to reconstruct the object on file
  reconstructShow::  ContextShow c a
  
runShow :: CompilerShow c a => ContextShow c a  -> c -> a -> Text
runShow f context object = T.concat $ D.toList $ runReader f context object 


embedText :: Text -> SuspendedText
embedText = D.singleton

embedString :: String -> SuspendedText
embedString = D.singleton . pack

embed :: Show a =>a -> SuspendedText
embed = embedString . show




