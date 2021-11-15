{-# LANGUAGE OverloadedStrings #-}
module Path(
  Path
  ,toList 
  ,toText
  ,toString
  ,fromText
  ,fromString
)where 

import Data.Text(Text, pack, unpack)
import qualified Data.Text as T

import NList hiding(head,tail)

type Path = NList Text


data PathError = 
  EmptyPathError

toText :: Path -> Text 
toText = T.intercalate "." . toList

toString :: Path -> String
toString = unpack . toText 


fromText :: Text -> Either PathError Path
fromText t =
  if T.null t then
    Left EmptyPathError
  else
    let split = T.splitOn "." t in
      Right $ NList (head split) (tail split)

fromString :: String -> Either PathError Path
fromString = fromText . pack
