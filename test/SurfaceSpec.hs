{-# LANGUAGE OverloadedStrings #-}

module SurfaceSpec (spec) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.IntMap as Mi
import qualified Data.Map as M
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set)


import Test.Hspec
import Test.Hspec.Expectations

import Surface
import Range
import qualified NList as N
import qualified RealSet as Rs
import qualified Path as P

r = Range 0 0 0 0 0 0

mkr :: Text -> Exp -> Rule
mkr n b= Rule Inline r name b Nothing
  where 
    name = 
      case P.fromText n of 
        Left _ -> N.singleton "" 
        Right x -> x

(#) = mkr
infix 6 #

start = "_start"
plus = "_Plus"
add = "add"
lparen = "_LParen"
rparen = "_RParen"
int = "Int"
atom = "atom"

ide :: Text -> Exp
ide x = 
  case P.fromText x of 
    Right y -> Identifier r y
    Left _ -> Empty

cat x = 
      case N.fromList x of 
        Just y -> Concat r y
        Nothing -> Empty

term x =  
  case P.fromText x of 
    Right y -> Terminal r y ""
    Left _ -> Terminal r (N.singleton "") ""


(#=)::(HasCallStack, Show a, Eq a) => a -> a -> Expectation
(#=) = shouldBe

infix 6 #= 

rules1 = [
    "usseless1" # cat [ide "usseless1", Empty]
    ,start # cat [Empty, ide "usseless1"]
  ]
rules2 = [
    "usseless2" # cat [cat [Empty, Empty], Empty]
    ,start # ide add
    ,add # cat [ide add, ide plus, Empty, ide atom]
    ,add # ide atom
    ,atom # ide int
    ,atom # cat [ide lparen, Empty, ide add , ide rparen]
  ]

noemptyRules = [
    "usseless1" # cat [ide "usseless1"]
    ,start # cat [ ide "usseless1"]
    ,"usseless2" # Empty
    ,start # ide add
    ,add # cat [ide add, ide plus, ide atom]
    ,add # ide atom
    ,atom # ide int
    ,atom # cat [ide lparen, ide add , ide rparen]
  ]

cleanRules = [
    start # ide add
    ,add # cat [ide add, ide plus, ide atom]
    ,add # ide atom
    ,atom # ide int
    ,atom # cat [ide lparen, ide add , ide rparen]
  ]


rules = rules1 ++ rules2

terminals1 = [
  term int
  ,term lparen
  ]
terminals2 = [ 
  term rparen
  ,term plus
  ]

terminals = terminals1 ++ terminals2

top = DeclareTerminals r terminals1 : map DeclareRule rules1
  ++ [DeclareTerminals r  terminals2] ++ map DeclareRule rules2



spec = do
  describe "Check the Separation Process" $
      let (new_terminals,new_rules) = divideTop top [] [] in  
        do
        it  "Separated Terminals" $
          shouldMatchList new_terminals terminals
        it "Separated Rules" $ 
          shouldMatchList new_rules rules
  describe "Clean process" $ do 
    it "clean intermidle empty" $ 
      shouldMatchList (cleanMidleEmpty rules) noemptyRules
    it "clean nonProductive" $
      shouldMatchList (cleanNonProductive noemptyRules terminals) (cleanMidleEmpty rules2)
    it "clean" $ 
      case cleanUseless rules terminals of 
        Right x -> shouldMatchList x cleanRules
        Left y -> show y #= ""
        
          
