{-# LANGUAGE OverloadedStrings #-}

module ParserCoreSpec (spec) where

import ParserCore as PC
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

padd =  Path ("add",[])
madd = MaybeTerminal padd
pmul = Path ("mul",[])
mmul = MaybeTerminal pmul
patom =  Path ("atom",[])
matom = MaybeTerminal patom
pint = Path ("int",[])
mint = MaybeTerminal pint
plpar = AnonTerminal "("
prpar = AnonTerminal ")"
pplus = AnonTerminal "+"
pstar = AnonTerminal "*"

mkpath x = Path (x,[])

parsedRules :: [TopLevel () SourfaceSymbol]
parsedRules = [
  Rule () NoInline padd ( Concat () $ map (Token ()) [madd, pplus, mmul]) Nothing
  ,Rule () NoInline padd (Token () mmul) Nothing
  ,Rule () NoInline pmul ( MacroCall () (mkpath "op_left") $ map (Token ()) [pstar, mmul, matom]) Nothing
  ,Rule () NoInline patom ( Concat () $ map (Token ()) [plpar, madd, prpar]) Nothing
  ,Rule () NoInline patom (Token () mint) Nothing
  ,Macro {
    macroInfo=()
    ,macroInline=NoInline 
    ,macroName=Path ("op_left",[])
    ,macroArgsCount=3
    ,macroArgs=[mkpath "op", mkpath "top_level", mkpath "lower_level" ] 
    ,macroBody=Concat () $ map (Token ()) 
      [
        MaybeTerminal $  mkpath "top_level"
        ,MaybeTerminal $ mkpath "op"
        ,MaybeTerminal $ mkpath "lower_level"
      ]
    ,macroFunction=Nothing
    }
  ,Macro {
    macroInfo=()
    ,macroInline=NoInline 
    ,macroName=Path ("op_left",[])
    ,macroArgsCount=3
    ,macroArgs=[mkpath "op", mkpath "top_level", mkpath "lower_level" ] 
    ,macroBody=Concat () $ map (Token ()) 
      [
        MaybeTerminal $  mkpath "top_level"
      ]
    ,macroFunction=Nothing
    }
  ]

 

terminalNames = S.fromList ["_Plus", "_Star", "_Lparen", "_Rparen", "int"]
nonTerminalNames = S.fromList ["add", "mul", "atom", "op_left"]


names = S.union terminalNames nonTerminalNames

replaceNonTerminals :: [TopLevel () SourfaceSymbol]
replaceNonTerminals = [
  Rule () NoInline padd 
    (
      Concat () $ map (Token ()) [MaybeNonTerminal padd, pplus, MaybeNonTerminal pmul]
    ) Nothing
  ,Rule () NoInline padd (Token () $ MaybeNonTerminal pmul) Nothing
  ,Rule () NoInline pmul 
    ( 
      MacroCall () (mkpath "op_left") $ map (Token ()) [pstar, MaybeNonTerminal pmul, MaybeNonTerminal patom]
    ) Nothing
  ,Rule () NoInline patom 
    (
      Concat () $ map (Token ()) [plpar, MaybeNonTerminal padd, prpar]
    ) Nothing
  ,Rule () NoInline patom (Token () mint) Nothing
  ,Macro {
    macroInfo=()
    ,macroInline=NoInline 
    ,macroName=Path ("op_left",[])
    ,macroArgsCount=3
    ,macroArgs=[mkpath "op", mkpath "top_level", mkpath "lower_level" ] 
    ,macroBody=Concat () $ map (Token ()) 
      [
        MaybeTerminal $  mkpath "top_level"
        ,MaybeTerminal $ mkpath "op"
        ,MaybeTerminal $ mkpath "lower_level"
      ]
    ,macroFunction=Nothing
    }
  ,Macro {
    macroInfo=()
    ,macroInline=NoInline 
    ,macroName=Path ("op_left",[])
    ,macroArgsCount=3
    ,macroArgs=[mkpath "op", mkpath "top_level", mkpath "lower_level" ] 
    ,macroBody=Concat () $ map (Token ()) 
      [
        MaybeTerminal $  mkpath "top_level"
      ]
    ,macroFunction=Nothing
    }
  ]

replaceMacroVars :: [TopLevel () SourfaceSymbol]
replaceMacroVars = [
  Rule () NoInline padd 
    (
      Concat () $ map (Token ()) [MaybeNonTerminal padd, pplus, MaybeNonTerminal pmul]
    ) Nothing
  ,Rule () NoInline padd (Token () $ MaybeNonTerminal pmul) Nothing
  ,Rule () NoInline pmul 
    ( 
      MacroCall () (mkpath "op_left") $ map (Token ()) [pstar, MaybeNonTerminal pmul, MaybeNonTerminal patom]
    ) Nothing
  ,Rule () NoInline patom 
    (
      Concat () $ map (Token ()) [plpar, MaybeNonTerminal padd, prpar]
    ) Nothing
  ,Rule () NoInline patom (Token () mint) Nothing
  ,Macro {
    macroInfo=()
    ,macroInline=NoInline 
    ,macroName=Path ("op_left",[])
    ,macroArgsCount=3
    ,macroArgs=[mkpath "op", mkpath "top_level", mkpath "lower_level" ] 
    ,macroBody=Concat () $ map (Token ()) 
      [
        MacroVar $  mkpath "top_level"
        ,MacroVar $ mkpath "op"
        ,MacroVar $ mkpath "lower_level"
      ]
    ,macroFunction=Nothing
    }
  ,Macro {
    macroInfo=()
    ,macroInline=NoInline 
    ,macroName=Path ("op_left",[])
    ,macroArgsCount=3
    ,macroArgs=[mkpath "op", mkpath "top_level", mkpath "lower_level" ] 
    ,macroBody=Concat () $ map (Token ()) 
      [
        MacroVar $  mkpath "top_level"
      ]
    ,macroFunction=Nothing
    }
  ]

(#=)::(HasCallStack, Show a, Eq a) => a -> a -> Expectation
(#=) = shouldBe

infix 6 #= 

checkNames = do
  describe "Names" $ do
    it "Extract all identifiers and symbols from grammar" $
      getAllNames parsedRules #= names

    it "Got all names at left of Rule or Macros definition" $
      findNonTerminalNames parsedRules #= nonTerminalNames

    it "Set NonTerminals as such" $
      sourfaceSetNonTerminals terminalNames parsedRules #= Right replaceNonTerminals 

    it "Subtitution of MaybeTerminal for MacroVar in Macros body" $
      sourfaceSetMacroVars replaceNonTerminals #= replaceMacroVars

    it "Check all definitions of same macros has same number of args and check all calls" $
      checkMacros replaceMacroVars #= Right () 

spec = checkNames

