module Translations(
  surface2Intern
)
where 

import qualified Data.Map as M
import Data.Text(Text)

import qualified Path as P
import NList(NList)
import qualified NList as N

import qualified Surface  as Su
import qualified Intern as I 

surface2Intern :: [Su.TopLevel] -> Either Su.Error I.Grammar
surface2Intern top = 
  do 
  cleanRules <- Su.cleanUseless rules terminals 
  pure $ iList2Grammar $ suRules2IRules cleanRules terminals
  where
    (terminals,rules) = Su.divideTop  top [] []

iList2Grammar :: [I.Rule] -> I.Grammar 
iList2Grammar rules = I.Grammar $ M.fromListWith (++) inter

  where 
    inter :: [(Text, [I.Rule])]
    inter = [(I.name x, [x]) | x <- rules]


suRules2IRules :: [Su.Rule]-> [Su.Terminal] -> [I.Rule]
suRules2IRules rules terminals = new_rules
  where 
  new_rules = map (\x ->suRule2IRule x terminals) rules

suRule2IRule :: Su.Rule -> [Su.Terminal] -> I.Rule
suRule2IRule rule terminals = 
  case suExp2NList (Su.body rule) terminals of 
    Nothing ->
      I.EmptyRule{
        I.range = Su.range rule
        ,I.name = P.toText $ Su.name rule
        }
    Just bd ->
      I.Rule{
        I.range = Su.range rule
        ,I.name = P.toText $ Su.name rule
        ,I.body = bd
        }

suExp2NList :: Su.Exp -> [Su.Terminal] -> Maybe (NList I.Kind)
suExp2NList exp terminals =  N.fromList $ convertIdentifiers exp
  where 
    convertIdentifiers :: Su.Exp -> [I.Kind]
    convertIdentifiers (Su.Concat r e) = concat $ N.toList $ fmap convertIdentifiers e
    convertIdentifiers (Su.Identifier _ e) = 
      if any ((e ==) . (\(Su.Terminal _ n)-> n)) terminals then
        [I.Terminal (P.toText e)]
      else 
        [I.NonTerminal (P.toText e)]
    convertIdentifiers Su.Empty =  []

suTerminal2Iterminal :: Su.Terminal  -> I.Kind
suTerminal2Iterminal (Su.Terminal _ p) = I.Terminal $ P.toText  p
