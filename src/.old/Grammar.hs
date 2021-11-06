{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Grammar where

import Data.Text(Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.List

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import qualified Data.Set as S


import CompilerShow

import Control.Monad.Reader
import qualified Data.DList as D

newtype Defined = Defined Int
  deriving(Show)
data Symbol = Terminal Int
              | NonTerminal Int
              | Start 
              | EOF
              | LexerError
              | Empty
              deriving(Eq, Ord)

symbol2int (NonTerminal x) = x
symbol2int (Terminal x) = x
symbol2int Start = -1
symbol2int EOF = -2
symbol2int LexerError = -3
symbol2int Empty = -4

isTerminal :: Symbol -> Bool
isTerminal (NonTerminal _) = False
isTerminal LexerError = False
isTerminal _ = True

isNonterminal (NonTerminal _ ) = True
isNonterminal _ = False

data Rule = Rule Defined [Symbol]

--instance Show Rule where
--  show (Rule _ ls) = concat $ Data.List.intersperse " " $ map  (show)  ls

show_rule (Rule _ ls) names = concat $ Data.List.intersperse " " $ map  (\ x -> show (names M.! (unsafe_symbol2int x)) )  ls


get_rule (Rule _ x) = x
-- | There must be a rule with Start Head and end EOF :  S' -> S EOF
data Grammar = Grammar {rules:: M.IntMap [Rule], names:: M.IntMap Text}

instance Show Grammar where
  show (Grammar rls names) = M.foldrWithKey f " " rls
    where f key rules acc = "\n" ++ show (names M.! key) ++ " : \n    "  ++ show_rules rules ++  "\n" ++ acc
          show_rules rls = concatMap (\ x -> "| " ++ show_rule x names ++ "\n    ") rls


data Item = Item [Rule] [Rule] 
newtype Items = Items { items:: M.IntMap [Item]} 


type GrammarT a = Reader Grammar a


findName :: GrammarT (Int -> Maybe Text)
findName = do
  gnames <- asks names 
  return (gnames M.!? )


instance CompilerShow Grammar Symbol where
  reconstructShow  = do  
      maybeGetName <- findName
      return $ \x -> case x of 
                       Start -> embed "_Start_"
                       EOF -> embed "_EOF_"
                       LexerError -> embed "_LexerError_"
                       Empty -> embed "_Empty_"
                       _ -> case maybeGetName $ symbol2int x of
                              Just x ->  embedText x
                              _ -> embedString $ unpack "Name_not_found_" ++ show ( symbol2int x)
              
          
instance CompilerShow Grammar Rule where
  reconstructShow = do
    maybeGetName <- findName
    return $ \ x -> 



first :: Grammar -> M.IntMap (S.Set Symbol)
first (Grammar rules _) = loop rules loop_begin_state
  where loop_begin_state = M.map add_rules_first rules  
        add_rules_first :: [Rule] -> S.Set Symbol
        add_rules_first rls =
          let list_of_list_of_symbols =  map get_rule rls in 
              let list_of_list_of_terminals = filter (is_terminal . head) list_of_list_of_symbols in 
                  let list_of_terminals = concat list_of_list_of_terminals in
                      S.fromList list_of_terminals

        loop :: M.IntMap [Rule] -> M.IntMap  (S.Set Symbol) -> M.IntMap (S.Set Symbol)
        loop rules old_first = 
          let new_first = M.map (add_rules_old_firsts old_first) rules in
            if new_first == old_first then new_first else loop rules new_first

        add_rules_old_firsts :: M.IntMap (S.Set Symbol) -> [Rule] -> S.Set Symbol
        add_rules_old_firsts  old_first rls =
          let list_of_list_of_symbols = map get_rule rls in 
            let list_of_list_of_nonterminals = map (\ x -> [head x]) $ filter (is_nonterminal . head ) list_of_list_of_symbols in 
              let list_of_nonterminals = concat list_of_list_of_nonterminals in
                let list_of_sets_of_first = map ( \ x -> old_first M.! nonterminal2int x) list_of_nonterminals in
                  S.unions list_of_sets_of_first

show_first :: Grammar -> M.IntMap (S.Set Symbol) -> String
show_first (Grammar rules names) first = M.foldrWithKey show_symbol "" first
  where 
    show_symbol :: Int -> S.Set Symbol -> String -> String
    show_symbol key symbols acc = acc ++ "\n\n" ++show (names M.! key) ++ " : " ++ Data.List.intercalate "," ( map (\ x -> show (names M.! unsafe_symbol2int x)) (S.toList symbols) )
        

expr = NonTerminal 1
term = NonTerminal 2
inte = Terminal 3
lpar = Terminal 4
rpar = Terminal 5
plus = Terminal 6
defined = Defined 0

names_list = [(1,"expression"),(2,"term"),(3,"integer"),(4,"("),(5,")"),(6, "+")]
grammar_names = M.fromList names_list

expr_rule = Rule defined [expr, plus, term]
expr_rule2 = Rule defined [term]
term_rule = Rule defined [inte]
term_rule2 = Rule defined [lpar, expr, rpar]

grammar_list = [(1, [expr_rule, expr_rule2]), (2, [term_rule, term_rule2])]
grammar_map = M.fromList grammar_list
grammar = Grammar grammar_map grammar_names

test = putStrLn $ show_first grammar $ first grammar
