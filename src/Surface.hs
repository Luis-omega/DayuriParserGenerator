{-# LANGUAGE OverloadedStrings, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- | Surface grammar is the result that parser give us form text files.
--
-- Definitions for grammar :
--
-- * Identifier is asccii letter followed by 
--   none or more of ascii letter, ascii number, or "_".
-- * A /symbol/ or /identifier/ is a 'Terminal' or 'NonTerminal'.
-- * /_start/ is the name of the grammar starting rule. 
-- * A rule is of the form 
--
--    >  ruleName : symbol1 symbol2 ... symbolN -> functionName
-- * A substitution for symbol __X__ inside 
--    > a b c __X__ d e f
--  is the change of __X__ by the body of some rule 
--    > __X__ : body
--  ending as 
--    > a b c body d e f
-- * A symbol __X__ is reachable if exists a sequence of substitutions 
--   beginning at /_start/ that 
--   ends with __X__  as one of the symbols that could be substituted.
-- * A productive symbol is one such that exists some 
--   sequence of substitutions whose all remaining symbols are 
--   terminals.
-- * A non terminal __X__ has a /loop/ if after a series of substitutions 
--   that begin in __X__ we end in the form 
--
--      > X symbol2 ... symbolN 
--
-- We have a series of compromises that grammars must comply 
--   to be acceptable:
--
--   * Grammar has a /_start/ rule.
--   * /_start/ is productive
--   * /_start/ isn't equivalent to 
--
--     > _start : _empty 
--   * All terminals are declared, have the same type and
--     every declaration have associated a test function.
--   * All reachable symbols are declared tokens or 
--     defined rules but not both.
--   * Every reachable symbols from "_star" is productive
--   * Reachable non terminals don't contains loops 
module Surface where
import Data.Text(Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Either
import Data.Maybe
import qualified Data.List as L


import Range
import qualified EqSet as Es



type TerminalName = Text
type TerminalTestFunction = Text

data Terminal = 
  Terminal  -- ^ > Name = testFunction
    Range -- ^ begins in letter /N/  and ends in /n/.
    TerminalName -- ^ is just  /Name/.
    TerminalTestFunction -- ^ is just /testFunction/.
  deriving (Eq, Show)

-- | Currently we only support two top level operations.
data TopLevel =
  DeclareTerminal -- ^  > Name=testFunction  
    Terminal
  | DeclareRule  -- ^ > ruleName : ruleBody -> functionName
      Rule
  deriving Eq


data Rule =
  Rule -- ^ > ruleName : ruleBody -> functionName
    {
      range::Range -- ^ Begins at /ruleName/ start and ends at /functionName/ end.
      ,name::Text -- ^ /ruleName/.
      ,body::Exp -- ^ /ruleBody/.
      ,function::Text -- ^ /functionName/.
    }
  deriving Eq

data Error =
    UndefinedStart -- ^ There isn't definitions of the form :
                   -- 
                   -- > _start : something
  | UndefinedIdentifiers -- ^ Reachable identifiers that aren't 
                         -- defined as rules or declared as 
                         -- terminals.
    [(Range,Text)]
  | StarIsOnlyEmpty   -- ^ After clean of grammar, /_start/ rules
                      -- are all of the form :
                      --
                      -- > _start : _empty.
  | StarIsUnProductive -- ^ Some reachable symbol is non productive.
  | OverlapingTerminalRule  -- ^ Identifiers that are declared as 
                            -- terminals and then defined as 
                            -- nonterminal. 
    [(Range,Range,Text)] -- ^ terminal declaration, nonterminal declaration.
  | OverlapingTerminals -- ^ A Terminal that is declared more than one 
                        -- time and with different test functions.
      [(Range,Range,Text)]
  deriving Show

-- | Data abstraction for body of rules. 
data Exp = 
  Concat -- ^ > exp1 exp2
    Range -- ^ Begins at exp1 being and ends at exp2 end.
    Exp -- ^ /exp1/. 
    Exp -- ^ /exp2/.
  | Identifier -- ^ > someId
      Range -- ^ Begin at /s/ end at /d/
      Text -- ^ /someId/ can be two things :
           --
           -- * Ascii letter followed by  any of ascii letters,numbers or "_"
           --
           -- * Keyword  /_start/
  | Empty -- ^ Keyword /_empty/ for the empty word 
  deriving Eq

-- | It used to define 'Ord' over 'Rule' not to pretty printing 
instance Show Exp where 
  show ( Concat _ el er) = show el ++ " " ++ show er
  show ( Identifier _ p ) = show p
  show Empty = "_Empty"

-- | Implemented just to be able to form a 'Set'.
--  Rules with same name, body and function are the same
--  independently of range.
instance Ord Rule where
  a <= b = show a <= show b

instance Show Rule where 
  show u = unwords [rname, ":", rbody, "->", rfun ]
    where
      rname = show $ name u
      rbody = show $ body u
      rfun =  show $ function u

-- | Split toplevel definitions in two list, one of 'Terminal' and
-- one of 'Rule'.
divideTop :: 
  [TopLevel]  -- ^ Intent to be all toplevel declarations
  -> [Terminal] -- ^ Must be /[]/ 
  -> [Rule]  -- ^ Must be /[]/
  ->([Terminal],[Rule])
divideTop [] ts rs = (ts,rs) 
divideTop ((DeclareTerminal x):ls) ts rs = divideTop ls (x:ts) rs
divideTop ((DeclareRule x):ls) ts rs = divideTop ls ts (x:rs)

-- | Find all declared terminals that are also defined as nonTerminals. 
getTerminalRuleOverlap :: [Terminal] -> [Rule] -> [(Range,Range,Text)]
getTerminalRuleOverlap terminals rules = 
  concatMap localfind terminals
  where 
    localfind (Terminal r1 n _ )= 
      case L.find ((n==). name) rules of
        Nothing -> []
        Just rule -> [(r1, range rule, n)]


-- | If terminals that are declared more than one time with 
-- | different functions.
getOverlapTerminals :: [Terminal] -> [(Range,Range,Text)]
getOverlapTerminals terminals = concatMap search terminals
  where 
    search (Terminal r n e) = [(r,r1,n) | (Terminal r1 n1 e1)<-terminals, n1==n, e/=e1]


cleanMidleEmptyExp :: 
  Exp -- ^ Expression with maybe some /_empty/ between 
      -- terminals and non terminals.
  -> Exp -- ^ Is /_empty/ if and only
         -- if original 'Exp' is equivalent to /_empty/.
         -- Otherwise it won't contain a /_empty/.
cleanMidleEmptyExp (Concat r el er) = 
   let cleanl = cleanMidleEmptyExp el
       cleanr = cleanMidleEmptyExp er
       in 
       if cleanl==Empty then
          if cleanr==Empty then
            Empty
          else 
            cleanr
       else
          if cleanr ==Empty then
            cleanl
          else 
            Concat r cleanl cleanr
cleanMidleEmptyExp w = w 
      
cleanMidleEmptyRule :: 
  Rule -- ^ > x : a b _empty c d e...
  ->Rule -- ^ A rule that have all /_empty/ between symbols
         -- striped.
         --
         -- > x: a b c d e..
cleanMidleEmptyRule r = r{body= cleanMidleEmptyExp $ body r}

-- | Apply 'cleanMidleEmptyRule' to list. 
cleanMidleEmpty :: [Rule] -> [Rule]
cleanMidleEmpty = map cleanMidleEmptyRule 



getIdentifiersExp :: Exp -> [(Range,Text)]
getIdentifiersExp (Concat _ el er) = getIdentifiersExp el ++ getIdentifiersExp er 
getIdentifiersExp (Identifier r e) = [(r,e)]
getIdentifiersExp Empty = []

getUsedIdentifiers :: [Rule] -> [(Range,Text)]
getUsedIdentifiers = concatMap (getIdentifiersExp . body)

getDefinedNonTerminals :: [Rule] -> [Text]
getDefinedNonTerminals = map name

getUndeclaredUndefinedIdentifiers :: [Terminal]->[Rule] -> [(Range,Text)]
getUndeclaredUndefinedIdentifiers terminals rules= 
  [(r,x) | (Terminal r x _)<- terminals, Es.elem x diff2 ] ++ [(range rl, name rl) | rl<- rules, Es.elem (name rl) diff2 ]
  where 
  tnames = map (\(Terminal _ x _)->x) terminals
  ntnames = getDefinedNonTerminals rules
  
  allUsed = [x | (_,x) <- getUsedIdentifiers rules]

  diff1 = Es.difference (Es.fromList allUsed) (Es.fromList tnames) 
  diff2 = Es.difference diff1 (Es.fromList ntnames) 


-- |Provided [Rule] and [Terminal] are productives 
-- | find if Exp is a productive expression
isProductiveExp :: [Rule] -> [Terminal] ->Exp -> Bool
isProductiveExp _ _ Empty = True
isProductiveExp rules terminals (Identifier _ n) = inTerminal || inRules
  where 
    inTerminal = any (\(Terminal _ m _)->n==m) terminals
    inRules = any ((n ==) .name ) rules
isProductiveExp rules terminals (Concat _ er el) = 
  isProductiveExp rules terminals er &&
    isProductiveExp rules terminals el

-- |Provided [Rule] and [Terminal] are productives 
-- | find if Rule is a productive rule
isProductiveRule :: [Rule] -> [Terminal] ->Rule -> Bool
isProductiveRule rules terminals = isProductiveExp rules terminals . body 

-- |  
divideProductiveNonProductive :: [Rule] -> [Terminal] -> ([Rule],[Rule])
divideProductiveNonProductive rules terminals =  loop mempty terminals 
  where 
  loop alreadyProductive terminals =
    let newProductive = filter (isProductiveRule alreadyProductive terminals) rules  in
      if newProductive == alreadyProductive then
        let nonProducitive = [x | x<-rules, notElem x newProductive] in
          (newProductive, nonProducitive)
      else
        loop newProductive terminals

cleanNonProductive :: [Rule] -> [Terminal] -> [Rule]
cleanNonProductive rls = fst . divideProductiveNonProductive rls

isStartDeclared :: [Rule] -> Bool
isStartDeclared = any ( ("_start" ==) . name)



getExpReachables :: Exp -> Set Text
getExpReachables = S.fromList . map snd . getIdentifiersExp

getRuleReachables :: Rule -> Set Text
getRuleReachables = getExpReachables . body

getReachables :: [Rule] -> Either Error [Rule]
getReachables rls = 
  case filter (("_start"==) . name) rls of 
    [] -> Left  UndefinedStart
    start -> Right $ loop start
  where 
    rules2set rules = S.fromList $ map name rules 
    loop :: [Rule] -> [Rule]
    loop reachables = 
      let new_reachables_text = foldr ((<>) . getRuleReachables) (rules2set reachables) reachables
          new_reachables = filter (\ x -> S.member (name x) new_reachables_text) rls
          in
          if new_reachables == reachables then
            reachables
          else 
            loop new_reachables 
              

      


-- | Quit all unproductive and then all unreachables
-- | in that order is guarantied that no more clean steps 
-- | of those types are needed.
-- | Also Unproductive clean removes undefined items
-- | but you could still want to call
-- | getUndeclaredIdentifiers to know if some UnProductive 
-- | was already undeclared (there's no difference 
-- | between undefined and undeclared)
cleanUseless :: [Rule] -> [Terminal] -> Either Error [Rule]
cleanUseless rules terminals = 
  case getTerminalRuleOverlap terminals rules of 
    [] ->case getOverlapTerminals terminals of 
      [] ->
        if isStartDeclared rules then
          if isStartDeclared nonMidleEmpty then
            if isStartDeclared productive then
              getReachables productive
            else
              Left StarIsUnProductive
          else 
            Left StarIsOnlyEmpty 
        else 
          Left UndefinedStart
      y-> Left $ OverlapingTerminals y
    y-> Left $ OverlapingTerminalRule y
  where 
    nonMidleEmpty = cleanMidleEmpty  rules

    productive = cleanNonProductive nonMidleEmpty terminals 


getFirst :: M.Map Text (S.Set Text) -> [Text] -> S.Set Text
getFirst firsts symbols= 
  case symbols of 
    [] -> S.singleton "_Empty"
    x:xs -> 
      let xFirst = fromMaybe S.empty (M.lookup x firsts ) in
        if S.member "_Empty" xFirst then
          case xs of 
            [] -> 
              S.insert "_Empty" xFirst 
            _ -> S.union xFirst $ getFirst firsts xs
        else 
          xFirst
    
-- | Group all rules that have the same name 
condenseRules :: [Rule] -> M.Map Text [Rule]
condenseRules rules = M.fromListWith (++) [(name r, [r]) | r<- rules ]
   
-- | Get all symbols used by 'Exp'  
exp2TextList :: Exp -> [Text]
exp2TextList (Concat _ el er) = exp2TextList el ++ exp2TextList er
exp2TextList (Identifier _ i) = [i]
exp2TextList Empty = ["_Empty"]

rule2TextList :: Rule -> [Text]
rule2TextList = exp2TextList . body 


findFirst :: [Rule] -> S.Set Text -> M.Map Text (S.Set Text)
findFirst rules terminals = loop start
  where 
    loop :: M.Map Text (S.Set Text) -> M.Map Text (S.Set Text)
    loop current =  
      let newFirst = M.mapWithKey (\k a ->getRuleFirst current k) rules_names in
        if current == M.union newFirst terminal_names then
          current
        else 
          loop newFirst
   

    rules_names = M.fromList [(name r,S.empty)| r <- rules]
    terminal_names = M.fromList [(x,S.singleton x)| x<- S.toList terminals ]

    start = M.union rules_names terminal_names

    condensed :: M.Map Text [Rule]
    condensed = condenseRules rules

    getRuleFirst :: M.Map Text (S.Set Text) -> Text -> S.Set Text
    getRuleFirst firstSets rule = 
      let relatedRules = condensed M.! rule
          relatedText = map rule2TextList relatedRules in
            foldr (S.union . getFirst firstSets) mempty relatedText

-- | TODO : 
-- | Fix This, you found the FIRST terminals set 
-- | you need the nonterminals reachables as first 
-- | in the derivation as in x -> a b c , a -> w | _Empty
-- | the you got FIRST_NonTerminals x = {a, w, b}
hasLoops :: [Rule] -> S.Set Text -> Either [(Text,S.Set Text)] ()
hasLoops rules terminals =  
  case withLoops of 
    [] -> Right ()
    _ -> Left withLoops
  where
    firstSets = M.toList $ findFirst rules terminals 
    withLoops = filter (uncurry elem) firstSets


findCycle :: [Rule] -> M.Map Text (Set Text) -> Either () [Text]
findCycle rules firstSets = Left ()
    

