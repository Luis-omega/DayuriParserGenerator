{-# LANGUAGE OverloadedStrings, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
--Todo: Check Rule InlineFlag is consistent
-- Check that Macros won't capture already defined symbols
-- so macros won't redefine locally bound variables

module Surface where
import Data.Text(Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Either
import Data.Maybe
import qualified Data.List as L


import Path(Path)
import qualified Path as P
import Range
import qualified RealSet as Rs
import NList (NList)
import qualified NList as N



data InlineFlag = Inline
  | NoInline
  deriving (Eq, Show)

data Terminal = Terminal Range Path Text
  deriving (Eq, Show)


data TopLevel =
    DeclareTerminals Range [Terminal]
  | DeclareRule Rule
  deriving Eq

data Rule =
  Rule
    {
      inline::InlineFlag
      ,range::Range
      ,name::Path
      ,body::Exp
      ,function::Maybe Text
    }
  deriving Eq


data Exp = 
    Concat Range (NList Exp)
  | Identifier Range Path
  | Empty
  deriving Eq


instance Show Exp where 
  show ( Concat _ e ) = concatMap show e 
  show ( Identifier _ p ) = show p
  show Empty = "_empty"


instance Ord Rule where
  a <= b = show a <= show b

instance Show Rule where 
  show u = concat [show $ inline u, " ",show $ name u, " : ", show $ body u, end ]
    where
      end = 
        case function u of 
          Just t -> " -> "++ show t  
          Nothing -> " "

divideTop :: [TopLevel] -> [Terminal] -> [Rule] ->([Terminal],[Rule])
divideTop [] ts rs = (ts,rs) 
divideTop ((DeclareTerminals _ x):ls) ts rs = divideTop ls (x++ts) rs
divideTop ((DeclareRule x):ls) ts rs = divideTop ls ts (x:rs)


getTerminalRuleOverlap :: [Terminal] -> [Rule] -> [(Range,Range,Path)]
getTerminalRuleOverlap terminals rules = 
  concatMap localfind terminals
  where 
    localfind (Terminal r1 n _ )= 
      case L.find ((n==). name) rules of
        Nothing -> []
        Just rule -> [(r1, range rule, n)]

cleanMidleEmptyExp :: Exp -> Exp
cleanMidleEmptyExp (Concat r e) = 
   let maped = fmap cleanMidleEmptyExp  e
       filtered = N.filter (Empty /=) maped
       in 
       case filtered of 
         Nothing -> Empty
         Just x -> Concat r x
cleanMidleEmptyExp w = w 
      
-- | Transform x->a  _empty  b,  to x->a b 
-- | it won't clear x->empty rules
cleanMidleEmptyRule :: Rule ->Rule
cleanMidleEmptyRule r = r{body= cleanMidleEmptyExp $ body r}

cleanMidleEmpty :: [Rule] -> [Rule]
cleanMidleEmpty = map cleanMidleEmptyRule 

getIdentifiersExp :: Exp -> [(Range,Path)]
getIdentifiersExp (Concat _ e) = concatMap getIdentifiersExp e 
getIdentifiersExp (Identifier r e) = [(r,e)]
getIdentifiersExp Empty = []

getUsedIdentifiers :: [Rule] -> [(Range,Path)]
getUsedIdentifiers = concatMap (getIdentifiersExp . body)

getNonTerminals :: [Rule] -> [Path]
getNonTerminals = map name

getUndeclaredIdentifiers :: [TopLevel] -> [(Range,Path)]
getUndeclaredIdentifiers top = 
  [(r,x) | (Terminal r x _)<- terms, Rs.elem x diff2 ] ++ [(range rl, name rl) | rl<- rls, Rs.elem (name rl) diff2 ]
  where 
  (terms, rls)= divideTop top mempty mempty  
  terminals = map (\(Terminal r x _)->x) terms
  nonterminals = getNonTerminals rls
  
  all = [x | (_,x) <- getUsedIdentifiers rls]

  diff1 = Rs.difference (Rs.fromList all) (Rs.fromList terminals) 
  diff2 = Rs.difference diff1 (Rs.fromList nonterminals) 


-- |Provided [Rule] and [Terminal] are productives 
-- | find if Exp is a productive expression
isProductiveExp :: [Rule] -> [Terminal] ->Exp -> Bool
isProductiveExp _ _ Empty = True
isProductiveExp rules terminals (Identifier _ n) = inTerminal || inRules
  where 
    inTerminal = any (\(Terminal _ m _)->n==m) terminals
    inRules = any ((n ==) .name ) rules
isProductiveExp rules terminals (Concat _ e) = all (isProductiveExp rules terminals) e

isProductiveRule :: [Rule] -> [Terminal] ->Rule -> Bool
isProductiveRule rules terminals = isProductiveExp rules terminals . body 

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
isStartDeclared = any ( ("_start" ==) . P.toString . name)

data Error =
    UndefinedStart
  | UndefinedIdentifiers [(Range,Path)]
  | NonProductiveStart
  | StarIsOnlyEmpty
  | StarIsUnProductive
  | OverlapingDefinitions [(Range,Range,Path)]
  deriving Show



getExpReachables :: Exp -> Set Text
getExpReachables = S.fromList . map (\(x,y)-> P.toText y) . getIdentifiersExp

getRuleReachables :: Rule -> Set Text
getRuleReachables = getExpReachables . body

getReachables :: [Rule] -> Either Error [Rule]
getReachables rls = 
  case filter (("_start"==). P.toText . name) rls of 
    [] -> Left  UndefinedStart
    start -> Right $ loop start
  where 
    rules2set rules = S.fromList $ map (P.toText . name) rules 
    loop :: [Rule] -> [Rule]
    loop reachables = 
      let new_reachables_text = foldr ((<>) . getRuleReachables) (rules2set reachables) reachables
          new_reachables = filter (\ x -> S.member (P.toText $ name x) new_reachables_text) rls
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
    y-> Left $ OverlapingDefinitions y
  where 
    nonMidleEmpty = cleanMidleEmpty  rules

    productive = cleanNonProductive nonMidleEmpty terminals 


getFirst :: M.Map Text (S.Set Text) -> [Text] -> S.Set Text
getFirst firsts symbols= 
  case symbols of 
    [] -> S.singleton "_empty"
    x:xs -> 
      let xFirst = fromMaybe S.empty (M.lookup x firsts ) in
        if S.member "_empty" xFirst then
          case xs of 
            [] -> 
              S.insert "_empty" xFirst 
            _ -> S.union xFirst $ getFirst firsts xs
        else 
          xFirst
    
    
condenseRules :: [Rule] -> M.Map Text [Rule]
condenseRules rules = M.fromListWith (++) [(P.toText $ name r, [r]) | r<- rules ]
   
exp2TextList :: Exp -> [Text]
exp2TextList (Concat _ e) = concatMap exp2TextList e
exp2TextList (Identifier _ i) = [P.toText i]
exp2TextList Empty = ["_empty"]

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
   

    rules_names = M.fromList [(P.toText $ name r,S.empty)| r <- rules]
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
-- | in the derivation as in x -> a b c , a -> w | _empty
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
    

