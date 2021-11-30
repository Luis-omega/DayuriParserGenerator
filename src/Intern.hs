{-# LANGUAGE OverloadedStrings #-}

module Intern where
import Prelude hiding (head,tail)
import Data.List (intercalate)
import Data.Text(Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.IntMap as Mi
import Data.Foldable hiding (toList)
import Data.Maybe
import Data.Either

import Path(Path)
import qualified Path as P
import Range
import NList
import qualified RealSet as Rs 

import Debug.Trace (trace)

data Kind = Terminal Text
  | NonTerminal Text
  deriving (Eq, Show)

kind2Text (Terminal t) = t
kind2Text (NonTerminal t) = t

instance Ord Kind where 
  i <= j = show i <= show j

-- | Is  assumed that no rule would be of form
-- | A -> B _Empty C
-- | That means parser must convert those rules to equivalent
-- | A -> B C
data Rule = Rule {range::Range, name::Text, body::NList Kind, function :: Text}
  | EmptyRule {range::Range, name :: Text, function::Text}
  deriving Eq

instance Show Rule where
  show r@Rule{} = show (name r ) ++ " : " ++ unwords ( map (unpack . kind2Text) (toList $ body r) )
  show r@EmptyRule{} = show (name r ) ++ " : _Empty"

newtype Grammar = Grammar (M.Map Text [Rule])

data Item = Item {rule::Rule, position::Int, lookAhead::S.Set Text}
  deriving Eq

instance Ord Item where 
  i1 <= i2 =  show i1 <= show i2

instance Show Item where
  show i = 
    case rule i of 
      r@Rule{} -> 
        let (head,tail) = splitAt (position i) (toList . body $ rule i) in
          unpack (name r)++ " : " ++ unwords (map (unpack . kind2Text) head) ++" • "++ unwords (map (unpack . kind2Text) tail) ++ " , { " ++ show ( lookAhead i) ++ " }\n"
      r@EmptyRule{} ->
        unpack (name r)++" " ++ show (position i) ++ " : _Empty • "++ " , { " ++ show ( lookAhead i) ++ " }\n"

newtype State = State {sitem::Set Item}
  deriving (Eq)

instance Show State where
  show (State set) = 
    "State : \n" ++ foldr (\ x y -> y ++ ((++ "    \n") . show) x ) "" set

newtype Shiftables = Shiftables {shiftables::M.Map Kind (Set Item)}
  deriving Eq
newtype Reducibles = Reducibles {reducibles::Set Item}
  deriving Eq


getRuleFirst :: Rule -> Maybe Kind
getRuleFirst (Rule _ _ xs _) = Just (head xs)
getRuleFirst (EmptyRule _ _ _) = Nothing



kind2Terminal :: Kind -> Maybe Text
kind2Terminal (Terminal t) = Just t
kind2Terminal _ = Nothing

rules2firstTerminal :: [Rule] -> [Text]
rules2firstTerminal rs = 
    [x | (Just x)<- map rule2Terminal rs]
  where 
  rule2Terminal (Rule _ _ xs _) = kind2Terminal (head xs)
  rule2Terminal (EmptyRule _ _ _) = Nothing 


getFirst :: M.Map Text (S.Set Text) -> [Kind]-> S.Set Text
getFirst firstMap [] = S.singleton "_Empty"
getFirst firstMap (x:xs) = 
 case x of 
   NonTerminal t -> 
     let current_new_first = firstMap M.! t  in
       if S.member "_Empty" current_new_first then
         S.union (S.delete "_Empty" current_new_first ) (getFirst firstMap xs)
       else 
         current_new_first 
   Terminal t -> S.singleton t

constructFirst :: Grammar -> M.Map Text (S.Set Text)
constructFirst (Grammar grammar) = loop start
  where 
    start :: M.Map Text (S.Set Text)
    start = M.map (S.fromList . rules2firstTerminal) grammar

    rule_step :: M.Map Text (S.Set Text) -> Rule -> Maybe (S.Set Text)
    rule_step provisionaFirst rule@Rule{} = 
      let 
          newFirst = getFirst provisionaFirst (toList $ body rule)
          currentFirst = provisionaFirst M.!  name rule in
            return $ S.union currentFirst newFirst
    rule_step provisionaFirst rule@EmptyRule{} = 
      return $ S.union (provisionaFirst M.! name rule) $ S.singleton "_Empty"

    production_step :: M.Map Text (S.Set Text) -> [Rule] -> S.Set Text
    production_step provisionaFirst rules = 
          foldl S.union S.empty [x | Just x <- map (rule_step provisionaFirst) rules]
    
    loop :: M.Map Text (S.Set Text) -> M.Map Text (S.Set Text)
    loop provisionaFirst = 
      let new = M.map (production_step provisionaFirst) grammar in
        if new == provisionaFirst then
          provisionaFirst
        else 
          loop new
            
isItemAtEnd :: Item -> Bool
isItemAtEnd item = 
  case rule item of 
    r@Rule{} -> length ((toList . body) r) == position item 
    r@EmptyRule{} -> True

splitItem :: Item -> ([Kind],[Kind])
splitItem item =
  case rule item of 
    r@Rule{} -> splitAt (position item) (toList . body $ rule item)
    r@EmptyRule{} -> ([],[])

getItemHead :: Item -> [Kind]
getItemHead item = snd $ splitItem  item

advanceItem :: Item -> M.Map Text (S.Set Text)-> Maybe Item
advanceItem item first = 
  case getItemHead item of
    [] -> Nothing
    _:_ ->
          Just item{
            position = 1 + position item
            }
          
    

clousure :: M.Map Text (Set Text) -> Grammar -> State -> State
clousure first (Grammar grammar) items = unify $ loop items
  where 
    loop items = 
      let new_items = State $ S.union (sitem items) $ sitem $ fromSet items in
        if new_items == items then
          items
        else 
          loop new_items

    fromSet :: State -> State
    fromSet items = State $ foldl S.union S.empty $ map (sitem . fromItem) (S.toList $ sitem items)

    fromItem :: Item -> State
    fromItem item@(Item r p look) = 
      case  getItemHead item of
        [] -> State S.empty
        [NonTerminal h] -> State $ S.fromList $ map (buildItem look) (grammar M.!  h) 
        [Terminal h] -> State  S.empty
        (NonTerminal h):hs -> 
          let new_look=  getFirst first hs in
            if S.member "_Empty" new_look then
              State $ S.fromList $ map (buildItem (S.union new_look look)) (grammar M.! h)
            else 
              State $ S.fromList $ map (buildItem new_look) (grammar M.! h)
        (Terminal h):hs -> State  S.empty
        

buildItem ::  Set Text -> Rule -> Item
buildItem terminals r@Rule{} = Item r 0 terminals 
buildItem terminals r@EmptyRule{} = Item r 1 terminals 

unifyOne :: State -> Item -> State
unifyOne items item = State $  S.singleton item{lookAhead=combined}
  where
    test item2 = (name . rule) item == name (rule item2)  && (position item == position item2)
    related = S.filter test (sitem items)
    combined = S.foldr (S.union . lookAhead) S.empty related

unify :: State -> State
unify items =
  let listItems = S.toList (sitem items) in
    State $ S.unions $ map (sitem . unifyOne items) listItems
      



itemTransition :: M.Map Text (Set Text) -> Item -> Maybe (Kind, Item)
itemTransition first item = 
  case getItemHead item of
    [] -> Nothing
    x:item_head ->
      let head_first = getFirst first item_head in 
        if S.member "_Empty" head_first then
          Just (x,item{
            position = 1 + position item
            ,lookAhead = 
                S.union 
                  (S.difference head_first (S.singleton "_Empty")) 
                  (lookAhead item)
            })
        else 
          Just (x,item{
            position = 1 + position item
            ,lookAhead = head_first
            })

divideItems :: State -> (Shiftables, Reducibles)
divideItems (State items) = foldl separateStep start (S.toList items)
  where 
    start =  (Shiftables M.empty, Reducibles S.empty)

    separateStep :: (Shiftables, Reducibles) -> Item -> (Shiftables, Reducibles)
    separateStep (Shiftables s, Reducibles r) item =
      case getItemHead item of 
        [] -> (Shiftables s, Reducibles $ S.insert item r)
        k:ks -> (Shiftables $ M.insertWith S.union k (S.singleton item) s, Reducibles r)


handleStateShifts :: M.Map Text (Set Text) -> Grammar -> Rs.RealSet State -> Shiftables -> Rs.RealSet State
handleStateShifts firstSets grammar currentStates (Shiftables shiftables) = 
  currentStates <> orderedNewSets
  where 
    completeSet :: State -> State
    completeSet = clousure firstSets grammar 

    advanceSet :: Set Item -> Set Item
    advanceSet = S.map  (\x -> fromJust $ advanceItem x  firstSets) 

    completed = M.map (completeSet . State . advanceSet ) shiftables

    orderedNewSets = foldr Rs.append currentStates completed
    
  

findAllStates :: M.Map Text (Set Text) -> Grammar -> State -> Rs.RealSet State
findAllStates firstSets grammar start = loop (Rs.singleton start) $ Rs.singleton start
  where 
  loop ::  Rs.RealSet State -> Rs.RealSet State -> Rs.RealSet State
  loop currentStates unHandledStates = 
    let divided = fmap divideItems unHandledStates
        newStatesDispersed = fmap (handleStateShifts firstSets grammar currentStates . fst) divided
        newStates :: Rs.RealSet State
        newStates = fold newStatesDispersed 
        newUnHandled = Rs.difference newStates currentStates
        in 
          if Rs.null newUnHandled then
            newStates
          else 
            loop newStates newUnHandled
        

stateSet2Map :: Rs.RealSet State -> Mi.IntMap State
stateSet2Map = Mi.fromList . Rs.enumerate 

setShifts2Map :: M.Map Text (Set Text) -> Grammar -> [(Int,State)] -> Shiftables -> M.Map Text Int
setShifts2Map firstSets grammar states (Shiftables shiftables) = 
  M.mapKeys kind2Text $  fmap (fromJust . find states) completed
  where 
    completeSet :: State -> State
    completeSet = clousure firstSets grammar 

    advanceSet :: Set Item -> Set Item
    advanceSet = S.map  (\x -> fromJust $ advanceItem x  firstSets) 

    completed = M.map (completeSet . State . advanceSet ) shiftables

    find [] x = Nothing 
    find ((n,y):ys) x = 
      if x == y then
        Just n
      else 
        find ys x

setReduces2Map :: Reducibles -> Either (M.Map Text [Action]) ActionRow
setReduces2Map (Reducibles set) =  
  if length  (M.toList maybeEnd) == length  (M.toList $ justUnic maybeEnd) then
    Right $ ActionRow (justUnic maybeEnd)
  else 
    Left maybeEnd
  where 
    item2Actions :: Item -> M.Map Text [Action]
    item2Actions item = foldr (forFold item) mempty (forFold2 item)

    forFold :: Item -> Text -> M.Map Text [Action] -> M.Map Text [Action]
    forFold item key m = M.insert key [Reduce $ rule item] m

    forFold2 :: Item -> Set Text
    forFold2 item = lookAhead item

    
    items2Actions :: Set Item -> [M.Map Text [Action]]
    items2Actions = map item2Actions  . S.toList

    combine :: [M.Map Text [Action]] -> M.Map Text [Action]
    combine = foldr (M.unionWith (++)) mempty 

    maybeEnd :: M.Map Text [Action]
    maybeEnd = (combine . items2Actions ) set

    justUnic :: M.Map Text [Action] -> M.Map Text Action
    justUnic  = M.map (\[x] -> x) . M.filter ((1 ==) . length)  


data Conflict = 
    ShiftReduce (M.Map Text Int) (M.Map Text Action)
  | ReduceReduce (M.Map Text [Action])


makeStateRow :: M.Map Text (Set Text) -> Grammar -> [(Int,State)] -> State -> Either Conflict (State, ActionRow,GotoRow)
makeStateRow  firstSets grammar states state = 
  case reduces of 
    Left x -> Left $ ReduceReduce x
    Right x ->
      let (ActionRow realReduces) = x 
          reduceKeys = M.keysSet realReduces in 
            if S.intersection shiftsKeys reduceKeys /= S.empty then
              Left $ ShiftReduce shifts realReduces
            else 
              Right (state, ActionRow $ M.union onlyAction realReduces, GotoRow shifts)
  where 
    (s,r) = divideItems state 
    shifts :: M.Map Text Int
    shifts = setShifts2Map firstSets grammar states s
    reduces :: Either (M.Map Text [Action]) ActionRow
    reduces = setReduces2Map r 

    shiftsKeys = M.keysSet shifts

    realShiftAction :: Text -> Bool
    realShiftAction x = 
      let (Grammar g)  = grammar in 
        M.notMember x g 


    onlyAction = M.fromList [ (x,Shift) | (x,m) <-M.toList shifts, realShiftAction x]


makeTables :: M.Map Text (Set Text) -> Grammar -> [(Int,State)] -> Either [Conflict] (ActionTable, GotoTable)
makeTables firstSets grammar states = 
  let processed = map (makeStateRow firstSets  grammar states . snd) states 
      l = lefts processed
      r = rights processed
      in
      if null l then
        Right $ combine (ActionTable mempty) (GotoTable mempty) r
      else 
        Left  l
  where 
    combine :: ActionTable -> GotoTable -> [(State, ActionRow, GotoRow)] -> (ActionTable, GotoTable)
    combine actions gotos [] = (actions,gotos)
    combine (ActionTable actions) (GotoTable gotos) ((state, action, goto):xs) = 
      let (n,_)= (fromJust $ find state states)
          new_actions = Mi.insert n action actions
          new_gotos = Mi.insert n goto gotos
          in
            combine (ActionTable new_actions) (GotoTable new_gotos) xs

    find :: State -> [(Int,State)] ->Maybe (Int,State)
    find _ [] = Nothing
    find x ((n,y):ys) = 
      if x ==y then
        Just (n,y)
      else 
        find x ys
      
      
      

  
   
data Action = 
    Shift
  | Reduce Rule
  deriving (Show,Eq)

newtype ActionRow = ActionRow (M.Map Text Action)
  deriving (Show, Eq)

newtype ActionTable = ActionTable (Mi.IntMap ActionRow)
  deriving (Show, Eq)

newtype GotoRow = GotoRow (M.Map Text  Int )
  deriving (Show, Eq)

newtype GotoTable = GotoTable (Mi.IntMap GotoRow)
  deriving (Show, Eq)




constructFollow :: Grammar -> M.Map Text (S.Set Text)-> M.Map Text (S.Set Text)
constructFollow (Grammar grammar) first =  loop start
  where 
    start = M.map (const S.empty) grammar

    add_list_follows :: Text ->M.Map Text (S.Set Text) -> [Kind]-> M.Map Text (S.Set Text)
    add_list_follows _ provi [] = provi
    add_list_follows rule_name provi [x]= 
      case x of 
      Terminal _ -> provi
      NonTerminal y -> let rule_old = provi M.! rule_name in
        M.insertWith S.union y  rule_old provi 
    add_list_follows rule_name provi (x:xs) =  
      case x of 
        Terminal _ -> add_list_follows rule_name provi xs
        NonTerminal y -> 
          let current_first = getFirst first xs in
            if S.member "_Empty" current_first then
              let newFollows = S.union current_first $ provi M.! rule_name in
                  add_list_follows rule_name (M.insertWith S.union y newFollows provi) xs
            else 
                add_list_follows rule_name (M.insertWith S.union y current_first  provi) xs

    add_rule_body_follows ::  M.Map Text (S.Set Text ) -> Rule -> M.Map Text (S.Set Text)
    add_rule_body_follows provi r@Rule{} = add_list_follows (name r) provi (toList $ body r)
    add_rule_body_follows provi EmptyRule{} = provi
  
    add_rules_body_follows :: [Rule] -> M.Map Text (S.Set Text ) -> M.Map Text (S.Set Text)
    add_rules_body_follows rs provi = foldl fuse provi transformedRules
      where 
      transformedRules :: [M.Map Text (S.Set Text)]
      transformedRules = map  (add_rule_body_follows provi)  rs
      fuse :: M.Map Text (S.Set Text ) -> M.Map Text (S.Set Text) -> M.Map Text (S.Set Text )
      fuse acc new = M.mapWithKey (\ k r -> S.union (new M.! k) r) acc  

    loop :: M.Map Text (S.Set Text) -> M.Map Text (S.Set Text)
    loop provi = 
      let new =  M.foldr add_rules_body_follows provi grammar in
        if provi ==new then
          provi
        else 
          loop new
      
