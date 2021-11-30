{-# LANGUAGE OverloadedStrings, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Generate where

import Prelude hiding (head,tail)
import Data.List (intercalate)
import qualified Data.IntMap as Mi
import Data.Foldable hiding (toList)
import Data.Text(Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Either
import Data.Maybe
import Control.Monad.Reader



import Path(Path)
import qualified Path as P
import Range
import qualified RealSet as Rs
import NList (NList)
import qualified NList as N


import qualified Surface as Su
import qualified Intern as I
import qualified Translations as Tr



data Doc = 
    Str Text
  | Block [Doc]
  | Cat [Doc]

instance Show Doc where
  show = unpack . doc2Text 0


--Todo: Get right the block level, current one probably just grows 
--exponentially
doc2Text :: Int -> Doc -> Text 
doc2Text level (Str t) =  t
doc2Text level (Cat docs) = T.concat $ map (doc2Text 0) docs 
doc2Text level (Block docs) = T.intercalate (genIdent level) $ map (doc2Text (level+1)) docs
  where
    genIdent n = T.replicate n " " 

data Action = 
    Shift 
  | Reduce [Int] Text

data Environment = Environment {
  actions :: Mi.IntMap (Mi.IntMap Action)
  ,gotos :: Mi.IntMap (Mi.IntMap Int)
  ,t2i :: M.Map Text Int
  ,i2t :: Mi.IntMap Text
  ,nonTerminals :: [Int]
  ,tokenType :: Text
  }

type Env a = Reader Environment a

kind2Name :: I.Kind -> Text
kind2Name (I.Terminal name) = name
kind2Name (I.NonTerminal name) = name


names2Int :: I.Grammar -> (M.Map Text Int, Mi.IntMap Text)
names2Int (I.Grammar g) = 
  let enumerated = enumerate (names g) 1 in
      (M.fromList $ fliped enumerated, Mi.fromList enumerated )
  where
  body2Names :: NList I.Kind -> [Text]
  body2Names = N.toList . fmap kind2Name 

  rule2Names :: I.Rule -> [Text]
  rule2Names = body2Names . I.body

  grammar2List :: M.Map Text [I.Rule] -> [Text]
  grammar2List = concatMap (concatMap rule2Names . snd) . M.toList


  names :: M.Map Text [I.Rule] -> [Text]
  names = S.toList . S.fromList . grammar2List

  enumerate :: [a]-> Int -> [(Int,a)]
  enumerate [] n = []
  enumerate (x:xs) n = (n,x):enumerate xs (n+1)

  fliped = map (\(x,y)->(y,x)) 


internal2Generate :: Text-> I.Grammar-> I.ActionTable -> I.GotoTable ->Environment
internal2Generate tokenType (I.Grammar g) atable gtable =
  Environment {
  actions = iactionTable2Map atable
  ,gotos = igotoTable2Map gtable
  ,t2i = t2i
  ,i2t = i2t 
  ,tokenType = tokenType
  ,nonTerminals = nonTerminals
  }
  where 
    (t2i,i2t) = names2Int (I.Grammar g)

    nonTerminals = 
      let ls = [n | (n,y) <- M.toList g] in
          map (t2i M.! ) . S.toList . S.fromList $ ls
    
    iaction2Action :: I.Action -> Action
    iaction2Action I.Shift = Shift
    iaction2Action (I.Reduce r) = 
      Reduce (irule2IntList (I.body r)) (I.function r)

    irule2IntList :: NList I.Kind -> [Int]
    irule2IntList = map ( (t2i M.!) . kind2Name) . N.toList

    iactionRow2Map :: I.ActionRow -> Mi.IntMap Action
    iactionRow2Map (I.ActionRow r) = 
      let almost = M.map iaction2Action (M.mapKeys (t2i M.!) r) in
          Mi.fromList $ M.toList almost

    iactionTable2Map :: I.ActionTable -> Mi.IntMap (Mi.IntMap Action)
    iactionTable2Map (I.ActionTable rowMap) = 
      Mi.map iactionRow2Map rowMap

    igotoRow2Map :: I.GotoRow -> Mi.IntMap Int
    igotoRow2Map (I.GotoRow row) = 
      let almost = M.mapKeys (t2i M.!) row in
          Mi.fromList $ M.toList almost

    igotoTable2Map :: I.GotoTable -> Mi.IntMap (Mi.IntMap Int)
    igotoTable2Map (I.GotoTable table) = 
      Mi.map igotoRow2Map table
      

makeVariables :: Env Text
makeVariables = 
  do
    nonTerminals <- asks nonTerminals
    return $ T.concat $ map tovar nonTerminals

  where 
    tovar i = " t" <> cast i

    cast = pack . show

        
makeValueData :: Env Doc
makeValueData =
  do 
  tokensType <- asks tokenType
  nonTerminals <- asks nonTerminals
  vars <- makeVariables
  return $ 
    Block [
    Str $ "data Value " <> vars <> " = "
    ,makeConstructors tokensType nonTerminals
    ]

  where 
  constructors :: Int -> Doc
  constructors n = Str $ T.concat ["| V", cast n, " t", cast n]
  cast = pack . show

  makeNonTerminals :: [Int] -> [Doc]
  makeNonTerminals ls = map constructors ls 

  makeTerminals :: Text -> Doc
  makeTerminals t = Str $ "  VT"<> t

  makeConstructors t c = Block (makeTerminals t : makeNonTerminals c)


makeReduceOrFunctionData :: Doc
makeReduceOrFunctionData = 
      Block [
        Str  "data ReduceFunctionOrValue a = "
        ,Block [
          Str "  ReduceValue a"
          ,Str "| ReduceFunction (a -> ReduceFunction a)"
        ]
      ]

getActionFunctions :: Env [([Int],Text)]
getActionFunctions = 
  do 
    actions <- asks actions 
    return $ uniqueActions actions
  where 
    action2tuple Shift = []
    action2tuple (Reduce ls name) =[(ls,name)]

    row2tuples :: Mi.IntMap Action -> [([Int],Text)]
    row2tuples row = concatMap (action2tuple . snd) $ Mi.toList row
    
    actions2tuples :: Mi.IntMap (Mi.IntMap Action) ->[([Int],Text)]
    actions2tuples actions = concatMap (row2tuples . snd) $ Mi.toList actions

    removeDups :: Eq b =>[(a,b)] ->[(a,b)]
    removeDups [] = []
    removeDups (x:xs) = 
      x: removeDups (filter ((/= snd x) . snd) xs)

    uniqueActions :: Mi.IntMap (Mi.IntMap Action) -> [([Int],Text)]
    uniqueActions = removeDups . actions2tuples
        
    

makeReduceOrFunctionConversion :: Env Doc
makeReduceOrFunctionConversion = 
  do 
    makeFunctions <$> getActionFunctions 
  where
    reverseInfos = map (\(x,y)->(reverse x ,y))
   
    makeOneFunction :: ([Int],Text) -> [Int] -> Text
    makeOneFunction ([],name) vars = "ReduceValue Usr."<> name <>" " <> vars2text vars
    makeOneFunction (x:xs,name) vars = "ReduceFunction (\\(ReduceValue t"<> cast x <> ") -> " <> makeOneFunction (xs,name) (x:vars)

    cast :: Int -> Text
    cast = pack . show

    vars2text :: [Int] -> Text
    vars2text = T.intercalate " t" . map cast

    makeOneFunctionMain (xs,name) = name <> " = " <> makeOneFunction (xs,name) []

    makeFunctions infos = Block $ map (Str . makeOneFunctionMain) infos


    
makePeekToken :: Doc
makePeekToken = 
  Block [
    Str "do", 
    Block [
      Str "lookAheadToken <- St.gets Usr.peekToken"
    ]
  ]

makeMainCase :: Int -> Env Doc
makeMainCase stateNumber = 
  return $ Block [ 
    Str "case action lookAheadToken of"
  ]

--makeMainCases :: Int -> Env Doc
--makeMainCases stateNumber = 1
   


