{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module ParserCore where
import Data.List(intersperse, intercalate)
import Data.Text(Text, pack, unpack)
import qualified Data.Text as T
import Data.Map (Map)
import Data.IntMap (IntMap)
import qualified Data.Map.Strict as M
import qualified Data.IntMap as Mi
import qualified Data.Set as S
import Data.Set (Set)
import Data.Maybe
import Data.Either
import Data.Foldable

import Debug.Trace (trace)


class HasNames a where
  getNames :: a->[Text]

instance HasNames a => HasNames [a] where
  getNames = concatMap getNames

newtype Path = Path (Text,[Text])
  deriving Eq

instance Ord Path where
  compare x y= compare (show x) (show y) 

instance Show Path where
  show (Path (head,tail)) = intercalate "."   $ map unpack  $ head : tail 

instance HasNames Path where
  getNames (Path (head,tail)) = [T.intercalate (pack ".") (head:tail)]

data SourfaceSymbol = 
    MaybeTerminal Path
  | AnonTerminal Text
  | MaybeNonTerminal Path
  | MacroVar Path
  | UserEmpty
  deriving Eq

instance Ord SourfaceSymbol where
  compare x y = compare (show x) (show y)
   

instance HasNames SourfaceSymbol where
  getNames ( MaybeTerminal p) = getNames p
  getNames ( AnonTerminal n) = [pack $ anonTerminal2String n]
  getNames ( MaybeNonTerminal p) = getNames p
  getNames ( MacroVar p) = getNames p
  getNames UserEmpty = [pack "_empty"]

instance Show SourfaceSymbol where
  show (MaybeNonTerminal a) = show a
  show (AnonTerminal a) = anonTerminal2String a
  show (MaybeTerminal a) = show a
  show (MacroVar a) = show a
  show UserEmpty = "_empty"

anonSymbols2String :: Map String String
anonSymbols2String = 
  M.fromList 
    [
      ("(","_Lparen") 
      ,(")","_Rparen") 
      ,("+","_Plus") 
      ,("*","_Star") 
      ,("@","_At") 
    ]

anonTerminal2String :: Text -> String
anonTerminal2String x =
  let str = unpack x in
    M.findWithDefault ("_anon_" ++ str) str anonSymbols2String 


data Symbol = 
    Terminal Path
  | NonTerminal Path
  -- | name of macro followed by rule name 
  | ExpandedMacro Path Path
  | Star [Path]
  | Empty 
  deriving Eq


instance Show Symbol  where
  show (Terminal v) = show v
  show (NonTerminal v) = show v
  show (ExpandedMacro name reason) = show name ++ "@" ++ show reason
  show (Star _) = "_start" 
  show Empty = "_empty"

instance HasNames Symbol where
  getNames (Terminal v) = getNames v
  getNames (NonTerminal v) = getNames v
  getNames (ExpandedMacro name reason) = getNames name ++ [pack "@"] ++ getNames reason
  getNames (Star _) = [pack "_start"]
  getNames Empty = [pack "_empty"]


data Bnf info  symbol = 
    Token info symbol
  | Concat info [Bnf info symbol]
  | MacroCall info Path [Bnf info symbol] 
  deriving (Functor, Foldable, Eq)


instance Show symbol => Show (Bnf i symbol) where
  show (Token _ v) = show v
  show (Concat _ v) = unwords $ map show v
  show (MacroCall _ name args) = show name ++ "{" ++ show args ++"}"

instance HasNames symbol => HasNames (Bnf info symbol) where
  getNames (Token _ v) = getNames v
  getNames (Concat _ v) = getNames v
  getNames (MacroCall _ name args) = getNames name ++ getNames args


data InlineFlag = Inline 
  | NoInline
  deriving (Eq, Show)

data TopLevel info symbol = 
    Rule {
      ruleInfo::info
      ,ruleInline::InlineFlag
      ,ruleName::Path
      ,ruleBody::Bnf info symbol
      ,ruleFunction::Maybe Path
      } 
  | Macro {
    macroInfo::info
    ,macroInline::InlineFlag
    ,macroName::Path
    ,macroArgsCount::Int 
    ,macroArgs::[Path]
    ,macroBody::Bnf info symbol
    ,macroFunction::Maybe Path
    }
  deriving (Functor, Foldable, Eq)

instance Show b => Show (TopLevel i b) where
  show (Rule _ Inline name body fun) =
    "inline "++ show name ++ " : "++ show body ++ " -> " ++ show fun
  show (Rule _ NoInline name body fun) =
    show name ++ " : "++ show body ++ " -> " ++ show fun
  show (Macro _ NoInline _ args name body fun) =
    show name ++" {"++ show args  ++ "} : "++ show body ++ " -> " ++ show fun
  show (Macro _ Inline _ args name body fun) =
    "inline "++show name ++" {"++ show args  ++ "} : "++ show body ++ " -> " ++ show fun

instance HasNames s => HasNames (TopLevel i s) where
  getNames Rule{ruleName=name, ruleBody=body} = getNames name ++ getNames body
  getNames Macro{macroName=name, macroArgs=args, macroBody=body} = 
    getNames name ++ [x | x<-getNames body,  notElem x (getNames args) ]


newtype Productions info symbol = Productions (Map Text (TopLevel info symbol))
  deriving (Functor, Foldable, Eq)

data Position = Position {absolute::Int, line::Int, column::Int} deriving(Show,Eq)


getAllNames :: (Ord symbol, HasNames symbol) => [TopLevel i symbol] -> S.Set Text
getAllNames = S.fromList . getNames

findNonTerminalNames :: (Ord symbol) => [TopLevel i symbol] -> S.Set Text
findNonTerminalNames = S.fromList . concatMap getName
  where 
    getName Rule {ruleName=name} = getNames name
    getName Macro {macroName=name} = getNames name


checkAllDeclared :: S.Set Text -> S.Set Text -> S.Set Text -> Either (S.Set Text) ()
checkAllDeclared terminals nonterminals all = 
  if S.null diff then
    Right  ()
  else 
    Left diff
  where 
    diff = S.difference all (S.union terminals nonterminals) 


sourfaceSetNonTerminals :: S.Set Text -> [TopLevel info SourfaceSymbol] -> Either (S.Set Text) [TopLevel info SourfaceSymbol]
sourfaceSetNonTerminals terminals rules =
  do 
  _ <- checkAllDeclared terminals nonterminals (getAllNames rules)
  return $map (fmap replaceSourface) rules
  where 
    nonterminals = findNonTerminalNames rules
    replaceSourface :: SourfaceSymbol -> SourfaceSymbol
    replaceSourface original@(MaybeTerminal p) = 
      if S.member (cast p) nonterminals then
        MaybeNonTerminal p
      else 
        original
    replaceSourface l = l
    cast = pack . show 



sourfaceSetMacroVars :: [TopLevel info SourfaceSymbol] -> [TopLevel info SourfaceSymbol]
sourfaceSetMacroVars = map replaceInTop
  where
  replaceInTop :: TopLevel i SourfaceSymbol -> TopLevel i SourfaceSymbol
  replaceInTop old@Rule{}= old
  replaceInTop old@Macro {macroArgs=args, macroBody=body}= 
    old{macroBody=  loop (map replace args) body}

  loop [] body = body
  loop (f:fs) body = loop fs (f body)

  replace :: Path -> Bnf i SourfaceSymbol -> Bnf i SourfaceSymbol
  replace x term = 
    case term of 
      Token info symbol -> Token info (symbolReplace x symbol)
      Concat info ls -> Concat info $ map (replace x) ls
      old -> old
  
  symbolReplace :: Path -> SourfaceSymbol -> SourfaceSymbol
  symbolReplace x  term =
    case term of 
      old@(MaybeNonTerminal p) -> if x==p then MacroVar p else old
      old@(MaybeTerminal p) -> if x==p then MacroVar p else old
      old -> old
          

getMacroCalls :: [TopLevel info SourfaceSymbol] -> [Bnf info SourfaceSymbol]
getMacroCalls ls =
  case ls of 
  [] -> []
  Rule{ruleBody=body}:xs -> getCalls body++getMacroCalls xs
  old@Macro{macroBody=body}:xs -> getCalls body++getMacroCalls xs
 where 
  getCalls :: Bnf info SourfaceSymbol -> [Bnf info SourfaceSymbol]
  getCalls (Concat info ls) =  concatMap getCalls ls
  getCalls old@(MacroCall _ _ args) = old: concatMap getCalls args
  getCalls other = []


getMacroDefinitions :: [TopLevel info SourfaceSymbol] -> [TopLevel info SourfaceSymbol]
getMacroDefinitions [] = []
getMacroDefinitions (Rule{}:xs) = getMacroDefinitions xs
getMacroDefinitions (old@Macro{}:xs) =  old:getMacroDefinitions xs


data MacroError info symbol= 
    MultipleDefinitions (TopLevel info SourfaceSymbol) (TopLevel info SourfaceSymbol)
  | BadArgsNumber (TopLevel info SourfaceSymbol) (Bnf info SourfaceSymbol)
  | MacroCalledNotFound (Bnf info SourfaceSymbol)
  deriving Eq

instance Show symbol => Show (MacroError i symbol )where
  show (MultipleDefinitions def1 def2) = "Bad Definition of macro: " ++ show def1 ++ " with macro" ++ show def2
  show (BadArgsNumber def call) = "Bad macro call " ++ show call ++ " to macro " ++ show def
  show (MacroCalledNotFound call) = "Call to non existent macro " ++ show call

checkMacros :: [TopLevel info SourfaceSymbol] -> Either [MacroError info SourfaceSymbol] ()
checkMacros ls = 
  case  checkDefinitions definitions ++ checkCalls of
    [] -> Right ()
    ls -> Left ls
  where 
  calls = getMacroCalls ls
  definitions = getMacroDefinitions ls
  checkDefinitionCoherent x defs = lefts $ map (coherent x) defs
  coherent m1@Macro{macroName=name1, macroArgs=args1} m2@Macro{macroName=name2, macroArgs=args2} =
    if name1==name2 then
      if args1==args2 then
        Right ()
      else 
        Left $ MultipleDefinitions m1 m2
    else Right ()
  getDefinition name []= Nothing
  getDefinition name (old@Macro{macroName=name2}:xs)= 
    if name ==name2 then 
      Just old
    else 
      getDefinition name xs
  checkDefinitions [] = []
  checkDefinitions (x:xs)= checkDefinitionCoherent x xs ++ checkDefinitions xs
  checkCall call@(MacroCall info name args) = 
    case getDefinition name definitions of 
      Just def@Macro{macroArgsCount=count} ->
        if count /= length args then  
          Left $ BadArgsNumber def call
        else 
          Right ()
      _ -> Left $ MacroCalledNotFound call
  checkCalls = lefts $ map checkCall calls
