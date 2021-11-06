{-# LANGUAGE OverloadedStrings #-}
module RawGrammar where

import Data.Text(Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.DList as D
import Data.DList(DList)
import CompilerShow


-- | A File position indicated using Text chars (Unicode code points) to count. 
data Position = Position {
  -- | Text chars form file begin
  offset :: Integer, 
  -- | Count of system linebreaks in the file 
  line :: Integer,
  -- | Text chars from last system linebreak
  column :: Integer
  }

instance CompilerShow Position where
  -- | Since Position isn't expected inside of pretty we could implement just default
  reconstructShow (Position uco l c) = embed uco <> D.singleton "::" <> embed l <> D.singleton "::" <> embed c


-- | A 'Region' is what is used instead of just position start, this way we could add spaces to reconstruct accurately
data Region = Region {
  initial :: Position,
  final :: Position
  }

instance CompilerShow Region where
  reconstructShow (Region i f) = D.singleton "<" <> reconstructShow i <> D.singleton "," <> reconstructShow f <> D.singleton ">"

class HasRegion a where
  getRegion :: a -> Region
  setRegion :: a -> Region -> Region

class AppendableRegion a where
  appendRegion :: HasRegion b => a -> Region -> b 
  original :: HasRegion b => b -> a


data Token  = 
  -- | @terminal
  Tterminal Region
  | Tidentifier Region Text 
  -- | :
  | TruleSeparator Region
  -- | = 
  | TterminalSeparator Region
  -- |  *
  | Tstar Region
  -- | +
  | Tplus Region
  -- | ?
  | Toptional Region
  | TlParen Region
  | TrParen Region
  | TlBrace Region
  | TrBrace Region
  | TlBracket Region
  | TrBracket Region
  | TindentStart Region
  | TindentEnd Region
  | Tstring Region 
  | Tfunction Region
  | TnameBinder Region 
  | TnameDerefer Region
  | TcodeString Region
  -- | @import  
  | Timport Region
  -- | @start
  | Tstart Region
  | TimportPath Region Text

-- | All grammar rules are added as data types
-- | There some repeats as Importable 

data ImportPath =  ImportPath Region Text

data Importable = ImportRule Region RuleName
  | ImportTerminal Region Terminal  

data Import = ImportAll Region ImportPath
  | ImportQualified Region ImportPath
  | ImportSome Region ImportPath [Importable] 
  | ImportHide Region ImportPath [ImportPath]

data Terminal = Terminal Region Text 

data TerminalDeclaration = TerminalDeclaration Region [Terminal] 

data TerminalDefinition = TerminalDefinition Region Terminal TerminalExpression

data TerminalExpression = TerminalString Region Text
  | TerminalRegex Region Text
  | TerminalConcat Region [TerminalExpression]
  | TerminalFromTerminal Region Text


data RuleName = RuleName Region Text

data RuleDeclaration = RuleDeclaration Region RuleName [RuleExpression]

data RuleExpression = RuleString Region Text
  -- | The regular name for rules and terminals inside rules
  | RuleIdentifier Region Text
  -- | Bind a rule expression to variable 
  | RuleVariable Region Text RuleExpression
  | RuleOr Region RuleExpression RuleExpression
  | RuleGroup Region [RuleExpression]
  | RuleStar Region RuleExpression
  | RulePlus Region RuleExpression
  | RuleOptional Region RuleExpression

