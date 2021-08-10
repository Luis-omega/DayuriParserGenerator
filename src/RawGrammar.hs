{-# LANGUAGE OverloadedStrings #-}
module RawGrammar where

import Data.Text(Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.DList as D
import Data.DList(DList)

-- | Functions used for error format, debug format and synthesis format
class CompilerShow a where
  -- | A nice way to watch the code
  prettyShow :: a -> DList Text
  prettyShow = reconstructShow
  -- | Must be full of annotations and easy parseable
  debugShow :: a -> DList Text
  debugShow = reconstructShow
  -- | Must attempt to reconstruct the object on file
  reconstructShow:: a -> DList Text
  
runShow :: CompilerShow a => (a-> DList Text) -> a -> Text
runShow f = T.concat . D.toList . f  

embed = D.singleton . pack . show

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
  | TImport Region


data Terminal = Terminal Region Text 

data DeclareTerminals = DeclareTerminals Region [Terminal] 

data DefineTerminal = DefineTerminal Region Terminal TerminalExpression

data TerminalExpression = TerminalString Region Text
  | TerminalRegex Region Text
  | TerminalConcat Region TerminalExpression
  | TerminalFromTerminal Region Text



