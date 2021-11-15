{-# LANGUAGE OverloadedStrings #-}

module InternSpec (spec) where

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

import Intern as I
import Range
import NList
import qualified RealSet as Rs

int = Terminal "Int"
plus = Terminal "_Plus"
star = Terminal "_Star"
lpar = Terminal "_LParen"
rpar = Terminal "_RParen"

e = NonTerminal "E"
t = NonTerminal "T"


mkrule name (x:xs) = Rule (Range 0 0 0 0 0 0)  name (NList x xs)
mkrule name [] = EmptyRule (Range 0 0 0 0 0 0)  name

(#) = mkrule

infix 5 # 

grammar = Grammar $ M.fromList [
    ("_start", ["_start" # [e] ])
    ,("E", [
            "E" # [e, plus, t]
            ,"E" # [ t ]
            ]
    )
    ,("T", [
              "T" # [ int ]
              ,"T" # [ lpar, e, rpar ]
              ]
    )
  ]

firstSets =  M.fromList [
          ("_start", S.fromList ["_LParen", "Int"])
          ,("E", S.fromList ["_LParen", "Int"])
          ,("T", S.fromList ["_LParen", "Int"])
        ]

followSets =  M.fromList [
          ("_start", S.fromList [])
          ,("E", S.fromList ["_Plus", "_RParen"])
          ,("T", S.fromList ["_Plus","_RParen"])
        ]


firstStartSet  = S.fromList [
  Item ("_start" # [ e ]) 0  $ S.singleton "_EOF"
  ,Item ("E" # [e, plus, t] ) 0 $ S.fromList ["_EOF", "_Plus"]
  ,Item ("E" # [t] ) 0 $ S.fromList ["_EOF", "_Plus"]
  ,Item ("T" # [lpar, e ,rpar] ) 0 $ S.fromList ["_EOF", "_Plus"]
  ,Item ("T" # [int] ) 0 $ S.fromList ["_EOF", "_Plus"]
  ]

state1 = firstStartSet

state2 = S.fromList [
  Item ("E" # [t]) 1 $ S.fromList ["_Plus", "_EOF"]
  ]

state3 = S.fromList [
  Item ("T" # [int]) 1 $ S.fromList ["_Plus", "_EOF"]
  ]

state4 = S.fromList [
  Item ("_start" # [e]) 1 $ S.fromList ["_EOF"]
  ,Item ("E" # [e, plus, t]) 1 $ S.fromList ["_EOF", "_Plus"]
  ]

state6 = S.fromList [
  Item ("T" # [lpar, e, rpar]) 1 $ S.fromList ["_EOF", "_Plus"]
  ,Item ("E" # [e, plus, t]) 0 $ S.fromList ["_RParen", "_Plus"]
  ,Item ("E" # [t]) 0 $ S.fromList ["_RParen", "_Plus"]
  ,Item ("T" # [int]) 0 $ S.fromList ["_RParen", "_Plus"]
  ,Item ("T" # [lpar, e, rpar]) 0 $ S.fromList ["_RParen", "_Plus"]
  ]

state7 = S.fromList [
  Item ("E" # [e, plus, t]) 2 $ S.fromList ["_EOF", "_Plus"]
  ,Item ("T" # [int]) 0 $ S.fromList ["_EOF", "_Plus"]
  ,Item ("T" # [lpar, e, rpar]) 0 $ S.fromList ["_EOF", "_Plus"]
  ]

state8 = S.fromList [
  Item ("E" # [e, plus, t]) 3 $ S.fromList ["_EOF", "_Plus"]
  ]

state9 = S.fromList [
  Item ("E" # [t]) 1 $ S.fromList ["_RParen", "_Plus"]
  ]

state10 = S.fromList [
  Item ("T" # [int]) 1 $ S.fromList ["_RParen", "_Plus"]
  ]

state11 = S.fromList [
  Item ("T" # [lpar, e, rpar]) 2 $ S.fromList ["_EOF", "_Plus"]
  ,Item ("E" # [e, plus, t]) 1 $ S.fromList ["_RParen", "_Plus"]
  ]

state12 = S.fromList [
  Item ("T" # [lpar, e, rpar]) 3 $ S.fromList ["_EOF", "_Plus"]
  ]

state13 = S.fromList [
  Item ("T" # [lpar, e, rpar]) 1 $ S.fromList ["_RParen", "_Plus"]
  ,Item ("E" # [e,plus,t]) 0 $ S.fromList ["_RParen", "_Plus"]
  ,Item ("E" # [t]) 0 $ S.fromList ["_RParen", "_Plus"]
  ,Item ("T" # [int]) 0 $ S.fromList ["_RParen", "_Plus"]
  ,Item ("T" # [lpar, e, rpar]) 0 $ S.fromList ["_RParen", "_Plus"]
  ]

state14 = S.fromList [
  Item ("E" # [e,plus,t]) 2 $ S.fromList ["_RParen", "_Plus"]
  ,Item ("T" # [int]) 0 $ S.fromList ["_RParen", "_Plus"]
  ,Item ("T" # [lpar, e, rpar]) 0 $ S.fromList ["_RParen", "_Plus"]
  ]

state15 = S.fromList [
  Item ("E" # [e,plus,t]) 3 $ S.fromList ["_RParen", "_Plus"]
  ]

state16 = S.fromList [
  Item ("T" # [lpar, e, rpar]) 2 $ S.fromList ["_RParen", "_Plus"]
  ,Item ("E" # [e,plus,t]) 1 $ S.fromList ["_RParen", "_Plus"]
  ]

state17 = S.fromList [
  Item ("T" # [lpar, e, rpar]) 3 $ S.fromList ["_RParen", "_Plus"]
  ]



states = map State [
  state1
  ,state2
  ,state3
  ,state4
  -- missing state5 in purpose as book does the same
  ,state6
  ,state7
  ,state8
  ,state9
  ,state10
  ,state11
  ,state12
  ,state13
  ,state14
  ,state15
  ,state16
  ,state17
  ]


firstDivideShifts = 
  Shiftables $ M.fromList [
  (e, S.fromList [
        Item ("_start" # [e] ) 0 $ S.fromList ["_EOF"]
        ,Item ("E" # [e, plus, t] ) 0 $ S.fromList ["_EOF", "_Plus"] 
        ]
    )
  ,(t , S.fromList [
      Item ("E" # [t] ) 0 $ S.fromList ["_EOF", "_Plus"]
    ]
   )
  ,(int , S.fromList [
      Item ("T" # [int] ) 0 $ S.fromList ["_EOF", "_Plus"]
    ]
   )
  ,(lpar , S.fromList [
      Item ("T" # [lpar, e, rpar] ) 0 $ S.fromList ["_EOF", "_Plus"]
    ]
   )
  ]

firstDivideReduces = Reducibles  S.empty

firstDivide = (firstDivideShifts, firstDivideReduces) 


advancedFirst = 
 [
  S.fromList [
        Item ("_start" # [e] ) 1 $ S.fromList ["_EOF"]
        ,Item ("E" # [e, plus, t] ) 1 $ S.fromList ["_EOF", "_Plus"] 
        ]
  ,S.fromList [
      Item ("E" # [t] ) 1 $ S.fromList ["_EOF", "_Plus"]
    ]
  ,S.fromList [
      Item ("T" # [int] ) 1 $ S.fromList ["_EOF", "_Plus"]
    ]
  ,S.fromList [
      Item ("T" # [lpar, e, rpar] ) 1 $ S.fromList ["_EOF", "_Plus"]
    ]
  ]


advanceSet = S.map  (\x -> fromJust $ advanceItem x  firstSets) 
advanceTest = S.fromList [
        Item ("_start" # [e] ) 0 $ S.fromList ["_EOF"]
        ,Item ("E" # [e, plus, t] ) 0 $ S.fromList ["_EOF", "_Plus"] 
        ]
advanceResult = S.fromList [
        Item ("_start" # [e] ) 1 $ S.fromList ["_EOF"]
        ,Item ("E" # [e, plus, t] ) 1 $ S.fromList ["_EOF","_Plus"] 
        ]


instance Show Shiftables  where 
  show (Shiftables s ) = show s

instance Show Reducibles  where 
  show (Reducibles s ) = show s


statesDic = map (\(x,y)->(x,State y)) [
    (1,state1)
    ,(2,state2)
    ,(3,state3)
    ,(4,state4)
    ,(6,state6)
    ,(7,state7)
    ,(8,state8)
    ,(9,state9)
    ,(10,state10)
    ,(11,state11)
    ,(12,state12)
    ,(13,state13)
    ,(14,state14)
    ,(15,state15)
    ,(16,state16)
    ,(17,state17)
  ]

numeratedRules = [
    (1, "_start" # [e] )
    ,(2, "E" # [e, plus, t])
    ,(3,"E" # [ t ])
    ,(4, "T" # [ int ])
    ,(5, "T" # [ lpar, e, rpar ])
  ]

nr :: Int -> Action
nr n = Reduce (M.fromList numeratedRules M.! n)
 

actions = ActionTable $ Mi.fromList
  [
    (1,ActionRow $ M.fromList [
      ("Int",Shift) 
      ,("_LParen",Shift) 
      ])
    ,(2,ActionRow $ M.fromList [
      ("_Plus", nr 3) 
      ,("_EOF", nr 3) 
      ])
    ,(3,ActionRow $ M.fromList [
      ("_Plus", nr 4) 
      ,("_EOF", nr 4) 
      ])
    ,(4,ActionRow $ M.fromList [
      ("_Plus", Shift) 
      ,("_EOF", nr 1) 
      ])
    ,(6,ActionRow $ M.fromList [
      ("Int", Shift) 
      ,("_LParen", Shift) 
      ])
    ,(7,ActionRow $ M.fromList [
      ("Int", Shift) 
      ,("_LParen", Shift) 
      ])
    ,(8,ActionRow $ M.fromList [
      ("_Plus", nr 2) 
      ,("_EOF", nr 2) 
      ])
    ,(9,ActionRow $ M.fromList [
      ("_Plus", nr 3) 
      ,("_RParen", nr 3) 
      ])
    ,(10,ActionRow $ M.fromList [
      ("_Plus", nr 4) 
      ,("_RParen", nr 4) 
      ])
    ,(11,ActionRow $ M.fromList [
      ("_Plus", Shift) 
      ,("_RParen", Shift) 
      ])
    ,(12,ActionRow $ M.fromList [
      ("_Plus", nr 5) 
      ,("_EOF", nr 5) 
      ])
    ,(13,ActionRow $ M.fromList [
      ("Int", Shift) 
      ,("_LParen", Shift) 
      ])
    ,(14,ActionRow $ M.fromList [
      ("Int", Shift) 
      ,("_LParen", Shift) 
      ])
    ,(15,ActionRow $ M.fromList [
      ("_Plus", nr 2) 
      ,("_RParen", nr 2) 
      ])
    ,(16,ActionRow $ M.fromList [
      ("_Plus", Shift) 
      ,("_RParen", Shift) 
      ])
    ,(17,ActionRow $ M.fromList [
      ("_Plus", nr 5) 
      ,("_RParen", nr 5) 
      ])
  ]


gotos = GotoTable $ Mi.fromList [
    (1,GotoRow $ M.fromList [
      ("Int",3), ("_LParen",6), ("E",4), ("T",2)
      ])
    ,(2,GotoRow  M.empty)
    ,(3,GotoRow  M.empty)
    ,(4,GotoRow $ M.fromList [
      ("_Plus",7)
      ])
    ,(6,GotoRow $ M.fromList [
      ("Int",10), ("_LParen",13), ("E",11), ("T",9)
      ])
    ,(7,GotoRow $ M.fromList [
      ("Int",3), ("_LParen",6), ("T",8)
      ])
    ,(8,GotoRow  M.empty)
    ,(9,GotoRow  M.empty)
    ,(10,GotoRow  M.empty)
    ,(11,GotoRow $ M.fromList [
      ("_Plus",14), ("_RParen",12)
      ])
    ,(12,GotoRow  M.empty)
    ,(13,GotoRow $ M.fromList [
      ("Int",10), ("_LParen",13), ("E",16), ("T",9)
      ])
    ,(14,GotoRow $ M.fromList [
      ("Int",10), ("_LParen",13), ("T",15)
      ])
    ,(15,GotoRow  M.empty)
    ,(16,GotoRow $ M.fromList [
      ("_Plus",14), ("_RParen",17)
      ])
    ,(17,GotoRow  M.empty)
  ]

(#=)::(HasCallStack, Show a, Eq a) => a -> a -> Expectation
(#=) = shouldBe

infix 6 #= 

spec = do
  describe "Lr1 States preliminars" $ do
    it  "First Sets" $
      I.constructFirst grammar #= firstSets
    it "Follow Sets" $
      I.constructFollow grammar firstSets #= followSets
    it "Clousure of First" $
      shouldMatchList (S.toList $ (\(State x) ->x ) (I.clousure firstSets grammar (I.State $ S.fromList [ Item ("_start" # [ e ]) 0 $ S.singleton "_EOF"])) ) $ S.toList firstStartSet

    it "Divide of First set " $ 
      I.divideItems (State firstStartSet) #= firstDivide

    it "Test Advance Set " $
      advanceSet advanceTest #= advanceResult 

    it "Check First Step of find all states" $
      shouldMatchList ( Rs.toList $ I.handleStateShifts firstSets grammar (Rs.singleton $ State state1) firstDivideShifts) (map State [state1, state2,state3, state4, state6])

    it "States without transitions" $ 
      shouldMatchList (Rs.toList $ I.findAllStates firstSets grammar (State state1)) states

    it "Check action table" $
      case  makeTables firstSets grammar statesDic of 
        Right (act,_)-> 
          shouldMatchList ((\(ActionTable w) -> Mi.toList w)act) ((\(ActionTable w) -> Mi.toList w)actions)
    it "Check goto table" $
      case  makeTables firstSets grammar statesDic of 
        Right (_,got)-> 
          shouldMatchList ((\(GotoTable w) -> Mi.toList w)got) ((\(GotoTable w) -> Mi.toList w)gotos)
