import Lexer
import Test.HUnit hiding (State)

initialState :: String -> ParserState
initialState input = State input "" allTokens

test1 = TestCase (
    assertEqual          -- Assertion type
    "suitsToken \"\""    -- Description
    (False, False)       -- Expected output
    (suitsToken "" TEof) -- Input
  )

test2In :: [String]
test2In = [ ""
          , "0"
          , "1"
          , "a"
          , "e"
          , "." -- 5
          , "$"
          , " "
          , "00"
          , "12"
          , "1 2" -- 10
          , "1."
          , ".1"
          , "1e"
          , "1d"
          , "1q" -- 15
          , ".e"
          , ".a"
          , "a."
          , ".."
          , "000" -- 20
          , "123"
          , "1.e"
          , ".1e"
          , ".1."
          , "1.." -- 25
          , "..1"
          , "1e."
          , "1ee"
          , "1ed"
          , "11." -- 30
          , ".11"
          , "1.1"
          , "1.1e"
          , ".1e1"
          , "1.e1" -- 35
          , "1.e+"
          , ".1e+"
          , "1.e-"
          , "1e11"
          , "1e+1" -- 40
          , "1e-1"
          , "e-11"
          , "e+11"
          , "e111"
          , ".e+1" -- 45
          , ".e-1"
          , "1.e23"
          , ".1e23"
          , "1.e+-"
          , "1.e2+" -- 50
          , "1.e2-"
          , "1.2.3"
          , "12.e34"
          , ".12e34"
          , "12.e+34" -- 55
          , ".12e-34"
          , "12.34e56"
          , "12.34e+56"
          , "12.34e-56"
          , "12.34e-5." -- 60
          , "12.34e-5.6"
          , "1234567890"
          , "1234567890.1234567890e+1234567890"
          ]

test2Out :: [Validity]
test2Out = [ (False, False)
           , (True, True)
           , (True, True)
           , (False, False)
           , (False, False)
           , (False, True) -- 5
           , (False, False)
           , (False, False)
           , (True, True)
           , (True, True)
           , (False, False) -- 10
           , (True, True)
           , (True, True)
           , (False, True)
           , (False, True)
           , (False, True) -- 15
           , (False, False)
           , (False, False)
           , (False, False)
           , (False, False)
           , (True, True) -- 20
           , (True, True)
           , (False, True)
           , (False, True)
           , (False, False)
           , (False, False) -- 25
           , (False, False)
           , (False, False)
           , (False, False)
           , (False, False)
           , (True, True) -- 30
           , (True, True)
           , (True, True)
           , (False, True)
           , (True, True)
           , (True, True) -- 35
           , (False, True)
           , (False, True)
           , (False, True)
           , (True, True)
           , (True, True) -- 40
           , (True, True)
           , (False, False)
           , (False, False)
           , (False, False)
           , (False, False) -- 45
           , (False, False)
           , (True, True)
           , (True, True)
           , (False, False)
           , (False, False) -- 50
           , (False, False)
           , (False, False)
           , (True, True)
           , (True, True)
           , (True, True) -- 55
           , (True, True)
           , (True, True)
           , (True, True)
           , (True, True)
           , (False, False) -- 60
           , (False, False)
           , (True, True)
           , (True, True)
           ]

test2 = TestCase (
    assertEqual
    "suitsToken TNumber"
    test2Out
    (map (\s -> suitsToken s (TNumber "")) test2In)
  )

test3 = TestCase (
    assertEqual
    "proceedChar (State \".e+3\" \"12\" [TNumber])"
    (State "e+3" "12." [TNumber ""], Nothing)
    (proceedChar $ State ".e+3" "12" [TNumber ""])
  )

test4 = TestCase (
    assertEqual
    "proceedChar (State \" ::\" \"12\" [TNumber])"
    (State " ::" "" allTokens,  Just $ TNumber "12.e-10")
    (proceedChar $ State " ::" "12.e-10" [TNumber ""])
  )

test5 = TestCase (
    assertEqual
    "parse \"\""
    [
      TKeyword "integer",TComma,TKeyword "parameter",TComma,TKeyword "private",
      TDblColon,TId "sp",TEquals,TId "selected_real_kind",TLParen,TId "p",
      TEquals,TNumber "6",TComma,TId "r",TEquals,TNumber "37",TRParen,TEof
    ]
    (parse "integer,parameter,private :: sp = selected_real_kind(p=6,r=37)")
  )

tests :: Test
tests = TestList [ TestLabel "Test suitsToken for empty" test1
                 , TestLabel "Test suitsToken for TNumber" test2
                 , TestLabel "Test proceedChar" test3
                 , TestLabel "Test proceedChar" test4
                 , TestLabel "Test parse" test5
                 ]

main :: IO Counts
main = runTestTT tests
