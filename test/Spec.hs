import Test.HUnit
import Lexer

newState :: String -> ParserState
newState input = Lexer.State input "" allTokens

test1 = TestCase (
  assertEqual                 -- Assert
  "showTokens \"3.0\""        -- Comment
  "TReal \"3.0\"\nTEof\n"     -- Expected output
  (showTokens "3.0") )        -- Input

test2 = TestCase (
  assertEqual 
  "nextToken State \"3.0\" \"\" []" 
  (newState "", TReal "3.0") 
  (nextToken (newState "3.0")))


tests = TestList [TestLabel "Test showTokens" test1, TestLabel "Test nextToken" test2]

main :: IO Counts
main = runTestTT tests 
