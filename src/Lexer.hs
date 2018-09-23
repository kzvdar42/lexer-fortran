{-# LANGUAGE OverloadedStrings #-}

-- | Parse the FORTRAN95 programming language source file.
module Lexer where

import Data.Char (toUpper, isLetter, isDigit)
import Data.List (find, findIndex)
import Data.Text (pack)
import System.Environment
import System.IO
import Text.Read (readMaybe)

-- | Validity of a string by token.
--
-- * Is it valid at this moment?
-- * May it become valid later (or is it valid now)?
type Validity = (Bool, Bool)

-- | State of the parser.
--
-- * String remaining to process
-- * Current buffer
-- * Set of currently applicable tokens (default - allTokens)
data ParserState = State String String [Token]
  deriving (Eq, Show)

-- ******************** LANGUAGE-DEPENDENT ********************

-- | Set of tokens for FORTRAN95 programming language.
data Token
  = TComment String
  | TKeyword String
  | TId String
  | TBinConstant String
  | TOctConstant String
  | THexConstant String
  | TNumber String
  | TCharacter String
  | TEquals
  | TPlus
  | TMinus
  | TAsterisk
  | TSlash
  | TDot
  | TComma
  | TColon
  | TSemicolon
  | TLParen
  | TRParen
  | TLBracket
  | TRBracket
  | TPercent
  | TDollar
  | TAmpersand
  | TQuestion
  | TDblColon
  | TDblSlash
  | TDblAsterisk
  | TArrow
  | TMArrow
  | TLArrowConst
  | TRArrowConst
  | TNewLine
  | TEq
  | TNotEq
  | TLess
  | TLessEq
  | TGreater
  | TGreaterEq
  | TNot
  | TAnd
  | TOr
  | TXor
  | TLogEq
  | TLogNotEq
  | TTrue
  | TFalse
  | TWs
-- Language-independent
  | TEof
  | TError String
  deriving (Eq, Show)

-- | Set of default values of all tokens
-- from the FORTRAN95 programming language
-- sorted by priority.
allTokens :: [Token]
allTokens =
  [ TComment ""
  , TKeyword ""
  , TId ""
  , TBinConstant ""
  , TOctConstant ""
  , THexConstant ""
  , TNumber ""
  , TCharacter ""
  , TEquals
  , TPlus
  , TMinus
  , TAsterisk
  , TSlash
  , TDot
  , TComma
  , TColon
  , TSemicolon
  , TLParen
  , TRParen
  , TLBracket
  , TRBracket
  , TPercent
  , TDollar
  , TAmpersand
  , TQuestion
  , TDblColon
  , TDblSlash
  , TDblAsterisk
  , TArrow
  , TMArrow
  , TLArrowConst
  , TRArrowConst
  , TNewLine
  , TEq
  , TNotEq
  , TLess
  , TLessEq
  , TGreater
  , TGreaterEq
  , TNot
  , TAnd
  , TOr
  , TXor
  , TLogEq
  , TLogNotEq
  , TTrue
  , TFalse
  , TWs
  ]

-- | Set of all keywords from
-- the FORTRAN95 programming language.
keywords :: [String]
keywords = ["NULL()", "ALLOCATE", "ALLOCATABLE", "ASSIGN",
  "ASSIGNMENT", "AUTOMATIC", "BACKSPACE", "BLOCK", "CALL", "CASE",
  "CHARACTER", "CLOSE", "COMMON", "COMPLEX", "CONTAINS", "CONTINUE", "CYCLE",
  "DATA", "DEALLOCATE", "DEFAULT", "DIMENSION", "DO",
  "DOUBLE", "ELEMENTAL", "ELSE", "ELSEIF", "ELSEWHERE",
  "END", "ENDDO", "ENDIF", "ENDFILE", "ENTRY",
  "EQUIVALENCE", "EXIT", "EXTERNAL",
  "FORALL", "FORMAT", "FUNCTION", "GO", "GOTO", "IOLENGTH",
  "IF", "IMPLICIT", "IN", "INCLUDE", "INOUT", "INTEGER", "INTENT", "INTERFACE",
  "INTRINSIC", "INQUIRE", "KIND", "LEN", "LOGICAL", "MODULE",
  "NAMELIST", "NONE", "NULLIFY",
  "ONLY", "OPEN", "OPERATOR", "OPTIONAL", "OUT", "PARAMETER",
  "PAUSE", "POINTER", "PRECISION", "PRINT", "PRIVATE", "PROCEDURE",
  "PROGRAM", "PUBLIC", "PURE", "REAL", "READ", "RECURSIVE", "RESULT",
  "RETURN", "REWIND", "SAVE", "SELECT", "SEQUENCE", "SOMETYPE", "SQRT", "STAT",
  "STOP", "SUBROUTINE", "TARGET", "TO", "THEN", "TYPE",
  "UNIT", "USE", "VOLATILE", "WHERE", "WHILE", "WRITE"]

-- | Does a given string suits to be
-- a representation of a given token
-- in terms of data type 'Validity'?
suitsToken :: String -> Token -> Validity
suitsToken "" _ = (False, False)
-- === Comments
suitsToken s (TComment _)
  | (head s) == '!' && (noNewLineInside $ tail s) &&
    (last s) == '\n' && length s > 1 = (True, True)
  | (head s) == '!' && (noNewLineInside $ tail s) = (False, True)
  | otherwise = (False, False)
  where
    noNewLineInside []       = True
    noNewLineInside (ch:[])  = True
    noNewLineInside (ch:chs) = ch /= '\n' && noNewLineInside chs
-- === Keywords
suitsToken s (TKeyword _) = isSubForList keywords s
-- === Identifier
suitsToken (ch:chs) (TId _)
  | isValidBegin ch && isValidTail chs = (True, True)
  | isValidBegin ch && chs == "" = (False, True)
  | otherwise = (False, False)
  where
    isValidBegin c = isLetter c || c == '_'
    isValidTail s = and $ map (
        \c -> isLetter c || isDigit c || c == '_' || c == '$' || c == '@'
      ) s
-- === Numeric constants
suitsToken s (TBinConstant _)
  | first && length s == 1 = (False, True)
  | first &&
    (second == '\'' || second == '"') && isBinary (drop 2 s) = (False, True)
  | first && second == '\'' && fromThird s 
    && last s == '\'' && length s > 3 = (True, True)
  | first && second == '"'  && fromThird s 
    && last s == '"'  && length s > 3 = (True, True)
  | otherwise = (False, False)
  where
    first = (toUpper $ head s) == 'B' 
    second = s !! 1
    fromThird str = isBinary . drop 2 $ init str
    isBinary [] = True
    isBinary (ch:chs) = (ch == '1' || ch == '0') && isBinary chs
suitsToken s (TOctConstant _)
  | first && length s == 1 = (False, True)
  | first &&
    (second == '\'' || second == '"') && isOct (drop 2 s) = (False, True)
  | first && second == '\'' && fromThird s 
    && last s == '\'' && length s > 3 = (True, True)
  | first && second == '"'  && fromThird s 
    && last s == '"'  && length s > 3 = (True, True)
  | otherwise = (False, False)
  where
    first = (toUpper $ head s) == 'O' 
    second = s !! 1
    fromThird str = isOct . drop 2 $ init str
    isOct []  = True
    isOct (ch:chs) = (
        fst $ isSubForList ["0","1","2","3","4","5","6","7"] [ch]
      ) && isOct chs
suitsToken s (THexConstant _)
  | first && length s == 1 = (False, True)
  | first &&
    (second == '\'' || second == '"') && isHec (drop 2 s) = (False, True)
  | first && second == '\'' && fromThird s 
    && last s == '\'' && length s > 3 = (True, True)
  | first && second == '"'  && fromThird s 
    && last s == '"'  && length s > 3 = (True, True)
  | otherwise = (False, False)
  where
    first = (toUpper $ head s) == 'Z' 
    second = s !! 1
    fromThird str = isHec . drop 2 $ init str
    isHec [] = True
    isHec (ch:chs) = (
        isDigit ch || (fst $ isSubForList ["A","B","C","D","E","F"] [ch])
      ) && isHec chs
-- === Number
suitsToken s (TNumber _) =
  let 
    isDot = (\c -> c == '.')
    isE   = (\c -> fst $ isSubForList ["E", "D", "Q"] [c])
    isOp  = (\c -> c == '+' || c == '-')
    isInt str = and $ map isDigit str
    toTuple b = (b,b)
    divideBy pos str = (left, right)
      where
        right = tail rightWith
        (left, rightWith) = splitAt pos str
  in case findIndex isDot s of
    Nothing -> 
      case findIndex isE s of
        Nothing -> (isInt s, isInt s)
        Just posOfE -> 
          let 
            (beforeE, afterE) = divideBy posOfE s
            beforeEIsGood = isInt beforeE && length beforeE > 0
          in case findIndex isOp afterE of
            Nothing -> 
              if length afterE == 0 
              then (False, beforeEIsGood) 
              else toTuple (beforeEIsGood && isInt afterE)
            Just _ ->
              let (_, afterOp) = splitAt 1 afterE
              in if length afterOp == 0 
              then (False, beforeEIsGood) 
              else toTuple (beforeEIsGood && isInt afterOp)
    Just posOfDot ->
      let (beforeDot, afterDot) = divideBy posOfDot s
      in case findIndex isE afterDot of
        Nothing -> 
          if length s == 1 
          then (False, True)
          else toTuple(isInt beforeDot && isInt afterDot)
        Just posOfE ->
          let
          beforeEIsGood = (length beforeDot > 0 || length beforeE > 0)
            && isInt beforeDot && isInt beforeE
          (beforeE, afterE) = divideBy posOfE afterDot
          in case findIndex isOp afterE of
            Nothing ->
              if (length afterE == 0)
              then (False, beforeEIsGood && isInt afterE)
              else toTuple (beforeEIsGood && isInt afterE)
            Just _ ->
              let (_, afterOp) = splitAt 1 afterE
              in if (length afterOp == 0)
              then (False, beforeEIsGood)
              else toTuple(beforeEIsGood && isInt afterOp)
-- === Character
suitsToken s (TCharacter _)
  | (head s) == '\'' && (notInside '\'' $ tail s) &&
    (last s) == '\'' && length s > 1 = (True, True)
  | (head s) == '"'  && (notInside '"' $ tail s) &&
    (last s) == '"'  && length s > 1 = (True, True)
  | (head s) == '\'' && (notInside '\'' $ tail s) = (False, True)
  | (head s) == '"'  && (notInside '"'  $ tail s) = (False, True)
  | otherwise = (False, False)
  where
    notInside _ []       = True
    notInside _ (ch:[])  = True
    notInside c (ch:chs) = c /= ch && notInside c chs
-- === Operators
suitsToken s TEquals      = isSubstring "=" s
suitsToken s TPlus        = isSubstring "+" s
suitsToken s TMinus       = isSubstring "-" s
suitsToken s TAsterisk    = isSubstring "*" s
suitsToken s TSlash       = isSubstring "/" s
suitsToken s TDot         = isSubstring "." s
suitsToken s TComma       = isSubstring "," s
suitsToken s TColon       = isSubstring ":" s
suitsToken s TSemicolon   = isSubstring ";" s
suitsToken s TLParen      = isSubstring "(" s
suitsToken s TRParen      = isSubstring ")" s
suitsToken s TLBracket    = isSubstring "[" s
suitsToken s TRBracket    = isSubstring "]" s
suitsToken s TPercent     = isSubstring "%" s
suitsToken s TDollar      = isSubstring "$" s
suitsToken s TAmpersand   = isSubstring "&" s
suitsToken s TQuestion    = isSubstring "?" s
suitsToken s TDblColon    = isSubstring "::" s
suitsToken s TDblSlash    = isSubstring "//" s
suitsToken s TDblAsterisk = isSubstring "**" s
suitsToken s TArrow       = isSubstring "=>" s
suitsToken s TMArrow      = isSubstring "->" s
suitsToken s TLArrowConst = isSubstring "(/" s
suitsToken s TRArrowConst = isSubstring "/)" s
suitsToken s TNewLine     = isSubForList ["\n", "\r", "\r\n"] s
suitsToken s TEq          = isSubForList ["==", ".EQ."] s
suitsToken s TNotEq       = isSubForList ["/=", ".NE."] s
suitsToken s TLess        = isSubForList ["<", ".LT."] s
suitsToken s TLessEq      = isSubForList ["<=", ".LE."] s
suitsToken s TGreater     = isSubForList [">", ".GT."] s
suitsToken s TGreaterEq   = isSubForList [">=", ".GE."] s
suitsToken s TNot         = isSubstring ".NOT." s
suitsToken s TAnd         = isSubstring ".AND." s
suitsToken s TOr          = isSubstring ".OR."  s
suitsToken s TXor         = isSubstring ".XOR." s
suitsToken s TLogEq       = isSubstring ".EQV." s
suitsToken s TLogNotEq    = isSubstring ".NEQV." s
suitsToken s TTrue        = isSubstring ".TRUE." s
suitsToken s TFalse       = isSubstring ".FALSE." s
-- === Whitespace
suitsToken s TWs = isSubForList [" ", "\t", "\v", "\f", "&\n"] s
-- === Others
suitsToken _ (TError _) = (False, False)
suitsToken _ TEof = (False, False)

-- | Pack given literal into the
-- token of a given type (if needed).
putLiteral :: String -> Token -> Token
putLiteral s (TComment _) = TComment . init . tail $ s
putLiteral s (TKeyword _)  = TKeyword s
putLiteral s (TId _)   = TId s
putLiteral s (TBinConstant _) = TBinConstant . init . drop 2 $ s
putLiteral s (TOctConstant _) = TOctConstant . init . drop 2 $ s
putLiteral s (THexConstant _) = THexConstant . init . drop 2 $ s
putLiteral s (TNumber _)  = TNumber s
putLiteral s (TCharacter _) = TCharacter . init . tail $ s
putLiteral _ token = token

-- ******************** LANGUAGE-INDEPENDENT ********************

-- | Is a given string - a substring
-- for one of given templates in terms
-- of the data type 'Validity'?
isSubForList :: [String] -> String -> Validity
isSubForList [] _ = (False, False)
isSubForList (template:templates) str = (now, future)
  where
    now    = now1    || now2
    future = future1 || future2
    (now1, future1) = isSubstring template str
    (now2, future2) = isSubForList templates str

-- | Is a given string - a substring (2nd)
-- for a given template (1st) in terms
-- of the data type 'Validity'?
isSubstring :: String -> String -> Validity
isSubstring template str = (now, future)
  where
    now = (length str == length template) && and res
    future = (length str <= length template) && and res
    res = zipWith (\ch1 ch2 -> toUpper ch1 == ch2) str template

-- | Get a token which fully suits the given string.
-- If no such token found - TError is returned.
whichToken :: String -> Token
whichToken s =
  case find (fst . suitsToken s) allTokens of
    Just token -> putLiteral s token
    Nothing    -> TError s

-- | Process the next character from the input
-- and try to make a token out of it considering
-- already processed buffer.
proceedChar :: ParserState -> (ParserState, Maybe Token)
proceedChar st@(State "" buffer _possToks)
  | buffer == "" = (st, Just TEof)
  | otherwise = (State "" "" allTokens, Just $ whichToken buffer)
proceedChar (State input@(ch:chs) buffer possToks)
  | length filtered > 0 = (State chs (buffer ++ [ch]) filtered, Nothing)
  | buffer == "" = (State chs "" allTokens, Just $ whichToken [ch])
  | otherwise = (State input "" allTokens, Just $ whichToken buffer)
  where
    filtered = filter (snd . suitsToken (buffer ++ [ch])) possToks

-- | Update parser state and get next
-- token for a given parser configuration.
nextToken :: ParserState -> (ParserState, Token)
nextToken state =
  let (newState, tokenMaybe) = proceedChar state
  in case tokenMaybe of
    Nothing -> nextToken newState
    Just TWs -> nextToken newState
    Just token -> (newState, token)

-- | Recursively parse a given parser state
-- to get the list of tokens to be produced.
parseUntilEnd :: (ParserState, [Token]) -> (ParserState, [Token])
parseUntilEnd (state, tokens) =
  let
    (newState, token) = nextToken state
    newToks = token : tokens
  in case token of
    TEof -> (newState, newToks)
    _    -> parseUntilEnd (newState, newToks)

-- | Get a list of tokens out of a given string.
parse :: String -> [Token]
parse str = reverse . snd $ parseUntilEnd (initiateParser str, [])
  where
    initiateParser s = State s "" allTokens

-- ******************** TOKENIZATION-INDEPENDENT ********************

-- | Convert a string to the string with
-- the corresponding token sequence for the input.
showTokens :: String -> String
showTokens str = concatMap ((++ "\n") . show) $ parse str

-- | Run parser to read from the specified file
-- (or from 'in.txt') and write tokens into the
-- other specified file (or into 'out.txt').
run :: IO ()
run = do
  args <- getArgs
  input <- readFile $
    if not (null args) && length args > 0
      then args !! 0
      else "in.txt"
  let result = showTokens input
  writeFile (
      if not (null args) && length args > 1
        then args !! 1
        else "out.txt"
    ) result
