# FORTRAN95 lexical analyzer
This project is a hand-written parser for FORTRAN95 programming language lexem parsing.\
Used language: Haskell\
Authors: Denis Chernikov, Vladislav Kuleykin
## How to run project
From main folder of the project (Haskell Environment could be installed):
```bash
stack build && stack exec lex-fortran-exe
```
Otherwise, find an executable at:
```
\.stack-work\dist\7d103d30\build\lex-fortran-exe\lex-fortran-exe.exe
```
Input file (by default): `in.txt`\
Output file (by default): `out.txt`
## How to run tests
From main folder of the project (Haskell Environment could be installed):
```bash
stack build && stack test
```
## Project assumptions
In our project we were about to implement a lexical analyzer for a given programming language (in case of our team - Fortran).
* There exist different versions of Fortran, we are considering FORTRAN95 - language standart of 1995 year edition.
* Because of harder symbol-positioning considering (and no actual need in modern versions) we are using free-identiation standart version, i.e. no exact sybol positions (columns), string lengths etc. are considered.
* There is no preprocessing in our lexical analyzer (we assume that input is already preprocessed on macros starting with hash `#`).
* Because used language (Haskell) is purely functional and "lazy" (data is calculated only "by demand", when it is actualy needed), it is more practical to implement not a function producing tokens on each call, but the function returning a list of tokens. Actual parsing will happen ONLY when the user of function will ask for that concrete token (for example, while "iterating" though the list of resulting tokens).
```Haskell
-- | This was required:
parse :: Parser -> Token
getParser :: String -> Parser

-- | This is actually implemented:
parse :: String -> [Token]
```
* Question mark `?` is parsed as a one that is allowed in the language, but no usage of it was actually found.
* FORTRAN95 has kind type parameters for literal constants, but our lexer parses them as individual `TId` tokens, so we left them for the syntax analyzer to be considered.
* No generalization of functions was applied, but places where changes could be done are obviously pointed by comments and docs.
* We have aggregated all numeric constants into `TNumber` token.
* All number signs (except the exponent sign inside `TNumber`) are left for the syntax analyzer as unary operators.
* Complex numbers, like `(3.5, -2.17e-5)` are left to the syntax anayzer (it is easy to handle them by syntax also).
* `Character` - name of datatype in Fortran, which is equivalent to a single character or a sequence of characters (actually - string).
## Short Haskell introduction
```Haskell
-- This is singe-line comment.

-- | This is documentary comment.

-- | Function type definition.
-- Takes 2 integers as args and returns a string.
f :: Int -> Int -> String

-- Next goes some kind of implementation.
-- Function name, arguments' names, then body.
f first second = show first ++ show second

-- Pattern matching - called when condition found.
f 1 second = "1" ++ show second
f first 2 = show first ++ "2"
f 0 _ = "0" -- We are not interested in the second one

-- Function call - first goes name, then args after whitespaces.
f 123 456
-- Complex args goes into parentheses.
f (12 + 34) 56
f 123 (45 + 67)
-- To avoid parentheses at the end - use dollar.
f 123 $ 45 + 67
-- Also function composition reduces parentheses.
(g . f) 123 456

-- | Some kind of loop over list.
-- [1, 2, 3] == (1 : [2, 3]) == (1 : 2 : [3]) == (1 : 2 : 3 : [])
sum :: [Integer] -> Integer
sum [] = 0
sum (head:tail) = head + sum tail

-- | And... Some "guardian expressions".
-- First true value will be taken.
sumEven :: [Integer] -> Integer
sumEven [] = 0
sumEven (head:tail)
  | isEven head = head + sumEven tail
  | otherwise = sumEven tail

-- Finally - carrying (partial function application).
-- f 1 (sum [1, 2, 3]) == (f) 1 (sum [1, 2, 3]) == (f 1) (sum [1, 2, 3])
```
## Tokens specification
### TComment
```regex
!.*\n
```
### TKeyword
```regex
"NULL()"|"ALLOCATE"|"ALLOCATABLE"|"ASSIGN"|"ASSIGNMENT"|"AUTOMATIC"|"BACKSPACE"|"BLOCK"|"CALL"|"CASE"|"CHARACTER"|"CLOSE"|"COMMON"|"COMPLEX"|"CONTAINS"|"CONTINUE"|"CYCLE"|"DATA"|"DEALLOCATE"|"DEFAULT"|"DIMENSION"|"DO"|"DOUBLE"|"ELEMENTAL"|"ELSE"|"ELSEIF"|"ELSEWHERE"|"END"|"ENDDO"|"ENDIF"|"ENDFILE"|"ENTRY"|"EQUIVALENCE"|"EXIT"|"EXTERNAL"|"FORALL"|"FORMAT"|"FUNCTION"|"GO"|"GOTO"|"IOLENGTH"|"IF"|"IMPLICIT"|"IN"|"INCLUDE"|"INOUT"|"INTEGER"|"INTENT"|"INTERFACE"|"INTRINSIC"|"INQUIRE"|"KIND"|"LEN"|"LOGICAL"|"MODULE"|"NAMELIST"|"NONE"|"NULLIFY"|"ONLY"|"OPEN"|"OPERATOR"|"OPTIONAL"|"OUT"|"PARAMETER"|"PAUSE"|"POINTER"|"PRECISION"|"PRINT"|"PRIVATE"|"PROCEDURE"|"PROGRAM"|"PUBLIC"|"PURE"|"REAL"|"READ"|"RECURSIVE"|"RESULT"|"RETURN"|"REWIND"|"SAVE"|"SELECT"|"SEQUENCE"|"SOMETYPE"|"SQRT"|"STAT"|"STOP"|"SUBROUTINE"|"TARGET"|"TO"|"THEN"|"TYPE"|"UNIT"|"USE"|"VOLATILE"|"WHERE"|"WHILE"|"WRITE"|"null()"|"allocate"|"allocatable"|"assign"|"assignment"|"automatic"|"backspace"|"block"|"call"|"case"|"character"|"close"|"common"|"complex"|"contains"|"continue"|"cycle"|"data"|"deallocate"|"default"|"dimension"|"do"|"double"|"elemental"|"else"|"elseif"|"elsewhere"|"end"|"enddo"|"endif"|"endfile"|"entry"|"equivalence"|"exit"|"external"|"forall"|"format"|"function"|"go"|"goto"|"iolength"|"if"|"implicit"|"in"|"include"|"inout"|"integer"|"intent"|"interface"|"intrinsic"|"inquire"|"kind"|"len"|"logical"|"module"|"namelist"|"none"|"nullify"|"only"|"open"|"operator"|"optional"|"out"|"parameter"|"pause"|"pointer"|"precision"|"print"|"private"|"procedure"|"program"|"public"|"pure"|"real"|"read"|"recursive"|"result"|"return"|"rewind"|"save"|"select"|"sequence"|"sometype"|"sqrt"|"stat"|"stop"|"subroutine"|"target"|"to"|"then"|"type"|"unit"|"use"|"volatile"|"where"|"while"|"write"
```
### TId
```regex
[A-Za-z_][0-9A-Za-z_$@]
```
### TBinConstant
```regex
B(\"[01]+\"|'[01]+')
```
### TOctConstant
```regex
O(\"[0-7]+\"|'[0-7]+')
```
### THexConstant
```regex
Z(\"[0-9A-Fa-f]+\"|'[0-9A-Fa-f]+')
```
### TNumber
```regex
([0-9]+(\.[0-9]*)?|\.[0-9]+)([DEQdeq][-+]?[0-9]+)?
```
### TCharacter
```regex
\"[^"]*\"|'[^']*'
```
### TPlus, TMinus etc.
Obvious :D
### TNewLine
```regex
\n|\r|\r\n
```
### TEq
```regex
==|\.EQ\.
```
### TNotEq etc.
See `TEq`.
### TNot
```regex
\.NOT\.
```
### TAnd etc.
See `TNot`.
### TTrue
```regex
\.TRUE\.
```
### TFalse
```regex
\.FALSE\.
```
### TWs
```regex
[ \t\v\f]|&\n
```
