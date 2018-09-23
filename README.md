h1 FORTRAN95 lexical analyzer
=====================
This project is a hand-written parser for FORTRAN95 programming language lexem parsing.
Used language: Haskell
Authors: Denis Chernikov, Vladislav Kuleykin
h2 How to run
-----------------------------------
From main folder of the project (Haskell Environment could be installed):
```bash
stack build && stack exec lex-fortran-exe
```
Otherwise, find an executable at:
```
\.stack-work\dist\7d103d30\build\lex-fortran-exe\lex-fortran-exe.exe
```
Input file (by default): `in.txt`
Output file (by default): `out.txt`
h2 Project assumptions
-----------------------------------
In our project we were about to implement a lexical analyzer for a given programming language (in case of our team - Fortran).
* Because there exist some different versions of it, we are considering FORTRAN95 - language standart of 1995 year edition.
* Also because of harder symbol-positioning considering (and no actual need in modern versions) we are using free-identiation standart version, i.e. no exact sybol positions (columns), string lengths etc. are considered.
* There is no preprocessing in out lexical analyzer (we assume that input is already preprocessed on macros starting with hash `#`).
* Because used language (Haskell) is purely functional and "lazy" (data is calculated only "by demand", when it is actualy needed), it is more practical to implement not a function producing tokens on each call, but the function returning a list of tokens. Actual parsing will happen ONLY when the user of function will ask for that concrete token (for example, while "iterating" though the list of resulting tokens).
```Haskell
-- | This was required:
parse :: Parser -> Token
getParser :: String -> Parser

-- | This is actually implemented:
parse :: String -> [Token]
```
* Question mark `?` is parsed as a one that is allowed in the language, but no usage of it was actually found.
* No generalization of functions was applied, but places where changes could be done are obviously pointed by comments and docs.
* All number signs (except the exponent sign in `Real`) are left for the syntax analyzer as unary operators.
* Complex numbers, like `(3.5, -2.17e-5)` are left to the syntax anayzer (it is easy to handle them by syntax also).
* `Character` - name of datatype in FORTRAN, which is equivalent to a single character or a sequence of characters (actually - string).
h2 Short Haskell introduction
-----------------------------------
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

-- Finally - carrying.
-- f 1 (sum [1, 2, 3]) == (f) 1 (sum [1, 2, 3]) == (f 1) (sum [1, 2, 3])
```