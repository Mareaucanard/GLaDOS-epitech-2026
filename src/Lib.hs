module Lib (SExpr (..), printTree, parseString, myMaybeMap) where

import Data.Char (isNumber, isSpace, toUpper)
import Data.List (dropWhileEnd)
import Text.Read (readMaybe)

data SExpr
  = Integer Int
  | Symbol String
  | List [SExpr]
  deriving (Show, Read)

{--
  print Items in lists for printTree
--}
handleListItem :: SExpr -> String
handleListItem (List l) = "(" ++ actualPrintTree (List l) ++ ")"
handleListItem e = actualPrintTree e

{--
  handle printTree of lists
--}
handleList :: [SExpr] -> Int -> String
handleList [] 0 = "an empty list"
handleList (x : xs) 0 = "a list with " ++ handleListItem x ++ handleList xs 1
handleList [x] _ = " and " ++ handleListItem x
handleList (x : xs) n = ", " ++ handleListItem x ++ handleList xs (n + 1)
handleList [] _ = ""

{--
  Logic behind print tree
--}
actualPrintTree :: SExpr -> String
actualPrintTree (Integer i) = "an integer " ++ show i
actualPrintTree (Symbol s) = "a symbol " ++ show s
actualPrintTree (List l) = handleList l 0

{--
  prints a Tree in a very nice sentence (is there for capitalization)
--}
printTree :: SExpr -> String
printTree tree = case actualPrintTree tree of
  [] -> []
  (x : xs) -> toUpper x : xs

{--
  Tries to add to a either monad
  if there is an error, returns the first error to come up
  otherwise, appends to the list
--}
addMaybe :: a -> Either [a] b -> Either [a] b
addMaybe _ (Right msg) = Right msg
addMaybe x (Left xs) = Left (x : xs)

{--
  Check if a string only contains spaces
--}
isEmpty :: String -> Bool
isEmpty [] = True
isEmpty (x : xs)
  | x == ' ' = isEmpty xs
  | otherwise = False

{--
  Remove the first and last parentheses from a string
  Left is the correct string
  Right is an error message (unmatched parentheses or stuff after parentheses (space don't count))
--}
stripParenthesis :: String -> Int -> Either String String
stripParenthesis ('(' : xs) 0 = stripParenthesis xs 1
stripParenthesis (')' : xs) 1 = if isEmpty xs then Left "" else Right "Something is after a parenthesis"
stripParenthesis ('(' : xs) n = addMaybe '(' (stripParenthesis xs (n + 1))
stripParenthesis (')' : xs) n = addMaybe ')' (stripParenthesis xs (n - 1))
stripParenthesis (x : xs) n = addMaybe x (stripParenthesis xs n)
stripParenthesis [] _ = Right "Unmatched parenthesis"

{--
  Tries to map with a load of either monads
  if any call fails, returns the first error
  otherwise returns the map return
--}
myMaybeMap :: (a -> Either b c) -> [a] -> Either [b] c
myMaybeMap _ [] = Left []
myMaybeMap f (x : xs) = case f x of
  Right msg -> Right msg
  Left r -> addMaybe r (myMaybeMap f xs)

parseList :: String -> Either SExpr String
parseList l = case stripParenthesis l 0 of
  Right msg -> Right msg
  Left x -> case myMaybeMap parseString (splitArgs x) of
    Right err -> Right err
    Left y -> Left $ List y

parseNumber :: String -> Either SExpr String
parseNumber x = case readMaybe x of
  Nothing -> Right $ "Could not read integer " ++ show x
  Just r -> Left (Integer r)

{--
  Removes leading and trailing spaces
--}
trim :: [Char] -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

{--
  Counts the number of item in list
--}
count :: (Eq a) => a -> [a] -> Int
count x = length . filter (x ==)

{--
  Tries to parse a symbol
  On success, returns the symbol in Left
  On failure, returns an error message on the Right
--}
parseSymbol :: String -> Either SExpr String
parseSymbol x
  | nb == 0 = Left $ Symbol trimmed
  | otherwise = Right "Symbol can't have spaces, did you try to make a list?"
  where
    trimmed = trim x
    nb = count ' ' trimmed

{--
  Tries to parse a LISP expression
  Left is the parsed SExpr
  Right is an error
--}
parseString :: String -> Either SExpr String
parseString (' ' : xs) = parseString xs
parseString ('(' : xs) = parseList ('(' : xs)
parseString (x : xs) = if isNumber x then parseNumber (x : xs) else parseSymbol (x : xs)
parseString [] = Right "Can't evaluate empty string"

{--
  Split args magic
--}
splitArgsLogic :: String -> Int -> String -> [String] -> [String]
splitArgsLogic [] _ l t = l : t
splitArgsLogic (' ' : xs) 0 "" t = splitArgsLogic xs 0 "" t
splitArgsLogic (' ' : xs) 0 l t = splitArgsLogic xs 0 "" (l : t)
splitArgsLogic ('(' : xs) 0 l t = splitArgsLogic xs 1 "(" (l : t)
splitArgsLogic (')' : xs) 1 l t = splitArgsLogic xs 0 "" ((')' : l) : t)
splitArgsLogic ('(' : xs) n l t = splitArgsLogic xs (n + 1) ('(' : l) t
splitArgsLogic (')' : xs) n l t = splitArgsLogic xs (n - 1) (')' : l) t
splitArgsLogic (x : xs) n l t = splitArgsLogic xs n (x : l) t

{--
  Split args and keeps parentheses
  so like "(1 (2 3 4) 5)" => ["1", "(2 3 4)", "5"]
  never errors (parent should check for bad parenthesis)
--}
splitArgs :: String -> [String]
splitArgs l = reverse (map reverse (filter (not . isEmpty) (splitArgsLogic l 0 "" [])))
