{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Lib
--}

module Lib (printTree, parseString, myMaybeMap) where

import Data.Char (isNumber, isSpace, toUpper)
import Data.List (dropWhileEnd)
import Text.Read (readMaybe)
import Types (SExpr (..))

-- |Prints Items in lists for printTree.
handleListItem :: SExpr -- ^ The item to print
  -> String -- ^ The return value
handleListItem (List l) = "(" ++ actualPrintTree (List l) ++ ")"
handleListItem e = actualPrintTree e

-- |Handles printTree of lists.
handleList :: [SExpr] -- ^ The list to print
  -> Int -- ^ The number of items already printed
  -> String -- ^ The return value
handleList [] 0 = "an empty list"
handleList (x : xs) 0 = "a list with " ++ handleListItem x ++ handleList xs 1
handleList [x] _ = " and " ++ handleListItem x
handleList (x : xs) n = ", " ++ handleListItem x ++ handleList xs (n + 1)
handleList [] _ = ""

-- |Logic behind print tree.
actualPrintTree :: SExpr -- ^ The tree to print
  -> String -- ^ The return value
actualPrintTree (Integer i) = "an integer " ++ show i
actualPrintTree (Symbol s) = "a symbol " ++ show s
actualPrintTree (List l) = handleList l 0
actualPrintTree (Boolan b) = "a boolean " ++ show b


-- |Prints a Tree in a very nice sentence (is there for capitalization).
printTree :: SExpr -- ^ The tree to print
  -> String -- ^ The return value
printTree tree = case actualPrintTree tree of
  [] -> []
  (x : xs) -> toUpper x : xs

-- |Tries to add to a either monad.
-- If there is an error, returns the first error to come up.
-- Otherwise, appends to the list.
addMaybe :: a -- ^ The item to add
  -> Either [a] b -- ^ The either monad to add to
  -> Either [a] b -- ^ The return value
addMaybe _ (Right msg) = Right msg
addMaybe x (Left xs) = Left (x : xs)

-- |Checks if a string only contains spaces.
isEmpty :: String -- ^ The string to check
  -> Bool -- ^ The return value
isEmpty [] = True
isEmpty (' ': xs) = isEmpty xs
isEmpty _ = False

-- |Remove the first and last parentheses from a string.
-- Left is the correct string.
-- Right is an error message (unmatched parentheses or stuff after parentheses (space don't count)).
stripParenthesis :: String -- ^ The string to strip
  -> Int -- ^ The number of parentheses to strip
  -> Either String String -- ^ The return value
stripParenthesis ('(' : xs) 0 = stripParenthesis xs 1
stripParenthesis (')' : xs) 1 =
  if isEmpty xs
    then Left ""
    else Right "Something is after a parenthesis"
stripParenthesis ('(' : xs) n = addMaybe '(' (stripParenthesis xs (n + 1))
stripParenthesis (')' : xs) n = addMaybe ')' (stripParenthesis xs (n - 1))
stripParenthesis (x : xs) n = addMaybe x (stripParenthesis xs n)
stripParenthesis [] _ = Right "Unmatched parenthesis"

-- |Tries to map with a load of either monads.
-- If any call fails, returns the first error.
-- Otherwise returns the map return.
myMaybeMap :: (a -> Either b c) -- ^ The function to map
  -> [a] -- ^ The list to map
  -> Either [b] c -- ^ The return value
myMaybeMap _ [] = Left []
myMaybeMap f (x : xs) = case f x of
  Right msg -> Right msg
  Left r -> addMaybe r (myMaybeMap f xs)

-- |Tries to parse a list.
parseList :: String -- ^ The string to parse
  -> Either SExpr String -- ^ The return value
parseList l = case stripParenthesis l 0 of
  Right msg -> Right msg
  Left x -> case myMaybeMap parseString (splitArgs x) of
    Right err -> Right err
    Left y -> Left $ List y

-- |Tries to parse a number.
parseNumber :: String -- ^ The string to parse
  -> Either SExpr String -- ^ The return value
parseNumber x = case readMaybe x of
  Nothing -> Right $ "Could not read integer " ++ show x
  Just r -> Left (Integer r)

-- |Removes leading and trailing spaces.
trim :: [Char] -- ^ The string to trim
  -> String -- ^ The return value
trim = dropWhileEnd isSpace . dropWhile isSpace

-- |Counts the number of item in list.
count :: (Eq a) => a -- ^ The item to count
  -> [a] -- ^ The list to count in
  -> Int -- ^ The return value
count x = length . filter (x ==)

-- |Tries to parse a symbol.
-- On success, returns the symbol in Left.
-- On failure, returns an error message on the Right.
parseSymbol :: String -- ^ The string to parse
  -> Either SExpr String -- ^ The return value
parseSymbol x
  | nb == 0 = Left $ Symbol trimmed
  | otherwise = Right "Symbol can't have spaces, did you try to make a list?"
  where
    trimmed = trim x
    nb = count ' ' trimmed

-- |Tries to parse a boolean.
-- On success, returns the boolean in Left.
-- On failure, returns an error message on the Right.
parseBool :: String -- ^ The string to parse
  -> Either SExpr String -- ^ The return value
parseBool ['t'] = Left $ Boolan True
parseBool ['f'] = Left $ Boolan False
parseBool _ = Right "Boolean not empty or not recognized"

-- |Tries to parse a LISP expression.
-- Left is the parsed SExpr.
-- Right is an error.
parseString :: String -- ^ The string to parse
  -> Either SExpr String -- ^ The return value
parseString (' ' : xs) = parseString xs
parseString ('\n' : xs) = parseString xs
parseString ('(' : xs) = parseList ('(' : xs)
parseString ('#' : xs) = parseBool (trim xs)
parseString (x : xs) =
  if isNumber x
    then parseNumber (x : xs)
    else parseSymbol (x : xs)
parseString [] = Right "Can't evaluate empty string"

-- |Splits args magic.
splitArgsLogic :: String -- ^ The string to split
  -> Int -- ^ The number of parentheses to strip
  -> String -- ^ The current argument
  -> [String] -- ^ The return value
  -> [String] -- ^ The return value
splitArgsLogic [] _ l t = l : t
splitArgsLogic (' ' : xs) 0 "" t = splitArgsLogic xs 0 "" t
splitArgsLogic (' ' : xs) 0 l t = splitArgsLogic xs 0 "" (l : t)
splitArgsLogic ('\n' : xs) 0 "" t = splitArgsLogic xs 0 "" t
splitArgsLogic ('\n' : xs) 0 l t = splitArgsLogic xs 0 "" (l : t)
splitArgsLogic ('(' : xs) 0 l t = splitArgsLogic xs 1 "(" (l : t)
splitArgsLogic (')' : xs) 1 l t = splitArgsLogic xs 0 "" ((')' : l) : t)
splitArgsLogic ('(' : xs) n l t = splitArgsLogic xs (n + 1) ('(' : l) t
splitArgsLogic (')' : xs) n l t = splitArgsLogic xs (n - 1) (')' : l) t
splitArgsLogic (x : xs) n l t = splitArgsLogic xs n (x : l) t

-- |Splits args and keeps parentheses.
-- (so like "(1 (2 3 4) 5)" => ["1", "(2 3 4)", "5"])
-- Never exepts (parent should check for bad parenthesis)
splitArgs :: String -- ^ The string to split
  -> [String] -- ^ The return value
splitArgs l = reverse (
  map reverse (filter (not . isEmpty) (splitArgsLogic l 0 "" []))
  )
