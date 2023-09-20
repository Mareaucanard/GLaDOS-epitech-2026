module Lib (SExpr (..), getSymbol, getInteger, getList, printTree, parseString, myMaybeMap) where

import Data.Char (isNumber, isSpace, toUpper)
import Data.List (dropWhileEnd)
import Text.Read (readMaybe)

data SExpr
  = Integer Int
  | Symbol String
  | List [SExpr]
  | Boolan Bool
  deriving (Show, Read)

getSymbol :: SExpr -> Maybe String
getSymbol (Symbol s) = Just s
getSymbol _ = Nothing

getInteger :: SExpr -> Maybe Int
getInteger (Integer i) = Just i
getInteger _ = Nothing

getList :: SExpr -> Maybe [SExpr]
getList (List l) = Just l
getList _ = Nothing

handleListItem :: SExpr -> String
handleListItem (List l) = "(" ++ actualPrintTree (List l) ++ ")"
handleListItem e = actualPrintTree e

handleList :: [SExpr] -> Int -> String
handleList [] 0 = "an empty list"
handleList (x : xs) 0 = "a list with " ++ handleListItem x ++ handleList xs 1
handleList [x] _ = " and " ++ handleListItem x
handleList (x : xs) n = ", " ++ handleListItem x ++ handleList xs (n + 1)
handleList [] _ = ""

actualPrintTree :: SExpr -> String
actualPrintTree (Integer i) = "an integer " ++ show i
actualPrintTree (Symbol s) = "a symbol " ++ show s
actualPrintTree (List l) = handleList l 0

printTree :: SExpr -> String
printTree tree = case actualPrintTree tree of
  [] -> []
  (x : xs) -> toUpper x : xs

addMaybe :: a -> Either [a] b -> Either [a] b
addMaybe _ (Right msg) = Right msg
addMaybe x (Left xs) = Left (x : xs)

isEmpty :: String -> Bool
isEmpty [] = True
isEmpty (x : xs)
  | x == ' ' = isEmpty xs
  | otherwise = False

stripParenthesis :: String -> Int -> Either String String
stripParenthesis ('(' : xs) 0 = stripParenthesis xs 1
stripParenthesis (')' : xs) 1 = if isEmpty xs then Left "" else Right "Something is after a parenthesis"
stripParenthesis ('(' : xs) n = addMaybe '(' (stripParenthesis xs (n + 1))
stripParenthesis (')' : xs) n = addMaybe ')' (stripParenthesis xs (n - 1))
stripParenthesis (x : xs) n = addMaybe x (stripParenthesis xs n)
stripParenthesis [] _ = Right "Unmatched parenthesis"

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

trim :: [Char] -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (x ==)

parseSymbol :: String -> Either SExpr String
parseSymbol x
  | nb == 0 = Left $ Symbol trimmed
  | otherwise = Right "Symbol can't have spaces, did you try to make a list?"
  where
    trimmed = trim x
    nb = count ' ' trimmed

parseBool :: String -> Either SExpr String
parseBool ['t'] = Left $ Boolan True
parseBool ['f'] = Left $ Boolan False
parseBool _ = Right "Boolean not empty or not recognized"

parseString :: String -> Either SExpr String
parseString (' ' : xs) = parseString xs
parseString ('\n' : xs) = parseString xs
parseString ('(' : xs) = parseList ('(' : xs)
parseString ('#' : xs) = parseBool (trim xs)
parseString (x : xs) = if isNumber x then parseNumber (x : xs) else parseSymbol (x : xs)
parseString [] = Right "Can't evaluate empty string"

splitArgsLogic :: String -> Int -> String -> [String] -> [String]
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

splitArgs :: String -> [String]
splitArgs l = reverse (map reverse (filter (not . isEmpty) (splitArgsLogic l 0 "" [])))
