-- |
-- = EPITECH PROJECT, 2023
-- glados
--
-- = File description:
-- Interpreter

module Interpreter (parseLine) where

-- | Counts parenthesis in a string
countParenthesis :: String -- ^ The string to count
  -> Int -- ^ The number of parenthesis allready counted (for recursion, should be 0 at first)
  -> Int -- ^ The number of parenthesis in total
countParenthesis [] n = n -- If the string is empty, return the number of parenthesis
countParenthesis ('(':xs) n = countParenthesis xs (n + 1) -- If the first char is a '(', add 1 to the number of parenthesis
countParenthesis (')':xs) n = countParenthesis xs (n - 1) -- If the first char is a ')', remove 1 to the number of parenthesis
countParenthesis (x:xs) n = countParenthesis xs n -- If the first char is not a parenthesis, call the function again with the tail of the string

-- | Concatenates a string and an IO String into an IO String
concatStringIOString :: String -- ^ The first string
  -> IO String -- ^ The second string
  -> IO String -- ^ The concatenated string
concatStringIOString s1 io = io >>= (\s2 -> return (s1 ++ " " ++ s2)) -- Concatenates the two strings

-- | Parsline function logic
parseLineLogic :: Int
  -> String
  -> IO String
parseLineLogic n line =
  if nbPar == 0
    then return line
    else concatStringIOString line (parseLine nbPar)
  where nbPar = countParenthesis line n

-- | Parses a line
parseLine :: Int -> 
  IO String
parseLine n = getLine >>= (\line -> parseLineLogic n line)
