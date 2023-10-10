-- |
-- = EPITECH PROJECT, 2023
-- glados
--
-- = File description:
-- Interpreter

module Interpreter (parseLineFromFile) where
import System.IO

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
parseLineFileLogic :: Int -> Handle -> String -> IO String
parseLineFileLogic n file line =
  if nbPar == 0
    then return line
    else concatStringIOString line (parseLineFromFile nbPar file)
  where nbPar = countParenthesis line n

parseLineFromFile :: Int -> Handle -> IO String
parseLineFromFile n file = do
  isClosed <- hIsEOF file
  if isClosed
    then return "quit"
    else hGetLine file >>= (\line -> parseLineFileLogic n file line)
    