{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Interpreter
--}

module Interpreter (parseLine) where
import System.IO


countParenthesis :: String -> Int -> Int
countParenthesis [] n = n
countParenthesis ('(':xs) n = countParenthesis xs (n + 1)
countParenthesis (')':xs) n = countParenthesis xs (n - 1)
countParenthesis (_:xs) n = countParenthesis xs n

concatStringIOString :: String -> IO String -> IO String
concatStringIOString s1 io = io >>= (\s2 -> return (s1 ++ " " ++ s2))

parseLineLogic :: Int -> String -> IO String
parseLineLogic n line =
  if nbPar == 0
    then return line
    else concatStringIOString line (parseLine nbPar)
  where nbPar = countParenthesis line n

parseLine :: Int -> IO String
parseLine n = do
  isClosed <- isEOF
  if isClosed 
    then  putStrLn "" >> return "quit"
    else getLine >>= (\line -> parseLineLogic n line)
