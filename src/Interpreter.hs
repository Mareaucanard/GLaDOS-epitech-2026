{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Interpreter
--}

module Interpreter (parseLineFromFile) where
import System.IO

countParenthesis :: String -> Int -> Int
countParenthesis [] n = n
countParenthesis ('(':xs) n = countParenthesis xs (n + 1)
countParenthesis (')':xs) n = countParenthesis xs (n - 1)
countParenthesis (_:xs) n = countParenthesis xs n

concatStringIOString :: String -> IO String -> IO String
concatStringIOString s1 io = io >>= (\s2 -> return (s1 ++ " " ++ s2))

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

