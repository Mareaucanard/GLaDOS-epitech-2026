module Interpreter (parseLine) where

countParenthesis :: String -> Int -> Int
countParenthesis [] n = n
countParenthesis (x : xs) n
  | x == '(' = countParenthesis xs (n + 1)
  | x == ')' = countParenthesis xs (n - 1)
  | otherwise = countParenthesis xs n

concatStringIOString :: String -> IO String -> IO String
concatStringIOString s1 io = io >>= (\s2 -> return (s1 ++ " " ++ s2))

parseLineLogic :: Int -> String -> IO String
parseLineLogic n line = if nbPar == 0 then return line else concatStringIOString line (parseLine nbPar)
  where nbPar = countParenthesis line n

parseLine :: Int -> IO String
parseLine n = getLine >>= (\line -> parseLineLogic n line)