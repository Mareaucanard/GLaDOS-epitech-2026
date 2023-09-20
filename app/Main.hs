module Main (main) where

import Ast
import Data.Char (toLower)
import Lib (parseString)

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

processInput :: IO String
processInput = do
  line <- parseLine 0
  return $ map toLower line

handleLine :: String -> VarMap -> IO ()
handleLine line m = case parseString line of
  Right x -> putStrLn x
  Left sexpr -> case sexprToAST sexpr of
    Right msg -> putStrLn msg
    Left ast -> case evalAst ast m of
      Right err -> putStrLn err
      Left (result, newVars) -> print result >> loop newVars

-- This is our main loop, it handles when to exit
loop :: VarMap -> IO ()
loop m = do
  line <- processInput
  case line of
    "quit" -> return ()
    _ -> handleLine line m

main :: IO ()
main = do
  putStrLn "Welcome to the LISP interpreter"
  loop defaultSymbols
