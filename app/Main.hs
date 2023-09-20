module Main (main) where

import Ast
import Data.Char (toLower)
import Lib (parseString)
import DefaultSymbol (defaultSymbols)

processInput :: IO String
processInput = do
  line <- getLine
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
